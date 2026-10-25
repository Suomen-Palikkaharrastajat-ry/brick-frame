module Gear.Phase exposing
    ( Constraint
    , Correction
    , Frame
    , Side(..)
    , contactPoint
    , gearFrame
    , groupCorrection
    , meshConstraints
    , meshCorrection
    , phaseOf
    , residual
    , toothFraction
    )

{-| Tooth phase of gear meshes: where the teeth of a gear sit relative to a
contact point, how far a mesh is from tooth-to-gap alignment, and the rigid
rotation about an axle that closes that gap.


## Angle convention

Every gear has a rotation axis `n` (its local +Z mapped through `worldMatrix`),
a reference direction `e1` (its local +X) and `e2 = n × e1`. The azimuth of a
world point `P` seen from the gear centre `C` is

    beta(P) = atan2 (w·e2) (w·e1),    w = (P − C) with its n-component removed

Tooth `k` of a gear whose tooth-phase is `phi0` (see `phaseOf`) sits at
`phi0 + 2·pi·k/N`, measured from `e1` about `+n`.

At a contact `P` a gear presents either a **tooth** (a tooth centred on
`beta`) or a **gap** (a gap centred on `beta`, i.e. a tooth centred on
`beta + pi/N`). The signed **residual** is the offset of the nearest tooth
from that target:

    residual = wrap (phi0 − target) (2·pi/N)     target = beta | beta + pi/N

with `wrap` mapping into `[-pitch/2, pitch/2)`. The residual is reported in
radians; `toothFraction` divides by the pitch, so `0.5` means half a tooth off.

Rotating a gear by `d` about `+n` (`Mat4.makeRotate d n`, right-handed in the
frame of its `worldMatrix`) changes its residual by `+d`, so the correction
that aligns it is `d = −residual`. Gears on one rigid axle all receive the
same `d`.

A mesh has two options: the fixed gear shows a tooth and the moving gear a
gap, or the reverse. Worms are not aligned (`Nothing`). Only rotation of
the moving group is considered; fixed gears are never moved.

-}

import Dict exposing (Dict)
import Gear.Detect exposing (canonicalAxis, isBevelLike, isCrownLike, isWorm)
import Gear.Types exposing (GearInstance, GearSpec)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)


{-| The local frame of a gear in world space.

  - `centre` — axle centre
  - `axis` — unit rotation axis (local +Z)
  - `e1` — unit reference direction (local +X, perpendicular to `axis`)
  - `e2` — `axis × e1`

-}
type alias Frame =
    { centre : Vec3
    , axis : Vec3
    , e1 : Vec3
    , e2 : Vec3
    }


{-| Which feature of a gear faces the contact point.
-}
type Side
    = Tooth
    | Gap


{-| One mesh option for a moving gear, seen from the group that rotates it.

  - `movingResidual` — residual of the moving gear for this option (radians)
  - `movingPitch` — circular pitch of the moving gear (`2·pi/N`, radians)
  - `fixedFraction` — tooth fraction left on the fixed partner for this
    option; rotating the moving group cannot change it

-}
type alias Constraint =
    { movingResidual : Float
    , movingPitch : Float
    , fixedFraction : Float
    }


{-| Result of a group rotation.

  - `radians` — rotation `d` about the group's `+axis` (right-handed)
  - `worstFraction` — largest remaining tooth-fraction misalignment on the
    moving side, over all meshes, after the rotation (0 = every moving gear
    shows a gap at its contact)
  - `fixedFloor` — largest tooth fraction on the fixed partners of the chosen
    options. Rotating the group cannot change it, so it is reported only and
    never traded against `worstFraction`

-}
type alias Correction =
    { radians : Float
    , worstFraction : Float
    , fixedFloor : Float
    }


eps : Float
eps =
    1.0e-9



-- ── Tooth phase table ─────────────────────────────────────────────────────────


{-| Tooth phase φ0 (radians from local +X to tooth 0), per part file. Any
spur, crown or bevel part not listed defaults to 0.
-}
phi0Table : Dict String Float
phi0Table =
    -- 32198 bevel measured at about +0.8°, not trusted: treated as 0.
    Dict.fromList [ ( "32198.dat", 0.0 ) ]


{-| Worm parts: their thread phase is unresolved, so they are never aligned.
-}
wormParts : List String
wormParts =
    [ "4716.dat", "32905.dat" ]


{-| Tooth phase φ0 for a gear part, or `Nothing` for worms (no alignment).
-}
phaseOf : GearSpec -> Maybe Float
phaseOf spec =
    if isWorm spec || List.member spec.partFile wormParts then
        Nothing

    else
        Just (Dict.get spec.partFile phi0Table |> Maybe.withDefault 0.0)



-- ── Frames and contact points ─────────────────────────────────────────────────


{-| The world frame of a gear, from its `worldMatrix`.
-}
gearFrame : GearInstance -> Frame
gearFrame gear =
    let
        m =
            gear.worldMatrix

        origin =
            Mat4.transform m (Vec3.vec3 0 0 0)

        rawAxis =
            Vec3.sub (Mat4.transform m (Vec3.vec3 0 0 1)) origin

        rawX =
            Vec3.sub (Mat4.transform m (Vec3.vec3 1 0 0)) origin

        axis =
            if Vec3.length rawAxis < eps then
                Vec3.vec3 0 0 1

            else
                Vec3.normalize rawAxis

        xPerp =
            Vec3.sub rawX (Vec3.scale (Vec3.dot rawX axis) axis)

        e1 =
            if Vec3.length xPerp < eps then
                anyPerpendicular axis

            else
                Vec3.normalize xPerp
    in
    { centre = origin
    , axis = axis
    , e1 = e1
    , e2 = Vec3.cross axis e1
    }


anyPerpendicular : Vec3 -> Vec3
anyPerpendicular axis =
    if abs (Vec3.getX axis) < 0.9 then
        Vec3.normalize (Vec3.cross axis (Vec3.vec3 1 0 0))

    else
        Vec3.normalize (Vec3.cross axis (Vec3.vec3 0 1 0))


{-| The pitch point `P` where two meshing gears touch.

  - Spur pairs (parallel axes): the point on the centre line, at the pitch
    radius of the first gear.
  - Crown or bevel pairs (perpendicular axes): the point on the crown's pitch
    circle nearest the other gear's centre. If that centre lies on the crown
    axis, the point lies along the other gear's axis (sign canonicalised).

Worms have no contact point (`Nothing`), as do coincident centres.

-}
contactPoint : GearInstance -> GearInstance -> Maybe Vec3
contactPoint a b =
    if isWorm a.spec || isWorm b.spec then
        Nothing

    else
        let
            fa =
                gearFrame a

            fb =
                gearFrame b
        in
        if isPlaneGear a.spec then
            rimContact fa a.spec.pitchRadius fb

        else if isPlaneGear b.spec then
            rimContact fb b.spec.pitchRadius fa

        else
            parallelContact fa a.spec.pitchRadius fb


isPlaneGear : GearSpec -> Bool
isPlaneGear spec =
    isCrownLike spec || isBevelLike spec


parallelContact : Frame -> Float -> Frame -> Maybe Vec3
parallelContact fa radiusA fb =
    let
        d =
            Vec3.sub fb.centre fa.centre

        len =
            Vec3.length d
    in
    if len < eps then
        Nothing

    else
        Just (Vec3.add fa.centre (Vec3.scale (radiusA / len) d))


rimContact : Frame -> Float -> Frame -> Maybe Vec3
rimContact rim radius other =
    let
        inPlane v =
            Vec3.sub v (Vec3.scale (Vec3.dot v rim.axis) rim.axis)

        towardOther =
            inPlane (Vec3.sub other.centre rim.centre)

        direction =
            if Vec3.length towardOther > eps then
                towardOther

            else
                canonicalAxis (inPlane other.axis)
    in
    if Vec3.length direction < eps then
        Nothing

    else
        Just (Vec3.add rim.centre (Vec3.scale (radius / Vec3.length direction) direction))



-- ── Residuals ─────────────────────────────────────────────────────────────────


{-| Signed residual (radians, in `[-pitch/2, pitch/2)`) of a gear presenting
`side` at the contact point `p`. `Nothing` for worms.
-}
residual : GearInstance -> Vec3 -> Side -> Maybe Float
residual gear p side =
    phaseOf gear.spec
        |> Maybe.map
            (\phi0 ->
                let
                    pitch =
                        toothPitch gear.spec.teeth

                    beta =
                        azimuth (gearFrame gear) p

                    target =
                        case side of
                            Tooth ->
                                beta

                            Gap ->
                                beta + pitch / 2
                in
                wrapAngle pitch (phi0 - target)
            )


{-| The residual as a fraction of one tooth pitch: `0` aligned, `0.5` half a
tooth off. `Nothing` for worms.
-}
toothFraction : GearInstance -> Vec3 -> Side -> Maybe Float
toothFraction gear p side =
    residual gear p side
        |> Maybe.map (\r -> abs r / toothPitch gear.spec.teeth)


azimuth : Frame -> Vec3 -> Float
azimuth frame p =
    let
        w0 =
            Vec3.sub p frame.centre

        w =
            Vec3.sub w0 (Vec3.scale (Vec3.dot w0 frame.axis) frame.axis)
    in
    atan2 (Vec3.dot w frame.e2) (Vec3.dot w frame.e1)


toothPitch : Int -> Float
toothPitch teeth =
    2 * pi / toFloat teeth


wrapAngle : Float -> Float -> Float
wrapAngle pitch x =
    x - pitch * toFloat (round (x / pitch))



-- ── Mesh and group corrections ────────────────────────────────────────────────


{-| The two mesh options (fixed gear tooth / moving gear gap, and fixed gear
gap / moving gear tooth) for one mesh. `Nothing` when the pair has no
contact point (worms, coincident centres).
-}
meshConstraints : { fixed : GearInstance, moving : GearInstance } -> Maybe (List Constraint)
meshConstraints pair =
    contactPoint pair.fixed pair.moving
        |> Maybe.andThen
            (\p ->
                let
                    option fixedSide movingSide =
                        Maybe.map2
                            (\fixedResidual movingResidual ->
                                { movingResidual = movingResidual
                                , movingPitch = toothPitch pair.moving.spec.teeth
                                , fixedFraction = abs fixedResidual / toothPitch pair.fixed.spec.teeth
                                }
                            )
                            (residual pair.fixed p fixedSide)
                            (residual pair.moving p movingSide)

                    options =
                        List.filterMap identity
                            [ option Tooth Gap
                            , option Gap Tooth
                            ]
                in
                if List.isEmpty options then
                    Nothing

                else
                    Just options
            )


{-| The rotation of a moving gear that best aligns it with a fixed partner.
The option with the smaller fixed-side misalignment is chosen. `Nothing`
when `meshConstraints` is.
-}
meshCorrection : { fixed : GearInstance, moving : GearInstance } -> Maybe Correction
meshCorrection pair =
    meshConstraints pair
        |> Maybe.map (\options -> groupCorrection [ options ])


{-| Minimax rotation for a rigid group that touches several meshes.

Each element of the list is one mesh and its options (see `meshConstraints`).
Every combination of options is tried. For each, the rotation is chosen from
the candidates (zero, each mesh's exact closing rotation, and midpoints
between pairs of them) to minimise the worst moving-side tooth fraction.

A fixed partner's offset cannot be changed by rotating the group, so it is
not traded against the rotation: it is reported as `fixedFloor`. It still
decides between option combinations, which are ranked by the larger of the
floor and the moving-side residual; ties prefer the smaller rotation, then
the smaller floor. The search is exponential in the number of meshes, which
is small for real gear groups.

An empty list needs no rotation.

-}
groupCorrection : List (List Constraint) -> Correction
groupCorrection meshes =
    case List.map bestRotation (combinations meshes) of
        first :: rest ->
            List.foldl
                (\c best ->
                    if compareCorrection c best == LT then
                        c

                    else
                        best
                )
                first
                rest

        [] ->
            { radians = 0, worstFraction = 0, fixedFloor = 0 }


{-| The best rotation for one choice of options: the moving-side minimum over
the candidates, ties to the smaller rotation. Its fixed floor is fixed by the
choice, not by the rotation.
-}
bestRotation : List Constraint -> Correction
bestRotation chosen =
    let
        floor =
            fixedFloorOf chosen

        scored delta =
            { radians = delta
            , worstFraction = worstAfter delta chosen
            , fixedFloor = floor
            }
    in
    case List.map scored (rotationCandidates chosen) of
        first :: rest ->
            List.foldl
                (\c best ->
                    if compare ( c.worstFraction, abs c.radians ) ( best.worstFraction, abs best.radians ) == LT then
                        c

                    else
                        best
                )
                first
                rest

        [] ->
            scored 0


{-| Orders corrections by what the caller sees: the worst tooth fraction of
any mesh (fixed floor or moving side), then the rotation, then the floor.
Fixed partners decide which option is chosen; the moving side decides the
rotation.
-}
compareCorrection : Correction -> Correction -> Order
compareCorrection a b =
    compare
        ( max a.fixedFloor a.worstFraction, abs a.radians, a.fixedFloor )
        ( max b.fixedFloor b.worstFraction, abs b.radians, b.fixedFloor )


combinations : List (List a) -> List (List a)
combinations =
    List.foldr
        (\options acc ->
            List.concatMap (\o -> List.map (\rest -> o :: rest) acc) options
        )
        [ [] ]


rotationCandidates : List Constraint -> List Float
rotationCandidates chosen =
    let
        zero =
            [ 0.0 ]

        exact =
            List.map (\c -> negate c.movingResidual) chosen

        midpoints =
            List.concatMap
                (\a ->
                    List.concatMap
                        (\b ->
                            let
                                mid =
                                    negate (a.movingResidual + b.movingResidual) / 2
                            in
                            [ mid, mid + a.movingPitch / 2, mid + b.movingPitch / 2 ]
                        )
                        chosen
                )
                chosen
    in
    zero ++ exact ++ midpoints


{-| Worst moving-side tooth fraction after rotating the group by `delta`.
Only the moving gears count: a fixed partner's offset is the same for every
`delta`.
-}
worstAfter : Float -> List Constraint -> Float
worstAfter delta chosen =
    List.foldl
        (\c acc ->
            max acc (abs (wrapAngle c.movingPitch (c.movingResidual + delta)) / c.movingPitch)
        )
        0
        chosen


fixedFloorOf : List Constraint -> Float
fixedFloorOf chosen =
    List.foldl (\c acc -> max acc c.fixedFraction) 0 chosen
