module Gear.PhaseTest exposing (suite)

{-| Unit tests for tooth phase, mesh contact points, mesh and group
corrections, and rolling consistency of the signed gear ratios.

Geometry uses pitch 1 (`pitchRadius = teeth / (2·pi)`), so the centre distance
of a spur mesh is the sum of the two pitch radii.

-}

import Array
import Dict
import Expect exposing (Expectation)
import Gear.Phase as Phase exposing (Constraint, Side(..))
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearInstance, GearSpec)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (Test, describe, test)



-- ── Helpers ───────────────────────────────────────────────────────────────────


spurSpec : Int -> GearSpec
spurSpec teeth =
    { partFile = spurPartFile teeth
    , teeth = teeth
    , pitchRadius = toFloat teeth / (2 * pi)
    }


spurPartFile : Int -> String
spurPartFile teeth =
    case teeth of
        8 ->
            "3647.dat"

        16 ->
            "4019.dat"

        40 ->
            "3649.dat"

        _ ->
            "3648.dat"


{-| The 16T spur of a crown mesh: its axis lies along ±X (sign `ss`), its
centre at `(0, 0, 16 / 2pi)` above the crown plane, rolled by `roll`.
-}
crownSpur : Int -> Float -> Mat4
crownSpur ss roll =
    Mat4.mul (Mat4.makeTranslate3 0 0 (16 / (2 * pi)))
        (Mat4.mul (Mat4.makeRotate (toFloat ss * pi / 2) (vec3 0 1 0))
            (Mat4.makeRotate roll (vec3 0 0 1))
        )


crownSpec : GearSpec
crownSpec =
    { partFile = "3650b.dat", teeth = 24, pitchRadius = 24 / (2 * pi) }


wormSpec : GearSpec
wormSpec =
    { partFile = "4716.dat", teeth = 1, pitchRadius = 1 / (2 * pi) }


{-| World pose: translate to (x, y, z), optionally flip the axis (rotate pi
about X, so local +Z points along −Z), then roll about the local Z by `roll`.
-}
pose : Float -> Float -> Float -> Bool -> Float -> Mat4
pose x y z flipped roll =
    let
        flip =
            if flipped then
                Mat4.makeRotate pi (vec3 1 0 0)

            else
                Mat4.identity
    in
    Mat4.mul (Mat4.makeTranslate3 x y z)
        (Mat4.mul flip (Mat4.makeRotate roll (vec3 0 0 1)))


gear : Int -> GearSpec -> Mat4 -> GearInstance
gear id spec m =
    { id = id
    , spec = spec
    , color = 16
    , worldPosition = Mat4.transform m (vec3 0 0 0)
    , worldMatrix = m
    }


{-| Rotate a gear about its own local +Z by `d` radians (right-handed).
-}
turn : Float -> GearInstance -> GearInstance
turn d g =
    { g | worldMatrix = Mat4.mul g.worldMatrix (Mat4.makeRotate d (vec3 0 0 1)) }


{-| A spur pair along world +X: the first gear at the origin, the second at
`(rA + rB, 0, 0)`, with the given flips and rolls.
-}
alongX : Int -> Int -> Bool -> Bool -> Float -> ( GearInstance, GearInstance )
alongX na nb flipA flipB roll =
    let
        sa =
            spurSpec na

        sb =
            spurSpec nb

        d =
            sa.pitchRadius + sb.pitchRadius
    in
    ( gear 0 sa (pose 0 0 0 flipA 0)
    , gear 1 sb (pose d 0 0 flipB roll)
    )


expectZero : Maybe Float -> Expectation
expectZero m =
    case m of
        Just r ->
            Expect.within (Expect.Absolute 1.0e-9) 0 r

        Nothing ->
            Expect.fail "expected a residual"


{-| Check that a mesh closes: the fixed gear shows a tooth at the contact,
and after the moving gear is rotated by the correction it shows a gap.
-}
expectClosed : GearInstance -> GearInstance -> Expectation
expectClosed fixed moving =
    case ( Phase.meshCorrection { fixed = fixed, moving = moving }, Phase.contactPoint fixed moving ) of
        ( Just c, Just p ) ->
            let
                moved =
                    turn c.radians moving
            in
            Expect.all
                [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.worstFraction
                , \_ -> expectZero (Phase.residual fixed p Tooth)
                , \_ -> expectZero (Phase.residual moved p Gap)
                ]
                ()

        _ ->
            Expect.fail "expected a correction and a contact point"


closesAlongX : Int -> Int -> Bool -> Float -> Expectation
closesAlongX na nb flipB roll =
    let
        ( a, b ) =
            alongX na nb False flipB roll
    in
    expectClosed a b


diagonalCase : Int -> Int -> Float -> Float -> Expectation
diagonalCase na nb phi roll =
    let
        sa =
            spurSpec na

        sb =
            spurSpec nb

        d =
            sa.pitchRadius + sb.pitchRadius

        a =
            gear 0 sa (pose 0 0 0 False phi)

        b =
            gear 1 sb (pose (d * cos phi) (d * sin phi) 0 False (phi + roll))
    in
    expectClosed a b


constraint : Float -> Float -> Constraint
constraint residualValue fixed =
    { movingResidual = residualValue
    , movingPitch = 2 * pi / 24
    , fixedFraction = fixed
    }


{-| Wrap a value in pitch units to [-0.5, 0.5).
-}
wrapUnit : Float -> Float
wrapUnit x =
    x - toFloat (round x)


{-| Rotate the moving gear so the mesh closes (the fixture's fixed gear shows
a tooth at the contact).
-}
applyCorrection : GearInstance -> GearInstance -> GearInstance
applyCorrection fixed moving =
    case Phase.meshCorrection { fixed = fixed, moving = moving } of
        Just c ->
            turn c.radians moving

        Nothing ->
            moving


contactOf : GearInstance -> GearInstance -> Vec3
contactOf a b =
    Phase.contactPoint a b |> Maybe.withDefault (vec3 0 0 0)


{-| `+1` when the two gears' surface velocities at the contact point the same
way, `-1` otherwise. Computed from geometry alone, not from `Physics`.
-}
slipSign : GearInstance -> GearInstance -> Vec3 -> Float
slipSign a b p =
    let
        fa =
            Phase.gearFrame a

        fb =
            Phase.gearFrame b

        va =
            Vec3.cross fa.axis (Vec3.sub p fa.centre)

        vb =
            Vec3.cross fb.axis (Vec3.sub p fb.centre)
    in
    if Vec3.dot va vb > 0 then
        1

    else
        -1


pairGraph : GearInstance -> GearInstance -> GearGraph
pairGraph a b =
    { instances = Array.fromList [ a, b ]
    , connections = Dict.fromList [ ( 0, [ 1 ] ), ( 1, [ 0 ] ) ]
    , rigidAxles = Dict.empty
    }


{-| The ratio of gear b's angle to gear a's, as computed by `Gear.Physics`.
-}
physicsRatio : GearInstance -> GearInstance -> Float
physicsRatio a b =
    Physics.propagate (pairGraph a b) 0 1
        |> Physics.angleAt
        |> (\f -> f 1)


{-| Arc invariant of a rolling mesh, in pitch units, when gear a has turned
`t` radians and gear b has turned `k·t`. Rolling without slip keeps
`S(t) − S(0)` an integer.
-}
slipInvariant : Float -> Float -> GearInstance -> GearInstance -> Vec3 -> Float -> Maybe Float
slipInvariant k t a0 b0 p sign =
    let
        a =
            turn t a0

        b =
            turn (k * t) b0
    in
    Maybe.map2
        (\ra rb ->
            toFloat a.spec.teeth / (2 * pi) * ra - sign * (toFloat b.spec.teeth / (2 * pi) * rb)
        )
        (Phase.residual a p Tooth)
        (Phase.residual b p Gap)


rollingCase : Int -> Int -> Bool -> Test
rollingCase na nb flipB =
    test
        ("no slip for "
            ++ String.fromInt na
            ++ "T-"
            ++ String.fromInt nb
            ++ "T"
            ++ (if flipB then
                    ", one gear flipped"

                else
                    ""
               )
            ++ " with the signed ratio"
        )
    <|
        \_ ->
            let
                ( a0, b0raw ) =
                    alongX na nb False flipB 0

                b0 =
                    applyCorrection a0 b0raw

                p =
                    contactOf a0 b0

                sign =
                    slipSign a0 b0 p

                k =
                    physicsRatio a0 b0
            in
            case slipInvariant k 0 a0 b0 p sign of
                Just s0 ->
                    Expect.all
                        (List.map
                            (\t () ->
                                case slipInvariant k t a0 b0 p sign of
                                    Just st ->
                                        Expect.within (Expect.Absolute 1.0e-6) 0 (wrapUnit (st - s0))

                                    Nothing ->
                                        Expect.fail "expected a residual"
                            )
                            [ 0, 0.37, 1.9 ]
                        )
                        ()

                Nothing ->
                    Expect.fail "expected a residual"



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Gear.Phase"
        [ describe "phaseOf"
            [ test "spur, crown and bevel parts default to phase 0" <|
                \_ ->
                    Expect.equal
                        [ Just 0.0, Just 0.0, Just 0.0 ]
                        [ Phase.phaseOf (spurSpec 8)
                        , Phase.phaseOf crownSpec
                        , Phase.phaseOf { partFile = "32269.dat", teeth = 20, pitchRadius = 25 }
                        ]
            , test "bevel 32198 is treated as phase 0 (measured value not trusted)" <|
                \_ ->
                    Phase.phaseOf { partFile = "32198.dat", teeth = 20, pitchRadius = 25 }
                        |> Expect.equal (Just 0.0)
            , test "worms have no phase (no alignment)" <|
                \_ ->
                    Expect.equal
                        [ Nothing, Nothing ]
                        [ Phase.phaseOf wormSpec
                        , Phase.phaseOf { partFile = "32905.dat", teeth = 1, pitchRadius = 5 }
                        ]
            ]
        , describe "contactPoint"
            [ test "spur pair: contact is at the first gear's pitch radius along the centre line" <|
                \_ ->
                    let
                        ( a, b ) =
                            alongX 8 24 False False 0
                    in
                    case Phase.contactPoint a b of
                        Just p ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 1.0e-9) (8 / (2 * pi)) (Vec3.getX p)
                                , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Vec3.getY p)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected a contact point"
            , test "crown pair: contact lies on the crown rim in the direction of the spur" <|
                \_ ->
                    let
                        crown =
                            gear 0 crownSpec (pose 0 0 0 False 0)

                        spur =
                            gear 1 (spurSpec 16) (crownSpur 1 0)
                    in
                    case Phase.contactPoint crown spur of
                        Just p ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 1.0e-9) (24 / (2 * pi)) (Vec3.getX p)
                                , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Vec3.getY p)
                                , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 (Vec3.getZ p)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected a contact point"
            , test "worm pairs have no contact point" <|
                \_ ->
                    Phase.contactPoint (gear 0 wormSpec Mat4.identity) (gear 1 (spurSpec 24) (pose 3 0 0 False 0))
                        |> Expect.equal Nothing
            ]
        , describe "residual and tooth fraction"
            [ test "a tooth centred on the contact has zero residual" <|
                \_ ->
                    let
                        ( a, _ ) =
                            alongX 8 24 False False 0
                    in
                    Phase.residual a (vec3 (8 / (2 * pi)) 0 0) Tooth
                        |> expectZero
            , test "a gap centred on the contact has zero residual for the Gap side" <|
                \_ ->
                    -- Rolling the 24T by pi/24 centres a gap on the contact.
                    Phase.residual (gear 1 (spurSpec 24) (pose 0 0 0 False (pi / 24))) (vec3 (24 / (2 * pi)) 0 0) Gap
                        |> expectZero
            , test "a gear half a tooth off reports a tooth fraction of 0.5" <|
                \_ ->
                    -- Half of the 8T pitch (pi/4) is pi/8.
                    Phase.toothFraction (gear 0 (spurSpec 8) (pose 0 0 0 False (pi / 8))) (vec3 (8 / (2 * pi)) 0 0) Tooth
                        |> Maybe.map (\f -> abs (f - 0.5))
                        |> expectZero
            , test "rolling a gear by d changes its residual by +d" <|
                \_ ->
                    let
                        p =
                            vec3 (24 / (2 * pi)) 0 0

                        r0 =
                            Phase.residual (gear 0 (spurSpec 24) (pose 0 0 0 False 0)) p Tooth

                        r1 =
                            Phase.residual (gear 0 (spurSpec 24) (pose 0 0 0 False 0.1)) p Tooth
                    in
                    case ( r0, r1 ) of
                        ( Just a, Just b ) ->
                            Expect.within (Expect.Absolute 1.0e-9) 0.1 (b - a)

                        _ ->
                            Expect.fail "expected residuals"
            , test "worms have no residual" <|
                \_ ->
                    Phase.residual (gear 0 wormSpec Mat4.identity) (vec3 1 0 0) Tooth
                        |> Expect.equal Nothing
            ]
        , describe "meshCorrection — spur pairs along X"
            [ test "8T with 24T" <|
                \_ -> closesAlongX 8 24 False 0.3
            , test "16T with 24T" <|
                \_ -> closesAlongX 16 24 False 1.1
            , test "8T with 40T" <|
                \_ -> closesAlongX 8 40 False 2.0
            , test "24T with 40T" <|
                \_ -> closesAlongX 24 40 False -0.7
            , test "16T with 16T" <|
                \_ -> closesAlongX 16 16 False 0.9
            ]
        , describe "meshCorrection — diagonal and flips"
            [ test "8T with 24T along a 45 degree diagonal" <|
                \_ -> diagonalCase 8 24 (pi / 4) 0.4
            , test "24T with 8T along a 30 degree diagonal" <|
                \_ -> diagonalCase 24 8 (pi / 6) -1.2
            , test "one gear flipped: fixed 16T flipped, moving 24T" <|
                \_ ->
                    let
                        ( a, b ) =
                            alongX 16 24 True False 0.5
                    in
                    expectClosed a b
            , test "one gear flipped: moving 8T flipped" <|
                \_ ->
                    let
                        ( a, b ) =
                            alongX 24 8 False True 0.6
                    in
                    expectClosed a b
            , test "crown 24T with spur 16T, both flips, four combinations" <|
                \_ ->
                    Expect.all
                        (List.map
                            (\( ss, sc ) () ->
                                expectClosed
                                    (gear 0 crownSpec (pose 0 0 0 (sc < 0) 0))
                                    (gear 1 (spurSpec 16) (crownSpur ss 0.25))
                            )
                            [ ( 1, 1 ), ( 1, -1 ), ( -1, 1 ), ( -1, -1 ) ]
                        )
                        ()
            , test "a worm mesh returns no correction" <|
                \_ ->
                    Phase.meshCorrection
                        { fixed = gear 0 (spurSpec 24) (pose 0 0 0 False 0)
                        , moving = gear 1 wormSpec (pose 3 0 0 False 0)
                        }
                        |> Expect.equal Nothing
            ]
        , describe "groupCorrection"
            [ test "no meshes needs no rotation" <|
                \_ ->
                    Phase.groupCorrection []
                        |> Expect.equal { radians = 0, worstFraction = 0, fixedFloor = 0 }
            , test "a single option rotates the moving gear onto its gap" <|
                \_ ->
                    let
                        c =
                            Phase.groupCorrection [ [ constraint 0.3 0.0 ] ]
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) -0.3 c.radians
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.worstFraction
                        ]
                        ()
            , test "fixed-side misalignment is reported as a floor, not traded" <|
                \_ ->
                    let
                        c =
                            Phase.groupCorrection [ [ constraint 0.0 0.5 ] ]
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) 0.5 c.fixedFloor
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.worstFraction
                        ]
                        ()
            , test "a fixed-side offset does not stop a new mesh from closing (tie-break)" <|
                \_ ->
                    -- The existing mesh's fixed partner is 0.2 tooth off, which no
                    -- rotation can change. Its moving gear is misaligned by the same
                    -- 0.15 tooth as the new mesh, so one rotation closes both. Before
                    -- the fix every rotation scored the same 0.2 and the tie-break
                    -- picked 0, leaving the new mesh open.
                    let
                        pitch =
                            2 * pi / 24

                        residual =
                            0.15 * pitch

                        c =
                            Phase.groupCorrection
                                [ [ { movingResidual = residual, movingPitch = pitch, fixedFraction = 0.2 } ]
                                , [ { movingResidual = residual, movingPitch = pitch, fixedFraction = 0 } ]
                                ]
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) -residual c.radians
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.worstFraction
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0.2 c.fixedFloor
                        ]
                        ()
            , test "the option whose fixed side is aligned wins, the moving gear turning as needed" <|
                \_ ->
                    -- Option one: fixed side 0.4 off, moving gear already aligned.
                    -- Option two: fixed side aligned, moving gear 0.1 tooth off.
                    -- The fixed side cannot be turned, so option two is taken and
                    -- the group turns to close its moving gear.
                    let
                        pitch =
                            2 * pi / 24

                        c =
                            Phase.groupCorrection
                                [ [ { movingResidual = 0, movingPitch = pitch, fixedFraction = 0.4 }
                                  , { movingResidual = 0.1 * pitch, movingPitch = pitch, fixedFraction = 0 }
                                  ]
                                ]
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-9) (-0.1 * pitch) c.radians
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.worstFraction
                        , \_ -> Expect.within (Expect.Absolute 1.0e-9) 0 c.fixedFloor
                        ]
                        ()
            ]
        , describe "rolling keeps the mesh aligned with the signed ratios"
            [ rollingCase 8 24 False
            , rollingCase 8 24 True
            , rollingCase 16 24 True
            , rollingCase 24 40 False
            , test "the same-orientation ratio drifts on a flipped pair (control)" <|
                \_ ->
                    let
                        ( a0, b0raw ) =
                            alongX 16 24 False True 0

                        b0 =
                            applyCorrection a0 b0raw

                        p =
                            contactOf a0 b0

                        sign =
                            slipSign a0 b0 p

                        naive =
                            -16 / 24

                        drift =
                            Maybe.map2 (\s0 s1 -> abs (wrapUnit (s1 - s0)))
                                (slipInvariant naive 0 a0 b0 p sign)
                                (slipInvariant naive 0.37 a0 b0 p sign)
                    in
                    case drift of
                        Just d ->
                            Expect.greaterThan 0.01 d

                        Nothing ->
                            Expect.fail "expected invariant values"
            ]
        ]
