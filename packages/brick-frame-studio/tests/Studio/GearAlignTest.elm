module Studio.GearAlignTest exposing (suite)

{-| Gear alignment in `Studio.Core`: a committed add that meshes a gear turns the
gear's axis line so the teeth interlock, as one history step.

Residuals come from `Gear.Phase.meshConstraints`: the worst tooth fraction over
the two mesh options, `0` when the teeth interlock and `0.5` when one tooth
sits on another.

-}

import Data
import Expect
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Types exposing (GearInstance)
import LDraw.Document as Document
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3)
import Studio.Core as Core
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Gear alignment (Studio.Core)"
        [ describe "Off by default"
            [ test "a 24T added at mesh distance, tooth on tooth, is left as it was put" <|
                \_ ->
                    startModel False
                        |> addPart "3648.dat" Mat4.identity
                        |> addPart "3648.dat" (gearAt 60)
                        |> meshWorst
                        |> Maybe.map (\v -> Expect.within (Expect.Absolute 1.0e-6) 0.5 v)
                        |> Maybe.withDefault (Expect.fail "no mesh constraints")
            ]
        , describe "Alignment on"
            [ test "a 24T dropped at mesh distance, tooth on tooth, turns until the teeth interlock" <|
                \_ ->
                    startModel True
                        |> addPart "3648.dat" Mat4.identity
                        |> addPart "3648.dat" (gearAt 60)
                        |> meshWorst
                        |> Maybe.map (\v -> Expect.within (Expect.Absolute 1.0e-6) 0 v)
                        |> Maybe.withDefault (Expect.fail "no mesh constraints")
            , test "the turn is one undo step with the add: one undo removes the gear and its turn" <|
                \_ ->
                    let
                        withGear =
                            startModel True
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3648.dat" (gearAt 60)

                        once =
                            Core.undo withGear |> (\( m, _, _ ) -> m)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 (List.length (Core.getPlacements withGear))
                        , \_ -> Expect.equal 1 (List.length (Core.getPlacements once))
                        , \_ -> Expect.equal True (Core.canUndo once)
                        ]
                        ()
            , test "an axle on the gear's axle line turns with the gear" <|
                \_ ->
                    let
                        axleMatrix =
                            Mat4.mul (Mat4.makeTranslate3 60 0 0) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                        before =
                            startModel True
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3706.dat" axleMatrix

                        after =
                            before
                                |> addPart "3648.dat" (gearAt 60)

                        placements =
                            Core.getPlacements after
                    in
                    case placements of
                        [ _, axle, gear ] ->
                            Expect.all
                                [ \_ -> Expect.equal True (not (approxMatrix gear.matrix (gearAt 60)))
                                , \_ -> Expect.equal True (sameMotion ( gearAt 60, gear.matrix ) ( axleMatrix, axle.matrix ))
                                ]
                                ()

                        _ ->
                            Expect.fail "expected a gear, an axle and a gear"
            , test "a flipped coaxial 24T on the same axle closes its mesh and the first gear's mesh stays closed" <|
                \_ ->
                    -- A (identity) on the axle at x = 60 meshes F1 at the origin; F1 is
                    -- turned so its tooth faces A. A is added with alignment off, so it
                    -- is 2.5 degrees out. B, the same 24T turned over about X (axis -Z),
                    -- sits 40 LDU up the same axle and meshes F2. A 3706 axle spans both
                    -- so they form one rigid group. F2 is turned so its tooth faces B.
                    -- Both meshes need the same group turn of -2.5 degrees, and B's sign
                    -- only comes out right when its reversed axis is taken into account.
                    let
                        psi =
                            degrees 20

                        dir =
                            vec3 (cos psi) (sin psi) 0

                        centre =
                            vec3 60 0 0

                        -- A partner gear one pitch-diameter away along `dir`, turned so
                        -- its tooth faces back at the mesh point.
                        partnerAt origin =
                            Mat4.mul (Mat4.makeTranslate origin)
                                (Mat4.makeRotate (psi + pi) (vec3 0 0 1))

                        f1Pos =
                            Vec3.add centre (Vec3.scale 60 dir)

                        bPos =
                            vec3 60 0 40

                        bridge =
                            Mat4.mul (Mat4.makeTranslate (vec3 60 0 20)) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                        f2Pos =
                            Vec3.add bPos (Vec3.scale 60 dir)

                        flipped =
                            Mat4.mul (Mat4.makeTranslate bPos) (Mat4.makeRotate pi (vec3 1 0 0))

                        model =
                            startModel False
                                |> addPart "3648.dat" (partnerAt f1Pos)
                                |> addPart "3648.dat" (Mat4.makeTranslate centre)
                                |> addPart "3706.dat" bridge
                                |> addPart "3648.dat" (partnerAt f2Pos)
                                |> Core.setGearAlignment True
                                |> addPart "3648.dat" flipped
                    in
                    Expect.all
                        [ \_ -> Expect.lessThan 1.0e-3 (pairWorst model f1Pos centre)
                        , \_ -> Expect.lessThan 1.0e-3 (pairWorst model f2Pos bPos)
                        ]
                        ()
            , test "an axle on the same line with a gap from the gear does not turn" <|
                \_ ->
                    let
                        axleAt z =
                            Mat4.mul (Mat4.makeTranslate3 60 0 z) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))

                        axleNear =
                            axleAt 0

                        axleFar =
                            axleAt 300

                        model =
                            startModel True
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3706.dat" axleNear
                                |> addPart "3706.dat" axleFar
                                |> addPart "3648.dat" (gearAt 60)
                    in
                    case Core.getPlacements model of
                        [ _, near, far, gear ] ->
                            Expect.all
                                [ \_ -> Expect.equal True (not (approxMatrix gear.matrix (gearAt 60)))
                                , \_ -> Expect.equal True (sameMotion ( gearAt 60, gear.matrix ) ( axleNear, near.matrix ))
                                , \_ -> Expect.equal True (approxMatrix far.matrix axleFar)
                                ]
                                ()

                        _ ->
                            Expect.fail "expected a gear, two axles and a gear"
            , test "moving a part that is not a gear or axle does not turn the gears" <|
                \_ ->
                    let
                        model =
                            startModel False
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3648.dat" (gearAt 60)
                                |> Core.setGearAlignment True
                                |> addPart "3001.dat" (Mat4.makeTranslate3 0 0 100)
                    in
                    case Core.getPlacements model of
                        [ _, gear, _ ] ->
                            Expect.equal True (approxMatrix gear.matrix (gearAt 60))

                        _ ->
                            Expect.fail "expected two gears and a brick"
            , test "a gear within half a degree of interlocking is not turned" <|
                \_ ->
                    let
                        nearly =
                            Mat4.mul (gearAt 60) (Mat4.makeRotate (degrees 7.7) (vec3 0 0 1))

                        model =
                            startModel True
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3648.dat" nearly
                    in
                    case Core.getPlacements model of
                        [ _, gear ] ->
                            Expect.equal True (approxMatrix gear.matrix nearly)

                        _ ->
                            Expect.fail "expected two gears"
            , test "a gear that does not mesh anything new is not turned" <|
                \_ ->
                    let
                        far =
                            gearAt 200

                        model =
                            startModel True
                                |> addPart "3648.dat" Mat4.identity
                                |> addPart "3648.dat" far
                    in
                    case Core.getPlacements model of
                        [ _, gear ] ->
                            Expect.equal True (approxMatrix gear.matrix far)

                        _ ->
                            Expect.fail "expected two gears"
            ]
        ]



-- ── Helpers ───────────────────────────────────────────────────────────────────


startModel : Bool -> Core.Model
startModel alignment =
    let
        config =
            { ldrawBase = "/ldraw"
            , ldrawFallbackBase = ""
            , defaultModelUrl = ""
            , initialHash = ""
            , supportsPointerEvents = True
            , document = Document.empty
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( model, _ ) =
            Core.init config
    in
    Core.setGearAlignment alignment model


{-| A 24T on the Z axis, its centre `x` along X.
-}
gearAt : Float -> Mat4
gearAt x =
    Mat4.makeTranslate3 x 0 0


addPart : String -> Mat4 -> Core.Model -> Core.Model
addPart file matrix model =
    Core.addPartAt file 14 matrix model |> (\( m, _, _ ) -> m)


{-| The worst tooth fraction of the two-gear model, over both mesh options.
-}
meshWorst : Core.Model -> Maybe Float
meshWorst model =
    case gearsOf model of
        [ fixed, moving ] ->
            Phase.meshConstraints { fixed = fixed, moving = moving }
                |> Maybe.andThen
                    (\options ->
                        options
                            |> List.map (\c -> max c.fixedFraction (abs c.movingResidual / c.movingPitch))
                            |> List.minimum
                    )

        _ ->
            Nothing


{-| The worst-option tooth fraction of the mesh between the gears nearest to
two points (`fixedAt` is the fixed gear, `movingAt` the moving one).
-}
pairWorst : Core.Model -> Vec3.Vec3 -> Vec3.Vec3 -> Float
pairWorst model fixedAt movingAt =
    let
        gears =
            gearsOf model

        nearest point =
            List.head
                (List.sortBy (\g -> Vec3.distance (Phase.gearFrame g).centre point) gears)
    in
    case ( nearest fixedAt, nearest movingAt ) of
        ( Just fixed, Just moving ) ->
            Phase.meshConstraints { fixed = fixed, moving = moving }
                |> Maybe.andThen
                    (\options ->
                        options
                            |> List.map (\c -> max c.fixedFraction (abs c.movingResidual / c.movingPitch))
                            |> List.minimum
                    )
                |> Maybe.withDefault 1

        _ ->
            1


gearsOf : Core.Model -> List GearInstance
gearsOf model =
    Detect.extractGears Data.gearParts (Document.toLines model.document) model.viewport.partCache


approxMatrix : Mat4 -> Mat4 -> Bool
approxMatrix a b =
    let
        points =
            [ vec3 1 2 3, vec3 -5 4 7, vec3 0 0 10 ]
    in
    List.all (\p -> closeVec (Mat4.transform a p) (Mat4.transform b p)) points


{-| Two placements move the same way: applying `after` to `before`'s inverse
gives the same map for both.
-}
sameMotion : ( Mat4, Mat4 ) -> ( Mat4, Mat4 ) -> Bool
sameMotion ( gearBefore, gearAfter ) ( axleBefore, axleAfter ) =
    case ( Mat4.inverse gearBefore, Mat4.inverse axleBefore ) of
        ( Just gi, Just ai ) ->
            List.all
                (\p ->
                    closeVec
                        (Mat4.transform gearAfter (Mat4.transform gi p))
                        (Mat4.transform axleAfter (Mat4.transform ai p))
                )
                [ vec3 1 2 3, vec3 -5 4 7, vec3 0 0 10 ]

        _ ->
            False


closeVec : Vec3.Vec3 -> Vec3.Vec3 -> Bool
closeVec a b =
    Vec3.distance a b < 1.0e-3
