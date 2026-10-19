module Gear.PhysicsTest exposing (suite)

{-| Unit tests for gear-ratio propagation and angle sampling.
-}

import Array
import Dict
import Expect
import Gear.Physics
    exposing
        ( KinematicLinkage
        , angleAt
        , crankSliderPosition
        , crankSliderRodAngle
        , fourBarAngles
        , propagate
        , propagateWithLinkages
        )
import Gear.Types exposing (GearGraph, GearId, GearSpec)
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)



-- ── Helpers ───────────────────────────────────────────────────────────────────


spec : Int -> Float -> GearSpec
spec teeth pitchRadius =
    { partFile = "test.dat", teeth = teeth, pitchRadius = pitchRadius }


makeInstance : GearId -> Int -> Float -> Gear.Types.GearInstance
makeInstance id teeth pr =
    { id = id
    , spec = spec teeth pr
    , color = 16
    , worldPosition = vec3 0 0 0
    , worldMatrix = Mat4.identity
    }


{-| Build a simple linear chain: gear 0 → gear 1 → gear 2.
-}
chainGraph : Int -> Int -> Int -> GearGraph
chainGraph t0 t1 t2 =
    { instances =
        Array.fromList
            [ makeInstance 0 t0 16
            , makeInstance 1 t1 24
            , makeInstance 2 t2 16
            ]
    , connections =
        Dict.fromList
            [ ( 0, [ 1 ] )
            , ( 1, [ 0, 2 ] )
            , ( 2, [ 1 ] )
            ]
    , rigidAxles = Dict.empty
    }


twoGearGraph : Int -> Int -> GearGraph
twoGearGraph t0 t1 =
    { instances =
        Array.fromList
            [ makeInstance 0 t0 16
            , makeInstance 1 t1 24
            ]
    , connections =
        Dict.fromList
            [ ( 0, [ 1 ] )
            , ( 1, [ 0 ] )
            ]
    , rigidAxles = Dict.empty
    }


isolatedGraph : GearGraph
isolatedGraph =
    { instances =
        Array.fromList
            [ makeInstance 0 8 16
            , makeInstance 1 16 24
            ]
    , connections = Dict.empty
    , rigidAxles = Dict.empty
    }



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Gear.Physics"
        [ describe "propagate — basic"
            [ test "motor gear maps to motorAngle" <|
                \_ ->
                    propagate (twoGearGraph 8 16) 0 1.0
                        |> angleAt
                        |> (\f -> f 0)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "disconnected gear is absent from result" <|
                \_ ->
                    propagate isolatedGraph 0 1.0
                        |> Dict.member 1
                        |> Expect.equal False
            , test "empty graph returns only motor gear" <|
                \_ ->
                    let
                        emptyGraph =
                            { instances = Array.fromList [ makeInstance 0 8 16 ]
                            , connections = Dict.empty
                            , rigidAxles = Dict.empty
                            }

                        result =
                            propagate emptyGraph 0 2.0
                    in
                    Expect.equal (Dict.fromList [ ( 0, 2.0 ) ]) result
            ]
        , describe "propagate — gear ratios"
            [ test "8T driving 16T: neighbour angle is -0.5 × motor" <|
                \_ ->
                    propagate (twoGearGraph 8 16) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            , test "16T driving 8T: neighbour angle is -2 × motor" <|
                \_ ->
                    propagate (twoGearGraph 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -2.0
            , test "equal gears (8T → 8T): neighbour angle is -1 × motor" <|
                \_ ->
                    propagate (twoGearGraph 8 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -1.0
            , test "worm (1T) driving 24T: neighbour angle is +1/24 × motor (no sign inversion)" <|
                \_ ->
                    propagate (twoGearGraph 1 24) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) (1 / 24)
            ]
        , describe "propagate — 3-gear chain (8T → 16T → 8T)"
            [ test "gear 0 ratio = 1.0 (motor)" <|
                \_ ->
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 0)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "gear 1 ratio = -0.5" <|
                \_ ->
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            , test "gear 2 ratio = +1.0 (two sign inversions, same tooth count as gear 0)" <|
                \_ ->
                    -- gear 1 angle = -0.5; gear 2 = -0.5 * -(16/8) = +1.0
                    -- Net: same speed as motor, positive direction (two inversions cancel)
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 2)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "all three gears are in the result" <|
                \_ ->
                    let
                        result =
                            propagate (chainGraph 8 16 8) 0 1.0
                    in
                    Expect.equal 3 (Dict.size result)
            ]
        , describe "angleAt"
            [ test "returns correct angle for present key" <|
                \_ ->
                    angleAt (Dict.fromList [ ( 0, 1.5 ) ]) 0
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.5
            , test "returns 0.0 for absent key" <|
                \_ ->
                    angleAt Dict.empty 99
                        |> Expect.within (Expect.Absolute 1.0e-6) 0.0
            ]
        , describe "propagate — rigid axle"
            [ test "gear on same axle rotates at the same angle" <|
                \_ ->
                    let
                        rigidGraph =
                            { instances =
                                Array.fromList
                                    [ makeInstance 0 8 10
                                    , makeInstance 1 8 10
                                    ]
                            , connections = Dict.empty
                            , rigidAxles =
                                Dict.fromList
                                    [ ( 0, [ 1 ] )
                                    , ( 1, [ 0 ] )
                                    ]
                            }
                    in
                    propagate rigidGraph 0 2.5
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) 2.5
            , test "rigid coupling chains through to a meshing gear" <|
                \_ ->
                    -- Gear 0 (motor, 8T) is rigid-coupled to gear 1 (8T),
                    -- and gear 1 meshes with gear 2 (16T).
                    -- Expected angle for gear 2: motor * 1 * -(8/16) = -0.5.
                    let
                        mixedGraph =
                            { instances =
                                Array.fromList
                                    [ makeInstance 0 8 10
                                    , makeInstance 1 8 10
                                    , makeInstance 2 16 20
                                    ]
                            , connections =
                                Dict.fromList
                                    [ ( 1, [ 2 ] )
                                    , ( 2, [ 1 ] )
                                    ]
                            , rigidAxles =
                                Dict.fromList
                                    [ ( 0, [ 1 ] )
                                    , ( 1, [ 0 ] )
                                    ]
                            }
                    in
                    propagate mixedGraph 0 1.0
                        |> angleAt
                        |> (\f -> f 2)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            ]
        , describe "Kinematic Linkages"
            [ test "crank-slider calculates exact position at top/bottom dead center and quadrature" <|
                \_ ->
                    let
                        r =
                            10.0

                        l =
                            30.0

                        -- At theta = 0: x = r + l = 40
                        x0 =
                            crankSliderPosition r l 0.0

                        -- At theta = pi: x = l - r = 20
                        xPi =
                            crankSliderPosition r l pi

                        -- At theta = pi/2: x = sqrt(30^2 - 10^2) = sqrt(800) ≈ 28.284
                        xQuad =
                            crankSliderPosition r l (pi / 2)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 40.0 x0
                        , \_ -> Expect.within (Expect.Absolute 0.001) 20.0 xPi
                        , \_ -> Expect.within (Expect.Absolute 0.001) (sqrt 800) xQuad
                        ]
                        ()
            , test "connecting rod angle tilts and aligns with stroke" <|
                \_ ->
                    let
                        r =
                            10.0

                        l =
                            20.0

                        phi0 =
                            crankSliderRodAngle r l 0.0

                        phiQuad =
                            crankSliderRodAngle r l (pi / 2)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0.0 phi0
                        , \_ -> Expect.within (Expect.Absolute 0.001) (asin -0.5) phiQuad
                        ]
                        ()
            , test "propagateWithLinkages updates slider positions from driven gears" <|
                \_ ->
                    let
                        graph =
                            twoGearGraph 8 16

                        linkage : KinematicLinkage
                        linkage =
                            { id = 101
                            , drivingGearId = 1
                            , crankRadius = 10.0
                            , rodLength = 30.0
                            , crankOffset = 0.0
                            }

                        -- Motor at gear 0 rotates 2.0 rad. Gear 1 (ratio -8/16 = -0.5) rotates -1.0 rad.
                        result =
                            propagateWithLinkages graph [ linkage ] 0 2.0
                    in
                    case Dict.get 101 result.linkagePositions of
                        Just state ->
                            let
                                expectedX =
                                    crankSliderPosition 10.0 30.0 -1.0
                            in
                            Expect.within (Expect.Absolute 0.001) expectedX state.sliderX

                        Nothing ->
                            Expect.fail "expected linkage state"
            ]
        ]
