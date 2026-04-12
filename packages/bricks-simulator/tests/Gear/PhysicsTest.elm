module Gear.PhysicsTest exposing (suite)

{-| Unit tests for gear-ratio propagation and angle sampling.
-}

import Array
import Dict
import Expect
import Gear.Physics exposing (angleAt, propagate)
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
    }


isolatedGraph : GearGraph
isolatedGraph =
    { instances =
        Array.fromList
            [ makeInstance 0 8 16
            , makeInstance 1 16 24
            ]
    , connections = Dict.empty
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
        ]
