module Render.PickTest exposing (suite)

{-| Unit tests for ray-AABB, ray-sphere and ray-triangle picking tests.
-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Pick as Pick
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Render.Pick"
        [ describe "rayAabbHit"
            [ test "hits front face of a box aligned with axes" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 10

                        dir =
                            vec3 0 0 -1

                        boxMin =
                            vec3 -1 -1 -1

                        boxMax =
                            vec3 1 1 1

                        hit =
                            Pick.rayAabbHit origin dir boxMin boxMax
                    in
                    case hit of
                        Just t ->
                            Expect.within (Expect.Absolute 1.0e-5) 9.0 t

                        Nothing ->
                            Expect.fail "expected hit at t=9"
            , test "misses a box when pointing away" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 10

                        dir =
                            vec3 0 1 0

                        boxMin =
                            vec3 -1 -1 -1

                        boxMax =
                            vec3 1 1 1
                    in
                    Expect.equal Nothing (Pick.rayAabbHit origin dir boxMin boxMax)
            , test "returns 0 when ray starts inside the box" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 0

                        dir =
                            vec3 0 0 1

                        boxMin =
                            vec3 -1 -1 -1

                        boxMax =
                            vec3 1 1 1
                    in
                    Expect.equal (Just 0.0) (Pick.rayAabbHit origin dir boxMin boxMax)
            , test "correctly differentiates closer thin part vs distant large part" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 100

                        dir =
                            vec3 0 0 -1

                        brickMin =
                            vec3 -5 -5 40

                        brickMax =
                            vec3 5 5 50

                        baseMin =
                            vec3 -200 -200 0

                        baseMax =
                            vec3 200 200 10

                        hitBrick =
                            Pick.rayAabbHit origin dir brickMin brickMax

                        hitBase =
                            Pick.rayAabbHit origin dir baseMin baseMax
                    in
                    case ( hitBrick, hitBase ) of
                        ( Just tBrick, Just tBase ) ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 1.0e-5) 50.0 tBrick
                                , \_ -> Expect.within (Expect.Absolute 1.0e-5) 90.0 tBase
                                , \_ -> Expect.lessThan tBase tBrick
                                ]
                                ()

                        _ ->
                            Expect.fail "expected both to hit"
            , test "rayAabbHitFrom ignores box ending before tMin" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 10

                        dir =
                            vec3 0 0 -1

                        boxMin =
                            vec3 -1 -1 6

                        boxMax =
                            vec3 1 1 8
                    in
                    -- Box spans t from 2 to 4. For tMin = 5, box is in front of tMin.
                    Expect.equal Nothing (Pick.rayAabbHitFrom 5 origin dir boxMin boxMax)
            , test "rayAabbHitFrom clamps hit to tMin when box spans across tMin" <|
                \_ ->
                    let
                        origin =
                            vec3 0 0 10

                        dir =
                            vec3 0 0 -1

                        boxMin =
                            vec3 -1 -1 0

                        boxMax =
                            vec3 1 1 8
                    in
                    -- Box spans t from 2 to 10. For tMin = 5, earliest visible point is 5.
                    case Pick.rayAabbHitFrom 5 origin dir boxMin boxMax of
                        Just t ->
                            Expect.within (Expect.Absolute 1.0e-5) 5.0 t

                        Nothing ->
                            Expect.fail "expected hit at t=5"
            ]
        , describe "rayTriangleHit"
            [ test "hits a triangle facing the ray" <|
                \_ ->
                    Pick.rayTriangleHit (vec3 0 0 10) (vec3 0 0 -1) ( vec3 -1 -1 0, vec3 1 -1 0, vec3 0 1 0 )
                        |> Maybe.map (Expect.within (Expect.Absolute 1.0e-5) 10)
                        |> Maybe.withDefault (Expect.fail "expected hit at t=10")
            , test "hits a triangle from its back face" <|
                \_ ->
                    Pick.rayTriangleHit (vec3 0 0 10) (vec3 0 0 -1) ( vec3 -1 -1 0, vec3 0 1 0, vec3 1 -1 0 )
                        |> Expect.notEqual Nothing
            , test "misses beside the triangle, inside its bounding box" <|
                \_ ->
                    Pick.rayTriangleHit (vec3 0.9 0.9 10) (vec3 0 0 -1) ( vec3 -1 -1 0, vec3 1 -1 0, vec3 0 1 0 )
                        |> Expect.equal Nothing
            , test "nearestTriangleHit takes the closest surface" <|
                \_ ->
                    Pick.nearestTriangleHit (vec3 0 0 10)
                        (vec3 0 0 -1)
                        [ ( vec3 -1 -1 -5, vec3 1 -1 -5, vec3 0 1 -5 )
                        , ( vec3 -1 -1 3, vec3 1 -1 3, vec3 0 1 3 )
                        ]
                        |> Maybe.map (Expect.within (Expect.Absolute 1.0e-5) 7)
                        |> Maybe.withDefault (Expect.fail "expected hit at t=7")
            , test "nearestTriangleHitByFrom ignores triangles closer than tMin" <|
                \_ ->
                    Pick.nearestTriangleHitByFrom 8
                        identity
                        (vec3 0 0 10)
                        (vec3 0 0 -1)
                        [ ( vec3 -1 -1 3, vec3 1 -1 3, vec3 0 1 3 )
                        , ( vec3 -1 -1 -5, vec3 1 -1 -5, vec3 0 1 -5 )
                        ]
                        |> Maybe.map (Expect.within (Expect.Absolute 1.0e-5) 15)
                        |> Maybe.withDefault (Expect.fail "expected hit at t=15 beyond tMin")
            , test "nearestTriangleHitByFrom ignores triangles behind ray origin" <|
                \_ ->
                    Pick.nearestTriangleHitByFrom 0
                        identity
                        (vec3 0 0 0)
                        (vec3 0 0 -1)
                        [ ( vec3 -1 -1 5, vec3 1 -1 5, vec3 0 1 5 ) ]
                        |> Expect.equal Nothing
            ]
        ]
