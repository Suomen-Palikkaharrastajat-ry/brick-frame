module Render.PickTest exposing (suite)

{-| Unit tests for ray-AABB and ray-sphere picking tests.
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
            ]
        ]
