module Render.SceneTest exposing (suite)

{-| Tests for the conditional-edge visibility logic.

`conditionalLineVisible` is the CPU reference implementation of the predicate now
evaluated per-vertex in `EdgeShader.conditionalVertexShader`. `conditionalToQuad`
builds the GPU mesh; the decisive test is that every one of a quad's six vertices
carries the identical control data, so the shader can never tear the quad.

-}

import Expect
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Scene as Scene
import Test exposing (Test, describe, test)


{-| Edge along +X with both control points on the +Y side.
-}
edge : Vec3 -> Vec3 -> { p1 : Vec3, p2 : Vec3, c1 : Vec3, c2 : Vec3 }
edge c1 c2 =
    { p1 = vec3 0 0 0, p2 = vec3 1 0 0, c1 = c1, c2 = c2 }


eye : Vec3
eye =
    vec3 0 0 5


quadVertices : List { position : Vec3, other : Vec3, side : Float, cp1 : Vec3, cp2 : Vec3, cc1 : Vec3, cc2 : Vec3 }
quadVertices =
    Scene.conditionalToQuad (edge (vec3 0 1 0) (vec3 0 2 0))
        |> List.concatMap (\( a, b, c ) -> [ a, b, c ])


suite : Test
suite =
    describe "Render.Scene conditional edges"
        [ describe "conditionalLineVisible (CPU reference)"
            [ test "control points on the same side as the eye → visible" <|
                \_ ->
                    Scene.conditionalLineVisible eye (edge (vec3 0 1 0) (vec3 0 2 0))
                        |> Expect.equal True
            , test "control points straddling the edge → hidden" <|
                \_ ->
                    Scene.conditionalLineVisible eye (edge (vec3 0 1 0) (vec3 0 -1 0))
                        |> Expect.equal False
            , test "degenerate zero-length edge does not crash" <|
                \_ ->
                    Scene.conditionalLineVisible eye
                        { p1 = vec3 0 0 0, p2 = vec3 0 0 0, c1 = vec3 0 1 0, c2 = vec3 0 2 0 }
                        |> Expect.equal False
            ]
        , describe "conditionalToQuad"
            [ test "produces six vertices" <|
                \_ ->
                    List.length quadVertices
                        |> Expect.equal 6
            , test "all six vertices carry identical control data (tearing guard)" <|
                \_ ->
                    let
                        ok v =
                            (v.cp1 == vec3 0 0 0)
                                && (v.cp2 == vec3 1 0 0)
                                && (v.cc1 == vec3 0 1 0)
                                && (v.cc2 == vec3 0 2 0)
                    in
                    List.all ok quadVertices
                        |> Expect.equal True
            ]
        ]
