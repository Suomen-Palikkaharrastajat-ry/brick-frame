module Render.SnapshotTest exposing (suite)

{-| Tests for `Render.Snapshot`, the flat triangle list the path tracer reads:
14 floats per triangle, render-space corners, colour, and material.
-}

import Expect
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)
import Render.Snapshot as Snapshot exposing (Placed)
import Test exposing (Test, describe, test)


vertex : Float -> Float -> Float -> Vec4 -> Float -> Vertex
vertex x y z colour inherit =
    { position = Vec3.vec3 x y z
    , normal = Vec3.vec3 0 1 0
    , color = colour
    , inherit = inherit
    }


red : Vec4
red =
    Vec4.vec4 0.5 0.25 0.125 1


white : Vec4
white =
    Vec4.vec4 1 1 1 1


{-| LDraw space is Y down and Z toward the viewer; the renderer's Y-up flip.
-}
yUpFlip : Mat4.Mat4
yUpFlip =
    Mat4.makeScale3 1 -1 -1


{-| A triangle on the underside of a 3001.dat-style brick, in LDraw coordinates.
-}
brickFace : Vec4 -> Float -> ( Vertex, Vertex, Vertex )
brickFace colour inherit =
    ( vertex 0 -24 0 colour inherit
    , vertex 20 -24 0 colour inherit
    , vertex 0 -24 20 colour inherit
    )


placed : Placed
placed =
    { matrix = yUpFlip
    , instanceColor = white
    , ghost = False
    }


suite : Test
suite =
    describe "Render.Snapshot"
        [ describe "placedTriangles"
            [ test "a brick face gives 14 floats in Y-up render space" <|
                \_ ->
                    Snapshot.placedTriangles placed [ brickFace red 0 ]
                        |> Expect.equal
                            [ 0, 24, 0, 20, 24, 0, 0, 24, -20, 0.5, 0.25, 0.125, 1, 0 ]
            , test "each triangle is floatsPerTriangle floats" <|
                \_ ->
                    Snapshot.placedTriangles placed [ brickFace red 0, brickFace red 0 ]
                        |> List.length
                        |> Expect.equal (2 * Snapshot.floatsPerTriangle)
            , test "a vertex that inherited its colour takes the placement's colour" <|
                \_ ->
                    Snapshot.placedTriangles placed [ brickFace red 1 ]
                        |> List.drop 9
                        |> Expect.equal [ 1, 1, 1, 1, 0 ]
            , test "a transparent colour is material 1 and keeps its alpha" <|
                \_ ->
                    Snapshot.placedTriangles placed [ brickFace (Vec4.vec4 0.5 0.25 0.125 0.5) 0 ]
                        |> List.drop 9
                        |> Expect.equal [ 0.5, 0.25, 0.125, 0.5, 1 ]
            , test "ghost geometry is drawn at ghost alpha, as material 1" <|
                \_ ->
                    Snapshot.placedTriangles { placed | ghost = True } [ brickFace red 0 ]
                        |> List.drop 9
                        |> Expect.equal [ 0.5, 0.25, 0.125, 0.25, 1 ]
            , test "the placement matrix moves every corner" <|
                \_ ->
                    Snapshot.placedTriangles { placed | matrix = Mat4.makeTranslate3 100 0 0 } [ brickFace red 0 ]
                        |> List.take 3
                        |> Expect.equal [ 100, -24, 0 ]
            , test "no triangles give no floats" <|
                \_ ->
                    Snapshot.placedTriangles placed []
                        |> Expect.equal []
            ]
        ]
