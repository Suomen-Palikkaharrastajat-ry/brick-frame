module Render.Snapshot exposing (Placed, Triangle, floatsPerTriangle, placedTriangles)

{-| Flatten drawn geometry into the triangle list the offline path tracer reads.

The list is flat, `floatsPerTriangle` (14) numbers per triangle:

  - `x y z` for each of the three corners, in render space (Y up, LDraw units);
  - `r g b a`, 0 to 1, the colour the WebGL pass resolves for the triangle;
  - `material`, `0` solid plastic or `1` transparent.

Each triangle is emitted the way `Render.Shader` draws it. The placement's
model matrix is applied, a vertex that inherited its colour takes the
placement's `instanceColor`, and ghost geometry gets the ghost alpha.

The generated colour table records no finish, so the material cannot tell
chrome, rubber or pearlescent paint apart. Transparency is the only material
that can be told from the colour alone.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)


{-| Three corners of one triangle, in whatever space the placement's matrix maps from.
-}
type alias Triangle =
    ( Vertex, Vertex, Vertex )


{-| One placement of geometry: where it sits, the colour its inherited vertices
take, and whether it is drawn as ghost context.
-}
type alias Placed =
    { matrix : Mat4
    , instanceColor : Vec4
    , ghost : Bool
    }


{-| Floats per triangle in the flat output.
-}
floatsPerTriangle : Int
floatsPerTriangle =
    14


{-| Alpha of ghost geometry, matching the WebGL ghost pass.
-}
ghostAlpha : Float
ghostAlpha =
    0.25


{-| Flatten triangles under one placement. Each corner's position is taken
through `matrix`; the colour comes from the first corner.
-}
placedTriangles : Placed -> List Triangle -> List Float
placedTriangles placed triangles =
    List.foldr (appendTriangle placed) [] triangles


appendTriangle : Placed -> Triangle -> List Float -> List Float
appendTriangle placed ( a, b, c ) acc =
    let
        colour =
            resolvedColour placed a

        alpha =
            if placed.ghost then
                ghostAlpha

            else
                Vec4.getW colour

        material =
            if alpha < 1 then
                1

            else
                0
    in
    corner placed
        a
        (corner placed
            b
            (corner placed
                c
                (Vec4.getX colour
                    :: Vec4.getY colour
                    :: Vec4.getZ colour
                    :: alpha
                    :: material
                    :: acc
                )
            )
        )


{-| The three position floats of one corner, prepended to `acc`.
-}
corner : Placed -> Vertex -> List Float -> List Float
corner placed vertex acc =
    let
        p =
            Mat4.transform placed.matrix vertex.position
    in
    Vec3.getX p :: Vec3.getY p :: Vec3.getZ p :: acc


{-| The colour a vertex is drawn in: its own, or the placement's where it inherited.
-}
resolvedColour : Placed -> Vertex -> Vec4
resolvedColour placed vertex =
    if vertex.inherit > 0.5 then
        placed.instanceColor

    else
        vertex.color
