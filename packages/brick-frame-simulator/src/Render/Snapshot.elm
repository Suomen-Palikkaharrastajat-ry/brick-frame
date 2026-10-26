module Render.Snapshot exposing (Placed, Triangle, floatsPerTriangle, materialFor, placedTriangles)

{-| Flatten drawn geometry into the triangle list the offline path tracer reads.

The list is flat, `floatsPerTriangle` (14) numbers per triangle:

  - `x y z` for each of the three corners, in render space (Y up, LDraw units);
  - `r g b a`, 0 to 1, the colour the WebGL pass resolves for the triangle;
  - `material`, the path tracer's code: `0` plastic, `1` transparent,
    `2` chrome or metal, `3` rubber, `4` pearlescent paint (see `materialFor`).

Each triangle is emitted the way `Render.Shader` draws it. The placement's
model matrix is applied, a vertex that inherited its colour takes the
placement's `instanceColor` and its finish, and ghost geometry gets the ghost
alpha and is always transparent.

A vertex with a fixed colour carries only its RGBA, not its LDraw code, so its
material comes from the alpha alone. Only inherited vertices take a finish.

-}

import LDraw.ColorData exposing (Finish(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)


{-| Three corners of one triangle, in whatever space the placement's matrix maps from.
-}
type alias Triangle =
    ( Vertex, Vertex, Vertex )


{-| One placement of geometry: where it sits, the colour and finish its
inherited vertices take, and whether it is drawn as ghost context.
-}
type alias Placed =
    { matrix : Mat4
    , instanceColor : Vec4
    , finish : Finish
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
through `matrix`; the colour and material come from the first corner.
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
            materialFor placed a alpha
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


{-| The path tracer's material code for a corner drawn at `alpha`.

Ghost context is transparent. An inherited corner takes the placement's
finish: chrome and metal are `2`, rubber `3`, pearlescent paint `4`, a
transparent colour `1`. Glitter and speckle are plastic, so they are `1` only
when translucent. A fixed colour, and any plastic, is `1` when its alpha is
below 1 and `0` otherwise.

-}
materialFor : Placed -> Vertex -> Float -> Float
materialFor placed vertex alpha =
    let
        finish =
            if vertex.inherit > 0.5 then
                placed.finish

            else
                Solid
    in
    if placed.ghost then
        1

    else
        case finish of
            Chrome ->
                2

            Metal ->
                2

            Rubber ->
                3

            Pearl ->
                4

            Transparent ->
                1

            Solid ->
                plasticMaterial alpha

            Glitter ->
                plasticMaterial alpha

            Speckle ->
                plasticMaterial alpha


plasticMaterial : Float -> Float
plasticMaterial alpha =
    if alpha < 1 then
        1

    else
        0


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
