module Render.Mesh exposing (Vertex)

{-| Shared mesh vertex types used by render modules.
-}

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| A WebGL vertex with position, surface normal, and RGBA colour.
Used for all triangle-based geometry (LDraw type-3 and type-4 lines).

`inherit` is `1.0` when this vertex's colour was LDraw code 16 all the way up
its sub-file chain, and `0.0` when some level fixed a concrete colour. The
shader uses it to blend in the `instanceColor` uniform, which is what lets
`Render.Instanced` bake a part **once** and draw it in any colour: without it,
every part would need a separate copy of its geometry per colour it appears in —
a 4.7× multiplication of the largest buffer in the scene on a typical model.

Geometry flattened for direct rendering resolves colours up front and leaves
this at `0.0`, so those paths ignore `instanceColor` entirely.

-}
type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    , inherit : Float
    }
