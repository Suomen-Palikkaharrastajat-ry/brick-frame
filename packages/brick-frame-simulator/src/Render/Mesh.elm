module Render.Mesh exposing (Vertex)

{-| Shared mesh vertex types used by render modules.
-}

import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)


{-| A WebGL vertex with position, surface normal, and RGBA colour.
Used for all triangle-based geometry (LDraw type-3 and type-4 lines).
-}
type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , color : Vec4
    }
