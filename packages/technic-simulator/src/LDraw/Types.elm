module LDraw.Types exposing
    ( ConditionalData
    , LDrawLine(..)
    , LineData
    , QuadData
    , SubFileData
    , TriangleData
    )

{-| Typed AST for the LDraw file format.

Each line in an LDraw file starts with a type digit 0–5. This module
provides a union type that models every line type, plus the record types
for the structured data they carry.

Reference: <https://www.ldraw.org/article/218.html>

-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)


{-| One parsed line from an LDraw file.
-}
type LDrawLine
    = Comment String
    | SubFileRef SubFileData
    | LineSegment LineData
    | Triangle TriangleData
    | Quad QuadData
    | ConditionalLine ConditionalData


{-| Type 1 — sub-file reference.

`transform` is the 4×4 matrix built from the 12 floats in the LDraw line:

    1 <color> x y z a b c d e f g h i <file>

The top-left 3×3 is [a b c / d e f / g h i] (rotation/scale) and the
translation column is [x, y, z].

`file` is normalised to lowercase with forward slashes, path prefix stripped.

-}
type alias SubFileData =
    { color : Int
    , transform : Mat4
    , file : String
    }


{-| Type 2 — line segment (used for hard edges).
-}
type alias LineData =
    { color : Int
    , p1 : Vec3
    , p2 : Vec3
    }


{-| Type 3 — filled triangle.
-}
type alias TriangleData =
    { color : Int
    , p1 : Vec3
    , p2 : Vec3
    , p3 : Vec3
    }


{-| Type 4 — filled quadrilateral (must be coplanar and convex).
Split into two triangles during geometry flattening.
-}
type alias QuadData =
    { color : Int
    , p1 : Vec3
    , p2 : Vec3
    , p3 : Vec3
    , p4 : Vec3
    }


{-| Type 5 — conditional line.

The edge from `p1` to `p2` is rendered only when `c1` and `c2` project to
the same side of the line through `p1`–`p2`. Used to outline curved surfaces.

-}
type alias ConditionalData =
    { color : Int
    , p1 : Vec3
    , p2 : Vec3
    , c1 : Vec3
    , c2 : Vec3
    }
