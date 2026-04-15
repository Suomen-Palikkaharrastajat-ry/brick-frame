module LDraw.Geometry exposing (ConditionalEdge, FlatGeometry, flatten)

{-| Flatten an LDraw part tree into WebGL-ready vertex buffers.

Takes a parsed list of `LDrawLine` values plus a populated `PartCache` and
recursively resolves sub-file references, accumulating the 4×4 transform
matrix and the current color at each level.

## Coordinate conversion

LDraw uses a right-handed coordinate system where **-Y is up**. This module
negates Y on all output positions so that the geometry is Y-up, matching
WebGL convention.

## Output

`flatten` returns a `FlatGeometry` record containing:

  - `triangles` — a flat list of `(Vertex, Vertex, Vertex)` tuples ready for
    `WebGL.triangles`.
  - `lines` — pairs of positions for edge rendering via `WebGL.lines`.
  - `bfcCertified` — whether the top-level file declared BFC CCW winding.
    When true, back-face culling can safely be enabled.

-}

import Dict
import LDraw.Colors as Colors
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4
import Render.Mesh exposing (Vertex)
import String


-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Result of flattening a LDraw part tree.
-}
type alias FlatGeometry =
    { triangles : List ( Vertex, Vertex, Vertex )
    , lines : List ( Vec3, Vec3 )
    , conditionalLines : List ConditionalEdge
    , bfcCertified : Bool
    }


type alias ConditionalEdge =
    { p1 : Vec3
    , p2 : Vec3
    , c1 : Vec3
    , c2 : Vec3
    }


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Flatten a parsed LDraw file into renderable geometry.

    flatten lines cache parentColor worldTransform

  - `lines` — the parsed top-level file content.
  - `cache` — part cache (all sub-files must already be in `Loaded` state).
    Sub-files absent from the cache are silently skipped.
  - `parentColor` — the inherited color for resolving color code 16.
    Pass `15` (white) for the root call.
  - `worldTransform` — the initial transformation matrix.
    Pass `Mat4.identity` for the root call.

-}
flatten : List LDrawLine -> PartCache -> Int -> Mat4 -> FlatGeometry
flatten lines cache parentColor worldTransform =
    let
        bfc =
            hasBfcCertify lines

        result =
            flattenLines lines cache parentColor worldTransform { triangles = [], lines = [], conditionalLines = [], bfcCertified = False }
    in
    { result
        | triangles = result.triangles
        , bfcCertified = bfc
    }


-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Whether the file declares BFC CCW certification.
-}
hasBfcCertify : List LDrawLine -> Bool
hasBfcCertify lines =
    List.any isBfcCertify lines


isBfcCertify : LDrawLine -> Bool
isBfcCertify line =
    case line of
        Comment text ->
            String.contains "BFC CERTIFY CCW" text

        _ ->
            False


{-| Accumulate geometry by walking the line list.
-}
flattenLines :
    List LDrawLine
    -> PartCache
    -> Int
    -> Mat4
    -> FlatGeometry
    -> FlatGeometry
flattenLines lines cache parentColor transform acc =
    List.foldl (flattenLine cache parentColor transform) acc lines


flattenLine :
    PartCache
    -> Int
    -> Mat4
    -> LDrawLine
    -> FlatGeometry
    -> FlatGeometry
flattenLine cache parentColor transform line acc =
    case line of
        SubFileRef ref ->
            -- Resolve the sub-file from cache and recurse
            case Dict.get ref.file cache of
                Just (Loaded subLines) ->
                    let
                        childTransform =
                            Mat4.mul transform ref.transform

                        childColor =
                            if ref.color == 16 then
                                parentColor

                            else
                                ref.color
                    in
                    flattenLines subLines cache childColor childTransform acc

                _ ->
                    -- Not loaded or failed — skip silently
                    acc

        Triangle tri ->
            let
                color =
                    Colors.toVec4 (Colors.resolveColor parentColor tri.color)

                p1 =
                    transformPoint transform tri.p1

                p2 =
                    transformPoint transform tri.p2

                p3 =
                    transformPoint transform tri.p3

                normal =
                    faceNormal p1 p2 p3
            in
            { acc
                | triangles =
                    ( mkVertex p1 normal color
                    , mkVertex p2 normal color
                    , mkVertex p3 normal color
                    )
                        :: acc.triangles
            }

        Quad quad ->
            -- Split quad into two triangles along the p1–p3 diagonal
            let
                color =
                    Colors.toVec4 (Colors.resolveColor parentColor quad.color)

                p1 =
                    transformPoint transform quad.p1

                p2 =
                    transformPoint transform quad.p2

                p3 =
                    transformPoint transform quad.p3

                p4 =
                    transformPoint transform quad.p4

                n1 =
                    faceNormal p1 p2 p3

                n2 =
                    faceNormal p1 p3 p4
            in
            { acc
                | triangles =
                    ( mkVertex p1 n1 color, mkVertex p2 n1 color, mkVertex p3 n1 color )
                        :: ( mkVertex p1 n2 color, mkVertex p3 n2 color, mkVertex p4 n2 color )
                        :: acc.triangles
            }

        LineSegment seg ->
            { acc
                | lines =
                    ( transformPoint transform seg.p1
                    , transformPoint transform seg.p2
                    )
                        :: acc.lines
            }

        -- Comments and conditional lines do not produce geometry
        Comment _ ->
            acc

        ConditionalLine cond ->
            { acc
                | conditionalLines =
                    { p1 = transformPoint transform cond.p1
                    , p2 = transformPoint transform cond.p2
                    , c1 = transformPoint transform cond.c1
                    , c2 = transformPoint transform cond.c2
                    }
                        :: acc.conditionalLines
            }


{-| Apply the accumulated world transform to a point.
Negates Y and Z to convert LDraw (X-right, Y-down, Z-away) →
WebGL right-handed (X-right, Y-up, Z-toward-viewer).
-}
transformPoint : Mat4 -> Vec3 -> Vec3
transformPoint mat p =
    let
        tp =
            Mat4.transform mat p
    in
    vec3 (Vec3.getX tp) -(Vec3.getY tp) -(Vec3.getZ tp)


{-| Compute a face normal from three points (CCW winding assumed).
Returns the zero vector for degenerate triangles; shaders handle this gracefully.
-}
faceNormal : Vec3 -> Vec3 -> Vec3 -> Vec3
faceNormal p1 p2 p3 =
    let
        edge1 =
            Vec3.sub p2 p1

        edge2 =
            Vec3.sub p3 p1

        cross =
            Vec3.cross edge1 edge2

        len =
            Vec3.length cross
    in
    if len < 1.0e-8 then
        vec3 0 1 0

    else
        Vec3.scale (1.0 / len) cross


mkVertex : Vec3 -> Vec3 -> Vec4.Vec4 -> Vertex
mkVertex position normal color =
    { position = position, normal = normal, color = color }


smoothTriangles : List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
smoothTriangles triangles =
    let
        normalMap =
            triangles
                |> List.foldl accumulateTriangleNormals Dict.empty
    in
    List.map (smoothTriangle normalMap) triangles


accumulateTriangleNormals : ( Vertex, Vertex, Vertex ) -> Dict.Dict String Vec3 -> Dict.Dict String Vec3
accumulateTriangleNormals ( v1, v2, v3 ) normalMap =
    normalMap
        |> accumulateVertexNormal v1
        |> accumulateVertexNormal v2
        |> accumulateVertexNormal v3


accumulateVertexNormal : Vertex -> Dict.Dict String Vec3 -> Dict.Dict String Vec3
accumulateVertexNormal vertex normalMap =
    let
        key =
            positionKey vertex.position
    in
    Dict.update key
        (\existing ->
            case existing of
                Just current ->
                    Just (Vec3.add current vertex.normal)

                Nothing ->
                    Just vertex.normal
        )
        normalMap


smoothTriangle : Dict.Dict String Vec3 -> ( Vertex, Vertex, Vertex ) -> ( Vertex, Vertex, Vertex )
smoothTriangle normalMap ( v1, v2, v3 ) =
    ( smoothVertex normalMap v1
    , smoothVertex normalMap v2
    , smoothVertex normalMap v3
    )


smoothVertex : Dict.Dict String Vec3 -> Vertex -> Vertex
smoothVertex normalMap vertex =
    let
        key =
            positionKey vertex.position
    in
    case Dict.get key normalMap of
        Just summed ->
            { vertex | normal = normalizeSafe summed }

        Nothing ->
            vertex


normalizeSafe : Vec3 -> Vec3
normalizeSafe value =
    let
        len =
            Vec3.length value
    in
    if len < 1.0e-8 then
        vec3 0 1 0

    else
        Vec3.scale (1.0 / len) value


positionKey : Vec3 -> String
positionKey p =
    String.join "|"
        [ quantize (Vec3.getX p)
        , quantize (Vec3.getY p)
        , quantize (Vec3.getZ p)
        ]


quantize : Float -> String
quantize value =
    let
        scaled =
            round (value * 1000)
    in
    String.fromInt scaled
