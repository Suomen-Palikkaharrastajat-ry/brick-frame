module Viewport.Wire exposing (flattenRequest, flattenResultDecoder)

{-| JSON wire format of the geometry worker.

`flattenRequest` serialises the top-level lines and the loaded part cache for
the worker; `flattenResultDecoder` reads back the baked triangles and edges.
The worker itself (`geometry-worker.js`) lives with the host application, which
also owns the ports that carry these strings.

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.ColorData as ColorData
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4
import Render.Mesh exposing (Vertex)


{-| The JSON payload for the geometry worker.
-}
flattenRequest : List LDrawLine -> PartCache -> String
flattenRequest lines cache =
    Encode.object
        [ ( "lines", Encode.list encodeLDrawLine lines )
        , ( "cache", encodeLoadedCache cache )
        , ( "parentColor", Encode.int 15 )
        , ( "colorTable", encodeColorTable ColorData.colorTable )
        ]
        |> Encode.encode 0


encodeLDrawLine : LDrawLine -> Encode.Value
encodeLDrawLine line =
    case line of
        Comment textValue ->
            Encode.object
                [ ( "k", Encode.string "comment" )
                , ( "text", Encode.string textValue )
                ]

        SubFileRef ref ->
            Encode.object
                [ ( "k", Encode.string "subfile" )
                , ( "file", Encode.string ref.file )
                , ( "color", Encode.int ref.color )
                , ( "transform", encodeAffine ref.transform )
                ]

        Triangle tri ->
            Encode.object
                [ ( "k", Encode.string "tri" )
                , ( "color", Encode.int tri.color )
                , ( "p1", encodeVec3 tri.p1 )
                , ( "p2", encodeVec3 tri.p2 )
                , ( "p3", encodeVec3 tri.p3 )
                ]

        Quad quad ->
            Encode.object
                [ ( "k", Encode.string "quad" )
                , ( "color", Encode.int quad.color )
                , ( "p1", encodeVec3 quad.p1 )
                , ( "p2", encodeVec3 quad.p2 )
                , ( "p3", encodeVec3 quad.p3 )
                , ( "p4", encodeVec3 quad.p4 )
                ]

        LineSegment seg ->
            Encode.object
                [ ( "k", Encode.string "line" )
                , ( "color", Encode.int seg.color )
                , ( "p1", encodeVec3 seg.p1 )
                , ( "p2", encodeVec3 seg.p2 )
                ]

        ConditionalLine cond ->
            Encode.object
                [ ( "k", Encode.string "cond" )
                , ( "color", Encode.int cond.color )
                , ( "p1", encodeVec3 cond.p1 )
                , ( "p2", encodeVec3 cond.p2 )
                , ( "c1", encodeVec3 cond.c1 )
                , ( "c2", encodeVec3 cond.c2 )
                ]


encodeAffine : Mat4.Mat4 -> Encode.Value
encodeAffine mat =
    let
        origin =
            Mat4.transform mat (Vec3.vec3 0 0 0)

        xAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 1 0 0)) origin

        yAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 0 1 0)) origin

        zAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 0 0 1)) origin
    in
    Encode.object
        [ ( "o", encodeVec3 origin )
        , ( "x", encodeVec3 xAxis )
        , ( "y", encodeVec3 yAxis )
        , ( "z", encodeVec3 zAxis )
        ]


encodeLoadedCache : PartCache -> Encode.Value
encodeLoadedCache cache =
    cache
        |> Dict.toList
        |> List.filterMap
            (\( name, status ) ->
                case status of
                    Loaded lines ->
                        Just ( name, Encode.list encodeLDrawLine lines )

                    _ ->
                        Nothing
            )
        |> Encode.object


encodeColorTable : Dict Int { r : Float, g : Float, b : Float, alpha : Float } -> Encode.Value
encodeColorTable table =
    table
        |> Dict.toList
        |> List.map
            (\( code, c ) ->
                ( String.fromInt code
                , Encode.object
                    [ ( "r", Encode.float c.r )
                    , ( "g", Encode.float c.g )
                    , ( "b", Encode.float c.b )
                    , ( "alpha", Encode.float c.alpha )
                    ]
                )
            )
        |> Encode.object


encodeVec3 : Vec3.Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]


flattenResultDecoder :
    Decode.Decoder
        { triangles : List ( Vertex, Vertex, Vertex )
        , lines : List ( Vec3.Vec3, Vec3.Vec3 )
        , conditionalLines : List Geometry.ConditionalEdge
        , bfcCertified : Bool
        }
flattenResultDecoder =
    Decode.map4
        (\tris lines conds bfc ->
            { triangles = tris
            , lines = lines
            , conditionalLines = conds
            , bfcCertified = bfc
            }
        )
        (Decode.field "triangles" (Decode.list triangleDecoder))
        (Decode.field "lines" (Decode.list linePairDecoder))
        (Decode.field "conditionalLines" (Decode.list conditionalEdgeDecoder))
        (Decode.field "bfcCertified" Decode.bool)


triangleDecoder : Decode.Decoder ( Vertex, Vertex, Vertex )
triangleDecoder =
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.index 0 vertexDecoder)
        (Decode.index 1 vertexDecoder)
        (Decode.index 2 vertexDecoder)


vertexDecoder : Decode.Decoder Vertex
vertexDecoder =
    -- The worker bakes world-space geometry with colours already resolved, so
    -- nothing it produces inherits from a placement.
    Decode.map3
        (\p n c -> { position = p, normal = n, color = c, inherit = 0 })
        (Decode.field "position" vec3Decoder)
        (Decode.field "normal" vec3Decoder)
        (Decode.field "color" vec4Decoder)


linePairDecoder : Decode.Decoder ( Vec3.Vec3, Vec3.Vec3 )
linePairDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 vec3Decoder)
        (Decode.index 1 vec3Decoder)


conditionalEdgeDecoder : Decode.Decoder Geometry.ConditionalEdge
conditionalEdgeDecoder =
    Decode.map4
        (\p1 p2 c1 c2 -> { p1 = p1, p2 = p2, c1 = c1, c2 = c2 })
        (Decode.field "p1" vec3Decoder)
        (Decode.field "p2" vec3Decoder)
        (Decode.field "c1" vec3Decoder)
        (Decode.field "c2" vec3Decoder)


vec3Decoder : Decode.Decoder Vec3.Vec3
vec3Decoder =
    Decode.map3 Vec3.vec3
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


vec4Decoder : Decode.Decoder Vec4.Vec4
vec4Decoder =
    Decode.map4 Vec4.vec4
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)
        (Decode.index 3 Decode.float)
