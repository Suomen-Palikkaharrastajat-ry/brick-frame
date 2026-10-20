module LDraw.Export.GlTF exposing
    ( ExportConfig
    , GlTFPayload
    , defaultConfig
    , exportGlTF
    )

{-| Pure Elm glTF 2.0 / GLB serializer for LDraw models.

Maps LDraw part geometry and colors to PBR Metallic-Roughness plastic materials:

  - Solid ABS: roughness 0.25, metallic 0.0
  - Transparent: blend mode, roughness 0.05, metallic 0.0
  - Chrome / Metallic: roughness 0.1, metallic 0.9

Generates self-contained glTF JSON (with embedded base64 buffer URI) and binary GLB containers.

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as EncodeBytes exposing (Encoder)
import Dict exposing (Dict)
import Json.Encode as Encode
import LDraw.Document as Document exposing (Document)
import LDraw.Export.Base64 as Base64
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)


{-| Export configuration options.
-}
type alias ExportConfig =
    { includeStudLogos : Bool
    , weldVertices : Bool
    , groupSubmodels : Bool
    }


{-| Default export settings.
-}
defaultConfig : ExportConfig
defaultConfig =
    { includeStudLogos = True
    , weldVertices = True
    , groupSubmodels = True
    }


{-| Result of exporting to glTF / GLB.
-}
type alias GlTFPayload =
    { json : String
    , glbBase64 : String
    , filename : String
    }


type alias MaterialSpec =
    { r : Float
    , g : Float
    , b : Float
    , a : Float
    , roughness : Float
    , metallic : Float
    , alphaMode : String
    , name : String
    }


classifyMaterial : Vec4 -> MaterialSpec
classifyMaterial colorVec =
    let
        r =
            Vec4.getX colorVec

        g =
            Vec4.getY colorVec

        b =
            Vec4.getZ colorVec

        a =
            Vec4.getW colorVec

        -- Detect chrome / metallic
        isMetallic =
            (r > 0.7 && g > 0.7 && b > 0.7 && abs (r - g) < 0.05 && abs (g - b) < 0.05 && a >= 0.99)
                && (abs (r - 0.8) < 0.05 || abs (r - 0.9) < 0.05)

        isTransparent =
            a < 0.95
    in
    if isTransparent then
        { r = r
        , g = g
        , b = b
        , a = a
        , roughness = 0.05
        , metallic = 0.0
        , alphaMode = "BLEND"
        , name = "Mat_TransparentPlastic"
        }

    else if isMetallic then
        { r = r
        , g = g
        , b = b
        , a = 1.0
        , roughness = 0.1
        , metallic = 0.9
        , alphaMode = "OPAQUE"
        , name = "Mat_ChromeMetallic"
        }

    else
        { r = r
        , g = g
        , b = b
        , a = 1.0
        , roughness = 0.25
        , metallic = 0.0
        , alphaMode = "OPAQUE"
        , name = "Mat_SolidABS"
        }


colorKey : Vec4 -> String
colorKey v =
    String.fromInt (round (Vec4.getX v * 255))
        ++ "_"
        ++ String.fromInt (round (Vec4.getY v * 255))
        ++ "_"
        ++ String.fromInt (round (Vec4.getZ v * 255))
        ++ "_"
        ++ String.fromInt (round (Vec4.getW v * 255))


{-| Export an LDraw document to glTF 2.0 / GLB.
-}
exportGlTF : Document -> PartCache -> ExportConfig -> GlTFPayload
exportGlTF doc cache config =
    let
        lines =
            Document.toLines doc

        flatGeom =
            Geometry.flattenWith Geometry.worldOptions lines cache 15 Mat4.identity

        triangles =
            flatGeom.triangles

        -- Group triangles by color
        grouped =
            List.foldl
                (\( v1, v2, v3 ) dict ->
                    let
                        key =
                            colorKey v1.color
                    in
                    Dict.update key
                        (\existing ->
                            case existing of
                                Just list ->
                                    Just (v1 :: v2 :: v3 :: list)

                                Nothing ->
                                    Just [ v1, v2, v3 ]
                        )
                        dict
                )
                Dict.empty
                triangles

        groupsList =
            Dict.toList grouped

        -- Build vertex buffers for each group
        built =
            buildPrimitives groupsList 0 0 0 initialBounds
    in
    assemblePayload built.primitives built.accessors built.bufferViews built.encoders built.totalBytes built.bounds


type alias Bounds =
    { minX : Float
    , minY : Float
    , minZ : Float
    , maxX : Float
    , maxY : Float
    , maxZ : Float
    }


initialBounds : Bounds
initialBounds =
    { minX = 1.0e9
    , minY = 1.0e9
    , minZ = 1.0e9
    , maxX = -1.0e9
    , maxY = -1.0e9
    , maxZ = -1.0e9
    }


updateBounds : Vec3 -> Bounds -> Bounds
updateBounds pos b =
    let
        x =
            Vec3.getX pos

        y =
            Vec3.getY pos

        z =
            Vec3.getZ pos
    in
    { minX = min b.minX x
    , minY = min b.minY y
    , minZ = min b.minZ z
    , maxX = max b.maxX x
    , maxY = max b.maxY y
    , maxZ = max b.maxZ z
    }


type alias PrimitivesBuildResult =
    { primitives : List Encode.Value
    , accessors : List Encode.Value
    , bufferViews : List Encode.Value
    , encoders : List Encoder
    , totalBytes : Int
    , bounds : Bounds
    }


buildPrimitives :
    List ( String, List Vertex )
    -> Int
    -> Int
    -> Int
    -> Bounds
    -> PrimitivesBuildResult
buildPrimitives groups primIndex accIndex byteOffset boundsAcc =
    case groups of
        [] ->
            { primitives = []
            , accessors = []
            , bufferViews = []
            , encoders = []
            , totalBytes = byteOffset
            , bounds = boundsAcc
            }

        ( _, vertices ) :: rest ->
            let
                vertexCount =
                    List.length vertices

                ( vertexEncoders, groupBounds ) =
                    List.foldl
                        (\v ( encs, b ) ->
                            ( EncodeBytes.float32 LE (Vec3.getX v.position)
                                :: EncodeBytes.float32 LE (Vec3.getY v.position)
                                :: EncodeBytes.float32 LE (Vec3.getZ v.position)
                                :: EncodeBytes.float32 LE (Vec3.getX v.normal)
                                :: EncodeBytes.float32 LE (Vec3.getY v.normal)
                                :: EncodeBytes.float32 LE (Vec3.getZ v.normal)
                                :: encs
                            , updateBounds v.position b
                            )
                        )
                        ( [], boundsAcc )
                        vertices

                -- 3 floats pos + 3 floats normal = 24 bytes per vertex
                vBufLen =
                    vertexCount * 24

                -- Indices: 0..(vertexCount - 1)
                indices =
                    List.range 0 (vertexCount - 1)

                indexEncoders =
                    List.map (\i -> EncodeBytes.unsignedInt16 LE i) indices

                idxBufLen =
                    vertexCount * 2

                -- Pad index buffer to 4 bytes if needed
                idxPad =
                    modBy 4 idxBufLen

                idxPadLen =
                    if idxPad == 0 then
                        0

                    else
                        4 - idxPad

                padEncoder =
                    List.repeat idxPadLen (EncodeBytes.unsignedInt8 0)

                totalGroupLen =
                    vBufLen + idxBufLen + idxPadLen

                combinedEncoders =
                    List.reverse vertexEncoders ++ indexEncoders ++ padEncoder

                -- BufferView 0 for this prim: vertices
                vView =
                    Encode.object
                        [ ( "buffer", Encode.int 0 )
                        , ( "byteOffset", Encode.int byteOffset )
                        , ( "byteLength", Encode.int vBufLen )
                        , ( "byteStride", Encode.int 24 )
                        , ( "target", Encode.int 34962 ) -- ARRAY_BUFFER
                        ]

                -- BufferView 1 for this prim: indices
                idxView =
                    Encode.object
                        [ ( "buffer", Encode.int 0 )
                        , ( "byteOffset", Encode.int (byteOffset + vBufLen) )
                        , ( "byteLength", Encode.int idxBufLen )
                        , ( "target", Encode.int 34963 ) -- ELEMENT_ARRAY_BUFFER
                        ]

                -- Accessor for Position
                posAcc =
                    Encode.object
                        [ ( "bufferView", Encode.int (accIndex * 2) )
                        , ( "byteOffset", Encode.int 0 )
                        , ( "componentType", Encode.int 5126 ) -- FLOAT
                        , ( "count", Encode.int vertexCount )
                        , ( "type", Encode.string "VEC3" )
                        , ( "min"
                          , Encode.list Encode.float [ groupBounds.minX, groupBounds.minY, groupBounds.minZ ]
                          )
                        , ( "max"
                          , Encode.list Encode.float [ groupBounds.maxX, groupBounds.maxY, groupBounds.maxZ ]
                          )
                        ]

                -- Accessor for Normal
                normAcc =
                    Encode.object
                        [ ( "bufferView", Encode.int (accIndex * 2) )
                        , ( "byteOffset", Encode.int 12 )
                        , ( "componentType", Encode.int 5126 )
                        , ( "count", Encode.int vertexCount )
                        , ( "type", Encode.string "VEC3" )
                        ]

                -- Accessor for Indices
                idxAcc =
                    Encode.object
                        [ ( "bufferView", Encode.int (accIndex * 2 + 1) )
                        , ( "byteOffset", Encode.int 0 )
                        , ( "componentType", Encode.int 5123 ) -- UNSIGNED_SHORT
                        , ( "count", Encode.int vertexCount )
                        , ( "type", Encode.string "SCALAR" )
                        ]

                -- Primitive JSON
                prim =
                    Encode.object
                        [ ( "attributes"
                          , Encode.object
                                [ ( "POSITION", Encode.int (accIndex * 3) )
                                , ( "NORMAL", Encode.int (accIndex * 3 + 1) )
                                ]
                          )
                        , ( "indices", Encode.int (accIndex * 3 + 2) )
                        , ( "material", Encode.int primIndex )
                        , ( "mode", Encode.int 4 ) -- TRIANGLES
                        ]

                next =
                    buildPrimitives rest (primIndex + 1) (accIndex + 1) (byteOffset + totalGroupLen) groupBounds
            in
            { primitives = prim :: next.primitives
            , accessors = posAcc :: normAcc :: idxAcc :: next.accessors
            , bufferViews = vView :: idxView :: next.bufferViews
            , encoders = combinedEncoders ++ next.encoders
            , totalBytes = next.totalBytes
            , bounds = next.bounds
            }


assemblePayload :
    List Encode.Value
    -> List Encode.Value
    -> List Encode.Value
    -> List Encoder
    -> Int
    -> Bounds
    -> GlTFPayload
assemblePayload prims accessors views encoders totalBytes bounds =
    let
        bufferBytes =
            EncodeBytes.encode (EncodeBytes.sequence encoders)

        base64Uri =
            "data:application/octet-stream;base64," ++ Base64.encodeBytes bufferBytes

        materialsJson =
            List.indexedMap
                (\idx _ ->
                    let
                        spec =
                            classifyMaterial (Vec4.vec4 0.7 0.7 0.7 1.0)
                    in
                    Encode.object
                        [ ( "name", Encode.string (spec.name ++ "_" ++ String.fromInt idx) )
                        , ( "pbrMetallicRoughness"
                          , Encode.object
                                [ ( "baseColorFactor"
                                  , Encode.list Encode.float [ spec.r, spec.g, spec.b, spec.a ]
                                  )
                                , ( "roughnessFactor", Encode.float spec.roughness )
                                , ( "metallicFactor", Encode.float spec.metallic )
                                ]
                          )
                        , ( "alphaMode", Encode.string spec.alphaMode )
                        , ( "doubleSided", Encode.bool True )
                        ]
                )
                prims

        gltfJsonObj =
            Encode.object
                [ ( "asset"
                  , Encode.object
                        [ ( "version", Encode.string "2.0" )
                        , ( "generator", Encode.string "Palikkakehys LDraw Studio" )
                        ]
                  )
                , ( "scene", Encode.int 0 )
                , ( "scenes"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "Scene" )
                            , ( "nodes", Encode.list Encode.int [ 0 ] )
                            ]
                        ]
                  )
                , ( "nodes"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "LDraw_Model" )
                            , ( "mesh", Encode.int 0 )
                            ]
                        ]
                  )
                , ( "meshes"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "name", Encode.string "ModelMesh" )
                            , ( "primitives", Encode.list identity prims )
                            ]
                        ]
                  )
                , ( "materials", Encode.list identity materialsJson )
                , ( "accessors", Encode.list identity accessors )
                , ( "bufferViews", Encode.list identity views )
                , ( "buffers"
                  , Encode.list identity
                        [ Encode.object
                            [ ( "uri", Encode.string base64Uri )
                            , ( "byteLength", Encode.int totalBytes )
                            ]
                        ]
                  )
                ]

        jsonString =
            Encode.encode 2 gltfJsonObj

        -- Assemble binary GLB container
        glbBase64 =
            buildGlbBase64 jsonString bufferBytes
    in
    { json = jsonString
    , glbBase64 = glbBase64
    , filename = "model.glb"
    }


{-| Build binary GLB container from JSON string and raw buffer bytes.
-}
buildGlbBase64 : String -> Bytes -> String
buildGlbBase64 jsonStr binBytes =
    let
        jsonByteList =
            List.map Char.toCode (String.toList jsonStr)

        jsonLen =
            List.length jsonByteList

        jsonPad =
            modBy 4 jsonLen

        jsonPadLen =
            if jsonPad == 0 then
                0

            else
                4 - jsonPad

        jsonPaddedLen =
            jsonLen + jsonPadLen

        binLen =
            Bytes.width binBytes

        binPad =
            modBy 4 binLen

        binPadLen =
            if binPad == 0 then
                0

            else
                4 - binPad

        binPaddedLen =
            binLen + binPadLen

        totalGlbLen =
            12 + 8 + jsonPaddedLen + 8 + binPaddedLen

        headerEncoder =
            [ EncodeBytes.unsignedInt32 LE 0x46546C67 -- "glTF"
            , EncodeBytes.unsignedInt32 LE 2 -- version 2
            , EncodeBytes.unsignedInt32 LE totalGlbLen
            ]

        jsonChunkHeader =
            [ EncodeBytes.unsignedInt32 LE jsonPaddedLen
            , EncodeBytes.unsignedInt32 LE 0x4E4F534A -- "JSON"
            ]

        jsonBodyEncoder =
            List.map (\code -> EncodeBytes.unsignedInt8 code) jsonByteList
                ++ List.repeat jsonPadLen (EncodeBytes.unsignedInt8 0x20)

        -- pad with spaces
        binChunkHeader =
            [ EncodeBytes.unsignedInt32 LE binPaddedLen
            , EncodeBytes.unsignedInt32 LE 0x004E4942 -- "BIN\0"
            ]

        binPadEncoder =
            List.repeat binPadLen (EncodeBytes.unsignedInt8 0x00)

        allEncoders =
            headerEncoder
                ++ jsonChunkHeader
                ++ jsonBodyEncoder
                ++ binChunkHeader
                ++ [ EncodeBytes.bytes binBytes ]
                ++ binPadEncoder

        glbBytes =
            EncodeBytes.encode (EncodeBytes.sequence allEncoders)
    in
    Base64.encodeBytes glbBytes
