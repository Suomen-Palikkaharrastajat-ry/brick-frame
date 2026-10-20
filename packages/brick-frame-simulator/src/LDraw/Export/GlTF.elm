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
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Export.Base64 as Base64
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4 exposing (Mat4)
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


lduToMeters : Float
lduToMeters =
    0.0004


metallicColorKeys : List String
metallicColorKeys =
    [ 80, 81, 82, 83, 135, 148, 179, 334, 383 ]
        |> List.map (\code -> colorKey (Colors.toVec4 (Colors.resolveColor 16 code)))


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

        isMetallic =
            List.member (colorKey colorVec) metallicColorKeys

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


firstColor : List Vertex -> Vec4
firstColor verts =
    case verts of
        first :: _ ->
            first.color

        [] ->
            Vec4.vec4 0.7 0.7 0.7 1.0


{-| Convert an LDraw placement matrix (-Y up, LDU) to a glTF 2.0 column-major
16-element matrix (+Y up, meters: 1 LDU = 0.0004m).
-}
ldrawMatrixToGltfColumnMajor : Mat4 -> List Float
ldrawMatrixToGltfColumnMajor mat =
    let
        rec =
            Mat4.toRecord mat
    in
    [ rec.m11
    , -rec.m12
    , -rec.m13
    , 0
    , -rec.m21
    , rec.m22
    , rec.m23
    , 0
    , -rec.m31
    , rec.m32
    , rec.m33
    , 0
    , rec.m41 * lduToMeters
    , -rec.m42 * lduToMeters
    , -rec.m43 * lduToMeters
    , 1
    ]


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

        placements =
            if config.groupSubmodels then
                Document.placements doc

            else
                Document.mainPlacements doc

        placementColors =
            placements
                |> List.map (\p -> Colors.toVec4 (Colors.resolveColor 16 p.color))
                |> List.foldl (\c dict -> Dict.insert (colorKey c) c dict) Dict.empty
                |> Dict.values

        materialColors =
            if List.isEmpty groupsList then
                if List.isEmpty placementColors then
                    [ Vec4.vec4 0.7 0.7 0.7 1.0 ]

                else
                    placementColors

            else
                List.map (\( _, verts ) -> firstColor verts) groupsList

        -- Build vertex buffers for each group
        built =
            buildPrimitives groupsList 0 0 0
    in
    assemblePayload built.primitives built.accessors built.bufferViews built.encoders built.totalBytes materialColors placements


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


updateBounds : Float -> Float -> Float -> Bounds -> Bounds
updateBounds x y z b =
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
    }


buildPrimitives :
    List ( String, List Vertex )
    -> Int
    -> Int
    -> Int
    -> PrimitivesBuildResult
buildPrimitives groups primIndex accIndex byteOffset =
    case groups of
        [] ->
            { primitives = []
            , accessors = []
            , bufferViews = []
            , encoders = []
            , totalBytes = byteOffset
            }

        ( _, vertices ) :: rest ->
            let
                vertexCount =
                    List.length vertices

                ( vertexEncoders, groupBounds ) =
                    List.foldl
                        (\v ( encs, b ) ->
                            let
                                px =
                                    Vec3.getX v.position * lduToMeters

                                py =
                                    Vec3.getY v.position * lduToMeters

                                pz =
                                    Vec3.getZ v.position * lduToMeters
                            in
                            ( EncodeBytes.float32 LE px
                                :: EncodeBytes.float32 LE py
                                :: EncodeBytes.float32 LE pz
                                :: EncodeBytes.float32 LE (Vec3.getX v.normal)
                                :: EncodeBytes.float32 LE (Vec3.getY v.normal)
                                :: EncodeBytes.float32 LE (Vec3.getZ v.normal)
                                :: encs
                            , updateBounds px py pz b
                            )
                        )
                        ( [], initialBounds )
                        vertices

                -- 3 floats pos + 3 floats normal = 24 bytes per vertex
                vBufLen =
                    vertexCount * 24

                -- Indices: 0..(vertexCount - 1)
                indices =
                    List.range 0 (vertexCount - 1)

                useUint32 =
                    vertexCount > 65535

                indexEncoders =
                    if useUint32 then
                        List.map (\i -> EncodeBytes.unsignedInt32 LE i) indices

                    else
                        List.map (\i -> EncodeBytes.unsignedInt16 LE i) indices

                idxBufLen =
                    if useUint32 then
                        vertexCount * 4

                    else
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

                -- Accessor for Indices (5125 = UNSIGNED_INT, 5123 = UNSIGNED_SHORT)
                idxAcc =
                    Encode.object
                        [ ( "bufferView", Encode.int (accIndex * 2 + 1) )
                        , ( "byteOffset", Encode.int 0 )
                        , ( "componentType"
                          , Encode.int
                                (if useUint32 then
                                    5125

                                 else
                                    5123
                                )
                          )
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
                    buildPrimitives rest (primIndex + 1) (accIndex + 1) (byteOffset + totalGroupLen)
            in
            { primitives = prim :: next.primitives
            , accessors = posAcc :: normAcc :: idxAcc :: next.accessors
            , bufferViews = vView :: idxView :: next.bufferViews
            , encoders = combinedEncoders ++ next.encoders
            , totalBytes = next.totalBytes
            }


assemblePayload :
    List Encode.Value
    -> List Encode.Value
    -> List Encode.Value
    -> List Encoder
    -> Int
    -> List Vec4
    -> List Placement
    -> GlTFPayload
assemblePayload prims accessors views encoders totalBytes materialColors placements =
    let
        bufferBytes =
            EncodeBytes.encode (EncodeBytes.sequence encoders)

        base64Uri =
            "data:application/octet-stream;base64," ++ Base64.encodeBytes bufferBytes

        materialsJson =
            List.indexedMap
                (\idx colorVec ->
                    let
                        spec =
                            classifyMaterial colorVec
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
                materialColors

        childIndices =
            List.range 1 (List.length placements)

        placementNodes =
            List.indexedMap
                (\idx p ->
                    Encode.object
                        [ ( "name", Encode.string ("Part_" ++ String.fromInt idx ++ "_" ++ p.part) )
                        , ( "matrix", Encode.list Encode.float (ldrawMatrixToGltfColumnMajor p.matrix) )
                        ]
                )
                placements

        rootNodeFields =
            [ ( "name", Encode.string "LDraw_Model" ) ]
                ++ (if List.isEmpty prims then
                        []

                    else
                        [ ( "mesh", Encode.int 0 ) ]
                   )
                ++ (if List.isEmpty childIndices then
                        []

                    else
                        [ ( "children", Encode.list Encode.int childIndices ) ]
                   )

        rootNode =
            Encode.object rootNodeFields

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
                  , Encode.list identity (rootNode :: placementNodes)
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
        jsonLen =
            EncodeBytes.getStringWidth jsonStr

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
            EncodeBytes.string jsonStr
                :: List.repeat jsonPadLen (EncodeBytes.unsignedInt8 0x20)

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
