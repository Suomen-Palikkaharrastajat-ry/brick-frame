module LDraw.Export.USDZ exposing
    ( ExportConfig
    , UsdzPayload
    , defaultConfig
    , exportUSDA
    , exportUSDZ
    , sanitizeUsdIdentifier
    )

{-| USDA and USDZ serializer for Apple AR QuickLook on iOS.

Generates:

  - USDA 1.0 human-readable Universal Scene Description text
  - USDZ archive container (uncompressed ZIP with 64-byte file alignment)
    conforming to Apple QuickLook specifications for 1:1 real-scale AR preview.

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as EncodeBytes exposing (Encoder)
import Dict exposing (Dict)
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Export.Base64 as Base64
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Render.Mesh exposing (Vertex)


{-| Export configuration.
-}
type alias ExportConfig =
    { includeStudLogos : Bool
    , weldVertices : Bool
    , groupSubmodels : Bool
    }


{-| Default export configuration.
-}
defaultConfig : ExportConfig
defaultConfig =
    { includeStudLogos = True
    , weldVertices = True
    , groupSubmodels = True
    }


{-| Result of exporting to USDZ.
-}
type alias UsdzPayload =
    { usda : String
    , usdzBase64 : String
    , filename : String
    }


{-| Sanitize an arbitrary string (such as an LDraw part filename `3001.dat` or
`s\3001s01.dat`) into a valid USD prim identifier (`[A-Za-z_][A-Za-z0-9_]*`).
-}
sanitizeUsdIdentifier : String -> String
sanitizeUsdIdentifier raw =
    let
        cleaned =
            raw
                |> String.toList
                |> List.map
                    (\c ->
                        if Char.isAlphaNum c || c == '_' then
                            c

                        else
                            '_'
                    )
                |> String.fromList
    in
    case String.uncons cleaned of
        Nothing ->
            "Part_Empty"

        Just ( first, _ ) ->
            if Char.isAlpha first || first == '_' then
                cleaned

            else
                "Part_" ++ cleaned


{-| Export an LDraw document to USDA text format.
-}
exportUSDA : Document -> PartCache -> ExportConfig -> String
exportUSDA doc cache config =
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
            List.map (\p -> Colors.toVec4 (Colors.resolveColor 16 p.color)) placements

        allColors =
            if List.isEmpty groupsList then
                placementColors
                    |> List.foldl (\c dict -> Dict.insert (colorKey c) c dict) Dict.empty
                    |> Dict.values

            else
                List.map (\( _, verts ) -> firstColor verts) groupsList

        materialDefinitions =
            allColors
                |> List.indexedMap (\idx color -> buildUsdMaterial idx color)
                |> String.join "\n"

        meshDefinitions =
            if List.isEmpty groupsList then
                placements
                    |> List.indexedMap buildUsdPlacementXform
                    |> String.join "\n"

            else
                groupsList
                    |> List.indexedMap (\idx ( _, verts ) -> buildUsdMesh idx verts)
                    |> String.join "\n"
    in
    String.join "\n"
        [ "#usda 1.0"
        , "("
        , "    defaultPrim = \"Palikkakehys_Model\""
        , "    metersPerUnit = 0.0004" -- 1 LDU = 0.4mm = 0.0004m
        , "    upAxis = \"Y\""
        , ")"
        , ""
        , "def Xform \"Palikkakehys_Model\" ("
        , "    kind = \"component\""
        , ")"
        , "{"
        , "    def Scope \"Materials\""
        , "    {"
        , materialDefinitions
        , "    }"
        , ""
        , "    def Scope \"Geom\""
        , "    {"
        , meshDefinitions
        , "    }"
        , "}"
        ]


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


buildUsdMaterial : Int -> Vec4 -> String
buildUsdMaterial idx colorVec =
    let
        matName =
            "Material_" ++ String.fromInt idx

        r =
            Vec4.getX colorVec

        g =
            Vec4.getY colorVec

        b =
            Vec4.getZ colorVec

        a =
            Vec4.getW colorVec

        isTransparent =
            a < 0.95

        ( roughness, metallic ) =
            if isTransparent then
                ( "0.05", "0.0" )

            else if r > 0.7 && g > 0.7 && b > 0.7 then
                ( "0.10", "0.9" )

            else
                ( "0.25", "0.0" )

        iorLines =
            if isTransparent then
                [ "                float inputs:ior = 1.49"
                , "                float inputs:opacityThreshold = 0.0"
                ]

            else
                []
    in
    String.join "\n"
        ([ "        def Material \"" ++ matName ++ "\""
         , "        {"
         , "            def Shader \"PreviewShader\""
         , "            {"
         , "                uniform token info:id = \"UsdPreviewSurface\""
         , "                color3f inputs:diffuseColor = ("
            ++ String.fromFloat r
            ++ ", "
            ++ String.fromFloat g
            ++ ", "
            ++ String.fromFloat b
            ++ ")"
         , "                float inputs:roughness = " ++ roughness
         , "                float inputs:metallic = " ++ metallic
         , "                float inputs:opacity = " ++ String.fromFloat a
         ]
            ++ iorLines
            ++ [ "                token outputs:surface"
               , "            }"
               , "            token outputs:surface.connect = </Palikkakehys_Model/Materials/"
                    ++ matName
                    ++ "/PreviewShader.outputs:surface>"
               , "        }"
               ]
        )


buildUsdPlacementXform : Int -> Placement -> String
buildUsdPlacementXform idx placement =
    let
        primName =
            sanitizeUsdIdentifier (String.fromInt idx ++ "_" ++ placement.part)

        rec =
            Mat4.toRecord placement.matrix
    in
    String.join "\n"
        [ "        def Xform \"" ++ primName ++ "\""
        , "        {"
        , "            double3 xformOp:translate = ("
            ++ String.fromFloat rec.m41
            ++ ", "
            ++ String.fromFloat -rec.m42
            ++ ", "
            ++ String.fromFloat -rec.m43
            ++ ")"
        , "            uniform token[] xformOpOrder = [\"xformOp:translate\"]"
        , "        }"
        ]


buildUsdMesh : Int -> List Vertex -> String
buildUsdMesh idx verts =
    let
        meshName =
            "Mesh_" ++ String.fromInt idx

        matName =
            "Material_" ++ String.fromInt idx

        pointStrings =
            List.map
                (\v ->
                    "("
                        ++ String.fromFloat (Vec3.getX v.position)
                        ++ ", "
                        ++ String.fromFloat (Vec3.getY v.position)
                        ++ ", "
                        ++ String.fromFloat (Vec3.getZ v.position)
                        ++ ")"
                )
                verts

        normalStrings =
            List.map
                (\v ->
                    "("
                        ++ String.fromFloat (Vec3.getX v.normal)
                        ++ ", "
                        ++ String.fromFloat (Vec3.getY v.normal)
                        ++ ", "
                        ++ String.fromFloat (Vec3.getZ v.normal)
                        ++ ")"
                )
                verts

        vCount =
            List.length verts

        indices =
            List.map String.fromInt (List.range 0 (vCount - 1))

        faceCount =
            vCount // 3

        faceVertexCounts =
            List.repeat faceCount "3"
    in
    String.join "\n"
        [ "        def Mesh \"" ++ meshName ++ "\""
        , "        {"
        , "            uniform bool doubleSided = 1"
        , "            rel material:binding = </Palikkakehys_Model/Materials/" ++ matName ++ ">"
        , "            point3f[] points = [" ++ String.join ", " pointStrings ++ "]"
        , "            normal3f[] normals = [" ++ String.join ", " normalStrings ++ "] ("
        , "                interpolation = \"vertex\""
        , "            )"
        , "            int[] faceVertexIndices = [" ++ String.join ", " indices ++ "]"
        , "            int[] faceVertexCounts = [" ++ String.join ", " faceVertexCounts ++ "]"
        , "        }"
        ]


{-| Export an LDraw document to USDZ archive format for iOS AR QuickLook.
-}
exportUSDZ : Document -> PartCache -> ExportConfig -> UsdzPayload
exportUSDZ doc cache config =
    let
        usdaText =
            exportUSDA doc cache config

        usdzBase64 =
            buildUsdzZip "model.usda" usdaText
    in
    { usda = usdaText
    , usdzBase64 = usdzBase64
    , filename = "model.usdz"
    }


utf8StringToByteList : String -> List Int
utf8StringToByteList str =
    let
        encoded =
            EncodeBytes.encode (EncodeBytes.string str)

        width =
            Bytes.width encoded

        decoder : Int -> Decoder (List Int)
        decoder count =
            Decode.loop ( count, [] )
                (\( remaining, acc ) ->
                    if remaining <= 0 then
                        Decode.succeed (Decode.Done (List.reverse acc))

                    else
                        Decode.map (\b -> Decode.Loop ( remaining - 1, b :: acc )) Decode.unsignedInt8
                )
    in
    Decode.decode (decoder width) encoded
        |> Maybe.withDefault []


{-| Build an uncompressed ZIP archive with 64-byte alignment conforming to Apple USDZ specs.
-}
buildUsdzZip : String -> String -> String
buildUsdzZip filename contentStr =
    let
        contentBytesList =
            utf8StringToByteList contentStr

        contentLen =
            List.length contentBytesList

        filenameByteList =
            utf8StringToByteList filename

        filenameLen =
            List.length filenameByteList

        crc =
            calculateCrc32 contentBytesList

        -- Apple USDZ requires file data to be aligned to a 64-byte boundary.
        -- Local file header is 30 bytes + filenameLen + extraLen.
        headerBaseLen =
            30 + filenameLen

        extraPad =
            modBy 64 headerBaseLen

        extraLen =
            if extraPad == 0 then
                0

            else
                64 - extraPad

        localHeader =
            [ EncodeBytes.unsignedInt32 LE 0x04034B50 -- Local file header signature
            , EncodeBytes.unsignedInt16 LE 20 -- Version needed to extract
            , EncodeBytes.unsignedInt16 LE 0 -- General purpose bit flag
            , EncodeBytes.unsignedInt16 LE 0 -- Compression method (0 = STORE)
            , EncodeBytes.unsignedInt16 LE 0 -- Last mod time
            , EncodeBytes.unsignedInt16 LE 0 -- Last mod date
            , EncodeBytes.unsignedInt32 LE crc
            , EncodeBytes.unsignedInt32 LE contentLen
            , EncodeBytes.unsignedInt32 LE contentLen
            , EncodeBytes.unsignedInt16 LE filenameLen
            , EncodeBytes.unsignedInt16 LE extraLen
            ]
                ++ List.map (\c -> EncodeBytes.unsignedInt8 c) filenameByteList
                ++ List.repeat extraLen (EncodeBytes.unsignedInt8 0)

        fileDataEncoders =
            List.map (\c -> EncodeBytes.unsignedInt8 c) contentBytesList

        localHeaderLen =
            headerBaseLen + extraLen

        centralDirOffset =
            localHeaderLen + contentLen

        centralDirHeader =
            [ EncodeBytes.unsignedInt32 LE 0x02014B50 -- Central directory file header signature
            , EncodeBytes.unsignedInt16 LE 20 -- Version made by
            , EncodeBytes.unsignedInt16 LE 20 -- Version needed
            , EncodeBytes.unsignedInt16 LE 0 -- Bit flag
            , EncodeBytes.unsignedInt16 LE 0 -- Compression (0 = STORE)
            , EncodeBytes.unsignedInt16 LE 0 -- Mod time
            , EncodeBytes.unsignedInt16 LE 0 -- Mod date
            , EncodeBytes.unsignedInt32 LE crc
            , EncodeBytes.unsignedInt32 LE contentLen
            , EncodeBytes.unsignedInt32 LE contentLen
            , EncodeBytes.unsignedInt16 LE filenameLen
            , EncodeBytes.unsignedInt16 LE 0 -- Extra field len
            , EncodeBytes.unsignedInt16 LE 0 -- Comment len
            , EncodeBytes.unsignedInt16 LE 0 -- Disk number start
            , EncodeBytes.unsignedInt16 LE 0 -- Internal file attr
            , EncodeBytes.unsignedInt32 LE 0 -- External file attr
            , EncodeBytes.unsignedInt32 LE 0 -- Relative offset of local header
            ]
                ++ List.map (\c -> EncodeBytes.unsignedInt8 c) filenameByteList

        centralDirLen =
            46 + filenameLen

        endOfCentralDir =
            [ EncodeBytes.unsignedInt32 LE 0x06054B50 -- End of central dir signature
            , EncodeBytes.unsignedInt16 LE 0 -- Number of this disk
            , EncodeBytes.unsignedInt16 LE 0 -- Disk with central dir
            , EncodeBytes.unsignedInt16 LE 1 -- Total entries on disk
            , EncodeBytes.unsignedInt16 LE 1 -- Total entries
            , EncodeBytes.unsignedInt32 LE centralDirLen
            , EncodeBytes.unsignedInt32 LE centralDirOffset
            , EncodeBytes.unsignedInt16 LE 0 -- Zip comment len
            ]

        allEncoders =
            localHeader ++ fileDataEncoders ++ centralDirHeader ++ endOfCentralDir

        zipBytes =
            EncodeBytes.encode (EncodeBytes.sequence allEncoders)
    in
    Base64.encodeBytes zipBytes


{-| Calculate CRC-32 checksum for ZIP headers.
-}
calculateCrc32 : List Int -> Int
calculateCrc32 bytes =
    let
        step byte crc =
            let
                c =
                    Bitwise.xor (Bitwise.and crc 0xFF) byte

                fold8 _ current =
                    if Bitwise.and current 1 /= 0 then
                        Bitwise.xor (Bitwise.shiftRightZfBy 1 current) 0xEDB88320

                    else
                        Bitwise.shiftRightZfBy 1 current

                crcEntry =
                    List.foldl fold8 c (List.range 0 7)
            in
            Bitwise.xor (Bitwise.shiftRightZfBy 8 crc) crcEntry
    in
    Bitwise.xor (List.foldl step 0xFFFFFFFF bytes) 0xFFFFFFFF
