module LDraw.ExportTest exposing (suite)

{-| Unit tests for universal 3D serializers (glTF, Blender Cycles, USDZ).
-}

import Dict
import Expect
import LDraw.Document as Document
import LDraw.Export as Export exposing (defaultConfig)
import LDraw.Export.Base64 as Base64
import LDraw.Fixtures as Fixtures
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import Test exposing (Test, describe, test)


resolvedBrickCache : PartCache
resolvedBrickCache =
    Dict.singleton "3001.dat"
        (Loaded
            (Parser.parseFile
                (String.join "\n"
                    [ "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10"
                    , "3 4 -10 -24 -10 10 -24 -10 10 -24 10"
                    ]
                )
            )
        )


suite : Test
suite =
    describe "LDraw.Export"
        [ describe "Base64 encoder"
            [ test "encodes ASCII and UTF-8 strings accurately" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "UGFsaWtrYWtlaHlz" (Base64.encodeString "Palikkakehys")
                        , \_ -> Expect.equal False (String.isEmpty (Base64.encodeString "ääöö"))
                        ]
                        ()
            ]
        , describe "GlTF exporter"
            [ test "serializes valid glTF 2.0 JSON structure" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.smile

                        payload =
                            Export.exportGlTF doc Resolve.initCache defaultConfig
                    in
                    Expect.all
                        [ \p -> String.contains "\"asset\"" p.json |> Expect.equal True
                        , \p -> String.contains "\"version\": \"2.0\"" p.json |> Expect.equal True
                        , \p -> String.contains "\"Palikkakehys" p.json |> Expect.equal True
                        , \p -> String.contains "\"scene\": 0" p.json |> Expect.equal True
                        , \p -> String.contains "\"materials\"" p.json |> Expect.equal True
                        , \p -> String.contains "\"data:application/octet-stream;base64," p.json |> Expect.equal True
                        , \p -> String.isEmpty p.glbBase64 |> Expect.equal False
                        , \p -> Expect.equal "model.glb" p.filename
                        ]
                        payload
            , test "classifies transparent and metallic LDraw materials and emits placement matrices" <|
                \_ ->
                    let
                        -- Color 47 = Trans-Clear (alpha < 0.95 -> BLEND), Color 80 = Metallic Silver
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 47 20 -24 40 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "1 80 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    ]
                                )

                        payload =
                            Export.exportGlTF doc Resolve.initCache defaultConfig
                    in
                    Expect.all
                        [ \p -> String.contains "\"alphaMode\": \"BLEND\"" p.json |> Expect.equal True
                        , \p -> String.contains "\"roughnessFactor\": 0.05" p.json |> Expect.equal True
                        , \p -> String.contains "\"metallicFactor\": 0.9" p.json |> Expect.equal True
                        , \p -> String.contains "\"Part_0_3001.dat\"" p.json |> Expect.equal True
                        , \p -> String.contains "\"matrix\"" p.json |> Expect.equal True
                        ]
                        payload
            , test "exports resolved mesh primitives with meter-scaled bounds" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"

                        payload =
                            Export.exportGlTF doc resolvedBrickCache defaultConfig
                    in
                    Expect.all
                        [ \p -> String.contains "\"POSITION\": 0" p.json |> Expect.equal True
                        , \p -> String.contains "\"NORMAL\": 1" p.json |> Expect.equal True
                        , \p -> String.contains "\"min\"" p.json |> Expect.equal True
                        , \p -> String.contains "-0.004" p.json |> Expect.equal True
                        , \p -> String.contains "0.004" p.json |> Expect.equal True
                        , \p -> String.length payload.glbBase64 |> Expect.greaterThan 100
                        ]
                        payload
            ]
        , describe "Blender Cycles exporter"
            [ test "generates Python script with Bevel shader node and Cycles setup" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.smile

                        script =
                            Export.exportBlenderScript doc defaultConfig
                    in
                    Expect.all
                        [ \s -> String.contains "import bpy" s |> Expect.equal True
                        , \s -> String.contains "'CYCLES'" s |> Expect.equal True
                        , \s -> String.contains "ShaderNodeBevel" s |> Expect.equal True
                        , \s -> String.contains "0.0004" s |> Expect.equal True
                        , \s -> String.contains "Key_Light" s |> Expect.equal True
                        , \s -> String.contains "Fill_Light" s |> Expect.equal True
                        , \s -> String.contains "Rim_Light" s |> Expect.equal True
                        , \s -> String.contains "ShaderNodeBsdfPrincipled" s |> Expect.equal True
                        ]
                        script
            , test "escapes quotes in part and submodel names" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 user's_part.dat\n"

                        script =
                            Export.exportBlenderScript doc defaultConfig
                    in
                    String.contains "Part_0_user\\'s_part.dat" script
                        |> Expect.equal True
            ]
        , describe "USDZ AR QuickLook exporter"
            [ test "sanitizes USD prim identifiers starting with digits or punctuation" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "Part_3001_dat" (Export.sanitizeUsdIdentifier "3001.dat")
                        , \_ -> Expect.equal "s_3001s01_dat" (Export.sanitizeUsdIdentifier "s\\3001s01.dat")
                        , \_ -> Expect.equal "Part_Empty" (Export.sanitizeUsdIdentifier "")
                        ]
                        ()
            , test "generates USDA ASCII format with sanitized prim names and transparent IOR" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 47 0 -24 0 1 0 0 0 1 0 0 0 1 3001.dat\n"

                        usda =
                            Export.exportUSDA doc Resolve.initCache defaultConfig
                    in
                    Expect.all
                        [ \u -> String.contains "#usda 1.0" u |> Expect.equal True
                        , \u -> String.contains "defaultPrim = \"Palikkakehys_Model\"" u |> Expect.equal True
                        , \u -> String.contains "metersPerUnit = 0.0004" u |> Expect.equal True
                        , \u -> String.contains "UsdPreviewSurface" u |> Expect.equal True
                        , \u -> String.contains "inputs:ior = 1.49" u |> Expect.equal True
                        , \u -> String.contains "def Xform \"Part_0_3001_dat\"" u |> Expect.equal True
                        ]
                        usda
            , test "generates USDZ zip archive payload with resolved geometry" <|
                \_ ->
                    let
                        doc =
                            Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"

                        payload =
                            Export.exportUSDZ doc resolvedBrickCache defaultConfig
                    in
                    Expect.all
                        [ \p -> Expect.equal "model.usdz" p.filename
                        , \p -> String.isEmpty p.usdzBase64 |> Expect.equal False
                        , \p -> String.contains "def Mesh \"Mesh_0\"" p.usda |> Expect.equal True
                        , \p -> String.contains "uniform bool doubleSided = 1" p.usda |> Expect.equal True
                        ]
                        payload
            ]
        ]
