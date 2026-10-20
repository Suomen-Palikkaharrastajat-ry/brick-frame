module LDraw.ExportTest exposing (suite)

{-| Unit tests for universal 3D serializers (glTF, Blender Cycles, USDZ).
-}

import Dict
import Expect
import LDraw.Document as Document
import LDraw.Export as Export exposing (defaultConfig)
import LDraw.Fixtures as Fixtures
import LDraw.Resolve as Resolve
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Export"
        [ describe "GlTF exporter"
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
            , test "encodes GLB binary container" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.studioModel

                        payload =
                            Export.exportGlTF doc Resolve.initCache defaultConfig
                    in
                    String.length payload.glbBase64 |> Expect.greaterThan 20
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
            ]
        , describe "USDZ AR QuickLook exporter"
            [ test "generates USDA ASCII format" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.smile

                        usda =
                            Export.exportUSDA doc Resolve.initCache defaultConfig
                    in
                    Expect.all
                        [ \u -> String.contains "#usda 1.0" u |> Expect.equal True
                        , \u -> String.contains "defaultPrim = \"Palikkakehys_Model\"" u |> Expect.equal True
                        , \u -> String.contains "metersPerUnit = 0.0004" u |> Expect.equal True
                        , \u -> String.contains "UsdPreviewSurface" u |> Expect.equal True
                        ]
                        usda
            , test "generates USDZ zip archive payload" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.smile

                        payload =
                            Export.exportUSDZ doc Resolve.initCache defaultConfig
                    in
                    Expect.all
                        [ \p -> Expect.equal "model.usdz" p.filename
                        , \p -> String.isEmpty p.usdzBase64 |> Expect.equal False
                        , \p -> String.contains "#usda 1.0" p.usda |> Expect.equal True
                        ]
                        payload
            ]
        ]
