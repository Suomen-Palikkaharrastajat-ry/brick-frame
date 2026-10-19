module LDraw.BOMTest exposing (suite)

{-| Tests for Bill of Materials (BOM) engine, aggregations, and marketplace export serializers.
-}

import Expect
import LDraw.BOM as BOM exposing (BOMDiff, ModelBOM, PartLot)
import LDraw.Catalog as Catalog exposing (ElementCatalog, PartIndex)
import LDraw.Document as Document
import Test exposing (Test, describe, test)


sampleModelLdr : String
sampleModelLdr =
    """0 Model With Mixed Bricks
0 Name: sample.ldr
1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 4 20 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 15 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 1 0 24 0 1 0 0 0 1 0 0 0 1 3023.dat
"""


sampleMpd : String
sampleMpd =
    """0 FILE main.ldr
0 Main Model
1 4 0 0 0 1 0 0 0 1 0 0 0 1 subassembly.ldr
1 1 20 0 0 1 0 0 0 1 0 0 0 1 subassembly.ldr
1 14 0 24 0 1 0 0 0 1 0 0 0 1 3001.dat

0 FILE subassembly.ldr
0 Subassembly with inherited color 16
1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 0 10 0 0 1 0 0 0 1 0 0 0 1 3023.dat
"""


testPartIndex : PartIndex
testPartIndex =
    Catalog.emptyPartIndex
        |> Catalog.insertPart "3001"
            { description = "Brick 2 x 4"
            , category = "Brick"
            , unitWeightGrams = Just 2.32
            , dimensionsLDU = Just ( 40.0, 24.0, 80.0 )
            }
        |> Catalog.insertPart "3023"
            { description = "Plate 1 x 2"
            , category = "Plate"
            , unitWeightGrams = Just 0.19
            , dimensionsLDU = Just ( 20.0, 8.0, 40.0 )
            }


testCatalog : ElementCatalog
testCatalog =
    Catalog.emptyCatalog
        |> Catalog.insertElement "3001"
            4
            { elementId = "300121"
            , bricklinkPartId = "3001"
            , bricklinkColorId = 5
            , colorName = "Red"
            }
        |> Catalog.insertElement "3001"
            15
            { elementId = "300101"
            , bricklinkPartId = "3001"
            , bricklinkColorId = 1
            , colorName = "White"
            }
        |> Catalog.insertElement "3001"
            1
            { elementId = "300123"
            , bricklinkPartId = "3001"
            , bricklinkColorId = 7
            , colorName = "Blue"
            }
        |> Catalog.insertElement "3023"
            1
            { elementId = "302323"
            , bricklinkPartId = "3023"
            , bricklinkColorId = 7
            , colorName = "Blue"
            }
        |> Catalog.insertElement "3023"
            0
            { elementId = "302326"
            , bricklinkPartId = "3023"
            , bricklinkColorId = 11
            , colorName = "Black"
            }


suite : Test
suite =
    describe "LDraw.BOM"
        [ describe "aggregation from simple document"
            [ test "aggregates correct total parts and unique lots" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog
                    in
                    Expect.all
                        [ \b -> Expect.equal 4 b.totalParts
                        , \b -> Expect.equal 3 b.uniqueLots
                        ]
                        bom
            , test "groups lots by part ID and color" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        red3001 =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 4) bom.lots
                                |> List.head

                        white3001 =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 15) bom.lots
                                |> List.head

                        blue3023 =
                            List.filter (\l -> l.partId == "3023" && l.colorId == 1) bom.lots
                                |> List.head
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just 2) (Maybe.map .count red3001)
                        , \_ -> Expect.equal (Just "300121") (Maybe.andThen .elementId red3001)
                        , \_ -> Expect.equal (Just 1) (Maybe.map .count white3001)
                        , \_ -> Expect.equal (Just "300101") (Maybe.andThen .elementId white3001)
                        , \_ -> Expect.equal (Just 1) (Maybe.map .count blue3023)
                        , \_ -> Expect.equal (Just "Plate 1 x 2") (Maybe.map .name blue3023)
                        ]
                        ()
            , test "calculates unit and total weight accurately" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        redLot =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 4) bom.lots
                                |> List.head
                    in
                    case redLot of
                        Just lot ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.01) 2.32 lot.unitWeightGrams
                                , \_ -> Expect.within (Expect.Absolute 0.01) 4.64 lot.totalWeightGrams
                                , \_ -> Expect.within (Expect.Absolute 0.01) (2.32 * 2 + 2.32 + 0.19) bom.estimatedWeightGrams
                                ]
                                ()

                        Nothing ->
                            Expect.fail "Red 3001 lot missing"
            , test "records placement IDs for viewport selection" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        redLot =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 4) bom.lots
                                |> List.head
                    in
                    case redLot of
                        Just lot ->
                            Expect.equal [ 0, 1 ] lot.placementIds

                        Nothing ->
                            Expect.fail "Red lot missing"
            ]
        , describe "MPD submodel traversal and color inheritance"
            [ test "expands nested submodels and propagates inherited parent color 16" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleMpd

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        -- Subassembly is placed twice: once with Red (4), once with Blue (1)
                        -- Inside subassembly: 1x 3001 (color 16), 1x 3023 (color 0)
                        -- Main model also has: 1x 3001 (Yellow, 14)
                        -- Expected leaf parts:
                        -- 1x 3001 Red (4)
                        -- 1x 3001 Blue (1)
                        -- 1x 3001 Yellow (14)
                        -- 2x 3023 Black (0)
                        red3001 =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 4) bom.lots
                                |> List.head

                        blue3001 =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 1) bom.lots
                                |> List.head

                        yellow3001 =
                            List.filter (\l -> l.partId == "3001" && l.colorId == 14) bom.lots
                                |> List.head

                        black3023 =
                            List.filter (\l -> l.partId == "3023" && l.colorId == 0) bom.lots
                                |> List.head
                    in
                    Expect.all
                        [ \b -> Expect.equal 5 b.totalParts
                        , \b -> Expect.equal 4 b.uniqueLots
                        , \_ -> Expect.equal (Just 1) (Maybe.map .count red3001)
                        , \_ -> Expect.equal (Just 1) (Maybe.map .count blue3001)
                        , \_ -> Expect.equal (Just 1) (Maybe.map .count yellow3001)
                        , \_ -> Expect.equal (Just 2) (Maybe.map .count black3023)
                        ]
                        bom
            ]
        , describe "marketplace serializers"
            [ test "serializes BrickLink Wanted List XML correctly" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        xml =
                            BOM.toBrickLinkXml bom
                    in
                    Expect.all
                        [ \s -> Expect.equal True (String.startsWith "<INVENTORY>\n" s)
                        , \s -> Expect.equal True (String.endsWith "</INVENTORY>\n" s)
                        , \s -> Expect.equal True (String.contains "<ITEMTYPE>P</ITEMTYPE>" s)
                        , \s -> Expect.equal True (String.contains "<ITEMID>3001</ITEMID>" s)
                        , \s -> Expect.equal True (String.contains "<COLOR>5</COLOR>" s)
                        , \s -> Expect.equal True (String.contains "<MINQTY>2</MINQTY>" s)
                        ]
                        xml
            , test "serializes Rebrickable CSV inventory correctly" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        csv =
                            BOM.toRebrickableCsv bom
                    in
                    Expect.all
                        [ \s -> Expect.equal True (String.startsWith "Part,Color,Quantity,Is Alternate\n" s)
                        , \s -> Expect.equal True (String.contains "3001,4,2,False" s)
                        , \s -> Expect.equal True (String.contains "3001,15,1,False" s)
                        , \s -> Expect.equal True (String.contains "3023,1,1,False" s)
                        ]
                        csv
            , test "formats Markdown table parts list" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        md =
                            BOM.toMarkdown bom
                    in
                    Expect.all
                        [ \s -> Expect.equal True (String.contains "# Bill of Materials" s)
                        , \s -> Expect.equal True (String.contains "| Part | Name | Category | Color | Element ID | Qty | Unit Wt (g) | Total Wt (g) |" s)
                        , \s -> Expect.equal True (String.contains "| 3001 | Brick 2 x 4 | Brick | Red (4) | 300121 | 2 | 2.32 | 4.64 |" s)
                        ]
                        md
            , test "formats plain text parts list" <|
                \_ ->
                    let
                        doc =
                            Document.parse sampleModelLdr

                        bom =
                            BOM.generateBOM doc testPartIndex testCatalog

                        txt =
                            BOM.toPlainText bom
                    in
                    Expect.all
                        [ \s -> Expect.equal True (String.contains "Bill of Materials" s)
                        , \s -> Expect.equal True (String.contains "- 2x 3001 Brick 2 x 4 - Red (Element: 300121) [4.64g]" s)
                        ]
                        txt
            ]
        , describe "revision diffing"
            [ test "correctly categorizes added, removed, and changed lots" <|
                \_ ->
                    let
                        oldDoc =
                            Document.parse sampleModelLdr

                        newText =
                            """0 Model Revised
0 Name: sample.ldr
1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat
1 2 50 0 0 1 0 0 0 1 0 0 0 1 3003.dat
"""

                        newDoc =
                            Document.parse newText

                        oldBom =
                            BOM.generateBOM oldDoc testPartIndex testCatalog

                        newBom =
                            BOM.generateBOM newDoc testPartIndex testCatalog

                        diff =
                            BOM.diffBOM oldBom newBom
                    in
                    Expect.all
                        [ \d -> Expect.equal -2 d.netPartsDiff
                        , \d -> Expect.equal 1 (List.length d.addedLots)
                        , \d -> Expect.equal 2 (List.length d.removedLots)
                        , \d -> Expect.equal 1 (List.length d.changedLots)
                        ]
                        diff
            ]
        ]
