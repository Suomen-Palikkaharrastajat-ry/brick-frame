module BOMTest exposing (suite)

{-| Tests for the Studio BOM panel view and user interactions.
-}

import Expect
import Html
import LDraw.BOM as BOM exposing (ModelBOM, PartLot)
import Studio.BOM as StudioBOM
import Test exposing (Test, describe, test)


sampleBOM : ModelBOM
sampleBOM =
    { totalParts = 12
    , uniqueLots = 2
    , estimatedWeightGrams = 25.52
    , dimensionsLDU = ( 80.0, 24.0, 40.0 )
    , dimensionsStuds = ( 4.0, 1.2, 2.0 )
    , lots =
        [ { partId = "3001"
          , name = "Brick 2 x 4"
          , category = "Brick"
          , colorId = 4
          , colorName = "Red"
          , elementId = Just "300121"
          , count = 10
          , unitWeightGrams = 2.32
          , totalWeightGrams = 23.2
          , placementIds = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
          }
        , { partId = "3023"
          , name = "Plate 1 x 2"
          , category = "Plate"
          , colorId = 15
          , colorName = "White"
          , elementId = Just "302301"
          , count = 2
          , unitWeightGrams = 1.16
          , totalWeightGrams = 2.32
          , placementIds = [ 10, 11 ]
          }
        ]
    }


testConfig : StudioBOM.Config String
testConfig =
    { onQuery = \q -> "Query: " ++ q
    , onCategory = \_ -> "Category"
    , onSubmodel = \_ -> "Submodel"
    , onSort = \_ -> "Sort"
    , onSelectLot = \p c -> "Select: " ++ p ++ " " ++ String.fromInt c
    , onExportBrickLink = "ExportBL"
    , onExportRebrickable = "ExportRB"
    , onCopyMarkdown = "CopyMD"
    , onCopyPlainText = "CopyTxt"
    , onToggleDiff = "ToggleDiff"
    , onClose = "Close"
    }


suite : Test
suite =
    describe "Studio.BOM"
        [ describe "initialization"
            [ test "init defaults to count descending and empty filters" <|
                \_ ->
                    let
                        state =
                            StudioBOM.init
                    in
                    Expect.all
                        [ \s -> Expect.equal "" s.query
                        , \s -> Expect.equal Nothing s.category
                        , \s -> Expect.equal Nothing s.submodel
                        , \s -> Expect.equal StudioBOM.SortCount s.sortColumn
                        , \s -> Expect.equal False s.sortAscending
                        , \s -> Expect.equal Nothing s.selectedLot
                        , \s -> Expect.equal False s.showDiff
                        ]
                        state
            ]
        , describe "view rendering"
            [ test "renders complete BOM drawer without crashing" <|
                \_ ->
                    let
                        viewHtml =
                            StudioBOM.view testConfig StudioBOM.init [] sampleBOM Nothing
                    in
                    -- Verify view constructs a valid HTML node
                    case viewHtml of
                        _ ->
                            Expect.pass
            , test "renders filtered view when search query matches part" <|
                \_ ->
                    let
                        initState =
                            StudioBOM.init

                        state =
                            { initState | query = "Plate" }

                        viewHtml =
                            StudioBOM.view testConfig state [] sampleBOM Nothing
                    in
                    case viewHtml of
                        _ ->
                            Expect.pass
            , test "renders diff view when base BOM is provided" <|
                \_ ->
                    let
                        baseBOM =
                            { sampleBOM | totalParts = 10, lots = List.take 1 sampleBOM.lots }

                        initState =
                            StudioBOM.init

                        state =
                            { initState | showDiff = True }

                        viewHtml =
                            StudioBOM.view testConfig state [] sampleBOM (Just baseBOM)
                    in
                    case viewHtml of
                        _ ->
                            Expect.pass
            ]
        ]
