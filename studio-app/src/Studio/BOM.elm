module Studio.BOM exposing
    ( Config
    , SortColumn(..)
    , State
    , init
    , view
    )

{-| Interactive Bill of Materials (BOM) modal and drawer view.

Provides:

  - Summary metrics (total parts, unique lots, estimated weight, dimensions)
  - Search filtering by part name, ID, or category
  - Submodel filtering
  - Column-based sorting
  - Element ID and color swatch presentation
  - Instance highlighting synchronization with the 3D viewport
  - Direct export buttons for BrickLink XML and Rebrickable CSV
  - Clipboard copy for Markdown table and plain text parts lists
  - Optional revision diff view

-}

import Html exposing (Html, button, div, input, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.BOM as BOM exposing (BOMDiff, DiffLot, ModelBOM, PartLot)
import LDraw.Catalog as Catalog
import LDraw.Colors as Colors
import Studio.Colours as Colours
import UI.Button as Button
import UI.Theme as Theme


{-| Available columns to sort the inventory table by.
-}
type SortColumn
    = SortPartId
    | SortName
    | SortCategory
    | SortColor
    | SortElementId
    | SortCount
    | SortUnitWeight
    | SortTotalWeight


{-| Interactive state of the BOM drawer/modal.
-}
type alias State =
    { query : String
    , category : Maybe String
    , submodel : Maybe String
    , sortColumn : SortColumn
    , sortAscending : Bool
    , selectedLot : Maybe ( String, Int )
    , showDiff : Bool
    }


{-| Initial default state: sorted by count descending.
-}
init : State
init =
    { query = ""
    , category = Nothing
    , submodel = Nothing
    , sortColumn = SortCount
    , sortAscending = False
    , selectedLot = Nothing
    , showDiff = False
    }


{-| Callbacks dispatched by the BOM panel.
-}
type alias Config msg =
    { onQuery : String -> msg
    , onCategory : Maybe String -> msg
    , onSubmodel : Maybe String -> msg
    , onSort : SortColumn -> msg
    , onSelectLot : String -> Int -> msg
    , onExportBrickLink : msg
    , onExportRebrickable : msg
    , onCopyMarkdown : msg
    , onCopyPlainText : msg
    , onToggleDiff : msg
    , onClose : msg
    }


{-| Main view for the BOM panel. Renders as a modal overlay drawer.
-}
view : Config msg -> State -> List String -> ModelBOM -> Maybe ModelBOM -> Html msg
view config state submodelNames currentBom maybeBaseBom =
    let
        filteredLots =
            currentBom.lots
                |> filterLots state
                |> sortLots state.sortColumn state.sortAscending

        categories =
            currentBom.lots
                |> List.map .category
                |> List.foldl
                    (\cat acc ->
                        if List.member cat acc then
                            acc

                        else
                            cat :: acc
                    )
                    []
                |> List.sort

        maybeDiff =
            maybeBaseBom
                |> Maybe.map (\baseBom -> BOM.diffBOM baseBom currentBom)
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "z-index" "50"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "background" "color-mix(in srgb, var(--color-bg-dark) 40%, transparent)"
        , Attr.style "backdrop-filter" "blur(4px)"
        , Attr.style "padding" "16px"
        , Attr.style "pointer-events" "auto"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "8px"
            , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.2)"
            , Attr.style "width" "100%"
            , Attr.style "max-width" "1100px"
            , Attr.style "height" "90vh"
            , Attr.style "margin" "auto"
            , Attr.style "overflow" "hidden"
            ]
            [ viewHeader config currentBom
            , viewSummaryMetrics currentBom
            , viewControls config state submodelNames categories maybeDiff
            , if state.showDiff then
                case maybeDiff of
                    Just diff ->
                        viewDiffTable diff

                    Nothing ->
                        viewInventoryTable config state filteredLots

              else
                viewInventoryTable config state filteredLots
            ]
        ]


viewHeader : Config msg -> ModelBOM -> Html msg
viewHeader config bom =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "padding" "12px 16px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "background" Theme.panelSubtleBackground
        ]
        [ div [ Attr.style "display" "flex", Attr.style "align-items" "baseline", Attr.style "gap" "10px" ]
            [ span
                [ Attr.style "font-size" "18px"
                , Attr.style "font-weight" "700"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text "Bill of Materials (BOM)" ]
            , span
                [ Attr.style "font-size" "12px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text (String.fromInt bom.totalParts ++ " total parts • " ++ String.fromInt bom.uniqueLots ++ " unique lots") ]
            ]
        , button
            [ Html.Events.onClick config.onClose
            , Attr.title "Close BOM"
            , Attr.attribute "aria-label" "Close"
            , Attr.style "background" "transparent"
            , Attr.style "border" "none"
            , Attr.style "font-size" "20px"
            , Attr.style "color" Theme.textMuted
            , Attr.style "cursor" "pointer"
            , Attr.style "padding" "4px 8px"
            ]
            [ text "✕" ]
        ]


viewSummaryMetrics : ModelBOM -> Html msg
viewSummaryMetrics bom =
    let
        ( lduX, lduY, lduZ ) =
            bom.dimensionsLDU

        ( sx, sy, sz ) =
            bom.dimensionsStuds

        cmX =
            round2 (lduX * 0.04)

        cmY =
            round2 (lduY * 0.04)

        cmZ =
            round2 (lduZ * 0.04)

        weightText =
            if bom.estimatedWeightGrams >= 1000.0 then
                String.fromFloat (round2 (bom.estimatedWeightGrams / 1000.0)) ++ " kg"

            else
                String.fromFloat bom.estimatedWeightGrams ++ " g"

        dimText =
            String.fromFloat cmX
                ++ " × "
                ++ String.fromFloat cmY
                ++ " × "
                ++ String.fromFloat cmZ
                ++ " cm ("
                ++ String.fromFloat sx
                ++ " × "
                ++ String.fromFloat sy
                ++ " × "
                ++ String.fromFloat sz
                ++ " studs)"

        metricCard label val =
            div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "2px"
                , Attr.style "padding" "8px 12px"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "6px"
                , Attr.style "flex" "1"
                , Attr.style "min-width" "130px"
                ]
                [ span [ Attr.style "font-size" "11px", Attr.style "color" Theme.textMuted, Attr.style "text-transform" "uppercase" ] [ text label ]
                , span [ Attr.style "font-size" "15px", Attr.style "font-weight" "600", Attr.style "color" Theme.textPrimary ] [ text val ]
                ]
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "12px 16px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "background" Theme.panelSubtleBackground
        ]
        [ metricCard "Total Parts" (String.fromInt bom.totalParts)
        , metricCard "Unique Lots" (String.fromInt bom.uniqueLots)
        , metricCard "Estimated Weight" weightText
        , metricCard "Dimensions" dimText
        ]


viewControls : Config msg -> State -> List String -> List String -> Maybe BOMDiff -> Html msg
viewControls config state submodels categories maybeDiff =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "10px 16px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        ]
        [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "8px", Attr.style "flex-wrap" "wrap" ]
            [ input
                [ Attr.type_ "search"
                , Attr.placeholder "Search part, color, element..."
                , Attr.attribute "aria-label" "Search inventory"
                , Attr.value state.query
                , Html.Events.onInput config.onQuery
                , Attr.style "padding" "6px 10px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "4px"
                , Attr.style "font-size" "13px"
                , Attr.style "color" Theme.textPrimary
                , Attr.style "background" Theme.panelSurface
                , Attr.style "width" "200px"
                ]
                []
            , select
                [ Html.Events.on "change"
                    (Decode.map
                        (\val ->
                            config.onCategory
                                (if val == "" then
                                    Nothing

                                 else
                                    Just val
                                )
                        )
                        (Decode.at [ "target", "value" ] Decode.string)
                    )
                , Attr.attribute "aria-label" "Filter Category"
                , Attr.style "padding" "6px 10px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "4px"
                , Attr.style "font-size" "13px"
                , Attr.style "color" Theme.textPrimary
                , Attr.style "background" Theme.panelSurface
                ]
                (option [ Attr.value "", Attr.selected (state.category == Nothing) ] [ text "All Categories" ]
                    :: List.map
                        (\cat ->
                            option [ Attr.value cat, Attr.selected (state.category == Just cat) ] [ text cat ]
                        )
                        categories
                )
            , if List.isEmpty submodels then
                text ""

              else
                select
                    [ Html.Events.on "change"
                        (Decode.map
                            (\val ->
                                config.onSubmodel
                                    (if val == "" then
                                        Nothing

                                     else
                                        Just val
                                    )
                            )
                            (Decode.at [ "target", "value" ] Decode.string)
                        )
                    , Attr.attribute "aria-label" "Filter Submodel"
                    , Attr.style "padding" "6px 10px"
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "4px"
                    , Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "background" Theme.panelSurface
                    ]
                    (option [ Attr.value "", Attr.selected (state.submodel == Nothing) ] [ text "Entire Model" ]
                        :: List.map
                            (\sub ->
                                option [ Attr.value sub, Attr.selected (state.submodel == Just sub) ] [ text sub ]
                            )
                            submodels
                    )
            , case maybeDiff of
                Just _ ->
                    Button.secondary config.onToggleDiff
                        (if state.showDiff then
                            "Show Inventory"

                         else
                            "Show Revision Diff"
                        )

                Nothing ->
                    text ""
            ]
        , div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "6px", Attr.style "flex-wrap" "wrap" ]
            [ Button.secondary config.onExportBrickLink "BrickLink XML"
            , Button.secondary config.onExportRebrickable "Rebrickable CSV"
            , Button.secondary config.onCopyMarkdown "Copy Markdown"
            , Button.secondary config.onCopyPlainText "Copy Text"
            ]
        ]


viewInventoryTable : Config msg -> State -> List PartLot -> Html msg
viewInventoryTable config state lots =
    let
        sortHeader col label =
            let
                arrow =
                    if state.sortColumn == col then
                        if state.sortAscending then
                            " ▲"

                        else
                            " ▼"

                    else
                        ""
            in
            th
                [ Html.Events.onClick (config.onSort col)
                , Attr.style "padding" "8px 10px"
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" Theme.textMuted
                , Attr.style "text-align" "left"
                , Attr.style "cursor" "pointer"
                , Attr.style "user-select" "none"
                , Attr.style "white-space" "nowrap"
                , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ text (label ++ arrow) ]

        numericHeader col label =
            let
                arrow =
                    if state.sortColumn == col then
                        if state.sortAscending then
                            " ▲"

                        else
                            " ▼"

                    else
                        ""
            in
            th
                [ Html.Events.onClick (config.onSort col)
                , Attr.style "padding" "8px 10px"
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" Theme.textMuted
                , Attr.style "text-align" "right"
                , Attr.style "cursor" "pointer"
                , Attr.style "user-select" "none"
                , Attr.style "white-space" "nowrap"
                , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ text (label ++ arrow) ]

        lotRow lot =
            let
                isSelected =
                    state.selectedLot == Just ( lot.partId, lot.colorId )

                rgba =
                    Colors.resolveColor 16 lot.colorId

                swatchColor =
                    Colours.css rgba

                elemText =
                    Maybe.withDefault "—" lot.elementId

                bgColor =
                    if isSelected then
                        "color-mix(in srgb, var(--color-brand-yellow) 25%, var(--color-bg-page))"

                    else
                        "transparent"
            in
            tr
                [ Html.Events.onClick (config.onSelectLot lot.partId lot.colorId)
                , Attr.style "cursor" "pointer"
                , Attr.style "background" bgColor
                , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ td [ Attr.style "padding" "6px 10px", Attr.style "width" "28px" ]
                    [ div
                        [ Attr.title (lot.colorName ++ " (" ++ String.fromInt lot.colorId ++ ")")
                        , Attr.style "width" "18px"
                        , Attr.style "height" "18px"
                        , Attr.style "border-radius" "3px"
                        , Attr.style "background" swatchColor
                        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                        ]
                        []
                    ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-family" "monospace", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                    [ text lot.partId ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "13px", Attr.style "color" Theme.textPrimary ]
                    [ text lot.name ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "color" Theme.textMuted ]
                    [ text lot.category ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                    [ text (lot.colorName ++ " (" ++ String.fromInt lot.colorId ++ ")") ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-family" "monospace", Attr.style "font-size" "12px", Attr.style "color" Theme.textMuted ]
                    [ text elemText ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "13px", Attr.style "font-weight" "600", Attr.style "text-align" "right", Attr.style "color" Theme.textPrimary ]
                    [ text (String.fromInt lot.count) ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "right", Attr.style "color" Theme.textMuted ]
                    [ text (String.fromFloat lot.unitWeightGrams ++ " g") ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "font-weight" "600", Attr.style "text-align" "right", Attr.style "color" Theme.textPrimary ]
                    [ text (String.fromFloat lot.totalWeightGrams ++ " g") ]
                ]
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "0 16px 16px 16px"
        ]
        [ table
            [ Attr.style "width" "100%"
            , Attr.style "border-collapse" "collapse"
            , Attr.style "text-align" "left"
            ]
            [ thead
                [ Attr.style "position" "sticky"
                , Attr.style "top" "0"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "z-index" "1"
                ]
                [ tr []
                    [ th [ Attr.style "padding" "8px 10px", Attr.style "width" "28px" ] []
                    , sortHeader SortPartId "Part ID"
                    , sortHeader SortName "Name"
                    , sortHeader SortCategory "Category"
                    , sortHeader SortColor "Color"
                    , sortHeader SortElementId "Element ID"
                    , numericHeader SortCount "Qty"
                    , numericHeader SortUnitWeight "Unit Wt"
                    , numericHeader SortTotalWeight "Total Wt"
                    ]
                ]
            , tbody [] (List.map lotRow lots)
            ]
        ]


viewDiffTable : BOMDiff -> Html msg
viewDiffTable diff =
    let
        diffRow badgeClass badgeText lot =
            tr
                [ Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ td [ Attr.style "padding" "6px 10px", Attr.style "font-weight" "700" ]
                    [ span
                        [ Attr.style "padding" "2px 6px"
                        , Attr.style "border-radius" "4px"
                        , Attr.style "font-size" "11px"
                        , Attr.style "background" badgeClass
                        ]
                        [ text badgeText ]
                    ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-family" "monospace", Attr.style "font-size" "12px" ] [ text lot.partId ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "13px" ] [ text lot.name ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px" ] [ text lot.colorName ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "text-align" "right", Attr.style "font-size" "12px" ] [ text (String.fromInt lot.oldCount) ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "text-align" "right", Attr.style "font-size" "12px" ] [ text (String.fromInt lot.newCount) ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "text-align" "right", Attr.style "font-weight" "600", Attr.style "font-size" "13px" ]
                    [ text
                        (if lot.diff > 0 then
                            "+" ++ String.fromInt lot.diff

                         else
                            String.fromInt lot.diff
                        )
                    ]
                ]

        greenBadge =
            "color-mix(in srgb, #22C55E 20%, transparent)"

        redBadge =
            "color-mix(in srgb, var(--color-brand-red) 20%, transparent)"

        amberBadge =
            "color-mix(in srgb, var(--color-brand-yellow) 25%, transparent)"

        rows =
            List.map (diffRow greenBadge "ADDED") diff.addedLots
                ++ List.map (diffRow redBadge "REMOVED") diff.removedLots
                ++ List.map (diffRow amberBadge "CHANGED") diff.changedLots
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "0 16px 16px 16px"
        ]
        [ div [ Attr.style "padding" "10px 0", Attr.style "font-size" "13px", Attr.style "color" Theme.textMuted ]
            [ text
                ("Revision Comparison: "
                    ++ String.fromInt (List.length diff.addedLots)
                    ++ " lots added, "
                    ++ String.fromInt (List.length diff.removedLots)
                    ++ " removed, "
                    ++ String.fromInt (List.length diff.changedLots)
                    ++ " changed. Net difference: "
                    ++ String.fromInt diff.netPartsDiff
                    ++ " parts."
                )
            ]
        , table
            [ Attr.style "width" "100%"
            , Attr.style "border-collapse" "collapse"
            ]
            [ thead
                [ Attr.style "position" "sticky"
                , Attr.style "top" "0"
                , Attr.style "background" Theme.panelSurface
                ]
                [ tr []
                    [ th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "left", Attr.style "color" Theme.textMuted ] [ text "Status" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "left", Attr.style "color" Theme.textMuted ] [ text "Part ID" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "left", Attr.style "color" Theme.textMuted ] [ text "Name" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "left", Attr.style "color" Theme.textMuted ] [ text "Color" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "right", Attr.style "color" Theme.textMuted ] [ text "Old Qty" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "right", Attr.style "color" Theme.textMuted ] [ text "New Qty" ]
                    , th [ Attr.style "padding" "8px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "right", Attr.style "color" Theme.textMuted ] [ text "Diff" ]
                    ]
                ]
            , tbody [] rows
            ]
        ]


filterLots : State -> List PartLot -> List PartLot
filterLots state lots =
    let
        query =
            String.toLower (String.trim state.query)

        matchesQuery lot =
            if query == "" then
                True

            else
                String.contains query (String.toLower lot.partId)
                    || String.contains query (String.toLower lot.name)
                    || String.contains query (String.toLower lot.colorName)
                    || (case lot.elementId of
                            Just el ->
                                String.contains query (String.toLower el)

                            Nothing ->
                                False
                       )

        matchesCategory lot =
            case state.category of
                Just cat ->
                    lot.category == cat

                Nothing ->
                    True
    in
    lots
        |> List.filter (\l -> matchesQuery l && matchesCategory l)


sortLots : SortColumn -> Bool -> List PartLot -> List PartLot
sortLots column ascending lots =
    let
        comparator a b =
            case column of
                SortPartId ->
                    compare a.partId b.partId

                SortName ->
                    compare a.name b.name

                SortCategory ->
                    compare a.category b.category

                SortColor ->
                    compare a.colorId b.colorId

                SortElementId ->
                    compare (Maybe.withDefault "" a.elementId) (Maybe.withDefault "" b.elementId)

                SortCount ->
                    compare a.count b.count

                SortUnitWeight ->
                    compare a.unitWeightGrams b.unitWeightGrams

                SortTotalWeight ->
                    compare a.totalWeightGrams b.totalWeightGrams

        sorted =
            List.sortWith comparator lots
    in
    if ascending then
        sorted

    else
        List.reverse sorted


round2 : Float -> Float
round2 value =
    toFloat (round (value * 100.0)) / 100.0
