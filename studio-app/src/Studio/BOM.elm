module Studio.BOM exposing
    ( Config
    , SortColumn(..)
    , State
    , init
    , view
    )

{-| Interactive parts list (bill of materials) modal.

The panel is a modal dialog (`role="dialog"`, `aria-modal`) labelled by its
heading. It closes from the close button, a click on the dimmed backdrop, or
Escape pressed while focus is inside it.

Provides:

  - Summary metrics (total parts, unique lots, estimated weight, dimensions)
  - Search filtering by part ID, name, colour or element ID
  - Category and submodel filtering
  - Sortable column headers (buttons carrying `aria-sort`)
  - Keyboard-reachable row selection that highlights the matching instances
    in the 3D viewport
  - Export buttons for BrickLink XML and Rebrickable CSV
  - Clipboard copy as a Markdown table or plain text
  - Optional revision diff view

-}

import FeatherIcons
import Html exposing (Html, button, div, h2, input, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.BOM as BOM exposing (BOMDiff, ModelBOM, PartLot)
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


{-| The id of the dialog heading, referenced by `aria-labelledby`.
-}
titleId : String
titleId =
    "bom-title"


{-| Main view for the parts list. Renders as a modal dialog over a dimmed backdrop.
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
        , Attr.style "padding" "16px"
        , Attr.style "pointer-events" "auto"
        ]
        [ div
            [ Attr.attribute "aria-hidden" "true"
            , Html.Events.onClick config.onClose
            , Attr.style "position" "absolute"
            , Attr.style "inset" "0"
            , Attr.style "background" "color-mix(in srgb, var(--color-brand) 18%, transparent)"
            , Attr.style "backdrop-filter" "blur(4px)"
            ]
            []
        , div
            [ Attr.attribute "role" "dialog"
            , Attr.attribute "aria-modal" "true"
            , Attr.attribute "aria-labelledby" titleId
            , Html.Events.preventDefaultOn "keydown" (escapeKeyDecoder config.onClose)
            , Attr.style "position" "relative"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "10px"
            , Attr.style "box-shadow" floatingShadow
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


{-| Closes the dialog on Escape. Other keys are left alone, so the decoder
fails and the browser keeps its default handling.
-}
escapeKeyDecoder : msg -> Decode.Decoder ( msg, Bool )
escapeKeyDecoder onClose =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" || key == "Esc" then
                    Decode.succeed ( onClose, True )

                else
                    Decode.fail "not the Escape key"
            )


{-| A Feather icon sized for toolbar buttons, hidden from assistive technology.
-}
icon : FeatherIcons.Icon -> Html msg
icon featherIcon =
    featherIcon
        |> FeatherIcons.withSize 18
        |> FeatherIcons.withStrokeWidth 1.75
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true", Attr.attribute "focusable" "false" ]


{-| A small chevron showing the active sort direction, hidden from assistive technology.
-}
sortIcon : Bool -> Html msg
sortIcon ascending =
    (if ascending then
        FeatherIcons.chevronUp

     else
        FeatherIcons.chevronDown
    )
        |> FeatherIcons.withSize 14
        |> FeatherIcons.withStrokeWidth 2
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true", Attr.attribute "focusable" "false" ]


floatingShadow : String
floatingShadow =
    "0 6px 20px color-mix(in srgb, var(--color-brand) 12%, transparent), 0 1px 2px color-mix(in srgb, var(--color-brand) 8%, transparent)"


viewHeader : Config msg -> ModelBOM -> Html msg
viewHeader config bom =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "gap" "12px"
        , Attr.style "padding" "12px 16px"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "background" Theme.panelSubtleBackground
        ]
        [ div [ Attr.style "display" "flex", Attr.style "align-items" "baseline", Attr.style "gap" "10px", Attr.style "flex-wrap" "wrap" ]
            [ h2
                [ Attr.id titleId
                , Attr.style "margin" "0"
                , Attr.style "font-size" "18px"
                , Attr.style "font-weight" "700"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text "Parts list" ]
            , span
                [ Attr.style "font-size" "12px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text (String.fromInt bom.totalParts ++ " total parts, " ++ String.fromInt bom.uniqueLots ++ " unique lots") ]
            ]
        , button
            [ Attr.id "bom-close"
            , Attr.type_ "button"
            , Html.Events.onClick config.onClose
            , Attr.attribute "aria-label" "Close parts list"
            , Attr.title "Close (Esc)"
            , Attr.style "display" "inline-flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "background" "transparent"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "6px"
            , Attr.style "color" Theme.textMuted
            , Attr.style "cursor" "pointer"
            , Attr.style "padding" "6px"
            ]
            [ icon FeatherIcons.x ]
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
                , Attr.attribute "aria-label" "Search parts"
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
                , Attr.attribute "aria-label" "Filter by category"
                , Attr.style "padding" "6px 10px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "4px"
                , Attr.style "font-size" "13px"
                , Attr.style "color" Theme.textPrimary
                , Attr.style "background" Theme.panelSurface
                ]
                (option [ Attr.value "", Attr.selected (state.category == Nothing) ] [ text "All categories" ]
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
                    , Attr.attribute "aria-label" "Filter by submodel"
                    , Attr.style "padding" "6px 10px"
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "4px"
                    , Attr.style "font-size" "13px"
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "background" Theme.panelSurface
                    ]
                    (option [ Attr.value "", Attr.selected (state.submodel == Nothing) ] [ text "Entire model" ]
                        :: List.map
                            (\sub ->
                                option [ Attr.value sub, Attr.selected (state.submodel == Just sub) ] [ text sub ]
                            )
                            submodels
                    )
            , case maybeDiff of
                Just _ ->
                    Button.toggle { active = state.showDiff, enabled = True } config.onToggleDiff "Revision diff"

                Nothing ->
                    text ""
            ]
        , div
            [ Attr.attribute "role" "group"
            , Attr.attribute "aria-label" "Export"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "6px"
            , Attr.style "flex-wrap" "wrap"
            ]
            [ exportButton FeatherIcons.download "BrickLink XML" config.onExportBrickLink
            , exportButton FeatherIcons.download "Rebrickable CSV" config.onExportRebrickable
            , exportButton FeatherIcons.copy "Copy as Markdown" config.onCopyMarkdown
            , exportButton FeatherIcons.copy "Copy as text" config.onCopyPlainText
            ]
        ]


{-| A labelled export or copy button with a leading Feather icon.
-}
exportButton : FeatherIcons.Icon -> String -> msg -> Html msg
exportButton featherIcon label msg =
    Button.icon
        { icon = icon featherIcon
        , label = label
        , shortcut = Nothing
        , pressed = Nothing
        , enabled = True
        , showLabel = True
        }
        msg


{-| A column header that sorts the table. The button inside carries the
click; the `th` carries the sort direction for assistive technology.
-}
sortableHeader : Config msg -> State -> SortColumn -> String -> String -> Html msg
sortableHeader config state column label align =
    let
        isActive =
            state.sortColumn == column

        ariaSort =
            if not isActive then
                "none"

            else if state.sortAscending then
                "ascending"

            else
                "descending"

        arrow =
            if isActive then
                [ sortIcon state.sortAscending ]

            else
                []
    in
    th
        [ Attr.attribute "scope" "col"
        , Attr.attribute "aria-sort" ariaSort
        , Attr.style "padding" "8px 10px"
        , Attr.style "text-align" align
        , Attr.style "white-space" "nowrap"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        ]
        [ button
            [ Attr.type_ "button"
            , Html.Events.onClick (config.onSort column)
            , Attr.style "display" "inline-flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "4px"
            , Attr.style "padding" "0"
            , Attr.style "background" "transparent"
            , Attr.style "border" "none"
            , Attr.style "font-family" "inherit"
            , Attr.style "font-size" "12px"
            , Attr.style "font-weight" "600"
            , Attr.style "color" Theme.textMuted
            , Attr.style "cursor" "pointer"
            ]
            (text label :: arrow)
        ]


{-| A non-sortable column header.
-}
plainHeader : String -> String -> Html msg
plainHeader align label =
    th
        [ Attr.attribute "scope" "col"
        , Attr.style "padding" "8px 10px"
        , Attr.style "font-size" "12px"
        , Attr.style "font-weight" "600"
        , Attr.style "text-align" align
        , Attr.style "color" Theme.textMuted
        , Attr.style "white-space" "nowrap"
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        ]
        [ text label ]


viewInventoryTable : Config msg -> State -> List PartLot -> Html msg
viewInventoryTable config state lots =
    let
        lotRow lot =
            let
                isSelected =
                    state.selectedLot == Just ( lot.partId, lot.colorId )

                rgba =
                    Colors.resolveColor 16 lot.colorId

                swatchColor =
                    Colours.css rgba

                colorLabel =
                    lot.colorName ++ " (" ++ String.fromInt lot.colorId ++ ")"

                elemText =
                    Maybe.withDefault "—" lot.elementId

                bgColor =
                    if isSelected then
                        "color-mix(in srgb, var(--color-brand-yellow) 25%, var(--color-bg-page))"

                    else
                        "transparent"

                selectedAttrs =
                    if isSelected then
                        [ Attr.attribute "aria-current" "true" ]

                    else
                        []
            in
            tr
                [ Html.Events.onClick (config.onSelectLot lot.partId lot.colorId)
                , Attr.style "cursor" "pointer"
                , Attr.style "background" bgColor
                , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                ]
                [ td [ Attr.style "padding" "6px 10px", Attr.style "width" "28px" ]
                    [ button
                        ([ Attr.type_ "button"
                         , Attr.attribute "aria-label" ("Select " ++ lot.name ++ " (" ++ lot.partId ++ "), " ++ colorLabel)
                         , Html.Events.stopPropagationOn "click" (Decode.succeed ( config.onSelectLot lot.partId lot.colorId, True ))
                         , Attr.style "display" "flex"
                         , Attr.style "padding" "0"
                         , Attr.style "background" "transparent"
                         , Attr.style "border" "none"
                         , Attr.style "border-radius" "3px"
                         , Attr.style "cursor" "pointer"
                         ]
                            ++ selectedAttrs
                        )
                        [ div
                            [ Attr.attribute "aria-hidden" "true"
                            , Attr.style "width" "18px"
                            , Attr.style "height" "18px"
                            , Attr.style "border-radius" "3px"
                            , Attr.style "background" swatchColor
                            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                            ]
                            []
                        ]
                    ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-family" "monospace", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                    [ text lot.partId ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "13px", Attr.style "color" Theme.textPrimary ]
                    [ text lot.name ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "color" Theme.textMuted ]
                    [ text lot.category ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                    [ text colorLabel ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "color" Theme.textMuted ]
                    [ text elemText ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "13px", Attr.style "font-weight" "600", Attr.style "text-align" "right", Attr.style "color" Theme.textPrimary ]
                    [ text (String.fromInt lot.count) ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "text-align" "right", Attr.style "color" Theme.textMuted ]
                    [ text (String.fromFloat lot.unitWeightGrams ++ " g") ]
                , td [ Attr.style "padding" "6px 10px", Attr.style "font-size" "12px", Attr.style "font-weight" "600", Attr.style "text-align" "right", Attr.style "color" Theme.textPrimary ]
                    [ text (String.fromFloat lot.totalWeightGrams ++ " g") ]
                ]

        bodyRows =
            if List.isEmpty lots then
                [ tr []
                    [ td
                        [ Attr.colspan 9
                        , Attr.style "padding" "16px 10px"
                        , Attr.style "font-size" "13px"
                        , Attr.style "color" Theme.textMuted
                        ]
                        [ text "No parts match the current filters." ]
                    ]
                ]

            else
                List.map lotRow lots
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "0 16px 16px 16px"
        ]
        [ table
            [ Attr.attribute "aria-label" "Parts inventory"
            , Attr.style "width" "100%"
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
                    [ th
                        [ Attr.attribute "scope" "col"
                        , Attr.style "padding" "8px 10px"
                        , Attr.style "width" "28px"
                        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
                        ]
                        [ span
                            [ Attr.style "position" "absolute"
                            , Attr.style "width" "1px"
                            , Attr.style "height" "1px"
                            , Attr.style "overflow" "hidden"
                            , Attr.style "clip" "rect(0 0 0 0)"
                            ]
                            [ text "Swatch" ]
                        ]
                    , sortableHeader config state SortPartId "Part ID" "left"
                    , sortableHeader config state SortName "Name" "left"
                    , sortableHeader config state SortCategory "Category" "left"
                    , sortableHeader config state SortColor "Color" "left"
                    , sortableHeader config state SortElementId "Element ID" "left"
                    , sortableHeader config state SortCount "Qty" "right"
                    , sortableHeader config state SortUnitWeight "Unit weight" "right"
                    , sortableHeader config state SortTotalWeight "Total weight" "right"
                    ]
                ]
            , tbody [] bodyRows
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
            "color-mix(in srgb, var(--color-brand-nougat) 30%, transparent)"

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
        [ div
            [ Attr.attribute "role" "status"
            , Attr.style "padding" "10px 0"
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text
                ("Revision comparison: "
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
            [ Attr.attribute "aria-label" "Revision diff"
            , Attr.style "width" "100%"
            , Attr.style "border-collapse" "collapse"
            ]
            [ thead
                [ Attr.style "position" "sticky"
                , Attr.style "top" "0"
                , Attr.style "background" Theme.panelSurface
                ]
                [ tr []
                    [ plainHeader "left" "Status"
                    , plainHeader "left" "Part ID"
                    , plainHeader "left" "Name"
                    , plainHeader "left" "Color"
                    , plainHeader "right" "Old qty"
                    , plainHeader "right" "New qty"
                    , plainHeader "right" "Diff"
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
