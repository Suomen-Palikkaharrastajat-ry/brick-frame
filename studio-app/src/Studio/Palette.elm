module Studio.Palette exposing
    ( ColorFilterMode(..)
    , Config
    , Index(..)
    , Shelf(..)
    , State
    , filterParts
    , init
    , initWith
    , view
    )

{-| The part palette: visual thumbnail grid, smart shelves, 3D turntable inspect,
and real LEGO color availability filtering.

The catalogue itself is `Studio.PartIndex`. Parts are browsed visually with
isometric thumbnails, favorite stars, recently placed shelves, and hover inspect cards.

-}

import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, option, select, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.ColorAvailability as ColorAvailability exposing (ColorAvailability(..), PartMetadata)
import Set exposing (Set)
import Studio.PartIndex as PartIndex exposing (Part, PartIndex)
import UI.Theme as Theme


{-| Whether the catalogue has arrived.
-}
type Index
    = Loading
    | Loaded PartIndex
    | Failed String


{-| Which shelf of parts is actively being browsed.
-}
type Shelf
    = AllShelf
    | FavoritesShelf
    | RecentShelf
    | ColorMatchedShelf


{-| Whether to show all parts or only those officially produced in the active colour.
-}
type ColorFilterMode
    = ShowAllColors
    | OfficialOnly


{-| What the palette remembers between renders: the catalogue, search query,
shelves, favorite parts, recently used parts, armed part, and inspect widget state.
-}
type alias State =
    { index : Index
    , query : String
    , category : Maybe String
    , armed : Maybe Part
    , manual : String
    , favorites : Set String
    , recentParts : List String
    , shelf : Shelf
    , colorFilterMode : ColorFilterMode
    , inspectingPart : Maybe Part
    , turntableAngle : Float
    }


{-| Everything the palette sends out, and the preview it shows.
-}
type alias Config msg =
    { onQuery : String -> msg
    , onCategory : Maybe String -> msg
    , onArm : Part -> msg
    , onAdd : msg
    , onManual : String -> msg
    , onAddManual : msg
    , onToggleFavorite : String -> msg
    , onSelectShelf : Shelf -> msg
    , onToggleColorFilter : msg
    , onInspect : Maybe Part -> msg
    , onRotateTurntable : Float -> msg
    , activeColour : Int
    , thumbnails : Dict String String
    , preview : Html msg
    }


{-| A palette waiting for its catalogue with empty shelves.
-}
init : State
init =
    { index = Loading
    , query = ""
    , category = Nothing
    , armed = Nothing
    , manual = ""
    , favorites = Set.empty
    , recentParts = []
    , shelf = AllShelf
    , colorFilterMode = ShowAllColors
    , inspectingPart = Nothing
    , turntableAngle = 35.0
    }


{-| Initialize with persisted favorites and recently used parts from local storage.
-}
initWith : { favorites : List String, recentParts : List String } -> State
initWith persisted =
    { index = Loading
    , query = ""
    , category = Nothing
    , armed = Nothing
    , manual = ""
    , favorites = Set.fromList persisted.favorites
    , recentParts = persisted.recentParts
    , shelf = AllShelf
    , colorFilterMode = ShowAllColors
    , inspectingPart = Nothing
    , turntableAngle = 35.0
    }


{-| Maximum results displayed in the visual grid at once.
-}
resultLimit : Int
resultLimit =
    64


{-| Filter and rank parts according to query, category, shelf, and color availability.
-}
filterParts : Int -> State -> Int -> PartIndex -> List Part
filterParts limit state activeColour index =
    let
        sourceParts =
            case state.shelf of
                AllShelf ->
                    index.parts

                FavoritesShelf ->
                    List.filter (\p -> Set.member p.file state.favorites) index.parts

                RecentShelf ->
                    -- Preserve recently used order
                    let
                        partsByFile =
                            List.foldl (\p d -> Dict.insert p.file p d) Dict.empty index.parts
                    in
                    List.filterMap (\file -> Dict.get file partsByFile) state.recentParts

                ColorMatchedShelf ->
                    List.filter (\p -> ColorAvailability.isOfficial p.file activeColour) index.parts

        words =
            state.query |> String.toLower |> String.words

        matchesQuery part =
            (state.category == Nothing || state.category == Just part.category)
                && List.all (\word -> String.contains word part.haystack) words

        matchesColorFilter part =
            case state.colorFilterMode of
                ShowAllColors ->
                    True

                OfficialOnly ->
                    ColorAvailability.checkAvailability part.file activeColour /= Unofficial

        whole =
            String.toLower (String.trim state.query)

        rank part =
            if part.file == whole || part.file == whole ++ ".dat" then
                0

            else if String.startsWith whole part.file then
                1

            else if String.startsWith whole (String.toLower part.description) then
                2

            else
                3
    in
    let
        filtered =
            sourceParts
                |> List.filter (\p -> matchesQuery p && matchesColorFilter p)
    in
    if state.shelf == RecentShelf && String.trim state.query == "" then
        List.take limit filtered

    else
        filtered
            |> List.sortBy (\part -> ( rank part, String.length part.description, part.file ))
            |> List.take limit


view : Config msg -> State -> Html msg
view config state =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "8px"
        , Attr.style "min-height" "0"
        , Attr.style "height" "100%"
        , Attr.style "position" "relative"
        ]
        (case state.index of
            Loading ->
                [ note "Loading the part catalogue…" ]
                    ++ manualEntry config state

            Failed reason ->
                [ note ("No part catalogue: " ++ reason ++ ". Sync the LDraw library (make sync-ldraw) to build one, or type a part number below.") ]
                    ++ manualEntry config state

            Loaded index ->
                [ searchBox config state
                , shelfTabs config state
                , categoryPicker config state index
                , config.preview
                , addButton config state
                , resultsGrid config state index
                , viewInspectModal config state
                ]
        )


note : String -> Html msg
note message =
    div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px", Attr.style "line-height" "1.5" ] [ text message ]


manualEntry : Config msg -> State -> List (Html msg)
manualEntry config state =
    [ div [ Attr.style "display" "flex", Attr.style "gap" "6px" ]
        [ input
            ([ Attr.type_ "text"
             , Attr.placeholder "Part number, e.g. 3001"
             , Attr.value state.manual
             , Html.Events.onInput config.onManual
             , Attr.style "flex" "1"
             , Attr.style "min-width" "0"
             ]
                ++ inputAttrs
            )
            []
        , button (Html.Events.onClick config.onAddManual :: buttonStyle) [ text "Add" ]
        ]
    ]


searchBox : Config msg -> State -> Html msg
searchBox config state =
    input
        ([ Attr.type_ "search"
         , Attr.placeholder "Search parts (name, id, keyword)"
         , Attr.attribute "aria-label" "Search parts"
         , Attr.value state.query
         , Html.Events.onInput config.onQuery
         ]
            ++ inputAttrs
        )
        []


shelfTabs : Config msg -> State -> Html msg
shelfTabs config state =
    div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "4px"
        , Attr.style "overflow-x" "auto"
        , Attr.style "padding-bottom" "2px"
        ]
        [ shelfButton config state AllShelf "All"
        , shelfButton config state FavoritesShelf ("★ (" ++ String.fromInt (Set.size state.favorites) ++ ")")
        , shelfButton config state RecentShelf ("⏱ (" ++ String.fromInt (List.length state.recentParts) ++ ")")
        , shelfButton config state ColorMatchedShelf "🎨 Official"
        ]


shelfButton : Config msg -> State -> Shelf -> String -> Html msg
shelfButton config state targetShelf label =
    let
        isActive =
            state.shelf == targetShelf
    in
    button
        [ Html.Events.onClick (config.onSelectShelf targetShelf)
        , Attr.style "padding" "3px 8px"
        , Attr.style "font-size" "11px"
        , Attr.style "font-family" "monospace"
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        , Attr.style "white-space" "nowrap"
        , Attr.style "border"
            (if isActive then
                "1px solid " ++ Theme.brand

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "background"
            (if isActive then
                Theme.brandYellow

             else
                Theme.panelSubtleBackground
            )
        , Attr.style "color"
            (if isActive then
                Theme.brand

             else
                Theme.textPrimary
            )
        ]
        [ text label ]


categoryPicker : Config msg -> State -> PartIndex -> Html msg
categoryPicker config state index =
    select
        ([ Html.Events.on "change"
            (Decode.map
                (\value ->
                    config.onCategory
                        (if value == "" then
                            Nothing

                         else
                            Just value
                        )
                )
                (Decode.at [ "target", "value" ] Decode.string)
            )
         , Attr.attribute "aria-label" "Category"
         ]
            ++ inputAttrs
        )
        (option [ Attr.value "", Attr.selected (state.category == Nothing) ] [ text "All categories" ]
            :: List.map
                (\( name, count ) ->
                    option
                        [ Attr.value name, Attr.selected (state.category == Just name) ]
                        [ text (name ++ " (" ++ String.fromInt count ++ ")") ]
                )
                index.categories
        )


addButton : Config msg -> State -> Html msg
addButton config state =
    case state.armed of
        Just part ->
            button
                (Html.Events.onClick config.onAdd :: Attr.style "background" Theme.brandYellow :: Attr.style "color" Theme.brand :: buttonStyle)
                [ text ("Add " ++ part.file) ]

        Nothing ->
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px" ] [ text "Pick a part to add it." ]


resultsGrid : Config msg -> State -> PartIndex -> Html msg
resultsGrid config state index =
    let
        found =
            filterParts resultLimit state config.activeColour index
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "min-height" "80px"
        , Attr.style "overflow-y" "auto"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(84px, 1fr))"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "2px"
        ]
        (if List.isEmpty found then
            [ div [ Attr.style "grid-column" "1 / -1" ] [ note "No parts match in this shelf." ] ]

         else
            List.map (partCard config state) found
        )


partCard : Config msg -> State -> Part -> Html msg
partCard config state part =
    let
        isArmed =
            Maybe.map .file state.armed == Just part.file

        isFavorite =
            Set.member part.file state.favorites

        availability =
            ColorAvailability.checkAvailability part.file config.activeColour

        thumbUrl =
            Dict.get part.file config.thumbnails
    in
    div
        [ Html.Events.onClick (config.onArm part)
        , Html.Events.on "dblclick" (Decode.succeed config.onAdd)
        , Attr.style "position" "relative"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "padding" "6px 4px"
        , Attr.style "border-radius" "6px"
        , Attr.style "border"
            (if isArmed then
                "2px solid " ++ Theme.brand

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "background"
            (if isArmed then
                "color-mix(in srgb, var(--color-brand-yellow) 22%, var(--color-bg-page))"

             else
                Theme.panelSurface
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        ]
        [ cardTopBar config part isFavorite availability
        , cardThumbnail thumbUrl part.file
        , div
            [ Attr.style "font-family" "monospace"
            , Attr.style "font-size" "10px"
            , Attr.style "font-weight" "600"
            , Attr.style "color" Theme.textPrimary
            , Attr.style "text-align" "center"
            , Attr.style "white-space" "nowrap"
            , Attr.style "overflow" "hidden"
            , Attr.style "text-overflow" "ellipsis"
            , Attr.style "width" "100%"
            , Attr.style "margin-top" "4px"
            ]
            [ text part.file ]
        , div
            [ Attr.style "font-size" "9px"
            , Attr.style "color" Theme.textMuted
            , Attr.style "text-align" "center"
            , Attr.style "white-space" "nowrap"
            , Attr.style "overflow" "hidden"
            , Attr.style "text-overflow" "ellipsis"
            , Attr.style "width" "100%"
            ]
            [ text part.description ]
        , cardInspectButton config part
        ]


cardTopBar : Config msg -> Part -> Bool -> ColorAvailability -> Html msg
cardTopBar config part isFavorite availability =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "space-between"
        , Attr.style "width" "100%"
        , Attr.style "align-items" "center"
        ]
        [ button
            [ Html.Events.on "click" (Decode.succeed (config.onToggleFavorite part.file))
            , Attr.style "background" "transparent"
            , Attr.style "border" "none"
            , Attr.style "cursor" "pointer"
            , Attr.style "padding" "0 2px"
            , Attr.style "font-size" "12px"
            , Attr.style "line-height" "1"
            , Attr.style "color"
                (if isFavorite then
                    Theme.brandYellow

                 else
                    Theme.textMuted
                )
            , Attr.attribute "aria-label" "Favorite"
            ]
            [ text
                (if isFavorite then
                    "★"

                 else
                    "☆"
                )
            ]
        , availabilityBadge availability
        ]


availabilityBadge : ColorAvailability -> Html msg
availabilityBadge availability =
    let
        ( color, title ) =
            case availability of
                InProduction _ ->
                    ( "color-mix(in srgb, var(--color-brand) 75%, white)", "In Production" )

                OutOfProduction _ ->
                    ( "var(--color-brand-yellow)", "Out of Production / Retired" )

                Unofficial ->
                    ( "var(--color-brand-red)", "Unofficial Color" )
    in
    div
        [ Attr.style "width" "7px"
        , Attr.style "height" "7px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" color
        , Attr.title title
        ]
        []


cardThumbnail : Maybe String -> String -> Html msg
cardThumbnail maybeUrl file =
    case maybeUrl of
        Just url ->
            img
                [ Attr.src url
                , Attr.alt file
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "object-fit" "contain"
                , Attr.style "pointer-events" "none"
                ]
                []

        Nothing ->
            -- Clean placeholder with isometric box symbol
            div
                [ Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "background" Theme.panelSubtleBackground
                , Attr.style "border-radius" "4px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "18px"
                ]
                [ text "⬡" ]


cardInspectButton : Config msg -> Part -> Html msg
cardInspectButton config part =
    button
        [ Html.Events.on "click" (Decode.succeed (config.onInspect (Just part)))
        , Attr.style "margin-top" "4px"
        , Attr.style "padding" "1px 4px"
        , Attr.style "font-size" "9px"
        , Attr.style "font-family" "monospace"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "3px"
        , Attr.style "color" Theme.textMuted
        , Attr.style "cursor" "pointer"
        ]
        [ text "Inspect 3D" ]


{-| Interactive 3D Turntable inspect card.
-}
viewInspectModal : Config msg -> State -> Html msg
viewInspectModal config state =
    case state.inspectingPart of
        Just part ->
            let
                meta =
                    ColorAvailability.getMetadata part.file part.description part.category

                ( w, l, h ) =
                    meta.dimensions

                availability =
                    ColorAvailability.checkAvailability part.file config.activeColour

                availText =
                    case availability of
                        InProduction { yearIntroduced, setFrequency } ->
                            "🟢 Official In Production (since " ++ String.fromInt yearIntroduced ++ ", " ++ String.fromInt setFrequency ++ " sets)"

                        OutOfProduction { yearIntroduced, yearDiscontinued } ->
                            "🟡 Official Out of Production (" ++ String.fromInt yearIntroduced ++ "–" ++ String.fromInt yearDiscontinued ++ ")"

                        Unofficial ->
                            "🔴 Unofficial Color Combination"
            in
            div
                [ Attr.style "position" "absolute"
                , Attr.style "bottom" "8px"
                , Attr.style "left" "8px"
                , Attr.style "right" "8px"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "box-shadow" "0 4px 12px rgba(0,0,0,0.15)"
                , Attr.style "border-radius" "8px"
                , Attr.style "padding" "10px"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "8px"
                , Attr.style "z-index" "50"
                ]
                [ div
                    [ Attr.style "display" "flex"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "align-items" "center"
                    ]
                    [ span [ Attr.style "font-family" "monospace", Attr.style "font-weight" "600", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                        [ text part.file ]
                    , button
                        [ Html.Events.onClick (config.onInspect Nothing)
                        , Attr.style "background" "transparent"
                        , Attr.style "border" "none"
                        , Attr.style "cursor" "pointer"
                        , Attr.style "color" Theme.textMuted
                        , Attr.style "font-size" "14px"
                        ]
                        [ text "✕" ]
                    ]
                , div [ Attr.style "font-size" "11px", Attr.style "color" Theme.textMuted ] [ text part.description ]
                , div
                    [ Attr.style "display" "flex"
                    , Attr.style "gap" "12px"
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "11px"
                    , Attr.style "color" Theme.textPrimary
                    ]
                    [ span [] [ text ("Size: " ++ String.fromInt w ++ "×" ++ String.fromInt l ++ "×" ++ String.fromInt h ++ " studs") ]
                    , span []
                        [ text
                            (if meta.isGearOrAxle then
                                "⚙ Gear/Axle"

                             else
                                meta.category
                            )
                        ]
                    ]
                , div [ Attr.style "font-size" "10px", Attr.style "font-family" "monospace", Attr.style "color" Theme.textPrimary ]
                    [ text availText ]
                , turntableWidget config state
                ]

        Nothing ->
            text ""


turntableWidget : Config msg -> State -> Html msg
turntableWidget config state =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "4px"
        , Attr.style "padding" "6px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "border-radius" "6px"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "font-family" "monospace"
            , Attr.style "font-size" "10px"
            ]
            [ span [ Attr.style "color" Theme.textMuted ] [ text "3D Turntable" ]
            , span [ Attr.style "color" Theme.textPrimary ] [ text (String.fromInt (round state.turntableAngle) ++ "°") ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.min "0"
            , Attr.max "360"
            , Attr.step "5"
            , Attr.value (String.fromFloat state.turntableAngle)
            , Html.Events.onInput
                (\val ->
                    case String.toFloat val of
                        Just deg ->
                            config.onRotateTurntable deg

                        Nothing ->
                            config.onRotateTurntable 0
                )
            , Attr.style "width" "100%"
            ]
            []
        ]


inputAttrs : List (Html.Attribute msg)
inputAttrs =
    [ Attr.style "padding" "6px 8px"
    , Attr.style "box-sizing" "border-box"
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "6px"
    , Attr.style "background" Theme.panelSurface
    , Attr.style "color" Theme.textPrimary
    , Attr.style "font-family" "monospace"
    , Attr.style "font-size" "12px"
    ]


buttonStyle : List (Html.Attribute msg)
buttonStyle =
    [ Attr.style "padding" "6px 10px"
    , Attr.style "background" Theme.panelSubtleBackground
    , Attr.style "color" Theme.textPrimary
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "6px"
    , Attr.style "font-family" "monospace"
    , Attr.style "font-size" "12px"
    , Attr.style "cursor" "pointer"
    ]
