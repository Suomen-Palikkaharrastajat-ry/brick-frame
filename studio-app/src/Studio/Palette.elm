module Studio.Palette exposing
    ( ColorFilterMode(..)
    , Config
    , Index(..)
    , Shelf(..)
    , State
    , filterParts
    , init
    , initWith
    , isSearchingEveryPart
    , refreshFiltered
    , resultLimit
    , view
    )

{-| The part palette: a search box, shelves (All, Favourites, Recent and
Official colours), build groups, a part preview, and a grid of part cards.

Browsing starts from the build groups (`Studio.BuildGroups`) — Bricks, Plates,
Tiles, Slopes and so on, Technic last — each listing its classic parts first.
"Everything" drops the group and offers LDraw's own categories instead. A search
looks through every part, everyday building parts first.

The catalogue itself is `Studio.PartIndex`. Clicking a card arms that part and
the Add button places it (double-clicking a card adds it at once). Each card
also has a favourite toggle and an "Inspect 3D" button that opens a turntable
view. Colour availability comes from `LDraw.ColorAvailability`.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, input, option, select, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.ColorAvailability as ColorAvailability exposing (ColorAvailability(..))
import Set exposing (Set)
import Studio.BuildGroups as BuildGroups exposing (Tier(..))
import Studio.Layout as Layout
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
build group (`Nothing` is "Everything"), LDraw category, shelves, favorite
parts, recently used parts, armed part, inspect widget state, and cached
filtered parts.
-}
type alias State =
    { index : Index
    , query : String
    , group : Maybe String
    , category : Maybe String
    , armed : Maybe Part
    , manual : String
    , favorites : Set String
    , recentParts : List String
    , shelf : Shelf
    , colorFilterMode : ColorFilterMode
    , inspectingPart : Maybe Part
    , turntableAngle : Float
    , filteredParts : List Part
    }


{-| Everything the palette sends out, and the preview it shows.
-}
type alias Config msg =
    { onQuery : String -> msg
    , onGroup : Maybe String -> msg
    , onGroupAndFocus : Maybe String -> String -> msg
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
    , layoutMode : Layout.Mode
    , thumbnails : Dict String String
    , preview : Html msg
    }


{-| A palette waiting for its catalogue with empty shelves.
-}
init : State
init =
    { index = Loading
    , query = ""
    , group = Just BuildGroups.defaultGroup
    , category = Nothing
    , armed = Nothing
    , manual = ""
    , favorites = Set.empty
    , recentParts = []
    , shelf = AllShelf
    , colorFilterMode = ShowAllColors
    , inspectingPart = Nothing
    , turntableAngle = 35.0
    , filteredParts = []
    }


{-| Initialize with persisted favorites and recently used parts from local storage.
-}
initWith : { favorites : List String, recentParts : List String } -> State
initWith persisted =
    { index = Loading
    , query = ""
    , group = Just BuildGroups.defaultGroup
    , category = Nothing
    , armed = Nothing
    , manual = ""
    , favorites = Set.fromList persisted.favorites
    , recentParts = persisted.recentParts
    , shelf = AllShelf
    , colorFilterMode = ShowAllColors
    , inspectingPart = Nothing
    , turntableAngle = 35.0
    , filteredParts = []
    }


{-| Recompute and cache the filtered and sorted parts for the current state.
-}
refreshFiltered : Int -> State -> State
refreshFiltered activeColour state =
    case state.index of
        Loaded index ->
            { state | filteredParts = filterParts resultLimit state activeColour index }

        _ ->
            { state | filteredParts = [] }


{-| Maximum results displayed in the visual grid at once.
-}
resultLimit : Int
resultLimit =
    64


{-| Filter and rank parts according to query, build group, category, shelf, and
colour availability.

With no search, the All and Official colours shelves show the chosen build
group: its classics in curated order, then the rest by size. A search ignores
the group and ranks every match: exact part number, part-number prefix,
description prefix, description match, then keyword-only matches; within each,
curated classics first, then everyday building parts before Technic and the
rest, printed or Duplo variants last. Favourites and Recent are personal lists
and ignore the group.

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

        whole =
            normaliseQuery state.query

        words =
            searchTerms whole

        activeGroup =
            if state.shelf == AllShelf || state.shelf == ColorMatchedShelf then
                state.group

            else
                Nothing

        matchesQuery part =
            (state.category == Nothing || state.category == Just part.category)
                && (case activeGroup of
                        Just id ->
                            List.member id part.shelving.groups

                        Nothing ->
                            True
                   )
                && List.all (\word -> termIn word part.haystack) words

        matchesColorFilter part =
            case state.colorFilterMode of
                ShowAllColors ->
                    True

                OfficialOnly ->
                    ColorAvailability.checkAvailability part.file activeColour /= Unofficial

        rank part =
            if part.file == whole || part.file == whole ++ ".dat" then
                0

            else if String.startsWith whole part.file then
                1

            else if String.startsWith whole part.descriptionLower then
                2

            else if List.all (\word -> termIn word part.descriptionLower) words then
                3

            else
                -- Matched only through keywords.
                4

        filtered =
            sourceParts
                |> List.filter (\p -> matchesQuery p && matchesColorFilter p)

        -- Keys are computed once per part, then sorted on: `sortBy` would
        -- recompute them on every comparison.
        sortedBy key parts =
            parts
                |> List.map (\p -> ( key p, p ))
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    if state.shelf == RecentShelf && whole == "" then
        List.take limit filtered

    else if whole == "" then
        case activeGroup of
            Just id ->
                filtered
                    |> sortedBy (\part -> ( classicPosition id part, part.size, ( String.length part.description, part.file ) ))
                    |> List.take limit

            Nothing ->
                filtered
                    |> sortedBy
                        (\part ->
                            ( 0
                            , 0
                            , ( tierOrder part.shelving.tier, part.shelving.classicOrder, ( String.length part.description, part.file ) )
                            )
                        )
                    |> List.take limit

    else
        filtered
            |> sortedBy
                (\part ->
                    ( rank part
                    , classicFirst part
                    , ( footprintOrder words part, tierOrder part.shelving.tier, ( part.shelving.classicOrder, ( String.length part.description, part.file ) ) )
                    )
                )
            |> List.take limit


{-| Footprint matches rank ahead of 3D extended sizes when a 2D footprint is queried.
-}
footprintOrder : List String -> Part -> Int
footprintOrder words part =
    let
        twoPartSizes =
            List.filter isTwoPartSize words
    in
    if List.isEmpty twoPartSizes then
        0

    else if List.all (\size -> hasExactFootprint size part.descriptionLower) twoPartSizes then
        0

    else
        1


{-| Whether a word represents a 2D footprint size like "1 x 2" or "2 x 4".
-}
isTwoPartSize : String -> Bool
isTwoPartSize word =
    case String.split " x " word of
        [ a, b ] ->
            String.toFloat a /= Nothing && String.toFloat b /= Nothing

        _ ->
            False


{-| Whether an occurrence of a 2D size in a description is an exact footprint match,
rather than being preceded or followed by another dimension (e.g. "1 x 2 x 3").
-}
hasExactFootprint : String -> String -> Bool
hasExactFootprint size text =
    String.indexes size text
        |> List.any
            (\i ->
                let
                    before =
                        String.slice 0 i text

                    after =
                        String.dropLeft (i + String.length size) text

                    beforeValid =
                        not (isDigitAt (i - 1) text) && not (endsInDimension before)

                    afterValid =
                        not (isDigitAt 0 after) && not (startsInDimension after)
                in
                beforeValid && afterValid
            )


{-| Whether a string ends with a dimension marker like " 2 x ".
-}
endsInDimension : String -> Bool
endsInDimension s =
    if String.endsWith " x " s then
        let
            before =
                String.dropRight 3 s
        in
        case String.uncons (String.right 1 before) of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False

    else
        False


{-| Whether a string starts with an extended dimension like " x 3" or " x 0.667".
-}
startsInDimension : String -> Bool
startsInDimension s =
    if String.startsWith " x " s then
        let
            remainder =
                String.dropLeft 3 s
        in
        case String.uncons remainder of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False

    else
        False


{-| A part's place among a group's classics, after every classic when it is
not one.
-}
classicPosition : String -> Part -> Int
classicPosition group part =
    part.shelving.classics
        |> List.filter (\( id, _ ) -> id == group)
        |> List.map Tuple.second
        |> List.minimum
        |> Maybe.withDefault BuildGroups.notClassic


{-| Curated classics lead a search whatever their tier, so "axle" or "gear"
starts with Technic Axle 4 and Gear 8 Tooth rather than every plate with an
axle hole.
-}
classicFirst : Part -> Int
classicFirst part =
    if List.isEmpty part.shelving.classics then
        1

    else
        0


{-| Whether a search term occurs in a text. A size ("2 x 4") must not run into
neighbouring digits, so it does not match "12 x 4" or "2 x 48".
-}
termIn : String -> String -> Bool
termIn term text =
    if String.contains " x " term then
        String.indexes term text
            |> List.any
                (\i ->
                    not (isDigitAt (i - 1) text || isDigitAt (i + String.length term) text)
                )

    else
        String.contains term text


isDigitAt : Int -> String -> Bool
isDigitAt i text =
    if i < 0 then
        False

    else
        case String.uncons (String.slice i (i + 1) text) of
            Just ( c, _ ) ->
                Char.isDigit c || c == '.'

            Nothing ->
                False


tierOrder : Tier -> Int
tierOrder tier =
    case tier of
        Basic ->
            0

        Other ->
            1

        Clutter ->
            2


{-| Lower-case and trim a search, and spell sizes the way LDraw does: "2x4",
"2 x4" and "2x 4" all become "2 x 4".
-}
normaliseQuery : String -> String
normaliseQuery query =
    query
        |> String.toLower
        |> String.words
        |> List.map spellSize
        |> String.join " "


{-| The words of a normalised search, with each size kept whole: "brick 1 x 2"
looks for "brick" and "1 x 2", not for "1", "x" and "2" anywhere.
-}
searchTerms : String -> List String
searchTerms whole =
    let
        isNumber w =
            String.toFloat w /= Nothing

        go ws =
            case ws of
                a :: "x" :: b :: rest ->
                    if isNumber a && isNumber b then
                        sizeRun (a ++ " x " ++ b) rest

                    else
                        a :: go ("x" :: b :: rest)

                w :: rest ->
                    w :: go rest

                [] ->
                    []

        sizeRun acc ws =
            case ws of
                "x" :: c :: rest ->
                    if isNumber c then
                        sizeRun (acc ++ " x " ++ c) rest

                    else
                        acc :: go ws

                _ ->
                    acc :: go ws
    in
    go (String.words whole)


spellSize : String -> String
spellSize word =
    let
        pieces =
            String.split "x" word

        isNumber piece =
            String.toFloat piece /= Nothing
    in
    -- "2x4" and also the halves of "2 x4" / "2x 4": "x4" and "2x".
    if List.length pieces >= 2 && List.any isNumber pieces && List.all (\piece -> piece == "" || isNumber piece) pieces then
        String.join " x " pieces

    else
        word


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
                , groupChips config state
                ]
                    ++ (if state.group == Nothing then
                            [ categoryPicker config state index ]

                        else
                            []
                       )
                    ++ [ config.preview
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
             , Attr.attribute "aria-label" "Part number"
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
         , Attr.id "part-search"
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
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Shelf"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "gap" "4px"
        , Attr.style "padding-bottom" "2px"
        ]
        [ shelfButton config state AllShelf "All"
        , shelfButton config state FavoritesShelf ("Favourites " ++ String.fromInt (Set.size state.favorites))
        , shelfButton config state RecentShelf ("Recent " ++ String.fromInt (List.length state.recentParts))
        , shelfButton config state ColorMatchedShelf "Official colours"
        ]


shelfButton : Config msg -> State -> Shelf -> String -> Html msg
shelfButton config state targetShelf label =
    chip (config.onSelectShelf targetShelf) (state.shelf == targetShelf) label


{-| Whether the palette is searching across every part (query present on All or
Official colours shelf, no category and no group chosen).
-}
isSearchingEveryPart : State -> Bool
isSearchingEveryPart state =
    String.trim state.query
        /= ""
        && (state.shelf == AllShelf || state.shelf == ColorMatchedShelf)
        && state.category
        == Nothing
        && state.group
        == Nothing


{-| DOM element id for a build group chip.
-}
chipDomId : Maybe String -> String
chipDomId maybeId =
    "palette-group-" ++ Maybe.withDefault "everything" maybeId


{-| The build groups as a radiogroup of chips, Technic last and "Everything" after it.
-}
groupChips : Config msg -> State -> Html msg
groupChips config state =
    let
        groupShelf =
            state.shelf == AllShelf || state.shelf == ColorMatchedShelf

        groupsList =
            List.map (\group -> ( Just group.id, group.label )) BuildGroups.all
                ++ [ ( Nothing, "Everything" ) ]

        groupsArray =
            Array.fromList groupsList

        hasActive =
            groupShelf && List.any (\( id, _ ) -> id == state.group) groupsList

        renderChip index ( id, label ) =
            let
                isActive =
                    groupShelf && state.group == id

                isTabStop =
                    if hasActive then
                        isActive

                    else
                        index == 0
            in
            compactChip
                (chipDomId id)
                (config.onGroup id)
                (groupChipDecoder config groupsArray index)
                isActive
                isTabStop
                label

        containerAttrs =
            [ Attr.attribute "role" "radiogroup"
            , Attr.attribute "aria-label" "Build group"
            , Attr.style "display" "flex"
            , Attr.style "gap" "4px"
            ]
                ++ (case config.layoutMode of
                        Layout.Narrow ->
                            [ Attr.style "flex-wrap" "nowrap"
                            , Attr.style "overflow-x" "auto"
                            , Attr.style "-webkit-overflow-scrolling" "touch"
                            , Attr.style "padding-bottom" "4px"
                            , Attr.style "scrollbar-width" "none"
                            ]

                        Layout.Wide ->
                            [ Attr.style "flex-wrap" "wrap"
                            ]
                   )
    in
    div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "4px" ]
        [ div
            containerAttrs
            (List.indexedMap renderChip groupsList)
        , div [ Attr.attribute "aria-live" "polite" ]
            (if isSearchingEveryPart state then
                [ note "Searching every part. Pick a group to narrow it." ]

             else
                []
            )
        ]


{-| Decodes arrow keys, Home and End on a group chip to navigate to the adjacent chip.
-}
groupChipDecoder : Config msg -> Array ( Maybe String, String ) -> Int -> Decode.Decoder ( msg, Bool )
groupChipDecoder config arr index =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                let
                    total =
                        Array.length arr

                    targetIndex =
                        case key of
                            "ArrowRight" ->
                                Just (modBy total (index + 1))

                            "ArrowDown" ->
                                Just (modBy total (index + 1))

                            "ArrowLeft" ->
                                Just (modBy total (index - 1 + total))

                            "ArrowUp" ->
                                Just (modBy total (index - 1 + total))

                            "Home" ->
                                Just 0

                            "End" ->
                                Just (total - 1)

                            _ ->
                                Nothing
                in
                case targetIndex of
                    Just target ->
                        case Array.get target arr of
                            Just ( targetId, _ ) ->
                                Decode.succeed ( config.onGroupAndFocus targetId (chipDomId targetId), True )

                            Nothing ->
                                Decode.fail "invalid target index"

                    Nothing ->
                        Decode.fail ("unhandled key " ++ key)
            )


chip : msg -> Bool -> String -> Html msg
chip =
    chipSized "4px 10px" "12px"


{-| Seventeen group chips have to share a 300 px pane with the results grid.
-}
compactChip : String -> msg -> Decode.Decoder ( msg, Bool ) -> Bool -> Bool -> String -> Html msg
compactChip domId onPress onKeyDown isActive isTabStop label =
    button
        [ Html.Events.onClick onPress
        , Html.Events.preventDefaultOn "keydown" onKeyDown
        , Attr.id domId
        , Attr.class "palette-item"
        , Attr.attribute "role" "radio"
        , Attr.attribute "aria-checked"
            (if isActive then
                "true"

             else
                "false"
            )
        , Attr.tabindex
            (if isTabStop then
                0

             else
                -1
            )
        , Attr.style "flex-shrink" "0"
        , Attr.style "padding" "3px 8px"
        , Attr.style "font-size" "11px"
        , Attr.style "font-family" "inherit"
        , Attr.style "border-radius" "12px"
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


chipSized : String -> String -> msg -> Bool -> String -> Html msg
chipSized padding fontSize onPress isActive label =
    button
        [ Html.Events.onClick onPress
        , Attr.class "palette-item"
        , Attr.attribute "aria-pressed" (ariaBool isActive)
        , Attr.style "padding" padding
        , Attr.style "font-size" fontSize
        , Attr.style "font-family" "inherit"
        , Attr.style "border-radius" "12px"
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
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px", Attr.style "line-height" "1.5" ]
                [ text "Click a part to arm it, then click Add. Double-click a part to add it straight away." ]


resultsGrid : Config msg -> State -> PartIndex -> Html msg
resultsGrid config state _ =
    let
        found =
            state.filteredParts
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "min-height" "80px"
        , Attr.style "overflow-y" "auto"
        ]
        (if List.isEmpty found then
            [ note "No parts match in this shelf." ]

         else
            [ div
                [ Attr.attribute "role" "list"
                , Attr.attribute "aria-label" "Parts"
                , Attr.style "display" "grid"
                , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(84px, 1fr))"
                , Attr.style "gap" "6px"
                , Attr.style "padding" "2px"
                ]
                (List.map (partCard config state) found)
            ]
        )


{-| The file id of a part: its file name without the ".dat" extension.
-}
fileId : Part -> String
fileId part =
    if String.endsWith ".dat" part.file then
        String.dropRight 4 part.file

    else
        part.file


{-| The name a screen reader uses for a part: its description, or the file id
when the description is blank.
-}
partName : Part -> String
partName part =
    if String.isEmpty (String.trim part.description) then
        fileId part

    else
        part.description


{-| Render a boolean as the "true" / "false" value that aria-pressed expects.
-}
ariaBool : Bool -> String
ariaBool flag =
    if flag then
        "true"

    else
        "false"


partCard : Config msg -> State -> Part -> Html msg
partCard config state part =
    let
        isArmed =
            Maybe.map .file state.armed == Just part.file

        isFavorite =
            Set.member part.file state.favorites

        availability =
            ColorAvailability.checkAvailability part.file config.activeColour

        name =
            partName part
    in
    div
        [ Attr.attribute "role" "listitem"
        , Attr.class "palette-card"
        , Attr.style "position" "relative"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "padding" "6px 4px"
        , Attr.style "min-width" "0"
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
        ]
        [ cardTopBar config part name isFavorite availability
        , cardPickButton config part name isArmed
        , cardInspectButton config part name
        ]


{-| The clickable body of a card: thumbnail, file id and description. Clicking
arms the part; double-clicking arms and adds it. Styled to look like plain
content inside the card.
-}
cardPickButton : Config msg -> Part -> String -> Bool -> Html msg
cardPickButton config part name isArmed =
    button
        [ Html.Events.onClick (config.onArm part)
        , Html.Events.on "dblclick" (Decode.succeed config.onAdd)
        , Attr.class "palette-item"
        , Attr.attribute "aria-pressed" (ariaBool isArmed)
        , Attr.attribute "aria-label" (name ++ " (" ++ fileId part ++ ")")
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "width" "100%"
        , Attr.style "min-width" "0"
        , Attr.style "margin" "0"
        , Attr.style "padding" "0"
        , Attr.style "background" "none"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "4px"
        , Attr.style "color" "inherit"
        , Attr.style "font" "inherit"
        , Attr.style "text-align" "inherit"
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        ]
        [ cardThumbnail (Dict.get part.file config.thumbnails)
        , div
            [ Attr.style "font-family" "inherit"
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
        ]


cardTopBar : Config msg -> Part -> String -> Bool -> ColorAvailability -> Html msg
cardTopBar config part name isFavorite availability =
    let
        favoriteLabel =
            if isFavorite then
                "Remove " ++ name ++ " from favourites"

            else
                "Add " ++ name ++ " to favourites"
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "space-between"
        , Attr.style "width" "100%"
        , Attr.style "align-items" "center"
        ]
        [ button
            [ Html.Events.onClick (config.onToggleFavorite part.file)
            , Attr.class "palette-item"
            , Attr.attribute "aria-pressed" (ariaBool isFavorite)
            , Attr.attribute "aria-label" favoriteLabel
            , Attr.title favoriteLabel
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
            ]
            [ span [ Attr.attribute "aria-hidden" "true" ]
                [ text
                    (if isFavorite then
                        "★"

                     else
                        "☆"
                    )
                ]
            ]
        , availabilityBadge availability
        ]


{-| A small coloured dot showing the part's colour availability. It is exposed
as an image with a label so the status is not lost for screen reader users.
-}
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
        [ Attr.attribute "role" "img"
        , Attr.attribute "aria-label" title
        , Attr.title title
        , Attr.style "width" "7px"
        , Attr.style "height" "7px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" color
        ]
        []


{-| The part thumbnail. It is decorative (alt is empty) because the card's
button already names the part.
-}
cardThumbnail : Maybe String -> Html msg
cardThumbnail maybeUrl =
    case maybeUrl of
        Just url ->
            img
                [ Attr.src url
                , Attr.alt ""
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "object-fit" "contain"
                , Attr.style "pointer-events" "none"
                ]
                []

        Nothing ->
            -- Clean placeholder with isometric box symbol
            div
                [ Attr.attribute "aria-hidden" "true"
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "background" Theme.panelSubtleBackground
                , Attr.style "border-radius" "4px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-family" "inherit"
                , Attr.style "font-size" "18px"
                ]
                [ text "⬡" ]


cardInspectButton : Config msg -> Part -> String -> Html msg
cardInspectButton config part name =
    button
        [ Html.Events.onClick (config.onInspect (Just part))
        , Attr.class "palette-item"
        , Attr.attribute "aria-label" ("Inspect " ++ name ++ " in 3D")
        , Attr.style "margin-top" "4px"
        , Attr.style "padding" "1px 4px"
        , Attr.style "font-size" "9px"
        , Attr.style "font-family" "inherit"
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
                            "Official In Production (since " ++ String.fromInt yearIntroduced ++ ", " ++ String.fromInt setFrequency ++ " sets)"

                        OutOfProduction { yearIntroduced, yearDiscontinued } ->
                            "Official Out of Production (" ++ String.fromInt yearIntroduced ++ "–" ++ String.fromInt yearDiscontinued ++ ")"

                        Unofficial ->
                            "Unofficial Color Combination"
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
                    [ span [ Attr.style "font-family" "inherit", Attr.style "font-weight" "600", Attr.style "font-size" "12px", Attr.style "color" Theme.textPrimary ]
                        [ text part.file ]
                    , button
                        [ Html.Events.onClick (config.onInspect Nothing)
                        , Attr.attribute "aria-label" "Close 3D inspect"
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
                    , Attr.style "font-family" "inherit"
                    , Attr.style "font-size" "11px"
                    , Attr.style "color" Theme.textPrimary
                    ]
                    [ span [] [ text ("Size: " ++ String.fromInt w ++ "×" ++ String.fromInt l ++ "×" ++ String.fromInt h ++ " studs") ]
                    , span []
                        [ text
                            (if meta.isGearOrAxle then
                                "Gear/Axle"

                             else
                                meta.category
                            )
                        ]
                    ]
                , div [ Attr.style "font-size" "10px", Attr.style "font-family" "inherit", Attr.style "color" Theme.textPrimary ]
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
            , Attr.style "font-family" "inherit"
            , Attr.style "font-size" "10px"
            ]
            [ span [ Attr.style "color" Theme.textMuted ] [ text "3D Turntable" ]
            , span [ Attr.style "color" Theme.textPrimary ] [ text (String.fromInt (round state.turntableAngle) ++ "°") ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.attribute "aria-label" "Turntable angle"
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
    , Attr.style "font-family" "inherit"
    , Attr.style "font-size" "12px"
    ]


buttonStyle : List (Html.Attribute msg)
buttonStyle =
    [ Attr.style "padding" "6px 10px"
    , Attr.style "background" Theme.panelSubtleBackground
    , Attr.style "color" Theme.textPrimary
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "6px"
    , Attr.style "font-family" "inherit"
    , Attr.style "font-size" "12px"
    , Attr.style "cursor" "pointer"
    ]
