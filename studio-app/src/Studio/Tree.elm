module Studio.Tree exposing (Row(..), rows, rowsWithActive, view)

{-| The document's structure as a WAI-ARIA tree: its submodels, and the placements
of the active one grouped by `0 STEP`.

Submodel rows and part rows are the tree items. Selecting a part row selects that
placement (shift extends the selection). The active submodel lists its parts,
while inactive submodels can be clicked to switch the active context. Step and
"more" rows are presentational text only.

Keyboard: a roving tabindex keeps one item in the page's tab order — the row
focused last, so Tab comes back to where the user left off. Arrow keys,
Home and End move focus between items through the host's `focus` message, since
only the host can reach the DOM; Enter and Space act like a click.

Part rows show a colour dot, the part's description, and its file id and colour
name as secondary text. Each part row is labelled with its description and colour.

-}

import Html exposing (Html, div, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document, Entry(..))
import Set exposing (Set)
import UI.Theme as Theme


{-| A line of the tree.
-}
type Row
    = SubmodelRow { index : Int, name : String, parts : Int, isMain : Bool, isActive : Bool }
    | StepRow Int
    | PartRow { id : Int, file : String, color : Int }
    | MoreRow Int


{-| How many part rows to list before saying how many more there are, so a
city-scale model does not become tens of thousands of DOM nodes.
-}
maxRows : Int
maxRows =
    300


{-| The tree for a document. The first step has no marker of its own: a step
row appears only after the first `0 STEP`.
-}
rows : Document -> List Row
rows document =
    rowsWithActive "" document


{-| The tree for a document highlighting the active submodel.
-}
rowsWithActive : String -> Document -> List Row
rowsWithActive activeSubmodel document =
    let
        activeNorm =
            Document.normalise activeSubmodel

        mainName =
            Document.normalise (Document.mainSubmodel document).name

        isTargetActive s idx =
            let
                sNorm =
                    Document.normalise s.name
            in
            if activeSubmodel == "" then
                idx == 0

            else
                sNorm == activeNorm || (idx == 0 && activeNorm == mainName)
    in
    document.submodels
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( index, submodel ) ->
                let
                    parts =
                        submodel.entries
                            |> List.filter
                                (\entry ->
                                    case entry of
                                        PlacementEntry _ ->
                                            True

                                        Raw _ ->
                                            False
                                )
                            |> List.length

                    isActive =
                        isTargetActive submodel index

                    header =
                        SubmodelRow
                            { index = index
                            , name =
                                if submodel.name == "" then
                                    "Model"

                                else
                                    submodel.name
                            , parts = parts
                            , isMain = index == 0
                            , isActive = isActive
                            }
                in
                if isActive then
                    header :: mainRows submodel.entries

                else
                    [ header ]
            )


mainRows : List Entry -> List Row
mainRows entries =
    let
        step entry ( step_, acc, count ) =
            case entry of
                PlacementEntry p ->
                    if count >= maxRows then
                        ( step_, acc, count + 1 )

                    else
                        ( step_, PartRow { id = p.id, file = p.part, color = p.color } :: acc, count + 1 )

                Raw text ->
                    if isStep text then
                        ( step_ + 1, StepRow (step_ + 2) :: acc, count )

                    else
                        ( step_, acc, count )

        ( _, reversed, total ) =
            List.foldl step ( 0, [], 0 ) entries
    in
    if total > maxRows then
        List.reverse (MoreRow (total - maxRows) :: reversed)

    else
        List.reverse reversed


isStep : String -> Bool
isStep text =
    String.words text == [ "0", "STEP" ]


{-| What the tree needs from its host.

`describe` maps a part file (e.g. "32123a.dat") to its description, or "" when
unknown. `colourName` and `colourCss` turn an LDraw colour code into a name and a
CSS colour. `focus` asks the host to move DOM focus to the element with that id.

-}
type alias Config msg =
    { selection : Set Int
    , onSelect : Int -> Bool -> msg
    , activeSubmodel : String
    , onSelectSubmodel : String -> msg
    , describe : String -> String
    , colourName : Int -> String
    , colourCss : Int -> String
    , focus : String -> msg
    , lastFocused : Maybe String
    }


{-| Per-render data shared by all rows: the DOM ids of the interactive rows in
order, and the one row that is in the tab order.
-}
type alias Ctx msg =
    { config : Config msg
    , ids : List String
    , tabStop : Maybe String
    }


{-| The tree as a list, with the selected placements highlighted.
-}
view : Config msg -> Document -> Html msg
view config document =
    let
        rendered =
            rowsWithActive config.activeSubmodel document

        ids =
            List.filterMap rowDomId rendered

        ctx =
            { config = config
            , ids = ids
            , tabStop = tabStop config rendered ids
            }
    in
    div
        [ Attr.attribute "role" "tree"
        , Attr.attribute "aria-label" "Model structure"
        , Attr.attribute "aria-multiselectable" "true"
        , Attr.style "color" Theme.textPrimary

        -- Tell the host which row has focus, so the tab stop stays on it.
        , Html.Events.on "focusin"
            (Decode.at [ "target", "id" ] Decode.string
                |> Decode.andThen
                    (\id ->
                        if String.startsWith "tree-" id then
                            Decode.succeed (config.focus id)

                        else
                            Decode.fail "not a row"
                    )
            )
        ]
        (List.map (viewRow ctx) rendered)


{-| The row focused last if it is still shown, else the first selected part
row, else the first interactive row.
-}
tabStop : Config msg -> List Row -> List String -> Maybe String
tabStop config rendered ids =
    case config.lastFocused of
        Just id ->
            if List.member id ids then
                Just id

            else
                firstSelectedOrFirst config rendered ids

        Nothing ->
            firstSelectedOrFirst config rendered ids


firstSelectedOrFirst : Config msg -> List Row -> List String -> Maybe String
firstSelectedOrFirst config rendered ids =
    let
        selectedIds =
            List.filterMap
                (\row ->
                    case row of
                        PartRow p ->
                            if Set.member p.id config.selection then
                                Just (partDomId p.id)

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                rendered
    in
    case selectedIds of
        first :: _ ->
            Just first

        [] ->
            List.head ids


rowDomId : Row -> Maybe String
rowDomId row =
    case row of
        SubmodelRow s ->
            Just (submodelDomId s.index)

        PartRow p ->
            Just (partDomId p.id)

        _ ->
            Nothing


submodelDomId : Int -> String
submodelDomId index =
    "tree-sub-" ++ String.fromInt index


partDomId : Int -> String
partDomId id =
    "tree-part-" ++ String.fromInt id


tabIndexFor : Ctx msg -> String -> Html.Attribute msg
tabIndexFor ctx domId =
    Attr.tabindex
        (if ctx.tabStop == Just domId then
            0

         else
            -1
        )


viewRow : Ctx msg -> Row -> Html msg
viewRow ctx row =
    case row of
        SubmodelRow { index, name, parts, isActive } ->
            let
                domId =
                    submodelDomId index

                activate =
                    ctx.config.onSelectSubmodel name
            in
            div
                [ Attr.id domId
                , Attr.attribute "role" "treeitem"
                , Attr.attribute "aria-level" "1"
                , Attr.attribute "aria-expanded" (boolString isActive)
                , Attr.attribute "aria-selected" "false"
                , tabIndexFor ctx domId
                , Attr.class "tree-row"
                , Html.Events.onClick activate
                , Html.Events.preventDefaultOn "keydown"
                    (keyDecoder { ids = ctx.ids, self = domId, focus = ctx.config.focus, activate = always activate })
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "8px"
                , Attr.style "min-height" "28px"
                , Attr.style "padding" "4px 8px"
                , Attr.style "margin-top" "6px"
                , Attr.style "border-left"
                    (if isActive then
                        "3px solid var(--color-brand-yellow)"

                     else
                        "3px solid transparent"
                    )
                , Attr.style "cursor" "pointer"
                , Attr.style "color"
                    (if isActive then
                        Theme.brand

                     else
                        Theme.textPrimary
                    )
                ]
                [ span
                    [ Attr.style "font-weight" "600"
                    , Attr.style "min-width" "0"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "text-overflow" "ellipsis"
                    ]
                    [ text name ]
                , span
                    [ Attr.style "color" Theme.textMuted
                    , Attr.style "font-size" "11px"
                    , Attr.style "flex-shrink" "0"
                    , Attr.style "white-space" "nowrap"
                    ]
                    [ text (pluralParts parts) ]
                ]

        StepRow n ->
            div
                [ Attr.attribute "role" "presentation"
                , Attr.style "margin" "4px 0 2px 8px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "text-transform" "uppercase"
                , Attr.style "font-size" "10px"
                , Attr.style "letter-spacing" "0.06em"
                ]
                [ text ("Step " ++ String.fromInt n) ]

        PartRow { id, file, color } ->
            let
                domId =
                    partDomId id

                description =
                    ctx.config.describe file

                title =
                    if description == "" then
                        stripDat file

                    else
                        description

                colour =
                    ctx.config.colourName color

                selected =
                    Set.member id ctx.config.selection
            in
            div
                [ Attr.id domId
                , Attr.attribute "role" "treeitem"
                , Attr.attribute "aria-level" "2"
                , Attr.attribute "aria-selected" (boolString selected)
                , Attr.attribute "aria-label" (title ++ ", " ++ colour)
                , tabIndexFor ctx domId
                , Attr.class "tree-row"
                , Html.Events.on "click" (shiftDecoder id ctx.config.onSelect)
                , Html.Events.preventDefaultOn "keydown"
                    (keyDecoder { ids = ctx.ids, self = domId, focus = ctx.config.focus, activate = ctx.config.onSelect id })
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "8px"
                , Attr.style "min-width" "0"
                , Attr.style "min-height" "28px"
                , Attr.style "padding" "4px 8px"
                , Attr.style "border-radius" "4px"
                , Attr.style "cursor" "pointer"
                , Attr.style "background"
                    (if selected then
                        "color-mix(in srgb, var(--color-brand-yellow) 25%, transparent)"

                     else
                        "transparent"
                    )
                , Attr.style "box-shadow"
                    (if selected then
                        "inset 0 0 0 1px var(--color-brand-yellow)"

                     else
                        "none"
                    )
                ]
                [ span
                    [ Attr.style "display" "block"
                    , Attr.style "flex-shrink" "0"
                    , Attr.style "width" "10px"
                    , Attr.style "height" "10px"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "background" (ctx.config.colourCss color)
                    ]
                    []
                , span
                    [ Attr.style "display" "block"
                    , Attr.style "min-width" "0"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "text-overflow" "ellipsis"
                    ]
                    [ span
                        [ Attr.style "font-size" "13px"
                        , Attr.style "color" Theme.textPrimary
                        ]
                        [ text title ]
                    , span
                        [ Attr.style "font-size" "11px"
                        , Attr.style "color" Theme.textMuted
                        , Attr.style "margin-left" "6px"
                        ]
                        [ text (stripDat file ++ " · " ++ colour) ]
                    ]
                ]

        MoreRow n ->
            div
                [ Attr.attribute "role" "presentation"
                , Attr.style "padding" "2px 8px 2px 16px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                ]
                [ text ("… " ++ String.fromInt n ++ " more") ]


stripDat : String -> String
stripDat file =
    if String.endsWith ".dat" (String.toLower file) then
        String.dropRight 4 file

    else
        file


pluralParts : Int -> String
pluralParts parts =
    String.fromInt parts
        ++ (if parts == 1 then
                " part"

            else
                " parts"
           )


boolString : Bool -> String
boolString flag =
    if flag then
        "true"

    else
        "false"


shiftDecoder : Int -> (Int -> Bool -> msg) -> Decode.Decoder msg
shiftDecoder id onSelect =
    Decode.map (onSelect id) (Decode.oneOf [ Decode.field "shiftKey" Decode.bool, Decode.succeed False ])


{-| What a focused row does with a key: its DOM id, the other interactive rows in
order, and the messages to send.
-}
type alias KeySpec msg =
    { ids : List String
    , self : String
    , focus : String -> msg
    , activate : Bool -> msg
    }


{-| Handles the keys the tree uses and leaves every other key alone.
-}
keyDecoder : KeySpec msg -> Decode.Decoder ( msg, Bool )
keyDecoder spec =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.oneOf [ Decode.field "shiftKey" Decode.bool, Decode.succeed False ])
        |> Decode.andThen
            (\( key, shift ) ->
                case keyAction spec key shift of
                    Just msg ->
                        Decode.succeed ( msg, True )

                    Nothing ->
                        Decode.fail ("unhandled key " ++ key)
            )


keyAction : KeySpec msg -> String -> Bool -> Maybe msg
keyAction spec key shift =
    case key of
        "ArrowDown" ->
            Just (spec.focus (neighbour 1 spec.ids spec.self))

        "ArrowUp" ->
            Just (spec.focus (neighbour -1 spec.ids spec.self))

        "Home" ->
            Maybe.map spec.focus (List.head spec.ids)

        "End" ->
            Maybe.map spec.focus (List.head (List.reverse spec.ids))

        "Enter" ->
            Just (spec.activate shift)

        " " ->
            Just (spec.activate shift)

        _ ->
            Nothing


{-| The id `delta` places away from `current`, staying put at either end.
-}
neighbour : Int -> List String -> String -> String
neighbour delta ids current =
    let
        target =
            clamp 0 (List.length ids - 1) (indexOf current ids + delta)
    in
    ids
        |> List.drop target
        |> List.head
        |> Maybe.withDefault current


indexOf : String -> List String -> Int
indexOf target ids =
    ids
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, id ) -> id == target)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0
