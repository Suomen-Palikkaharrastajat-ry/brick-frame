module Studio.Palette exposing (Config, Index(..), State, init, view)

{-| The part palette: search the library, look at a part, add it.

The catalogue itself is `Studio.PartIndex`. Without it (an LDraw library synced
before the index existed, or one that is not there) the palette still works: a
part number can be typed and added directly.

-}

import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Studio.PartIndex as PartIndex exposing (Part, PartIndex)
import UI.Theme as Theme


{-| Whether the catalogue has arrived.
-}
type Index
    = Loading
    | Loaded PartIndex
    | Failed String


{-| What the palette remembers between renders: the catalogue, what is being
searched for and which part is picked.
-}
type alias State =
    { index : Index
    , query : String
    , category : Maybe String
    , armed : Maybe Part
    , manual : String
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
    , preview : Html msg
    }


{-| A palette waiting for its catalogue.
-}
init : State
init =
    { index = Loading
    , query = ""
    , category = Nothing
    , armed = Nothing
    , manual = ""
    }


{-| Most results listed at once; the rest are reached by searching.
-}
resultLimit : Int
resultLimit =
    60


view : Config msg -> State -> Html msg
view config state =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "8px"
        , Attr.style "min-height" "0"
        , Attr.style "height" "100%"
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
                , categoryPicker config state index
                , config.preview
                , addButton config state
                , results config state index
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
         , Attr.placeholder "Search parts"
         , Attr.attribute "aria-label" "Search parts"
         , Attr.value state.query
         , Html.Events.onInput config.onQuery
         ]
            ++ inputAttrs
        )
        []


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


results : Config msg -> State -> PartIndex -> Html msg
results config state index =
    let
        found =
            PartIndex.search resultLimit state.query state.category index
    in
    div
        [ Attr.style "flex" "1"
        , Attr.style "min-height" "80px"
        , Attr.style "overflow-y" "auto"
        , Attr.style "font-size" "12px"
        ]
        (if List.isEmpty found then
            [ note "No parts match." ]

         else
            List.map (resultRow config state) found
        )


resultRow : Config msg -> State -> Part -> Html msg
resultRow config state part =
    let
        isArmed =
            Maybe.map .file state.armed == Just part.file
    in
    div
        [ Html.Events.onClick (config.onArm part)
        , Html.Events.on "dblclick" (Decode.succeed config.onAdd)
        , Attr.style "padding" "4px 6px"
        , Attr.style "cursor" "pointer"
        , Attr.style "border-radius" "4px"
        , Attr.style "background"
            (if isArmed then
                "color-mix(in srgb, var(--color-brand-yellow) 30%, transparent)"

             else
                "transparent"
            )
        ]
        [ div [ Attr.style "font-family" "monospace", Attr.style "color" Theme.textPrimary ] [ text part.file ]
        , div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px" ] [ text part.description ]
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
