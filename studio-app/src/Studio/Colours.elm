module Studio.Colours exposing
    ( Config
    , Swatch
    , all
    , css
    , nameOf
    , view
    )

{-| The colour picker: a WAI-ARIA listbox of swatches built from the generated
LDraw colour table, with names from `LDraw.ColorData` and availability
indicators validated against real LEGO colour data.

Selection does not follow focus. Arrow keys, Home and End only move DOM focus
between swatches (via the host's `focus` callback); a colour is picked by
Enter, Space or click, because picking recolours the selected parts. The grid
uses a roving tabindex, so the whole picker is one tab stop.

When a part is armed or selected, each swatch shows a small dot: green for in
production, yellow for out of production, red for not made in that part.

-}

import Array exposing (Array)
import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.ColorAvailability as ColorAvailability exposing (ColorAvailability(..))
import LDraw.ColorData as ColorData
import UI.Theme as Theme


{-| Number of swatch columns in the grid. The CSS grid uses exactly this many
columns, so Up and Down can move by one row.
-}
columns : Int
columns =
    8


{-| One colour: its LDraw code, its name and how to paint it.
-}
type alias Swatch =
    { code : Int
    , name : String
    , css : String
    }


{-| Configuration for rendering the colour picker.

`focus` asks the host to move DOM focus to the element with the given id.

-}
type alias Config msg =
    { current : Int
    , onPick : Int -> msg
    , focus : String -> msg
    , armedPart : Maybe String
    , filterOfficialOnly : Bool
    , onToggleFilter : msg
    }


{-| Every real colour, by code. 16 ("same as the parent") and 24 (edge colour)
are not colours of a part, so they are left out.
-}
all : List Swatch
all =
    ColorData.colorTable
        |> Dict.toList
        |> List.filter (\( code, _ ) -> code /= 16 && code /= 24)
        |> List.map (\( code, rgba ) -> { code = code, name = nameOf code, css = css rgba })


{-| The human name of an LDraw colour code, falling back to "Colour <code>"
for codes without a name.
-}
nameOf : Int -> String
nameOf code =
    Dict.get code ColorData.colorNames
        |> Maybe.withDefault ("Colour " ++ String.fromInt code)


{-| A CSS colour for channels in 0–1.
-}
css : { r : Float, g : Float, b : Float, alpha : Float } -> String
css c =
    let
        channel v =
            String.fromInt (round (clamp 0 1 v * 255))
    in
    "rgba(" ++ String.join "," [ channel c.r, channel c.g, channel c.b, String.fromFloat c.alpha ] ++ ")"


{-| The listbox of swatches with the current colour labelled above it, a
filter toggle and availability status for the armed or selected part.
-}
view : Config msg -> Html msg
view config =
    let
        filteredSwatches =
            case config.armedPart of
                Just partFile ->
                    if config.filterOfficialOnly then
                        List.filter
                            (\s -> ColorAvailability.checkAvailability partFile s.code /= Unofficial)
                            all

                    else
                        all

                Nothing ->
                    all

        codes =
            List.map .code filteredSwatches

        -- Roving tabindex: the current colour if it is visible, else the first.
        tabStop =
            if List.member config.current codes then
                config.current

            else
                List.head codes |> Maybe.withDefault -1

        grid =
            { codes = Array.fromList codes, tabStop = tabStop }
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        ]
        [ headerRow config
        , currentRow config
        , div
            [ Attr.attribute "role" "listbox"
            , Attr.attribute "aria-label" "Colours"
            , Attr.style "display" "grid"
            , Attr.style "grid-template-columns" ("repeat(" ++ String.fromInt columns ++ ", 24px)")
            , Attr.style "gap" "4px"
            , Attr.style "max-height" "140px"
            , Attr.style "overflow-y" "auto"
            , Attr.style "padding" "2px"
            ]
            (List.indexedMap (swatch config grid) filteredSwatches)
        , statusBanner config
        ]


headerRow : Config msg -> Html msg
headerRow config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "flex-end"
        , Attr.style "align-items" "center"
        , Attr.style "font-family" "inherit"
        , Attr.style "font-size" "11px"
        ]
        [ button
            [ Html.Events.onClick config.onToggleFilter
            , Attr.attribute "aria-pressed"
                (if config.filterOfficialOnly then
                    "true"

                 else
                    "false"
                )
            , Attr.type_ "button"
            , Attr.class "ui-button"
            , Attr.title "Only list the colours this part was made in"
            , Attr.style "padding" "4px 10px"
            , Attr.style "background"
                (if config.filterOfficialOnly then
                    Theme.brandYellow

                 else
                    Theme.panelSubtleBackground
                )
            , Attr.style "color"
                (if config.filterOfficialOnly then
                    Theme.brand

                 else
                    Theme.textPrimary
                )
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "12px"
            , Attr.style "cursor" "pointer"
            , Attr.style "font-size" "12px"
            , Attr.style "font-family" "inherit"
            ]
            [ text "Official colours only"
            ]
        ]


{-| The current colour as a 16px swatch followed by its name and code.
-}
currentRow : Config msg -> Html msg
currentRow config =
    let
        background =
            all
                |> List.filter (\s -> s.code == config.current)
                |> List.head
                |> Maybe.map .css
                |> Maybe.withDefault "transparent"
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "font-family" "sans-serif"
        , Attr.style "font-size" "13px"
        , Attr.style "color" Theme.textPrimary
        , Attr.style "min-width" "0"
        ]
        [ span
            [ Attr.attribute "aria-hidden" "true"
            , Attr.style "display" "inline-block"
            , Attr.style "flex-shrink" "0"
            , Attr.style "width" "16px"
            , Attr.style "height" "16px"
            , Attr.style "border-radius" "3px"
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "background" background
            ]
            []
        , span
            [ Attr.style "overflow" "hidden"
            , Attr.style "text-overflow" "ellipsis"
            , Attr.style "white-space" "nowrap"
            ]
            [ text (nameOf config.current) ]
        , span [ Attr.style "color" Theme.textMuted, Attr.style "flex-shrink" "0" ]
            [ text ("· " ++ String.fromInt config.current) ]
        ]


statusBanner : Config msg -> Html msg
statusBanner config =
    case config.armedPart of
        Just file ->
            let
                status =
                    ColorAvailability.checkAvailability file config.current

                ( statusText, statusColor ) =
                    case status of
                        InProduction { yearIntroduced, setFrequency } ->
                            ( "Official: In Production (since " ++ String.fromInt yearIntroduced ++ ", " ++ String.fromInt setFrequency ++ " sets)"
                            , "color-mix(in srgb, var(--color-brand) 85%, transparent)"
                            )

                        OutOfProduction { yearIntroduced, yearDiscontinued } ->
                            ( "Official: Retired (" ++ String.fromInt yearIntroduced ++ "–" ++ String.fromInt yearDiscontinued ++ ")"
                            , "var(--color-brand-yellow)"
                            )

                        Unofficial ->
                            ( "Unofficial color for " ++ file
                            , "var(--color-brand-red)"
                            )
            in
            div
                [ Attr.style "font-family" "inherit"
                , Attr.style "font-size" "10px"
                , Attr.style "color" statusColor
                , Attr.style "overflow" "hidden"
                , Attr.style "text-overflow" "ellipsis"
                , Attr.style "white-space" "nowrap"
                ]
                [ text statusText ]

        Nothing ->
            text ""


{-| The grid context a swatch needs for keyboard navigation: the visible codes
in order, and the code that holds the roving tab stop.
-}
type alias Grid =
    { codes : Array Int
    , tabStop : Int
    }


{-| Where a navigation key moves focus, as an index into the visible list.
Nothing means the key is not handled here.
-}
keyTarget : String -> Int -> Int -> Maybe Int
keyTarget key index count =
    case key of
        "ArrowRight" ->
            if index + 1 < count then
                Just (index + 1)

            else
                Nothing

        "ArrowLeft" ->
            if index > 0 then
                Just (index - 1)

            else
                Nothing

        "ArrowDown" ->
            if index + columns < count then
                Just (index + columns)

            else
                Nothing

        "ArrowUp" ->
            if index - columns >= 0 then
                Just (index - columns)

            else
                Nothing

        "Home" ->
            if count > 0 then
                Just 0

            else
                Nothing

        "End" ->
            if count > 0 then
                Just (count - 1)

            else
                Nothing

        _ ->
            Nothing


{-| The keydown handler for one swatch. Only navigation keys are handled (and
prevented); Enter and Space are left to the native button click, which picks.
-}
navigationDecoder : Config msg -> Grid -> Int -> Decode.Decoder ( msg, Bool )
navigationDecoder config grid index =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case keyTarget key index (Array.length grid.codes) of
                    Just target ->
                        case Array.get target grid.codes of
                            Just code ->
                                Decode.succeed ( config.focus (swatchId code), True )

                            Nothing ->
                                Decode.fail "no swatch at target"

                    Nothing ->
                        Decode.fail ("unhandled key " ++ key)
            )


swatchId : Int -> String
swatchId code =
    "swatch-" ++ String.fromInt code


swatch : Config msg -> Grid -> Int -> Swatch -> Html msg
swatch config grid index s =
    let
        selected =
            s.code == config.current

        availability =
            Maybe.map (\file -> ColorAvailability.checkAvailability file s.code) config.armedPart

        indicator =
            case availability of
                Just (InProduction _) ->
                    dot "color-mix(in srgb, var(--color-brand) 80%, white)"

                Just (OutOfProduction _) ->
                    dot "var(--color-brand-yellow)"

                Just Unofficial ->
                    dot "var(--color-brand-red)"

                Nothing ->
                    text ""

        availabilitySuffix =
            case availability of
                Just (InProduction _) ->
                    ", in production"

                Just (OutOfProduction _) ->
                    ", out of production"

                Just Unofficial ->
                    ", not made in this part"

                Nothing ->
                    ""
    in
    button
        [ Html.Events.onClick (config.onPick s.code)
        , Html.Events.preventDefaultOn "keydown" (navigationDecoder config grid index)
        , Attr.id (swatchId s.code)
        , Attr.class "swatch"
        , Attr.attribute "role" "option"
        , Attr.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-label" (s.name ++ availabilitySuffix)
        , Attr.title (s.name ++ " (code " ++ String.fromInt s.code ++ ")")
        , Attr.tabindex
            (if s.code == grid.tabStop then
                0

             else
                -1
            )
        , Attr.style "position" "relative"
        , Attr.style "width" "24px"
        , Attr.style "height" "24px"
        , Attr.style "padding" "0"
        , Attr.style "background" s.css
        , Attr.style "border"
            (if selected then
                "2px solid " ++ Theme.brand

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        ]
        [ indicator ]


dot : String -> Html msg
dot background =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "2px"
        , Attr.style "right" "2px"
        , Attr.style "width" "5px"
        , Attr.style "height" "5px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" background
        , Attr.style "border" "1px solid var(--color-bg-page)"
        ]
        []
