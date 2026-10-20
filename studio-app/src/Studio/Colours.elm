module Studio.Colours exposing
    ( Config
    , Swatch
    , all
    , css
    , view
    )

{-| The colour picker, built from the generated LDraw colour table and validated
against real LEGO color availability data.

Colours come from `LDraw.ColorData`, which is generated from `LDConfig.ldr`.
When a part is armed or selected, each swatch displays an official production
indicator (🟢 In Production, 🟡 Out of Production, or 🔴 Unavailable).

-}

import Dict
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events
import LDraw.ColorAvailability as ColorAvailability exposing (ColorAvailability(..))
import LDraw.ColorData as ColorData
import UI.Theme as Theme


{-| One colour: its LDraw code and how to paint it.
-}
type alias Swatch =
    { code : Int
    , css : String
    }


{-| Configuration for rendering the colour picker.
-}
type alias Config msg =
    { current : Int
    , onPick : Int -> msg
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
        |> List.map (\( code, rgba ) -> { code = code, css = css rgba })


{-| A CSS colour for channels in 0–1.
-}
css : { r : Float, g : Float, b : Float, alpha : Float } -> String
css c =
    let
        channel v =
            String.fromInt (round (clamp 0 1 v * 255))
    in
    "rgba(" ++ String.join "," [ channel c.r, channel c.g, channel c.b, String.fromFloat c.alpha ] ++ ")"


{-| A grid of swatches with real-LEGO availability indicators and filtering.
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
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        ]
        [ headerRow config
        , div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "repeat(auto-fill, 24px)"
            , Attr.style "gap" "4px"
            , Attr.style "max-height" "140px"
            , Attr.style "overflow-y" "auto"
            , Attr.style "padding" "2px"
            ]
            (List.map (swatch config) filteredSwatches)
        , statusBanner config
        ]


headerRow : Config msg -> Html msg
headerRow config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "space-between"
        , Attr.style "align-items" "center"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "11px"
        ]
        [ span [ Attr.style "color" Theme.textMuted ] [ text "Palette Color" ]
        , button
            [ Html.Events.onClick config.onToggleFilter
            , Attr.style "padding" "2px 6px"
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
            , Attr.style "border-radius" "4px"
            , Attr.style "cursor" "pointer"
            , Attr.style "font-size" "10px"
            , Attr.style "font-family" "monospace"
            ]
            [ text
                (if config.filterOfficialOnly then
                    "Official only"

                 else
                    "Show all"
                )
            ]
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
                [ Attr.style "font-family" "monospace"
                , Attr.style "font-size" "10px"
                , Attr.style "color" statusColor
                , Attr.style "overflow" "hidden"
                , Attr.style "text-overflow" "ellipsis"
                , Attr.style "white-space" "nowrap"
                ]
                [ text statusText ]

        Nothing ->
            text ""


swatch : Config msg -> Swatch -> Html msg
swatch config s =
    let
        indicator =
            case config.armedPart of
                Just file ->
                    case ColorAvailability.checkAvailability file s.code of
                        InProduction _ ->
                            div
                                [ Attr.style "position" "absolute"
                                , Attr.style "bottom" "2px"
                                , Attr.style "right" "2px"
                                , Attr.style "width" "5px"
                                , Attr.style "height" "5px"
                                , Attr.style "border-radius" "50%"
                                , Attr.style "background" "color-mix(in srgb, var(--color-brand) 80%, white)"
                                , Attr.style "border" "1px solid var(--color-bg-page)"
                                ]
                                []

                        OutOfProduction _ ->
                            div
                                [ Attr.style "position" "absolute"
                                , Attr.style "bottom" "2px"
                                , Attr.style "right" "2px"
                                , Attr.style "width" "5px"
                                , Attr.style "height" "5px"
                                , Attr.style "border-radius" "50%"
                                , Attr.style "background" "var(--color-brand-yellow)"
                                , Attr.style "border" "1px solid var(--color-bg-page)"
                                ]
                                []

                        Unofficial ->
                            div
                                [ Attr.style "position" "absolute"
                                , Attr.style "bottom" "2px"
                                , Attr.style "right" "2px"
                                , Attr.style "width" "5px"
                                , Attr.style "height" "5px"
                                , Attr.style "border-radius" "50%"
                                , Attr.style "background" "var(--color-brand-red)"
                                , Attr.style "border" "1px solid var(--color-bg-page)"
                                ]
                                []

                Nothing ->
                    text ""
    in
    button
        [ Html.Events.onClick (config.onPick s.code)
        , Attr.title ("LDraw colour " ++ String.fromInt s.code)
        , Attr.attribute "aria-label" ("Colour " ++ String.fromInt s.code)
        , Attr.attribute "aria-pressed"
            (if s.code == config.current then
                "true"

             else
                "false"
            )
        , Attr.style "position" "relative"
        , Attr.style "width" "24px"
        , Attr.style "height" "24px"
        , Attr.style "padding" "0"
        , Attr.style "background" s.css
        , Attr.style "border"
            (if s.code == config.current then
                "2px solid " ++ Theme.brand

             else
                "1px solid " ++ Theme.borderDefault
            )
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        ]
        [ indicator ]
