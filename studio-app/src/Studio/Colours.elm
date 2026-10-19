module Studio.Colours exposing (Swatch, all, css, view)

{-| The colour picker, built from the generated LDraw colour table.

Colours come from `LDraw.ColorData`, which is generated from `LDConfig.ldr`; to
offer another colour, sync a newer library — never edit the table by hand.

-}

import Dict
import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Html.Events
import LDraw.ColorData as ColorData
import UI.Theme as Theme


{-| One colour: its LDraw code and how to paint it.
-}
type alias Swatch =
    { code : Int
    , css : String
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


{-| A grid of swatches; the `current` one is outlined. Clicking sends `onPick`
with the colour's code.
-}
view : { current : Int, onPick : Int -> msg } -> Html msg
view config =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(auto-fill, 22px)"
        , Attr.style "gap" "4px"
        , Attr.style "max-height" "150px"
        , Attr.style "overflow-y" "auto"
        ]
        (List.map (swatch config) all)


swatch : { current : Int, onPick : Int -> msg } -> Swatch -> Html msg
swatch config s =
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
        , Attr.style "width" "22px"
        , Attr.style "height" "22px"
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
        []
