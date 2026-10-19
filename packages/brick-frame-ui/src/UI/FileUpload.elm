module UI.FileUpload exposing (Config, view)

{-| Upload control for loading local LDraw and Studio files.
-}

import Html exposing (Html, button, text)
import Html.Attributes as Attr
import Html.Events
import UI.Theme as Theme


type alias Config msg =
    { onRequestFileUpload : msg }


view : Config msg -> List (Html msg)
view config =
    [ button
        [ Html.Events.onClick config.onRequestFileUpload
        , Attr.style "padding" "8px 14px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "box-shadow" "0 10px 24px color-mix(in srgb, var(--color-brand) 10%, transparent)"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        ]
        [ text "Upload File" ]
    ]
