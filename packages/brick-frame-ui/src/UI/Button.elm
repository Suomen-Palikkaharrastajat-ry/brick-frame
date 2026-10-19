module UI.Button exposing (link, secondary)

{-| Toolbar buttons in the panel style shared by the viewer and the studio.
-}

import Html exposing (Html, a, button, text)
import Html.Attributes as Attr
import Html.Events
import UI.Theme as Theme


{-| Attributes of the quiet bordered button used for toolbar actions.
-}
secondaryStyle : List (Html.Attribute msg)
secondaryStyle =
    [ Attr.style "padding" "8px 14px"
    , Attr.style "background" Theme.panelSubtleBackground
    , Attr.style "color" Theme.textPrimary
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "6px"
    , Attr.style "box-shadow" "0 10px 24px color-mix(in srgb, var(--color-brand) 10%, transparent)"
    , Attr.style "font-family" "monospace"
    , Attr.style "font-size" "12px"
    , Attr.style "cursor" "pointer"
    ]


{-| A toolbar button that sends `msg` when clicked.
-}
secondary : msg -> String -> Html msg
secondary msg label =
    button (Html.Events.onClick msg :: secondaryStyle) [ text label ]


{-| A toolbar button that is a link, styled like `secondary`.
-}
link : String -> String -> Html msg
link href label =
    a (Attr.href href :: Attr.style "text-decoration" "none" :: Attr.style "display" "inline-block" :: secondaryStyle) [ text label ]
