module UI.ShortcutHelp exposing (Config, view)

{-| The keyboard shortcut reference dialog.

The rows come from the host (normally `Viewport.Shortcuts.helpRows`) so the text
cannot drift away from the bindings it documents.

-}

import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events
import UI.Icon as Icon
import UI.Theme as Theme
import UI.Touch as Touch


{-| Groups of ( keys, description ) rows, and the message that closes the dialog.
-}
type alias Config msg =
    { rows : List ( String, List ( String, String ) )
    , onClose : msg
    }


{-| The keyboard shortcut reference.

Content comes from `UI.Shortcuts.helpRows` so it cannot drift away from the
bindings it documents.

-}
view : Config msg -> Html msg
view config =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "50%"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translate(-50%, -50%)"
        , Attr.style "max-width" "min(560px, calc(100% - 24px))"
        , Attr.style "max-height" "calc(100% - 24px)"
        , Attr.style "overflow" "auto"
        , Attr.style "pointer-events" "auto"
        , Attr.style "padding" "16px 20px"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 16%, transparent)"
        , Attr.style "font-size" "12px"
        , Attr.attribute "role" "dialog"
        , Attr.attribute "aria-label" "Keyboard shortcuts"
        ]
        (div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "gap" "12px"
            , Attr.style "margin-bottom" "12px"
            ]
            [ div
                [ Attr.style "font-weight" "600"
                , Attr.style "font-size" "13px"
                ]
                [ text "Keyboard shortcuts" ]
            , button
                [ Html.Events.onClick config.onClose
                , Touch.onTap config.onClose
                , Attr.title "Close"
                , Attr.attribute "aria-label" "Close"
                , Attr.style "padding" "2px 4px"
                , Attr.style "background" "transparent"
                , Attr.style "color" Theme.textMuted
                , Attr.style "border" "none"
                , Attr.style "cursor" "pointer"
                , Attr.style "display" "flex"
                ]
                [ Icon.feather "x" ]
            ]
            :: List.map viewShortcutGroup config.rows
            ++ [ div
                    [ Attr.style "margin-top" "12px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text "Street level flies through walls — there is no collision." ]
               ]
        )


{-| One headed group of the shortcut reference.
-}
viewShortcutGroup : ( String, List ( String, String ) ) -> Html msg
viewShortcutGroup ( heading, rows ) =
    div [ Attr.style "margin-bottom" "10px" ]
        (div
            [ Attr.style "color" Theme.textMuted
            , Attr.style "text-transform" "uppercase"
            , Attr.style "letter-spacing" "0.06em"
            , Attr.style "font-size" "10px"
            , Attr.style "margin-bottom" "4px"
            ]
            [ text heading ]
            :: List.map viewShortcutRow rows
        )


{-| One key/description line of the shortcut reference.
-}
viewShortcutRow : ( String, String ) -> Html msg
viewShortcutRow ( keys, description ) =
    div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "12px"
        , Attr.style "line-height" "1.8"
        ]
        [ div
            [ Attr.style "flex" "0 0 110px"
            , Attr.style "font-family" "monospace"
            ]
            [ text keys ]
        , div [] [ text description ]
        ]
