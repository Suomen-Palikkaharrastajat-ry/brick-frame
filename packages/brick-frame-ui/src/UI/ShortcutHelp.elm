module UI.ShortcutHelp exposing (Config, view)

{-| The keyboard shortcut reference dialog.

The rows come from the host (normally `Viewport.Shortcuts.helpRows`) so the text
cannot drift away from the bindings it documents.

It is a modal `role="dialog"` labelled by its title. The close button carries
the id `shortcut-help-close` so the host can move focus there on open.

-}

import Html exposing (Html, button, dd, div, dl, dt, h2, h3, kbd, text)
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

Content comes from `Viewport.Shortcuts.helpRows` so it cannot drift away from the
bindings it documents.

-}
view : Config msg -> Html msg
view config =
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "shortcut-help-title"
        , Attr.style "position" "absolute"
        , Attr.style "top" "50%"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translate(-50%, -50%)"
        , Attr.style "max-width" "min(720px, calc(100% - 24px))"
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
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "gap" "12px"
            , Attr.style "margin-bottom" "12px"
            ]
            [ h2
                [ Attr.id "shortcut-help-title"
                , Attr.style "margin" "0"
                , Attr.style "font-weight" "600"
                , Attr.style "font-size" "13px"
                ]
                [ text "Keyboard shortcuts" ]
            , button
                [ Attr.id "shortcut-help-close"
                , Html.Events.onClick config.onClose
                , Touch.onTap config.onClose
                , Attr.title "Close (Esc)"
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
        , div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "repeat(auto-fit, minmax(220px, 1fr))"
            , Attr.style "gap" "8px 24px"
            ]
            (List.map viewShortcutGroup config.rows)
        , div
            [ Attr.style "margin-top" "12px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text "Street level flies through walls — there is no collision." ]
        ]


{-| One headed group of the shortcut reference.
-}
viewShortcutGroup : ( String, List ( String, String ) ) -> Html msg
viewShortcutGroup ( heading, rows ) =
    div []
        [ h3
            [ Attr.style "margin" "0 0 4px"
            , Attr.style "font-weight" "400"
            , Attr.style "color" Theme.textMuted
            , Attr.style "text-transform" "uppercase"
            , Attr.style "letter-spacing" "0.06em"
            , Attr.style "font-size" "10px"
            ]
            [ text heading ]
        , dl
            [ Attr.style "margin" "0"
            , Attr.style "display" "grid"

            -- A fixed key column, so descriptions line up from group to group.
            , Attr.style "grid-template-columns" "10.5em minmax(0, 1fr)"
            , Attr.style "column-gap" "12px"
            , Attr.style "line-height" "1.8"
            ]
            (List.concatMap viewShortcutRow rows)
        ]


{-| One key/description pair of the shortcut reference.
-}
viewShortcutRow : ( String, String ) -> List (Html msg)
viewShortcutRow ( keys, description ) =
    [ dt
        [ Attr.style "margin" "0"
        , Attr.style "font-weight" "400"
        ]
        (viewKeys keys)
    , dd
        [ Attr.style "margin" "0"
        ]
        [ text description ]
    ]


{-| Render a key combination such as "Ctrl Shift G" or "Enter / Double-click".

Alternatives are separated by " / ". Within an alternative, tokens that look
like keys become key caps and the rest stay as plain words.

-}
viewKeys : String -> List (Html msg)
viewKeys keys =
    String.split " / " keys
        |> List.map viewAlternative
        |> List.intersperse [ text " / " ]
        |> List.concat


{-| One alternative of a key combination, e.g. "Shift + drag".
-}
viewAlternative : String -> List (Html msg)
viewAlternative alternative =
    String.split " " alternative
        |> List.filter (\token -> token /= "")
        |> List.map (\token -> ( token, isKeyToken token ))
        |> viewTokens Nothing


{-| Render tokens in order. Two adjacent key caps get a thin space between them,
and every other neighbour gets an ordinary space.
-}
viewTokens : Maybe Bool -> List ( String, Bool ) -> List (Html msg)
viewTokens previous tokens =
    case tokens of
        [] ->
            []

        ( token, isKey ) :: rest ->
            let
                separator =
                    case previous of
                        Nothing ->
                            []

                        Just True ->
                            if isKey then
                                [ text "\u{2009}" ]

                            else
                                [ text " " ]

                        Just False ->
                            [ text " " ]

                piece =
                    if isKey then
                        [ kbd keyCapStyles [ text token ] ]

                    else
                        [ text token ]
            in
            separator ++ piece ++ viewTokens (Just isKey) rest


{-| Whether a token is a key cap rather than a word. Key names are listed
explicitly. Other short tokens count as keys only when they contain no lowercase
letters, so "W", "[" and "5" are keys while "Drag", "Click" and "a" are words.
A lone "+" is always a plain sign.
-}
isKeyToken : String -> Bool
isKeyToken token =
    if token == "+" then
        False

    else
        List.member token namedKeys
            || (String.length token <= 6 && not (String.any Char.isLower token))


{-| Key names that are longer than the short-token rule allows, or that would
otherwise be mistaken for plain words.
-}
namedKeys : List String
namedKeys =
    [ "Esc", "Enter", "Shift", "Ctrl", "Space", "Delete", "Arrows", "Home", "PageUp", "PageDown", "Tab", "Backspace" ]


{-| Styling for a key cap.
-}
keyCapStyles : List (Html.Attribute msg)
keyCapStyles =
    [ Attr.style "padding" "1px 6px"
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-bottom-width" "2px"
    , Attr.style "border-radius" "4px"
    , Attr.style "background" Theme.panelSubtleBackground
    , Attr.style "font-family" "monospace"
    , Attr.style "font-size" "11px"
    ]
