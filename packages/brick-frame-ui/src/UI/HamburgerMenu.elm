module UI.HamburgerMenu exposing (Config, Item, view)

{-| The top-left menu of the minimal apps (Brick Play): one
hamburger button that opens a small dropdown of actions, each with its keyboard
shortcut shown on the right.

The host owns the open flag and decides what the messages do. `onToggle` opens
or closes the menu, `onClose` closes it (a click on the page outside the panel,
which is covered by a transparent backdrop). Choosing an item sends its
`onSelect`; the host should close the menu in that same update. Escape is the
host's keyboard shortcut, not this view's.

Items are real buttons with `role="menuitem"`, so they take focus with Tab after
the hamburger. The wrapper has no transform or backdrop filter, so the backdrop
covers the whole window.

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import UI.Icon as Icon


{-| One row of the menu: its label, the shortcut shown on the right (`""` for
none), whether it can be chosen, and the message it sends.
-}
type alias Item msg =
    { label : String
    , shortcut : String
    , enabled : Bool
    , onSelect : msg
    }


{-| The menu's state and messages. `id` names the panel, for `aria-controls`.
-}
type alias Config msg =
    { id : String
    , open : Bool
    , onToggle : msg
    , onClose : msg
    , items : List (Item msg)
    }


{-| The hamburger button, and the panel and backdrop while the menu is open.
-}
view : Config msg -> Html msg
view config =
    div
        [ Attr.style "position" "relative"
        , Attr.style "pointer-events" "auto"
        ]
        (hamburgerButton config
            :: (if config.open then
                    [ backdrop config.onClose, panel config ]

                else
                    []
               )
        )


hamburgerButton : Config msg -> Html msg
hamburgerButton config =
    button
        [ onClick config.onToggle
        , Attr.attribute "aria-label" "Menu"
        , Attr.title "Menu"
        , Attr.class "touch-target"
        , Attr.attribute "aria-haspopup" "menu"
        , Attr.attribute "aria-expanded"
            (if config.open then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-controls" config.id
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        ]
        [ Icon.featherSized 18 "menu" ]


{-| The transparent layer behind an open menu: a click on it closes the menu.
-}
backdrop : msg -> Html msg
backdrop onClose =
    div
        [ onClick onClose
        , Attr.attribute "aria-hidden" "true"
        , Attr.style "position" "fixed"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "pointer-events" "auto"
        ]
        []


panel : Config msg -> Html msg
panel config =
    div
        [ Attr.id config.id
        , Attr.attribute "role" "menu"
        , Attr.attribute "aria-label" "Menu"
        , Attr.style "position" "absolute"
        , Attr.style "top" "calc(100% + 0.375rem)"
        , Attr.style "left" "0"
        , Attr.style "z-index" "1"
        , Attr.style "min-width" "13rem"
        , Attr.style "padding" "0.25rem"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "0.125rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.625rem"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        ]
        (List.map item config.items)


item : Item msg -> Html msg
item entry =
    button
        [ Attr.attribute "role" "menuitem"
        , Attr.class "touch-target"
        , Attr.disabled (not entry.enabled)
        , onClick entry.onSelect
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "gap" "1.5rem"
        , Attr.style "width" "100%"
        , Attr.style "padding" "0.4375rem 0.625rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background" "transparent"
        , Attr.style "color"
            (if entry.enabled then
                "var(--color-text-on-dark)"

             else
                "rgba(255, 255, 255, 0.3)"
            )
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "500"
        , Attr.style "text-align" "left"
        , Attr.style "white-space" "nowrap"
        , Attr.style "cursor"
            (if entry.enabled then
                "pointer"

             else
                "not-allowed"
            )
        ]
        [ span [] [ text entry.label ]
        , if String.isEmpty entry.shortcut then
            text ""

          else
            span
                [ Attr.style "font-size" "0.6875rem"
                , Attr.style "color" "var(--color-text-subtle)"
                , Attr.style "font-variant-numeric" "tabular-nums"
                ]
                [ text entry.shortcut ]
        ]
