module UI.Button exposing (IconConfig, action, icon, link, primary, secondary, toggle)

{-| Toolbar buttons in the panel style shared by the viewer and the studio.

Every button carries the `ui-button` class, which `theme.css` uses for the
hover state; keyboard focus gets the global `:focus-visible` ring.

A disabled button stays opaque. These buttons float over the 3D canvas, and a
translucent one took on the colour of whatever part was behind it.

-}

import Html exposing (Html, a, button, span, text)
import Html.Attributes as Attr
import Html.Events
import UI.Theme as Theme


{-| Attributes of the quiet bordered button used for toolbar actions.
-}
secondaryStyle : List (Html.Attribute msg)
secondaryStyle =
    [ Attr.class "ui-button"
    , Attr.style "padding" "8px 14px"
    , Attr.style "background" Theme.panelSubtleBackground
    , Attr.style "color" Theme.textPrimary
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "6px"
    , Attr.style "font-family" "inherit"
    , Attr.style "font-size" "13px"
    , Attr.style "font-weight" "500"
    , Attr.style "line-height" "1.2"
    , Attr.style "cursor" "pointer"
    , Attr.style "white-space" "nowrap"
    ]


{-| How a disabled button looks: still solid, with muted text.
-}
disabledStyle : Bool -> List (Html.Attribute msg)
disabledStyle enabled =
    if enabled then
        []

    else
        [ Attr.disabled True
        , Attr.style "color" "var(--color-text-subtle)"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "cursor" "default"
        ]


{-| How a switched-on toggle looks: the brand yellow.
-}
pressedStyle : Bool -> List (Html.Attribute msg)
pressedStyle active =
    if active then
        [ Attr.style "background" Theme.brandYellow
        , Attr.style "color" Theme.brand
        , Attr.style "border-color" "color-mix(in srgb, var(--color-brand) 25%, var(--color-brand-yellow))"
        ]

    else
        []


{-| A toolbar button that sends `msg` when clicked.
-}
secondary : msg -> String -> Html msg
secondary msg label =
    button (Attr.type_ "button" :: Html.Events.onClick msg :: secondaryStyle) [ text label ]


{-| The one button a toolbar leads with, in the dark brand colour.
-}
primary : msg -> String -> Html msg
primary msg label =
    button
        (Attr.type_ "button"
            :: Html.Events.onClick msg
            :: secondaryStyle
            ++ [ Attr.style "background" Theme.brand
               , Attr.style "color" Theme.textOnDark
               , Attr.style "border-color" Theme.brand
               ]
        )
        [ text label ]


{-| A plain action that can be unavailable, such as undo with nothing to undo.
-}
action : { enabled : Bool } -> msg -> String -> Html msg
action { enabled } msg label =
    button
        (Attr.type_ "button" :: Html.Events.onClick msg :: secondaryStyle ++ disabledStyle enabled)
        [ text label ]


{-| A toolbar button that shows whether it is switched on, in the brand colours.
`enabled = False` greys it out and stops it sending. Screen readers hear the
state through `aria-pressed`.
-}
toggle : { active : Bool, enabled : Bool } -> msg -> String -> Html msg
toggle { active, enabled } msg label =
    button
        (Attr.type_ "button"
            :: Html.Events.onClick msg
            :: Attr.attribute "aria-pressed" (boolString active)
            :: secondaryStyle
            ++ pressedStyle active
            ++ disabledStyle enabled
        )
        [ text label ]


{-| What an icon button shows and says.

  - `label` is its accessible name and the start of its tooltip.
  - `shortcut` is the key that does the same, shown in the tooltip
    ("Move (M)") and announced through `aria-keyshortcuts`.
  - `pressed` makes it a toggle; `Nothing` is a plain action.
  - `showLabel` prints the label next to the icon as well.

-}
type alias IconConfig msg =
    { icon : Html msg
    , label : String
    , shortcut : Maybe String
    , pressed : Maybe Bool
    , enabled : Bool
    , showLabel : Bool
    }


{-| A compact button led by an icon, for tool rails and app bars.
-}
icon : IconConfig msg -> msg -> Html msg
icon config msg =
    let
        tooltip =
            case config.shortcut of
                Just key ->
                    config.label ++ " (" ++ key ++ ")"

                Nothing ->
                    config.label

        pressedAttrs =
            case config.pressed of
                Just active ->
                    Attr.attribute "aria-pressed" (boolString active) :: pressedStyle active

                Nothing ->
                    []

        shortcutAttrs =
            case config.shortcut of
                Just key ->
                    [ Attr.attribute "aria-keyshortcuts" (ariaKeys key) ]

                Nothing ->
                    []

        content =
            if config.showLabel then
                [ config.icon, span [] [ text config.label ] ]

            else
                [ config.icon ]

        nameAttrs =
            if config.showLabel then
                []

            else
                [ Attr.attribute "aria-label" config.label ]
    in
    button
        (Attr.type_ "button"
            :: Html.Events.onClick msg
            :: Attr.title tooltip
            :: secondaryStyle
            ++ [ Attr.style "display" "inline-flex"
               , Attr.style "align-items" "center"
               , Attr.style "justify-content" "center"
               , Attr.style "gap" "6px"
               , Attr.style "min-width" "36px"
               , Attr.style "height" "36px"
               , Attr.style "padding"
                    (if config.showLabel then
                        "0 12px"

                     else
                        "0"
                    )
               ]
            ++ nameAttrs
            ++ shortcutAttrs
            ++ pressedAttrs
            ++ disabledStyle config.enabled
        )
        content


{-| `aria-keyshortcuts` spells modifiers out and joins chords with `+`:
"Ctrl Z" becomes "Control+Z", and alternatives separated by " / " become a
space-separated list.
-}
ariaKeys : String -> String
ariaKeys keys =
    keys
        |> String.split " / "
        |> List.map
            (\chord ->
                chord
                    |> String.words
                    |> List.map
                        (\key ->
                            case key of
                                "Ctrl" ->
                                    "Control"

                                "Esc" ->
                                    "Escape"

                                _ ->
                                    key
                        )
                    |> String.join "+"
            )
        |> String.join " "


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"


{-| A toolbar button that is a link, styled like `secondary`.
-}
link : String -> String -> Html msg
link href label =
    a (Attr.href href :: Attr.style "text-decoration" "none" :: Attr.style "display" "inline-block" :: secondaryStyle) [ text label ]
