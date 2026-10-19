module UI.ViewPad exposing (Config, Play, view)

{-| The on-screen navigation pad: fit view, rotate, walk, help and (optionally)
play/pause.

Rotate buttons send `onStartRotate` on press and `onEndRotate` on release or
when the pointer leaves, so the viewport can keep turning while held.

-}

import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import UI.Icon as Icon
import UI.Theme as Theme
import UI.Touch as Touch


{-| The centre button, shown when there is something to play.
-}
type alias Play msg =
    { running : Bool
    , onToggle : msg
    }


{-| Pad state and the messages its buttons send.
-}
type alias Config msg =
    { walking : Bool
    , helpOpen : Bool
    , play : Maybe (Play msg)
    , onFit : msg
    , onToggleWalk : msg
    , onToggleHelp : msg
    , onStartRotate : Float -> Float -> msg
    , onEndRotate : msg
    }


{-| A 3×3 grid of buttons anchored to the bottom right of its container.
-}
view : Config msg -> Html msg
view config =
    let
        step =
            0.08
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "right" "12px"
        , Attr.style "bottom" "12px"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(3, 36px)"
        , Attr.style "grid-template-rows" "repeat(3, 32px)"
        , Attr.style "gap" "4px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        , Attr.style "padding" "8px"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 8%, transparent)"
        ]
        [ fitViewButton config.onFit
        , rotateButton config (Icon.feather "chevron-up") (config.onStartRotate 0 -step)
        , walkButton config
        , rotateButton config (Icon.feather "chevron-left") (config.onStartRotate step 0)
        , case config.play of
            Just play ->
                playButton play

            Nothing ->
                div [] []
        , rotateButton config (Icon.feather "chevron-right") (config.onStartRotate -step 0)
        , div [] []
        , rotateButton config (Icon.feather "chevron-down") (config.onStartRotate 0 step)
        , helpButton config
        ]


{-| Drop to street level, or step back out.

Shown as engaged while walking, following the play button's precedent, because
the same drag means two different things depending on the mode.

-}
walkButton : Config msg -> Html msg
walkButton config =
    let
        label =
            if config.walking then
                "Leave street level"

            else
                "View from street level"
    in
    button
        (Html.Events.onClick config.onToggleWalk
            :: Touch.onTap config.onToggleWalk
            :: Attr.title label
            :: Attr.attribute "aria-label" label
            :: Attr.attribute "aria-pressed"
                (if config.walking then
                    "true"

                 else
                    "false"
                )
            :: toggleStyles config.walking
        )
        [ Icon.feather "user" ]


{-| Show or hide the keyboard shortcut list.
-}
helpButton : Config msg -> Html msg
helpButton config =
    button
        (Html.Events.onClick config.onToggleHelp
            :: Touch.onTap config.onToggleHelp
            :: Attr.title "Keyboard shortcuts"
            :: Attr.attribute "aria-label" "Keyboard shortcuts"
            :: toggleStyles config.helpOpen
        )
        [ Icon.feather "help-circle" ]


{-| Shared styling for the pad's toggle buttons.

`active` swaps in the brand colours, so a mode that changes what the mouse does
is visible at a glance.

-}
toggleStyles : Bool -> List (Html.Attribute msg)
toggleStyles active =
    [ Attr.style "width" "36px"
    , Attr.style "height" "32px"
    , Attr.style "padding" "0"
    , Attr.style "background"
        (if active then
            Theme.brandYellow

         else
            Theme.panelSubtleBackground
        )
    , Attr.style "color"
        (if active then
            Theme.brand

         else
            Theme.textPrimary
        )
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "4px"
    , Attr.style "cursor" "pointer"
    , Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    ]


{-| Re-frame the whole model. The only way back to the auto-fit view once the
user has orbited away from it.
-}
fitViewButton : msg -> Html msg
fitViewButton onFit =
    button
        (Html.Events.onClick onFit
            :: Touch.onTap onFit
            :: Attr.title "Fit model to view"
            :: Attr.attribute "aria-label" "Fit model to view"
            :: toggleStyles False
        )
        [ Icon.feather "maximize" ]


rotateButton : Config msg -> Html msg -> msg -> Html msg
rotateButton config icon msg =
    button
        [ Html.Events.on "pointerdown" (Decode.succeed msg)
        , Html.Events.on "pointerup" (Decode.succeed config.onEndRotate)
        , Html.Events.on "pointercancel" (Decode.succeed config.onEndRotate)
        , Html.Events.on "pointerleave" (Decode.succeed config.onEndRotate)
        , Html.Events.on "mouseup" (Decode.succeed config.onEndRotate)
        , Html.Events.preventDefaultOn "touchstart" (Decode.succeed ( msg, True ))
        , Html.Events.preventDefaultOn "touchend" (Decode.succeed ( config.onEndRotate, True ))
        , Html.Events.preventDefaultOn "touchcancel" (Decode.succeed ( config.onEndRotate, True ))
        , Attr.style "width" "36px"
        , Attr.style "height" "32px"
        , Attr.style "padding" "0"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ icon ]


playButton : Play msg -> Html msg
playButton play =
    button
        (Html.Events.onClick play.onToggle
            :: Touch.onTap play.onToggle
            :: Attr.style "font-family" "monospace"
            :: Attr.style "font-size" "12px"
            :: toggleStyles play.running
        )
        [ Icon.feather
            (if play.running then
                "pause"

             else
                "play"
            )
        ]
