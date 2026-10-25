module UI.ViewPad exposing (Config, Play, view, viewKeepingFocus)

{-| The on-screen navigation pad: fit view, rotate, walk, help and (optionally)
play/pause.

Rotate buttons send `onStartRotate` on press (or Enter/Space keydown when
focused) and `onEndRotate` on release, key up, blur or when the pointer leaves,
so the viewport can keep turning while held.

Every button carries an accessible name and the `pad-button` class, which the
global stylesheet uses for its focus ring.

-}

import Html exposing (Html, button, div)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import UI.Focus as Focus
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
    viewWith [] config


{-| `view`, with every button also keeping focus off itself after a mouse
press (`UI.Focus.keepFocusOnCanvas`), so the host's keyboard shortcuts keep
working after a click. The message is sent by nothing; hosts pass a no-op.
-}
viewKeepingFocus : msg -> Config msg -> Html msg
viewKeepingFocus noOp config =
    viewWith [ Focus.keepFocusOnCanvas noOp ] config


viewWith : List (Html.Attribute msg) -> Config msg -> Html msg
viewWith guard config =
    let
        step =
            0.08
    in
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Camera"
        , Attr.style "position" "absolute"
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
        [ fitViewButton guard config.onFit
        , rotateButton guard config "Orbit up" (Icon.feather "chevron-up") (config.onStartRotate 0 step)
        , walkButton guard config
        , rotateButton guard config "Orbit left" (Icon.feather "chevron-left") (config.onStartRotate step 0)
        , case config.play of
            Just play ->
                playButton guard play

            Nothing ->
                div [] []
        , rotateButton guard config "Orbit right" (Icon.feather "chevron-right") (config.onStartRotate -step 0)
        , div [] []
        , rotateButton guard config "Orbit down" (Icon.feather "chevron-down") (config.onStartRotate 0 -step)
        , helpButton guard config
        ]


{-| Drop to street level, or step back out.

Shown as engaged while walking, following the play button's precedent, because
the same drag means two different things depending on the mode.

-}
walkButton : List (Html.Attribute msg) -> Config msg -> Html msg
walkButton guard config =
    let
        label =
            "Street level view (G)"
    in
    button
        (Html.Events.onClick config.onToggleWalk
            :: Touch.onTap config.onToggleWalk
            :: Attr.title label
            :: labelled label
            :: ariaFlag "aria-pressed" config.walking
            :: Attr.class "pad-button"
            :: toggleStyles config.walking
            ++ guard
        )
        [ Icon.feather "user" ]


{-| Show or hide the keyboard shortcut list.
-}
helpButton : List (Html.Attribute msg) -> Config msg -> Html msg
helpButton guard config =
    let
        label =
            "Keyboard shortcuts (?)"
    in
    button
        (Html.Events.onClick config.onToggleHelp
            :: Touch.onTap config.onToggleHelp
            :: Attr.title label
            :: labelled label
            :: ariaFlag "aria-pressed" config.helpOpen
            :: ariaFlag "aria-expanded" config.helpOpen
            :: Attr.class "pad-button"
            :: toggleStyles config.helpOpen
            ++ guard
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
fitViewButton : List (Html.Attribute msg) -> msg -> Html msg
fitViewButton guard onFit =
    let
        label =
            "Fit model to view (F)"
    in
    button
        (Html.Events.onClick onFit
            :: Touch.onTap onFit
            :: Attr.title label
            :: labelled label
            :: Attr.class "pad-button"
            :: toggleStyles False
            ++ guard
        )
        [ Icon.feather "maximize" ]


{-| A press-and-hold button. Pointer, touch and keyboard (Enter or Space, while
focused) all start the rotation with `startMsg` and stop it on release.
-}
rotateButton : List (Html.Attribute msg) -> Config msg -> String -> Html msg -> msg -> Html msg
rotateButton guard config label icon startMsg =
    button
        (Html.Events.on "pointerdown" (Decode.succeed startMsg)
            :: Html.Events.on "pointerup" (Decode.succeed config.onEndRotate)
            :: Html.Events.on "pointercancel" (Decode.succeed config.onEndRotate)
            :: Html.Events.on "pointerleave" (Decode.succeed config.onEndRotate)
            :: Html.Events.on "mouseup" (Decode.succeed config.onEndRotate)
            :: Html.Events.on "blur" (Decode.succeed config.onEndRotate)
            :: Html.Events.preventDefaultOn "keydown" (holdKeyDown startMsg)
            :: Html.Events.preventDefaultOn "keyup" (holdKeyUp config.onEndRotate)
            :: Html.Events.preventDefaultOn "touchstart" (Decode.succeed ( startMsg, True ))
            :: Html.Events.preventDefaultOn "touchend" (Decode.succeed ( config.onEndRotate, True ))
            :: Html.Events.preventDefaultOn "touchcancel" (Decode.succeed ( config.onEndRotate, True ))
            :: Attr.title label
            :: labelled label
            :: Attr.class "pad-button"
            :: Attr.style "width" "36px"
            :: Attr.style "height" "32px"
            :: Attr.style "padding" "0"
            :: Attr.style "background" Theme.panelSubtleBackground
            :: Attr.style "color" Theme.textPrimary
            :: Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            :: Attr.style "border-radius" "4px"
            :: Attr.style "cursor" "pointer"
            :: Attr.style "font-family" "monospace"
            :: Attr.style "font-size" "12px"
            :: Attr.style "display" "flex"
            :: Attr.style "align-items" "center"
            :: Attr.style "justify-content" "center"
            :: []
            ++ guard
        )
        [ icon ]


{-| Keys that hold a rotate button down, as in a native button press.
-}
isHoldKey : String -> Bool
isHoldKey key =
    key == "Enter" || key == " "


{-| Start a hold on the first Enter or Space keydown. Auto-repeat is ignored so
the rotation starts once per press.
-}
holdKeyDown : msg -> Decode.Decoder ( msg, Bool )
holdKeyDown startMsg =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "repeat" Decode.bool)
        |> Decode.andThen
            (\( key, repeat ) ->
                if isHoldKey key && not repeat then
                    Decode.succeed ( startMsg, True )

                else
                    Decode.fail "not a hold key"
            )


{-| End a hold when Enter or Space is released.
-}
holdKeyUp : msg -> Decode.Decoder ( msg, Bool )
holdKeyUp endMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if isHoldKey key then
                    Decode.succeed ( endMsg, True )

                else
                    Decode.fail "not a hold key"
            )


{-| The `aria-label` that gives an icon-only button its accessible name. Pair it
with `Attr.title` of the same text so hovering shows what the button does.
-}
labelled : String -> Html.Attribute msg
labelled label =
    Attr.attribute "aria-label" label


{-| A boolean ARIA state attribute, rendered as "true" or "false".
-}
ariaFlag : String -> Bool -> Html.Attribute msg
ariaFlag name value =
    Attr.attribute name
        (if value then
            "true"

         else
            "false"
        )


playButton : List (Html.Attribute msg) -> Play msg -> Html msg
playButton guard play =
    let
        label =
            if play.running then
                "Pause (Space)"

            else
                "Play (Space)"
    in
    button
        (Html.Events.onClick play.onToggle
            :: Touch.onTap play.onToggle
            :: Attr.title label
            :: labelled label
            :: Attr.class "pad-button"
            :: Attr.style "font-family" "monospace"
            :: Attr.style "font-size" "12px"
            :: toggleStyles play.running
            ++ guard
        )
        [ Icon.feather
            (if play.running then
                "pause"

             else
                "play"
            )
        ]
