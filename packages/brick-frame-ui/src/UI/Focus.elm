module UI.Focus exposing (keepFocusOnCanvas)

{-| Keeping keyboard focus off buttons after a mouse click.

A focused button takes Space and Enter for itself, so after a click on a
toolbar, dock or camera button the host's keyboard shortcuts (Space to play,
Ctrl+Z to undo) would press that button again instead. `keepFocusOnCanvas`
cancels the focusing `mousedown` default; the click itself still fires.

Form fields (`input`, `select`, `textarea`, `option`, `label`) still take focus
when pressed.

Hosts pass their own no-op message, since the attribute must send something.

-}

import Html
import Html.Events
import Json.Decode as Decode


{-| Attach to a button or control that should not take focus on a mouse press.
Form fields still take focus.
-}
keepFocusOnCanvas : msg -> Html.Attribute msg
keepFocusOnCanvas noOp =
    Html.Events.preventDefaultOn "mousedown"
        (Decode.at [ "target", "tagName" ] Decode.string
            |> Decode.andThen
                (\tag ->
                    if List.member (String.toUpper tag) [ "INPUT", "SELECT", "TEXTAREA", "OPTION", "LABEL" ] then
                        Decode.fail "a form field keeps its focus"

                    else
                        Decode.succeed ( noOp, True )
                )
        )
