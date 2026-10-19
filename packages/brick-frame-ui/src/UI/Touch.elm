module UI.Touch exposing (onTap)

{-| Touch handling for buttons.

The canvas swallows touch events to implement gestures, which can leave a
tapped button without its `click`; firing on `touchstart` as well keeps controls
responsive on touch devices.

-}

import Html
import Html.Events
import Json.Decode as Decode


onTap : msg -> Html.Attribute msg
onTap msg =
    Html.Events.preventDefaultOn "touchstart" (Decode.succeed ( msg, True ))
