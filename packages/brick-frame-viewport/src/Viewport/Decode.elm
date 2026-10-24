module Viewport.Decode exposing
    ( TouchPoint
    , canvasOriginDecoder
    , coordinateDecoder
    , keyEventDecoder
    , pointerTouchIdDecoder
    , pointerTouchPointDecoder
    , touchesDecoder
    , wheelDeltaDecoder
    )

{-| Decoders for the browser input events the viewport listens to.

Kept separate from `Viewport` so the quirks they paper over (wheel units, iOS
sending both Touch and Pointer events, `target` missing on synthetic events)
are documented in one place.

-}

import Json.Decode as Decode
import Viewport.Shortcuts as Shortcuts


type alias TouchPoint =
    { id : Int
    , x : Float
    , y : Float
    }


{-| Wheel `deltaY`, normalised to pixel units.

Browsers report scroll amounts in three different units, and only Chrome uses
pixels: Firefox reports lines (~3 per notch) and some setups report pages. Left
unnormalised, the same gesture zoomed roughly 30x slower in Firefox. `ctrlKey`
marks a trackpad pinch, which arrives as very small deltas.

Scale factors match three.js `OrbitControls`.

-}
wheelDeltaDecoder : Decode.Decoder Float
wheelDeltaDecoder =
    Decode.map3
        (\delta mode ctrlKey ->
            let
                unitScale =
                    case mode of
                        1 ->
                            16.0

                        2 ->
                            100.0

                        _ ->
                            1.0

                pinchScale =
                    if ctrlKey then
                        10.0

                    else
                        1.0
            in
            delta * unitScale * pinchScale
        )
        (Decode.field "deltaY" Decode.float)
        (Decode.oneOf [ Decode.field "deltaMode" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "ctrlKey" Decode.bool, Decode.succeed False ])


{-| Everything `UI.Shortcuts` needs from a keyboard event.

`target.tagName`, `target.isContentEditable` and `target.role` drive the typing
and widget guards. All three are
read defensively — a synthetic event, or one whose target is the document rather
than an element, has neither.

-}
keyEventDecoder : Decode.Decoder Shortcuts.KeyEvent
keyEventDecoder =
    Decode.map2 (\event role -> { event | targetRole = role })
        (Decode.map8
            (\key shift ctrl meta alt repeat targetTag targetEditable ->
                { key = key
                , shift = shift
                , ctrl = ctrl
                , meta = meta
                , alt = alt
                , repeat = repeat
                , targetTag = targetTag
                , targetEditable = targetEditable
                , targetRole = ""
                }
            )
            (Decode.field "key" Decode.string)
            (optionalFlag "shiftKey")
            (optionalFlag "ctrlKey")
            (optionalFlag "metaKey")
            (optionalFlag "altKey")
            (optionalFlag "repeat")
            (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
            (Decode.oneOf [ Decode.at [ "target", "isContentEditable" ] Decode.bool, Decode.succeed False ])
        )
        -- `Element.role` reflects the `role` attribute; it is null when unset.
        (Decode.oneOf [ Decode.at [ "target", "role" ] Decode.string, Decode.succeed "" ])


{-| A boolean event field, defaulting to `False` when absent.
-}
optionalFlag : String -> Decode.Decoder Bool
optionalFlag field =
    Decode.oneOf [ Decode.field field Decode.bool, Decode.succeed False ]


{-| Fingers currently touching **this canvas**.

`targetTouches` rather than `touches`: the latter counts every finger on the
screen, so resting a thumb on the controls panel forced the canvas into a pinch
gesture.

-}
touchesDecoder : Decode.Decoder (List TouchPoint)
touchesDecoder =
    Decode.field "targetTouches" (Decode.list touchPointDecoder)


touchPointDecoder : Decode.Decoder TouchPoint
touchPointDecoder =
    Decode.map3
        (\identifier x y ->
            { id = identifier
            , x = x
            , y = y
            }
        )
        (Decode.field "identifier" intLikeDecoder)
        (coordinateDecoder [ "clientX", "pageX", "screenX" ])
        (coordinateDecoder [ "clientY", "pageY", "screenY" ])


pointerTouchPointDecoder : Decode.Decoder TouchPoint
pointerTouchPointDecoder =
    Decode.field "pointerType" Decode.string
        |> Decode.andThen
            (\pointerType ->
                if pointerType == "touch" then
                    Decode.map3
                        (\pointerId x y ->
                            { id = pointerId
                            , x = x
                            , y = y
                            }
                        )
                        (Decode.field "pointerId" intLikeDecoder)
                        (coordinateDecoder [ "clientX", "pageX", "screenX" ])
                        (coordinateDecoder [ "clientY", "pageY", "screenY" ])

                else
                    Decode.fail "Non-touch pointer"
            )


pointerTouchIdDecoder : Decode.Decoder Int
pointerTouchIdDecoder =
    Decode.field "pointerType" Decode.string
        |> Decode.andThen
            (\pointerType ->
                if pointerType == "touch" then
                    Decode.field "pointerId" intLikeDecoder

                else
                    Decode.fail "Non-touch pointer"
            )


intLikeDecoder : Decode.Decoder Int
intLikeDecoder =
    Decode.oneOf
        [ Decode.int
        , Decode.float |> Decode.map round
        ]


floatLikeDecoder : Decode.Decoder Float
floatLikeDecoder =
    Decode.oneOf
        [ Decode.float
        , Decode.int |> Decode.map toFloat
        ]


coordinateDecoder : List String -> Decode.Decoder Float
coordinateDecoder keys =
    keys
        |> List.map (\key -> Decode.field key floatLikeDecoder)
        |> Decode.oneOf


{-| Dynamically extract the canvas's top-left corner in window (client) coordinates
from a mouse or pointer press.

When the event fired on the canvas element itself, `clientX - offsetX` and
`clientY - offsetY` yield the canvas origin without querying the DOM. If `target`
is not the canvas or offset coordinates are absent (as in synthetic test events),
returns `Nothing`.

-}
canvasOriginDecoder : Decode.Decoder (Maybe ( Float, Float ))
canvasOriginDecoder =
    Decode.oneOf
        [ Decode.at [ "target", "tagName" ] Decode.string
            |> Decode.andThen
                (\tagName ->
                    if String.toUpper tagName == "CANVAS" then
                        Decode.map4
                            (\clientX offsetX clientY offsetY ->
                                Just ( clientX - offsetX, clientY - offsetY )
                            )
                            (Decode.field "clientX" floatLikeDecoder)
                            (Decode.field "offsetX" floatLikeDecoder)
                            (Decode.field "clientY" floatLikeDecoder)
                            (Decode.field "offsetY" floatLikeDecoder)

                    else
                        Decode.succeed Nothing
                )
        , Decode.succeed Nothing
        ]
