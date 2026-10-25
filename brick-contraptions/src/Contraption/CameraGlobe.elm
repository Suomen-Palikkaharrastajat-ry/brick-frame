module Contraption.CameraGlobe exposing
    ( Config
    , directionAt
    , dragOrbit
    , facingSign
    , flyToDelta
    , markerPosition
    , resetButtonId
    , view
    , wheelDeltaForZoom
    , zoomLevel
    , zoomSliderId
    )

{-| On-screen camera control: a globe the camera flies around, and a zoom slider.

The globe is drawn from a fixed viewpoint, slightly above the horizon and in
front of the model, so the camera marker moves over it as the real camera orbits.
Its surface stands for every direction the camera can look at the model from:
dragging carries the marker with the pointer, tapping flies the camera straight
to that point.

Everything here is pure; the host owns the pointer state and sends the camera
messages.

-}

import Html exposing (Html, div)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Render.Camera exposing (Camera)
import Svg
import Svg.Attributes as SvgAttr
import UI.Icon as Icon


{-| What the control shows, and the messages it sends.

`onPress`, `onMove` and `onRelease` carry pointer positions relative to the
globe's centre, in pixels, Y down. `onZoom` carries a level from 0 (as far out as
the model allows) to 1 (as close as it allows); `onZoomDone` follows when the
slider is let go, so the host can hand focus back (a focused slider keeps Space
and the other shortcuts for itself). `onReset` re-frames the whole model; the
host should then blur `resetButtonId` for the same reason.

-}
type alias Config msg =
    { camera : Camera
    , onPress : ( Float, Float ) -> msg
    , onMove : ( Float, Float ) -> msg
    , onRelease : msg
    , onZoom : Float -> msg
    , onZoomDone : msg
    , onReset : msg
    }


{-| The reset button's DOM id, for giving focus back once it is clicked.
-}
resetButtonId : String
resetButtonId =
    "camera-reset"


{-| The zoom slider's DOM id, for giving focus back once it is let go.
-}
zoomSliderId : String
zoomSliderId =
    "camera-zoom"


{-| Globe radius on screen, in pixels.
-}
radius : Float
radius =
    44


{-| Side of the square the globe is drawn in.
-}
size : Float
size =
    112


{-| How far above the horizon the globe is seen from.
-}
tilt : Float
tilt =
    degrees 25


{-| Where a camera at this azimuth and elevation sits on the globe, relative to
its centre in pixels (Y down), and whether that side faces the viewer.
-}
markerPosition : Float -> Float -> { x : Float, y : Float, front : Bool }
markerPosition azimuth elevation =
    let
        wx =
            sin azimuth * cos elevation

        wy =
            sin elevation

        wz =
            cos azimuth * cos elevation
    in
    { x = radius * wx
    , y = -radius * (wy * cos tilt - wz * sin tilt)
    , front = wy * sin tilt + wz * cos tilt >= 0
    }


{-| The azimuth and elevation of the globe point under a pointer (relative to the
centre, Y down), on the side facing the viewer. `Nothing` off the globe.
-}
directionAt : ( Float, Float ) -> Maybe { azimuth : Float, elevation : Float }
directionAt ( pointerX, pointerY ) =
    let
        u =
            pointerX / radius

        v =
            -pointerY / radius

        rr =
            u * u + v * v
    in
    if rr > 1 then
        Nothing

    else
        let
            d =
                sqrt (1 - rr)

            wx =
                u

            wy =
                v * cos tilt + d * sin tilt

            wz =
                -v * sin tilt + d * cos tilt
        in
        Just { azimuth = atan2 wx wz, elevation = asin (clamp -1 1 wy) }


{-| The orbit that carries the camera to the globe point under a pointer, as an
azimuth and elevation delta. Azimuth takes the short way round.
-}
flyToDelta : Camera -> ( Float, Float ) -> Maybe ( Float, Float )
flyToDelta camera point =
    directionAt point
        |> Maybe.map
            (\target ->
                ( wrapAngle (target.azimuth - camera.azimuth)
                , target.elevation - camera.elevation
                )
            )


{-| Which way a horizontal drag turns the camera so the marker follows the
pointer: right on the near side of the globe, left on the far side.
-}
facingSign : Camera -> Float
facingSign camera =
    if (markerPosition camera.azimuth camera.elevation).front then
        1

    else
        -1


{-| The orbit for a pointer movement over the globe, in pixels (Y down).
One radian per radius, so the marker keeps up with the pointer.
-}
dragOrbit : Float -> ( Float, Float ) -> ( Float, Float )
dragOrbit azimuthSign ( dx, dy ) =
    let
        rate =
            1 / radius
    in
    ( azimuthSign * dx * rate, -dy * rate )


wrapAngle : Float -> Float
wrapAngle angle =
    angle - 2 * pi * toFloat (round (angle / (2 * pi)))


{-| Current zoom as a level from 0 (furthest the model allows) to 1 (closest),
on a log scale so each step of the slider zooms by the same factor.
-}
zoomLevel : Camera -> Float
zoomLevel camera =
    if camera.maxDistance <= camera.minDistance then
        0

    else
        clamp 0 1 (logBase e (camera.maxDistance / camera.distance) / logBase e (camera.maxDistance / camera.minDistance))


{-| The wheel delta that takes the camera to a zoom level, inverting
`Render.Camera.onWheel` (`0.95 ^ (delta / 100)` per wheel unit).
-}
wheelDeltaForZoom : Camera -> Float -> Float
wheelDeltaForZoom camera level =
    let
        target =
            camera.maxDistance * ((camera.minDistance / camera.maxDistance) ^ clamp 0 1 level)

        ratio =
            target / max 0.0001 camera.distance
    in
    -100 * logBase e ratio / logBase e 0.95


{-| The globe and, beside it, the zoom slider.
-}
view : Config msg -> Html msg
view config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Camera"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "padding" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "pointer-events" "auto"
        ]
        [ div
            [ Attr.style "position" "relative" ]
            [ globe config
            , resetButton config.onReset
            ]
        , zoomSlider config
        ]


globe : Config msg -> Html msg
globe config =
    let
        half =
            size / 2

        pointer toMsg =
            Decode.map2 (\x y -> toMsg ( x - half, y - half ))
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)

        handle name toMsg =
            Html.Events.preventDefaultOn name (Decode.map (\m -> ( m, True )) (pointer toMsg))

        release name =
            Html.Events.on name (Decode.succeed config.onRelease)
    in
    div
        [ Attr.title "Drag to fly the camera around the model; tap to jump there"
        , Attr.attribute "aria-label" "Camera position"
        , Attr.style "position" "relative"
        , Attr.style "width" (px size)
        , Attr.style "height" (px size)
        , Attr.style "cursor" "grab"
        , Attr.style "touch-action" "none"
        , handle "pointerdown" config.onPress
        , handle "pointermove" config.onMove
        , release "pointerup"
        , release "pointercancel"
        , release "pointerleave"
        ]
        [ Svg.svg
            [ SvgAttr.width (String.fromFloat size)
            , SvgAttr.height (String.fromFloat size)
            , SvgAttr.viewBox ("0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size)
            , Attr.style "pointer-events" "none"
            , Attr.style "display" "block"
            ]
            [ Svg.g [ SvgAttr.transform ("translate(" ++ String.fromFloat half ++ " " ++ String.fromFloat half ++ ")") ]
                (globeShapes config.camera)
            ]
        ]


{-| Back to the fitted view, in the top-left corner clear of the globe. A sibling
of the globe rather than a child, so pressing it never starts a globe drag.
-}
resetButton : msg -> Html msg
resetButton onReset =
    let
        label =
            "Reset view (F)"
    in
    Html.button
        [ Html.Events.onClick onReset
        , Attr.id resetButtonId
        , Attr.title label
        , Attr.attribute "aria-label" label
        , Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "width" "1.75rem"
        , Attr.style "height" "1.75rem"
        , Attr.style "padding" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background" "rgba(255, 255, 255, 0.1)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        ]
        [ Icon.feather "maximize" ]


globeShapes : Camera -> List (Svg.Svg msg)
globeShapes camera =
    let
        marker =
            markerPosition camera.azimuth camera.elevation

        ( markerOpacity, sightDash ) =
            if marker.front then
                ( "1", "none" )

            else
                ( "0.45", "3 3" )

        line =
            "rgba(255, 255, 255, 0.25)"
    in
    [ Svg.circle
        [ SvgAttr.r (String.fromFloat radius)
        , SvgAttr.fill "rgba(255, 255, 255, 0.06)"
        , SvgAttr.stroke "rgba(255, 255, 255, 0.35)"
        , SvgAttr.strokeWidth "1"
        ]
        []

    -- Ground plane (the equator), seen from above the horizon
    , Svg.ellipse
        [ SvgAttr.rx (String.fromFloat radius)
        , SvgAttr.ry (String.fromFloat (radius * sin tilt))
        , SvgAttr.fill "none"
        , SvgAttr.stroke line
        ]
        []

    -- The front-back meridian, through the poles
    , Svg.ellipse
        [ SvgAttr.rx "0.5"
        , SvgAttr.ry (String.fromFloat radius)
        , SvgAttr.fill "none"
        , SvgAttr.stroke line
        ]
        []

    -- The model at the centre
    , Svg.rect
        [ SvgAttr.x "-6"
        , SvgAttr.y "-6"
        , SvgAttr.width "12"
        , SvgAttr.height "12"
        , SvgAttr.rx "2"
        , SvgAttr.fill "var(--color-text-subtle)"
        ]
        []

    -- Line of sight from the camera to the model
    , Svg.line
        [ SvgAttr.x1 (String.fromFloat marker.x)
        , SvgAttr.y1 (String.fromFloat marker.y)
        , SvgAttr.x2 "0"
        , SvgAttr.y2 "0"
        , SvgAttr.stroke "var(--color-brand-yellow)"
        , SvgAttr.strokeWidth "1.5"
        , SvgAttr.strokeDasharray sightDash
        , SvgAttr.opacity markerOpacity
        ]
        []
    , Svg.circle
        [ SvgAttr.cx (String.fromFloat marker.x)
        , SvgAttr.cy (String.fromFloat marker.y)
        , SvgAttr.r "7"
        , SvgAttr.fill "var(--color-brand-yellow)"
        , SvgAttr.stroke "var(--color-brand)"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.opacity markerOpacity
        ]
        []
    ]


zoomSlider : Config msg -> Html msg
zoomSlider config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "color" "var(--color-text-subtle)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "line-height" "1"
        ]
        [ Html.span [ Attr.attribute "aria-hidden" "true" ] [ Html.text "+" ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.id zoomSliderId
            , Html.Events.on "change" (Decode.succeed config.onZoomDone)
            , Attr.attribute "aria-label" "Zoom"
            , Attr.title "Zoom"
            , Attr.min "0"
            , Attr.max "1000"
            , Attr.value (String.fromInt (round (zoomLevel config.camera * 1000)))
            , Html.Events.onInput (\s -> config.onZoom (Maybe.withDefault 0 (String.toFloat s) / 1000))
            , Attr.style "writing-mode" "vertical-lr"
            , Attr.style "direction" "rtl"
            , Attr.style "width" "1.25rem"
            , Attr.style "height" (px (size - 36))
            , Attr.style "margin" "0"
            , Attr.style "accent-color" "var(--color-brand-yellow)"
            , Attr.style "cursor" "pointer"
            ]
            []
        , Html.span [ Attr.attribute "aria-hidden" "true" ] [ Html.text "−" ]
        ]


px : Float -> String
px value =
    String.fromFloat value ++ "px"
