module Render.Camera exposing
    ( Camera
    , farPlane
    , fovYDegrees
    , init
    , nearPlane
    , onMouseDown
    , onMouseMove
    , onMouseUp
    , onPan
    , onWheel
    , orbitBy
    , position
    , projectionMatrix
    , setViewportHeight
    , setZoomRange
    , viewMatrix
    , zoomByRatio
    )

{-| Orbit camera state and controls for interactive scene navigation.


## Clip planes

`nearPlane` and `farPlane` scale with the orbit distance rather than being
fixed. A city-scale model sits thousands of LDU from its centre, so constant
planes either clip the whole model away or waste the depth buffer's precision
on empty space in front of it.


## Sensitivity

Orbit and pan are expressed relative to `viewportHeight` rather than as fixed
pixels-to-units constants, following the convention three.js `OrbitControls`
uses. A fixed constant is only correct at one canvas size: it made panning
overshoot the cursor by ~2.7x on a tall desktop window while orbiting felt
sluggish on a short phone canvas. Normalising means a full-height drag is one
full turn and a panned point stays under the cursor at any size.


## Zoom range

`minDistance`/`maxDistance` travel with the camera instead of being hard-coded,
because the usable range depends entirely on model size. They are set from the
model's bounding sphere when it is framed (see `setZoomRange`).

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Orbit camera state.

The camera orbits around `target` at a given `distance`, with its
direction described by `azimuth` (horizontal angle, radians) and
`elevation` (vertical angle from XZ plane, radians).

-}
type alias Camera =
    { azimuth : Float
    , elevation : Float
    , distance : Float
    , target : Vec3
    , dragging : Bool
    , lastMousePos : Maybe ( Float, Float )
    , minDistance : Float
    , maxDistance : Float
    , viewportHeight : Float
    }


{-| Initial camera: positioned above and to the side of the origin.

The zoom range and viewport height are placeholders for the first frame; both
are replaced as soon as a model is framed and the canvas is measured.

-}
init : Camera
init =
    { azimuth = 0.75
    , elevation = 0.6
    , distance = 200.0
    , target = vec3 0 0 0
    , dragging = False
    , lastMousePos = Nothing
    , minDistance = 0.5
    , maxDistance = 2000.0
    , viewportHeight = 600.0
    }


{-| Vertical field of view, in degrees.

Shared with every projection matrix, frustum cull and pan calculation so they
cannot drift apart.

-}
fovYDegrees : Float
fovYDegrees =
    45.0


{-| Set the interactive zoom limits, in LDU, and re-clamp the current distance.
-}
setZoomRange : Float -> Float -> Camera -> Camera
setZoomRange minDistance maxDistance cam =
    let
        low =
            max 0.01 minDistance

        high =
            max low maxDistance
    in
    { cam
        | minDistance = low
        , maxDistance = high
        , distance = clamp low high cam.distance
    }


{-| Record the canvas height in CSS pixels. Orbit and pan sensitivity derive
from it, so it must be kept in sync with the rendered canvas.
-}
setViewportHeight : Float -> Camera -> Camera
setViewportHeight height cam =
    { cam | viewportHeight = max 1 height }


{-| World-space position of the camera eye.
-}
position : Camera -> Vec3
position cam =
    let
        x =
            cam.distance * sin cam.azimuth * cos cam.elevation

        y =
            cam.distance * sin cam.elevation

        z =
            cam.distance * cos cam.azimuth * cos cam.elevation
    in
    Vec3.add cam.target (vec3 x y z)


{-| View matrix: world → camera space.
-}
viewMatrix : Camera -> Mat4
viewMatrix cam =
    Mat4.makeLookAt (position cam) cam.target (vec3 0 1 0)


{-| Perspective projection matrix.

    projectionMatrix aspect near far

`aspect` is width/height. `near` and `far` are clip distances in LDU.
Field of view is fixed at `fovYDegrees`.

-}
projectionMatrix : Float -> Float -> Float -> Mat4
projectionMatrix aspect near far =
    Mat4.makePerspective fovYDegrees aspect near far


{-| Near clip distance for the current orbit distance.

Kept at a fixed ratio of the orbit distance so the near:far ratio — and with it
depth-buffer precision — stays constant no matter how large the model is.

-}
nearPlane : Camera -> Float
nearPlane cam =
    max 0.1 (cam.distance / 1000)


{-| Far clip distance for the current orbit distance.

Auto-fit places the camera roughly 2.6 model radii from the target, so ten
times the orbit distance clears the far side of any framed model with room to
spare.

-}
farPlane : Camera -> Float
farPlane cam =
    max 100 (cam.distance * 10)


{-| Begin a drag. Call on mousedown over the canvas.
-}
onMouseDown : Float -> Float -> Camera -> Camera
onMouseDown x y cam =
    { cam | dragging = True, lastMousePos = Just ( x, y ) }


{-| Update camera direction during a drag. Call on global mousemove.

Dragging the full height of the canvas is one full revolution, so sensitivity
is the same in feel on a phone and on a maximised desktop window.

-}
onMouseMove : Float -> Float -> Camera -> Camera
onMouseMove x y cam =
    case ( cam.dragging, cam.lastMousePos ) of
        ( True, Just ( lx, ly ) ) ->
            let
                radiansPerPixel =
                    2 * pi / max 1 cam.viewportHeight

                dx =
                    (x - lx) * radiansPerPixel

                dy =
                    (y - ly) * radiansPerPixel

                newAzimuth =
                    wrapAngle (cam.azimuth - dx)

                newElevation =
                    clamp (-pi / 2 + 0.01) (pi / 2 - 0.01) (cam.elevation + dy)
            in
            { cam
                | azimuth = newAzimuth
                , elevation = newElevation
                , lastMousePos = Just ( x, y )
            }

        _ ->
            cam


{-| Orbit by explicit angle deltas, in radians.

Used by the on-screen rotate buttons, so that they wrap azimuth and clamp
elevation exactly the way dragging does.

-}
orbitBy : Float -> Float -> Camera -> Camera
orbitBy azimuthDelta elevationDelta cam =
    { cam
        | azimuth = wrapAngle (cam.azimuth + azimuthDelta)
        , elevation = clamp (-pi / 2 + 0.01) (pi / 2 - 0.01) (cam.elevation + elevationDelta)
    }


{-| Fold an angle into (-pi, pi].

Azimuth would otherwise accumulate without bound across repeated drags and be
serialised into the URL hash as an ever-growing number.

-}
wrapAngle : Float -> Float
wrapAngle angle =
    let
        turns_ =
            2 * pi

        wrapped =
            angle - turns_ * toFloat (floor ((angle + pi) / turns_))
    in
    if wrapped <= -pi then
        wrapped + turns_

    else
        wrapped


{-| End a drag. Call on global mouseup.
-}
onMouseUp : Camera -> Camera
onMouseUp cam =
    { cam | dragging = False, lastMousePos = Nothing }


{-| Zoom by scrolling. `delta` is the wheel deltaY value (positive = zoom out),
already normalised for `deltaMode` by the caller.

The step is exponential rather than linear in `delta`, which makes zooming in
and back out by the same amount return to the same distance and makes a large
trackpad fling impossible to turn into a negative scale factor. One 100-unit
notch is 5%, matching three.js `OrbitControls`.

Distance is clamped to the camera's own `minDistance`/`maxDistance`.

-}
onWheel : Float -> Camera -> Camera
onWheel delta cam =
    let
        step =
            0.95 ^ (abs delta * 0.01)

        factor =
            if delta < 0 then
                step

            else
                1 / step
    in
    zoomByRatio factor cam


{-| Multiply the orbit distance by `ratio`, clamped to the camera's zoom range.

Pinch gestures use this directly: the ratio of successive finger separations is
already the exact zoom factor, with no pixels-to-units constant in between.

-}
zoomByRatio : Float -> Camera -> Camera
zoomByRatio ratio cam =
    if ratio <= 0 || isNaN ratio || isInfinite ratio then
        cam

    else
        { cam | distance = clamp cam.minDistance cam.maxDistance (cam.distance * ratio) }


{-| Pan camera target in screen-space pixels.

Positive `dx` means finger/mouse moved right, positive `dy` moved down.

The pixel-to-world scale is derived from the frustum height at the target
distance, so a point under the cursor stays under the cursor at any canvas size
or zoom level.

-}
onPan : Float -> Float -> Camera -> Camera
onPan dx dy cam =
    let
        eye =
            position cam

        forwardRaw =
            Vec3.sub cam.target eye

        forwardLen =
            Vec3.length forwardRaw

        forward =
            if forwardLen < 1.0e-6 then
                vec3 0 0 -1

            else
                Vec3.scale (1 / forwardLen) forwardRaw

        worldUp =
            vec3 0 1 0

        rightRaw =
            Vec3.cross forward worldUp

        rightLen =
            Vec3.length rightRaw

        right =
            if rightLen < 1.0e-6 then
                vec3 1 0 0

            else
                Vec3.scale (1 / rightLen) rightRaw

        upRaw =
            Vec3.cross right forward

        upLen =
            Vec3.length upRaw

        up =
            if upLen < 1.0e-6 then
                vec3 0 1 0

            else
                Vec3.scale (1 / upLen) upRaw

        pixelToWorld =
            2 * cam.distance * tan (degrees fovYDegrees / 2) / max 1 cam.viewportHeight

        deltaWorld =
            Vec3.add
                (Vec3.scale (-dx * pixelToWorld) right)
                (Vec3.scale (dy * pixelToWorld) up)
    in
    { cam | target = Vec3.add cam.target deltaWorld }
