module Render.Camera exposing
    ( Camera
    , Navigation(..)
    , dollyBy
    , enterWalk
    , farPlane
    , fovYDegrees
    , init
    , lookAround
    , moveBy
    , nearPlane
    , onMouseDown
    , onMouseMove
    , onMouseUp
    , onPan
    , onWheel
    , orbitBy
    , position
    , projectionMatrix
    , setNavigation
    , setSceneRadius
    , setViewportHeight
    , setZoomRange
    , viewMatrix
    , walkEyeHeight
    , walkSpeed
    , zoomByRatio
    )

{-| Camera state and controls for interactive scene navigation.


## Navigation modes

`Orbit` swings the eye around a fixed `target`; `Walk` holds the eye still and
swings the target around it, first-person style, with `moveBy` translating the
pair. Both maintain `position = target + eyeOffset`, so the view matrix, culling,
picking and level of detail never need to know which is active. Use `enterWalk`
to drop to street level; there is no collision detection.


## Clip planes

`nearPlane` and `farPlane` scale with the orbit distance rather than being
fixed. A city-scale model sits thousands of LDU from its centre, so constant
planes either clip the whole model away or waste the depth buffer's precision
on empty space in front of it. `sceneRadius` extends the far plane for the case
the orbit distance cannot describe: standing *inside* the model, where the
geometry is far away but the pivot is not.


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
    , navigation : Navigation
    , sceneRadius : Float
    }


{-| How pointer drags are interpreted.

  - `Orbit` — the eye swings around a fixed `target`. Good for inspecting a model
    from the outside.
  - `Walk` — the eye stays put and the `target` swings around it, first-person
    style, with `moveBy` translating the pair together. Good for standing on a
    street inside a city-scale model.

Both keep the same `position = target + eyeOffset` relation, so everything
downstream — view matrix, frustum culling, picking, level of detail — is
indifferent to which is active.

-}
type Navigation
    = Orbit
    | Walk


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
    , navigation = Orbit
    , sceneRadius = 0.0
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


{-| Record the radius of the model's bounding sphere, in LDU.

Feeds the far plane (see `farPlane`) and the walk speed. A host that leaves it at
zero can still orbit, but geometry further than ten orbit distances away will be
clipped.

-}
setSceneRadius : Float -> Camera -> Camera
setSceneRadius radius cam =
    { cam | sceneRadius = max 0 radius }


{-| Switch between orbit and walk navigation, leaving the viewpoint alone.

To *enter* walk mode at street level, use `enterWalk` instead — it also places
the eye on the ground and levels the horizon.

-}
setNavigation : Navigation -> Camera -> Camera
setNavigation navigation cam =
    { cam | navigation = navigation }


{-| Offset from `target` to the eye, in world space.

Both navigation modes maintain `position = target + eyeOffset`; they differ only
in which of the two they hold fixed while the angles change.

-}
eyeOffset : Camera -> Vec3
eyeOffset cam =
    let
        x =
            cam.distance * sin cam.azimuth * cos cam.elevation

        y =
            cam.distance * sin cam.elevation

        z =
            cam.distance * cos cam.azimuth * cos cam.elevation
    in
    vec3 x y z


{-| World-space position of the camera eye.
-}
position : Camera -> Vec3
position cam =
    Vec3.add cam.target (eyeOffset cam)


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


{-| Near clip distance.

Derived from the far plane so the near:far ratio — and with it depth-buffer
precision — stays constant no matter how large the model is or where the eye is.
In the orbit case `farPlane` is `distance * 10`, so this is `distance / 1000`.

-}
nearPlane : Camera -> Float
nearPlane cam =
    max 0.1 (farPlane cam / 10000)


{-| Far clip distance.

Two terms, whichever is larger:

  - `distance * 10` covers the orbit case. Auto-fit places the camera roughly
    2.6 model radii from the target, so ten times the orbit distance clears the
    far side of any framed model with room to spare.
  - `distance + 2.5 * sceneRadius` covers standing *inside* the model, where the
    orbit distance says nothing about how far away the geometry is. Walking a
    city at a 40 LDU eye height would otherwise get a 400 LDU far plane on a
    model thousands of LDU across — twenty studs of street and then nothing.

`sceneRadius` defaults to 0, which reduces this to the first term exactly, so a
host that never sets it keeps the previous behaviour.

-}
farPlane : Camera -> Float
farPlane cam =
    max 100 (max (cam.distance * 10) (cam.distance + 2.5 * cam.sceneRadius))


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

                rotated =
                    case cam.navigation of
                        Orbit ->
                            orbitBy -dx dy cam

                        Walk ->
                            lookAround -dx dy cam
            in
            { rotated | lastMousePos = Just ( x, y ) }

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


{-| Turn the head: apply the same angle deltas as `orbitBy`, but hold the eye
still and swing `target` around it instead.

The sign convention is identical to `orbitBy`, so a drag to the right turns you
right and a drag downwards looks down in both modes.

-}
lookAround : Float -> Float -> Camera -> Camera
lookAround azimuthDelta elevationDelta cam =
    let
        eye =
            position cam

        rotated =
            orbitBy azimuthDelta elevationDelta cam
    in
    { rotated | target = Vec3.sub eye (eyeOffset rotated) }


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


{-| Ground-plane movement basis for the current heading.

Derived from `azimuth` alone, so it is exact, cheap, and — unlike zeroing the Y
component of the view direction — never degenerates when looking straight up or
down. Both vectors are unit length and horizontal.

-}
walkBasis : Camera -> { forward : Vec3, right : Vec3 }
walkBasis cam =
    { forward = vec3 -(sin cam.azimuth) 0 -(cos cam.azimuth)
    , right = vec3 (cos cam.azimuth) 0 -(sin cam.azimuth)
    }


{-| Translate the whole camera, in LDU.

`target` moves and the eye follows, because the eye is derived from it. `forward`
and `right` follow the horizontal heading, so looking up and walking forward does
not lift you off the ground; `up` is world Y.

There is no collision detection — this walks through walls.

-}
moveBy : { forward : Float, right : Float, up : Float } -> Camera -> Camera
moveBy delta cam =
    let
        basis =
            walkBasis cam

        offset =
            Vec3.add
                (Vec3.add
                    (Vec3.scale delta.forward basis.forward)
                    (Vec3.scale delta.right basis.right)
                )
                (vec3 0 delta.up 0)
    in
    { cam | target = Vec3.add cam.target offset }


{-| Move along the horizontal heading, in LDU. Positive is forwards.

What the wheel and pinch gestures do in walk mode, where changing the orbit
distance would slide you out of the street rather than move you along it.

-}
dollyBy : Float -> Camera -> Camera
dollyBy amount cam =
    moveBy { forward = amount, right = 0, up = 0 } cam


{-| Walking speed in LDU per second.

Scaled to the model so the same key feels right on a gear assembly and on a city:
`example.io` (radius ~2295 LDU) gives ~287 LDU/s, crossing its 169-stud footprint
in about twelve seconds. Small models get the floor instead of a speed that would
throw them off the edge immediately.

-}
walkSpeed : Camera -> Float
walkSpeed cam =
    max 20 (cam.sceneRadius / 8)


{-| Eye height above ground in walk mode, in LDU — roughly minifig eye level.
-}
walkEyeHeight : Float
walkEyeHeight =
    40.0


{-| Drop to street level and switch to walk navigation.

`groundY` is the model's lowest point. The eye lands directly below whatever was
being looked at, at `walkEyeHeight` above the ground, with the horizon levelled
and the heading preserved — so entering walk mode keeps you facing the way you
already were.

-}
enterWalk : Float -> Camera -> Camera
enterWalk groundY cam =
    let
        levelled =
            { cam
                | navigation = Walk
                , elevation = 0
                , distance = clamp cam.minDistance cam.maxDistance walkFocalDistance
            }

        eye =
            vec3 (Vec3.getX cam.target) (groundY + walkEyeHeight) (Vec3.getZ cam.target)
    in
    { levelled | target = Vec3.sub eye (eyeOffset levelled) }


{-| Orbit distance held in walk mode, in LDU.

Nothing visual depends on it — look pivots about the eye and movement translates —
but it still sets the shift-drag pan scale, so it wants to be on the order of the
things you are walking between rather than the whole model.

-}
walkFocalDistance : Float
walkFocalDistance =
    60.0


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
