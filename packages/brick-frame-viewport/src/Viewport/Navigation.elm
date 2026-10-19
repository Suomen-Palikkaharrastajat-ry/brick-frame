module Viewport.Navigation exposing
    ( applyLook
    , applyPinchZoom
    , applyWheel
    , keyboardTurnRate
    , maxHeldKeyStep
    )

{-| Input intents interpreted for the camera's active navigation mode.

The same gesture means different things when orbiting and when walking, so every
camera-moving input goes through here instead of calling `Render.Camera`
directly.

-}

import Render.Camera as Camera exposing (Camera)


{-| Wheel zoom, interpreted for the active navigation mode.

Orbiting pulls the eye towards the pivot. Walking has no pivot to pull towards —
shrinking the orbit distance would slide you out of the street rather than along
it — so the wheel moves you forwards and backwards instead.

-}
applyWheel : Float -> Camera -> Camera
applyWheel delta camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.onWheel delta camera

        Camera.Walk ->
            Camera.dollyBy (-delta * 0.01 * Camera.walkSpeed camera * walkWheelSeconds) camera


{-| Pinch zoom, interpreted for the active navigation mode.

`ratio` is the same figure `Camera.zoomByRatio` takes: below 1 means zoom in.

-}
applyPinchZoom : Float -> Camera -> Camera
applyPinchZoom ratio camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.zoomByRatio ratio camera

        Camera.Walk ->
            -- A ratio is scale-free, so turn it into a stride: pinching out by a
            -- factor of two walks one second forwards.
            Camera.dollyBy ((1 - ratio) * Camera.walkSpeed camera * walkWheelSeconds) camera


{-| How many seconds of walking one full wheel notch is worth.

A whole second of travel per notch overshoots badly at street level — on a
city-scale model that is fourteen studs from one flick of the finger.

-}
walkWheelSeconds : Float
walkWheelSeconds =
    0.25


{-| Rotate the way a drag would in the active navigation mode.

Every rotation in this module goes through here — the arrow keys, the on-screen
chevron buttons and `RotateCameraBy` — so this is the only `Camera.orbitBy` call
site. Reaching for `orbitBy` directly is what made the chevron buttons swing the
eye in a small circle instead of turning the head while walking, which on a touch
device left dragging as the only way to turn.

-}
applyLook : Float -> Float -> Camera -> Camera
applyLook azimuthDelta elevationDelta camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.orbitBy azimuthDelta elevationDelta camera

        Camera.Walk ->
            Camera.lookAround azimuthDelta elevationDelta camera


{-| Arrow-key turn rate, in radians per second.
-}
keyboardTurnRate : Float
keyboardTurnRate =
    1.2


{-| Longest frame time a single held-key step may integrate over, in seconds.
-}
maxHeldKeyStep : Float
maxHeldKeyStep =
    0.1
