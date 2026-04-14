module Render.Camera exposing
    ( Camera
    , init
    , onMouseDown
    , onMouseMove
    , onMouseUp
    , onPan
    , onWheel
    , projectionMatrix
    , viewMatrix
    )

{-| Orbit camera state and controls for interactive scene navigation.
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
    }


{-| Initial camera: positioned above and to the side of the origin.
-}
init : Camera
init =
    { azimuth = 0.75
    , elevation = 0.6
    , distance = 200.0
    , target = vec3 0 0 0
    , dragging = False
    , lastMousePos = Nothing
    }


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
Field of view is fixed at 45°.

-}
projectionMatrix : Float -> Float -> Float -> Mat4
projectionMatrix aspect near far =
    Mat4.makePerspective 45 aspect near far


{-| Begin a drag. Call on mousedown over the canvas.
-}
onMouseDown : Float -> Float -> Camera -> Camera
onMouseDown x y cam =
    { cam | dragging = True, lastMousePos = Just ( x, y ) }


{-| Update camera direction during a drag. Call on global mousemove.
-}
onMouseMove : Float -> Float -> Camera -> Camera
onMouseMove x y cam =
    case ( cam.dragging, cam.lastMousePos ) of
        ( True, Just ( lx, ly ) ) ->
            let
                dx =
                    (x - lx) * 0.005

                dy =
                    (y - ly) * 0.005

                newAzimuth =
                    cam.azimuth - dx

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


{-| End a drag. Call on global mouseup.
-}
onMouseUp : Camera -> Camera
onMouseUp cam =
    { cam | dragging = False, lastMousePos = Nothing }


{-| Zoom by scrolling. `delta` is the wheel deltaY value (positive = zoom out).
Distance is clamped to [0.5, 2000] LDU.
-}
onWheel : Float -> Camera -> Camera
onWheel delta cam =
    let
        factor =
            1.0 + delta * 0.001

        newDistance =
            clamp 0.5 2000 (cam.distance * factor)
    in
    { cam | distance = newDistance }


{-| Pan camera target in screen-space pixels.

Positive `dx` means finger/mouse moved right, positive `dy` moved down.
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
            cam.distance * 0.0025

        deltaWorld =
            Vec3.add
                (Vec3.scale (-dx * pixelToWorld) right)
                (Vec3.scale (dy * pixelToWorld) up)
    in
    { cam | target = Vec3.add cam.target deltaWorld }
