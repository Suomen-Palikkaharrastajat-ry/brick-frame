module Render.CameraTest exposing (suite)

{-| Unit tests for camera interaction math.

These pin the properties that make navigation feel right — reciprocal zoom,
viewport-relative sensitivity, cursor-tracking pan, and a walk mode that pivots
about the eye — rather than the constants themselves.

-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Camera as Camera exposing (Camera)
import Test exposing (Test, describe, test)


{-| Bounding-sphere radius of the sample city model, in LDU.
-}
cityRadius : Float
cityRadius =
    2295


{-| A camera framing a city-scale model, as `cameraForBounds` would leave it.
-}
bigModelCamera : Camera
bigModelCamera =
    Camera.init
        |> Camera.setViewportHeight 900
        |> Camera.setZoomRange 0.5 60000
        |> Camera.setSceneRadius cityRadius
        |> (\cam -> { cam | distance = 7500 })


{-| The same model, entered at street level.
-}
walkCamera : Camera
walkCamera =
    Camera.enterWalk 0 bigModelCamera


dragging : Camera -> Camera
dragging cam =
    Camera.onMouseDown 100 100 cam


suite : Test
suite =
    describe "Render.Camera"
        [ describe "zoom range"
            [ test "a large model can be zoomed out past the old 2000 LDU ceiling" <|
                \_ ->
                    -- Regression: onWheel used to hard-clamp to [0.5, 2000], so the
                    -- first notch collapsed a 7500 LDU framing and could not recover.
                    List.foldl (\_ cam -> Camera.onWheel 100 cam) bigModelCamera (List.range 1 40)
                        |> .distance
                        |> Expect.greaterThan 7500
            , test "a single notch does not snap a framed model" <|
                \_ ->
                    (Camera.onWheel 100 bigModelCamera).distance
                        |> Expect.within (Expect.Relative 0.01) (7500 / 0.95)
            , test "zoom is clamped to maxDistance" <|
                \_ ->
                    List.foldl (\_ cam -> Camera.onWheel 500 cam) bigModelCamera (List.range 1 200)
                        |> .distance
                        |> Expect.within (Expect.Absolute 1.0e-6) 60000
            , test "zoom is clamped to minDistance" <|
                \_ ->
                    List.foldl (\_ cam -> Camera.onWheel -500 cam) bigModelCamera (List.range 1 200)
                        |> .distance
                        |> Expect.within (Expect.Absolute 1.0e-6) 0.5
            , test "setZoomRange re-clamps a distance outside the new range" <|
                \_ ->
                    (Camera.setZoomRange 10 100 bigModelCamera).distance
                        |> Expect.within (Expect.Absolute 1.0e-6) 100
            ]
        , describe "zoom curve"
            [ test "zooming out then in by the same amount returns to the same distance" <|
                \_ ->
                    bigModelCamera
                        |> Camera.onWheel 240
                        |> Camera.onWheel -240
                        |> .distance
                        |> Expect.within (Expect.Relative 1.0e-9) 7500
            , test "a huge trackpad fling cannot invert the scale factor" <|
                \_ ->
                    -- 1 + delta * 0.001 went negative below -1000 and snapped to min.
                    (Camera.onWheel -5000 bigModelCamera).distance
                        |> Expect.greaterThan 0.5
            , test "zoomByRatio ignores non-positive ratios" <|
                \_ ->
                    (Camera.zoomByRatio 0 bigModelCamera).distance
                        |> Expect.within (Expect.Absolute 1.0e-6) 7500
            ]
        , describe "orbit sensitivity"
            [ test "dragging the full canvas height is one full revolution" <|
                \_ ->
                    -- Checked on azimuth, the unclamped axis: half the height is
                    -- half a turn, so the full height is a full turn.
                    let
                        cam =
                            Camera.init
                                |> Camera.setViewportHeight 900
                                |> dragging
                    in
                    cam.azimuth
                        - (Camera.onMouseMove (100 + 450) 100 cam).azimuth
                        |> Expect.within (Expect.Relative 1.0e-9) pi
            , test "sensitivity scales with viewport height" <|
                \_ ->
                    let
                        turn height =
                            let
                                cam =
                                    Camera.init
                                        |> Camera.setViewportHeight height
                                        |> dragging
                            in
                            cam.azimuth - (Camera.onMouseMove 200 100 cam).azimuth
                    in
                    -- Half the height, twice the rotation for the same pixel drag.
                    turn 400
                        |> Expect.within (Expect.Relative 1.0e-9) (2 * turn 800)
            , test "elevation stays clear of the poles" <|
                \_ ->
                    let
                        cam =
                            Camera.init
                                |> Camera.setViewportHeight 900
                                |> dragging
                    in
                    (Camera.onMouseMove 100 100000 cam).elevation
                        |> Expect.lessThan (pi / 2)
            , test "azimuth wraps instead of accumulating" <|
                \_ ->
                    let
                        cam =
                            Camera.init
                                |> Camera.setViewportHeight 900
                                |> dragging
                    in
                    List.foldl (\_ c -> Camera.onMouseMove 100 100 (Camera.onMouseDown 0 100 c))
                        cam
                        (List.range 1 20)
                        |> .azimuth
                        |> abs
                        |> Expect.atMost pi
            ]
        , describe "pan"
            [ test "a point under the cursor stays under the cursor" <|
                \_ ->
                    let
                        height =
                            900

                        cam =
                            Camera.init
                                |> Camera.setViewportHeight height
                                |> (\c -> { c | distance = 1000, azimuth = 0, elevation = 0 })

                        -- World units spanned by the full viewport height at the target.
                        worldPerScreen =
                            2 * 1000 * tan (degrees Camera.fovYDegrees / 2)

                        panned =
                            Camera.onPan height 0 cam
                    in
                    Vec3.distance panned.target cam.target
                        |> Expect.within (Expect.Relative 1.0e-9) worldPerScreen
            , test "pan speed scales with orbit distance" <|
                \_ ->
                    let
                        travel distance =
                            let
                                cam =
                                    Camera.init
                                        |> Camera.setViewportHeight 900
                                        |> (\c -> { c | distance = distance })
                            in
                            Vec3.distance (Camera.onPan 50 0 cam).target cam.target
                    in
                    travel 2000
                        |> Expect.within (Expect.Relative 1.0e-9) (2 * travel 1000)
            , test "pan moves the target, not the orbit angles" <|
                \_ ->
                    let
                        cam =
                            Camera.init |> Camera.setViewportHeight 900

                        panned =
                            Camera.onPan 30 20 cam
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 1.0e-9) cam.azimuth c.azimuth
                        , \c -> Expect.within (Expect.Absolute 1.0e-9) cam.elevation c.elevation
                        , \c -> Expect.within (Expect.Absolute 1.0e-9) cam.distance c.distance
                        , \c -> Expect.notEqual cam.target c.target
                        ]
                        panned
            ]
        , describe "clip planes"
            [ test "near:far ratio is constant at any orbit distance" <|
                \_ ->
                    let
                        ratio distance =
                            let
                                cam =
                                    { bigModelCamera | distance = distance }
                            in
                            Camera.farPlane cam / Camera.nearPlane cam
                    in
                    ratio 50000
                        |> Expect.within (Expect.Relative 1.0e-9) (ratio 5000)
            , test "the far plane clears a model framed at maximum zoom-out" <|
                \_ ->
                    let
                        cam =
                            { bigModelCamera | distance = 60000, target = vec3 0 0 0 }
                    in
                    Camera.farPlane cam
                        |> Expect.greaterThan (Vec3.length (Camera.position cam))
            , test "the far plane spans the model from inside it" <|
                \_ ->
                    -- Regression: farPlane was max 100 (distance * 10), so a 40 LDU
                    -- street-level eye height gave 400 LDU on a 4590 LDU-wide city
                    -- — twenty studs of street, then nothing.
                    Camera.farPlane { bigModelCamera | distance = 40 }
                        |> Expect.greaterThan (2 * cityRadius)
            , test "a zero scene radius reproduces the distance-only planes" <|
                \_ ->
                    let
                        cam =
                            Camera.setSceneRadius 0 { bigModelCamera | distance = 7500 }
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Relative 1.0e-9) (7500 * 10) (Camera.farPlane c)
                        , \c -> Expect.within (Expect.Relative 1.0e-9) (7500 / 1000) (Camera.nearPlane c)
                        ]
                        cam
            , test "the scene radius never shrinks the orbit far plane" <|
                \_ ->
                    let
                        far r =
                            Camera.farPlane (Camera.setSceneRadius r bigModelCamera)
                    in
                    far cityRadius
                        |> Expect.atLeast (far 0)
            ]
        , describe "walk mode"
            [ test "entering walk puts the eye at eye height above the ground" <|
                \_ ->
                    Camera.position (Camera.enterWalk 120 bigModelCamera)
                        |> Vec3.getY
                        |> Expect.within (Expect.Absolute 1.0e-6) (120 + Camera.walkEyeHeight)
            , test "entering walk levels the horizon and keeps the heading" <|
                \_ ->
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 1.0e-9) 0 c.elevation
                        , \c -> Expect.within (Expect.Absolute 1.0e-9) bigModelCamera.azimuth c.azimuth
                        , \c -> Expect.equal Camera.Walk c.navigation
                        ]
                        walkCamera
            , test "entering walk keeps the eye over what was being looked at" <|
                \_ ->
                    let
                        eye =
                            Camera.position walkCamera
                    in
                    Expect.all
                        [ \p -> Expect.within (Expect.Absolute 1.0e-6) (Vec3.getX bigModelCamera.target) (Vec3.getX p)
                        , \p -> Expect.within (Expect.Absolute 1.0e-6) (Vec3.getZ bigModelCamera.target) (Vec3.getZ p)
                        ]
                        eye
            , test "looking around holds the eye still" <|
                \_ ->
                    -- The whole point of walk mode: turning your head must not slide
                    -- you sideways the way orbiting around a point ahead does.
                    let
                        before =
                            Camera.position walkCamera

                        after =
                            Camera.position (Camera.lookAround 1.0 0.3 walkCamera)
                    in
                    Vec3.distance before after
                        |> Expect.lessThan 1.0e-6
            , test "looking around still changes the view direction" <|
                \_ ->
                    let
                        forwardOf cam =
                            Vec3.normalize (Vec3.sub cam.target (Camera.position cam))
                    in
                    Vec3.distance (forwardOf walkCamera) (forwardOf (Camera.lookAround 1.0 0 walkCamera))
                        |> Expect.greaterThan 0.1
            , test "orbiting holds the target still" <|
                \_ ->
                    Vec3.distance (Camera.orbitBy 1.0 0.3 bigModelCamera).target bigModelCamera.target
                        |> Expect.lessThan 1.0e-9
            , test "a drag right turns the same way in both modes" <|
                \_ ->
                    let
                        turn cam =
                            let
                                dragged =
                                    Camera.onMouseMove 200 100 (dragging cam)
                            in
                            dragged.azimuth - cam.azimuth
                    in
                    -- Same sign and magnitude; only the pivot differs.
                    turn walkCamera
                        |> Expect.within (Expect.Relative 1.0e-9) (turn { bigModelCamera | distance = walkCamera.distance })
            ]
        , describe "movement"
            [ test "moving forward follows the heading along the ground" <|
                \_ ->
                    let
                        climbing =
                            Camera.lookAround 0 1.2 walkCamera

                        travelled =
                            Vec3.sub (Camera.moveBy { forward = 100, right = 0, up = 0 } climbing).target climbing.target
                    in
                    -- Looking up and walking forward must not lift you off the ground.
                    Expect.all
                        [ \v -> Expect.within (Expect.Absolute 1.0e-9) 0 (Vec3.getY v)
                        , \v -> Expect.within (Expect.Relative 1.0e-9) 100 (Vec3.length v)
                        ]
                        travelled
            , test "movement is proportional to the requested distance" <|
                \_ ->
                    let
                        travel amount =
                            Vec3.distance
                                (Camera.moveBy { forward = amount, right = 0, up = 0 } walkCamera).target
                                walkCamera.target
                    in
                    travel 200
                        |> Expect.within (Expect.Relative 1.0e-9) (2 * travel 100)
            , test "strafing is perpendicular to walking" <|
                \_ ->
                    let
                        step d =
                            Vec3.normalize (Vec3.sub (Camera.moveBy d walkCamera).target walkCamera.target)
                    in
                    Vec3.dot
                        (step { forward = 1, right = 0, up = 0 })
                        (step { forward = 0, right = 1, up = 0 })
                        |> Expect.within (Expect.Absolute 1.0e-9) 0
            , test "vertical movement is along world up" <|
                \_ ->
                    Vec3.sub (Camera.moveBy { forward = 0, right = 0, up = 50 } walkCamera).target walkCamera.target
                        |> Expect.equal (vec3 0 50 0)
            , test "moving leaves the orbit parameters alone" <|
                \_ ->
                    let
                        moved =
                            Camera.moveBy { forward = 100, right = 50, up = 10 } walkCamera
                    in
                    Expect.all
                        [ \c -> Expect.within (Expect.Absolute 1.0e-9) walkCamera.azimuth c.azimuth
                        , \c -> Expect.within (Expect.Absolute 1.0e-9) walkCamera.elevation c.elevation
                        , \c -> Expect.within (Expect.Absolute 1.0e-9) walkCamera.distance c.distance
                        ]
                        moved
            , test "dollying moves forward along the heading" <|
                \_ ->
                    Expect.equal
                        (Camera.dollyBy 100 walkCamera).target
                        (Camera.moveBy { forward = 100, right = 0, up = 0 } walkCamera).target
            , test "walk speed scales with the model" <|
                \_ ->
                    let
                        speed r =
                            Camera.walkSpeed (Camera.setSceneRadius r bigModelCamera)
                    in
                    speed (2 * cityRadius)
                        |> Expect.within (Expect.Relative 1.0e-9) (2 * speed cityRadius)
            , test "walk speed has a floor for small models" <|
                \_ ->
                    Camera.walkSpeed (Camera.setSceneRadius 10 bigModelCamera)
                        |> Expect.greaterThan 0
            ]
        ]
