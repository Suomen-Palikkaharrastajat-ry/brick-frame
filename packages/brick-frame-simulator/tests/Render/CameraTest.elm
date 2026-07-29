module Render.CameraTest exposing (suite)

{-| Unit tests for orbit camera interaction math.

These pin the properties that make navigation feel right — reciprocal zoom,
viewport-relative sensitivity, and cursor-tracking pan — rather than the
constants themselves.

-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Camera as Camera exposing (Camera)
import Test exposing (Test, describe, test)


{-| A camera framing a city-scale model, as `cameraForBounds` would leave it.
-}
bigModelCamera : Camera
bigModelCamera =
    Camera.init
        |> Camera.setViewportHeight 900
        |> Camera.setZoomRange 0.5 60000
        |> (\cam -> { cam | distance = 7500 })


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
            ]
        ]
