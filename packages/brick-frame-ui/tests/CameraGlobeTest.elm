module CameraGlobeTest exposing (suite)

{-| Tests for the shared camera globe (`UI.CameraGlobe`): the mapping between
globe points and camera directions, the zoom slider's scale, and the drag that
turns the camera from pointer moves or a tap.
-}

import Expect
import Render.Camera as Camera
import Test exposing (Test, describe, test)
import UI.CameraGlobe as CameraGlobe


suite : Test
suite =
    describe "UI.CameraGlobe"
        [ describe "Globe geometry"
            [ test "a point on the globe maps back to the camera direction it shows" <|
                \_ ->
                    let
                        marker =
                            CameraGlobe.markerPosition 0.7 0.4
                    in
                    case CameraGlobe.directionAt ( marker.x, marker.y ) of
                        Just direction ->
                            Expect.all
                                [ \d -> Expect.within (Expect.Absolute 1.0e-6) 0.7 d.azimuth
                                , \d -> Expect.within (Expect.Absolute 1.0e-6) 0.4 d.elevation
                                ]
                                direction

                        Nothing ->
                            Expect.fail "marker should lie on the globe"
            , test "off the globe there is no direction" <|
                \_ ->
                    Expect.equal Nothing (CameraGlobe.directionAt ( 100, 0 ))
            , test "a camera behind the model shows on the far side" <|
                \_ ->
                    Expect.equal False (CameraGlobe.markerPosition pi 0).front
            ]
        , describe "Zoom slider"
            [ test "zoom level round-trips through the wheel delta" <|
                \_ ->
                    let
                        camera =
                            Camera.init
                                |> Camera.setZoomRange 10 1000

                        zoomed =
                            Camera.onWheel (CameraGlobe.wheelDeltaForZoom camera 0.25) camera
                    in
                    Expect.within (Expect.Absolute 1.0e-6) 0.25 (CameraGlobe.zoomLevel zoomed)
            ]
        , describe "Dragging the globe"
            [ test "a press that does not move is a tap that flies to the pressed point" <|
                \_ ->
                    let
                        camera =
                            Camera.init

                        marker =
                            CameraGlobe.markerPosition 1.2 0.3

                        drag =
                            CameraGlobe.startDrag camera ( marker.x, marker.y )
                    in
                    case CameraGlobe.releaseDrag camera drag of
                        Just ( azimuthDelta, elevationDelta ) ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 1.0e-6) (1.2 - camera.azimuth) azimuthDelta
                                , \_ -> Expect.within (Expect.Absolute 1.0e-6) (0.3 - camera.elevation) elevationDelta
                                ]
                                ()

                        Nothing ->
                            Expect.fail "a tap should fly the camera"
            , test "a drag that travelled is not a tap" <|
                \_ ->
                    let
                        camera =
                            Camera.init

                        ( _, moved ) =
                            CameraGlobe.moveDrag (CameraGlobe.startDrag camera ( 0, 0 )) ( 10, 0 )
                    in
                    Expect.equal Nothing (CameraGlobe.releaseDrag camera moved)
            , test "each pointer move orbits by the distance moved, one radian per globe radius" <|
                \_ ->
                    let
                        camera =
                            Camera.init

                        drag =
                            CameraGlobe.startDrag camera ( 0, 0 )

                        ( ( azimuthDelta, elevationDelta ), next ) =
                            CameraGlobe.moveDrag drag ( 0, -44 )
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 azimuthDelta
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) 1 elevationDelta
                        , \_ -> Expect.within (Expect.Absolute 1.0e-6) 44 next.travel
                        ]
                        ()
            ]
        ]
