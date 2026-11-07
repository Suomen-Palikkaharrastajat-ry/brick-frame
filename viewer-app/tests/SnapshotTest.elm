module SnapshotTest exposing (suite)

{-| Escape closes the snapshot modal before the viewer's own key handling runs.
-}

import Expect
import Main exposing (Flags, Msg(..), init, update)
import Math.Vector3 as Vec3
import Test exposing (Test, describe, test)
import UI.CameraGlobe as CameraGlobe
import UI.SnapshotModal as SnapshotModal
import Viewport


testFlags : Flags
testFlags =
    { ldrawBase = ""
    , ldrawFallbackBase = ""
    , defaultModelUrl = ""
    , initialHash = ""
    , maxRpm = 120
    , uiMode = "simulator"
    , controlsEnabled = True
    , initialMotorIndex = -1
    , initialRpm = 0
    , useWindowResize = False
    , ambientStrength = Nothing
    , lightStrength = Nothing
    , vibrance = Nothing
    , edgeWidth = Nothing
    , supportsPointerEvents = True
    , studioUrl = ""
    }


{-| A key press delivered to the viewer the way the browser's keydown reaches it.
-}
pressKey : String -> Msg
pressKey key =
    ViewportMsg
        (Viewport.KeyPressed
            { key = key
            , ctrl = False
            , meta = False
            , alt = False
            , shift = False
            , repeat = False
            , targetTag = "BODY"
            , targetEditable = False
            , targetRole = ""
            }
        )


suite : Test
suite =
    describe "Snapshot modal"
        [ test "Escape closes an open snapshot modal" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags

                    opened =
                        { model | snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" }) }

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") opened
                in
                Expect.equal [ True, True ]
                    [ opened.snapshotModal /= Nothing, afterEscape.snapshotModal == Nothing ]
        , test "progress and a failure reach the open modal" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags

                    opened =
                        { model | snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" }) }

                    ( progressed, _ ) =
                        update (SnapshotProgress { fraction = 0.5, samples = 12 }) opened

                    ( failed, _ ) =
                        update (SnapshotFailed "Tracer ran out of memory") progressed
                in
                Expect.equal [ True, True ]
                    [ Maybe.map .progress progressed.snapshotModal == Just { fraction = 0.5, samples = 12 }
                    , Maybe.map .phase failed.snapshotModal == Just (SnapshotModal.Failed "Tracer ran out of memory")
                    ]
        , test "Close removes the modal, even while it is still rendering" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags

                    opened =
                        { model | snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" }) }

                    ( closed, _ ) =
                        update SnapshotClose opened
                in
                Expect.equal Nothing closed.snapshotModal
        , test "ToggleRealistic enables the globe sun marker and dragging it orbits the light without moving the camera" <|
            \_ ->
                let
                    ( draftModel, _ ) =
                        init testFlags

                    ( realisticModel, _ ) =
                        update ToggleRealistic draftModel

                    initialLight =
                        Viewport.lightDirection realisticModel.viewport
                            |> Maybe.withDefault (Vec3.vec3 0 1 0)

                    sunAngles =
                        CameraGlobe.lightAngles initialLight

                    sunPos =
                        CameraGlobe.markerPosition sunAngles.azimuth sunAngles.elevation

                    afterSunDrag =
                        realisticModel
                            |> (\m -> update (GlobePressed ( sunPos.x, sunPos.y )) m |> Tuple.first)
                            |> (\m -> update (GlobeMoved ( sunPos.x + 18, sunPos.y - 10 )) m |> Tuple.first)
                            |> (\m -> update GlobeReleased m |> Tuple.first)

                    nextLight =
                        Viewport.lightDirection afterSunDrag.viewport
                            |> Maybe.withDefault initialLight
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing (Viewport.lightDirection draftModel.viewport)
                    , \_ -> Expect.notEqual Nothing (Viewport.lightDirection realisticModel.viewport)
                    , \_ ->
                        Expect.within (Expect.Absolute 1.0e-6)
                            realisticModel.viewport.camera.azimuth
                            afterSunDrag.viewport.camera.azimuth
                    , \_ ->
                        Expect.greaterThan 0.1 (Vec3.distance initialLight nextLight)
                    ]
                    ()
        ]
