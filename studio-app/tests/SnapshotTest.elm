module SnapshotTest exposing (suite)

{-| Escape closes the snapshot modal before the page under it sees the key.
-}

import Expect
import Main exposing (Model, Msg(..), init, update)
import Studio.Workspace as Workspace
import Test exposing (Test, describe, test)
import UI.SnapshotModal as SnapshotModal
import Viewport


flags : Workspace.Flags
flags =
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
    , draft = Nothing
    , favorites = Nothing
    , recentParts = Nothing
    , playBaseUrl = Nothing
    }


{-| A key press on the workspace, as the browser's keydown reaches it.
-}
pressKey : String -> Msg
pressKey key =
    WorkspaceMsg
        (Workspace.ViewportMsg
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
        )


withSnapshot : Model -> Model
withSnapshot model =
    { model | snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" }) }


suite : Test
suite =
    describe "Snapshot modal"
        [ test "Escape closes an open snapshot modal" <|
            \_ ->
                let
                    ( model, _ ) =
                        init flags

                    opened =
                        withSnapshot model

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") opened
                in
                Expect.equal [ True, True ]
                    [ opened.snapshotModal /= Nothing, afterEscape.snapshotModal == Nothing ]
        , test "progress and a failure reach the open modal" <|
            \_ ->
                let
                    ( model, _ ) =
                        init flags

                    ( progressed, _ ) =
                        update (SnapshotProgress { fraction = 0.5, samples = 12 }) (withSnapshot model)

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
                        init flags

                    ( closed, _ ) =
                        update SnapshotClose (withSnapshot model)
                in
                Expect.equal Nothing closed.snapshotModal
        ]
