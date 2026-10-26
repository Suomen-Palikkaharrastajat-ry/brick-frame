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
    , contraptionsBaseUrl = Nothing
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
        ]
