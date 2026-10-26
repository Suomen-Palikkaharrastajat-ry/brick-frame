module SnapshotTest exposing (suite)

{-| Escape closes the snapshot modal before the viewer's own key handling runs.
-}

import Expect
import Main exposing (Flags, Msg(..), init, update)
import Test exposing (Test, describe, test)
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
        ]
