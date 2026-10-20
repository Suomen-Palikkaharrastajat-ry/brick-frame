module InstructionsTest exposing (suite)

{-| Tests for the building instructions viewer, navigation, playback,
camera framing, and the switch between it and the workspace.
-}

import Expect
import LDraw.Document as Document
import Main exposing (Msg(..))
import Math.Vector3 as Vec3
import Studio.Instructions as Instructions
import Studio.Workspace as Workspace
import Test exposing (Test, describe, test)
import Time
import Viewport
import Viewport.Shortcuts as Shortcuts


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
    }


modelWithSteps : String
modelWithSteps =
    String.join "\n"
        [ "0 Test Stepped Model"
        , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "0 STEP"
        , "1 4 20 -24 40 1 0 0 0 1 0 0 0 1 3002.dat"
        , "0 STEP"
        , "1 1 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
        ]


triangle : String
triangle =
    "3 16 0 0 0 10 0 0 0 0 10\n"


editing : Main.Model
editing =
    Main.init flags
        |> Tuple.first
        |> send (WorkspaceMsg (Workspace.FileTextReceived modelWithSteps))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3001.dat" (Ok triangle))))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3002.dat" (Ok triangle))))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3003.dat" (Ok triangle))))


send : Main.Msg -> Main.Model -> Main.Model
send msg model =
    Main.update msg model |> Tuple.first


withInstructions : Main.Model -> (Instructions.Model -> Expect.Expectation) -> Expect.Expectation
withInstructions model check =
    case model.instructions of
        Just inst ->
            check inst

        Nothing ->
            Expect.fail "Instructions viewer was not open"


stepInstructions : Instructions.Msg -> Instructions.Model -> Instructions.Model
stepInstructions msg model =
    Instructions.update msg model |> Tuple.first


keyDown : String -> Viewport.Msg
keyDown key =
    let
        event : Shortcuts.KeyEvent
        event =
            { key = key
            , shift = False
            , ctrl = False
            , meta = False
            , alt = False
            , repeat = False
            , targetTag = "BODY"
            , targetEditable = False
            }
    in
    Viewport.KeyPressed event


suite : Test
suite =
    describe "Studio.Instructions"
        [ describe "opening and closing instructions"
            [ test "InstructionsRequested opens the instructions page" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            Expect.all
                                [ \i -> Expect.equal 3 (List.length i.steps)
                                , \i -> Expect.equal 1 i.currentStepIndex
                                , \i -> Expect.equal False i.isPlaying
                                , \i -> Expect.equal True i.autoFrame
                                ]
                                inst
            , test "Leave returns to workspace" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing

                        closed =
                            send (InstructionsMsg Instructions.Leave) opened
                    in
                    Expect.all
                        [ \m -> Expect.equal Nothing m.instructions
                        , \m -> Expect.equal 3 (List.length (Document.placements m.workspace.document))
                        ]
                        closed
            ]
        , describe "navigation and playback"
            [ test "NextStep and PrevStep navigate within bounds" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                step2 =
                                    stepInstructions Instructions.NextStep inst

                                step3 =
                                    stepInstructions Instructions.NextStep step2

                                stepOver =
                                    stepInstructions Instructions.NextStep step3

                                back2 =
                                    stepInstructions Instructions.PrevStep stepOver

                                back1 =
                                    stepInstructions Instructions.PrevStep back2

                                backUnder =
                                    stepInstructions Instructions.PrevStep back1
                            in
                            Expect.all
                                [ \_ -> Expect.equal 2 step2.currentStepIndex
                                , \_ -> Expect.equal 3 step3.currentStepIndex
                                , \_ -> Expect.equal 3 stepOver.currentStepIndex
                                , \_ -> Expect.equal 2 back2.currentStepIndex
                                , \_ -> Expect.equal 1 back1.currentStepIndex
                                , \_ -> Expect.equal 1 backUnder.currentStepIndex
                                ]
                                ()
            , test "SetStepSlider jumps to specified step" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                jumped =
                                    stepInstructions (Instructions.SetStepSlider "3") inst
                            in
                            Expect.equal 3 jumped.currentStepIndex
            , test "TogglePlay and PlaybackTick advance steps" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                playing =
                                    stepInstructions Instructions.TogglePlay inst

                                tick1 =
                                    stepInstructions (Instructions.PlaybackTick (Time.millisToPosix 1000)) playing
                            in
                            Expect.all
                                [ \_ -> Expect.equal True playing.isPlaying
                                , \_ -> Expect.equal 2 tick1.currentStepIndex
                                ]
                                ()
            , test "ToggleViewMode switches between 3D and Sheet" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                sheet =
                                    stepInstructions Instructions.ToggleViewMode inst

                                mode3d =
                                    stepInstructions Instructions.ToggleViewMode sheet
                            in
                            Expect.all
                                [ \_ -> Expect.equal Instructions.ModeSheet sheet.viewMode
                                , \_ -> Expect.equal Instructions.Mode3D mode3d.viewMode
                                ]
                                ()
            , test "ToggleAutoFrame toggles auto-framing" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                off =
                                    stepInstructions Instructions.ToggleAutoFrame inst

                                onAgain =
                                    stepInstructions Instructions.ToggleAutoFrame off
                            in
                            Expect.all
                                [ \_ -> Expect.equal False off.autoFrame
                                , \_ -> Expect.equal True onAgain.autoFrame
                                ]
                                ()
            , test "AutoStepBy partitions an unstepped model and updates viewport placements per step" <|
                \_ ->
                    let
                        unsteppedLdr =
                            String.join "\n"
                                [ "0 Unstepped"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "1 1 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                ]

                        opened =
                            editing
                                |> send (WorkspaceMsg (Workspace.FileTextReceived unsteppedLdr))
                                |> send (WorkspaceMsg Workspace.InstructionsRequested)
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                steppedBy1 =
                                    stepInstructions (Instructions.AutoStepBy 1) inst

                                step1Count =
                                    steppedBy1.viewport.document
                                        |> Maybe.map (Document.placements >> List.length)

                                step2 =
                                    stepInstructions Instructions.NextStep steppedBy1

                                step2Count =
                                    step2.viewport.document
                                        |> Maybe.map (Document.placements >> List.length)
                            in
                            Expect.all
                                [ \_ -> Expect.equal 3 (List.length steppedBy1.steps)
                                , \_ -> Expect.equal (Just 1) step1Count
                                , \_ -> Expect.equal (Just 2) step2Count
                                ]
                                ()
            , test "auto-framing converts LDraw Y-down coordinates to render Y-up coordinates" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                step2 =
                                    stepInstructions Instructions.NextStep inst

                                target =
                                    step2.viewport.camera.target
                            in
                            Expect.all
                                [ \_ -> Expect.equal 20 (round (Vec3.getX target))
                                , \_ -> Expect.equal 24 (round (Vec3.getY target))
                                , \_ -> Expect.equal -40 (round (Vec3.getZ target))
                                ]
                                ()
            , test "keyboard shortcuts navigate steps and toggle help" <|
                \_ ->
                    let
                        opened =
                            send (WorkspaceMsg Workspace.InstructionsRequested) editing
                    in
                    withInstructions opened <|
                        \inst ->
                            let
                                next =
                                    stepInstructions (Instructions.ViewportMsg (keyDown "ArrowRight")) inst

                                prev =
                                    stepInstructions (Instructions.ViewportMsg (keyDown "ArrowLeft")) next

                                helpOpened =
                                    stepInstructions (Instructions.ViewportMsg (keyDown "?")) prev

                                helpClosed =
                                    stepInstructions (Instructions.ViewportMsg (keyDown "Escape")) helpOpened
                            in
                            Expect.all
                                [ \_ -> Expect.equal 2 next.currentStepIndex
                                , \_ -> Expect.equal 1 prev.currentStepIndex
                                , \_ -> Expect.equal True helpOpened.helpOpen
                                , \_ -> Expect.equal False helpClosed.helpOpen
                                ]
                                ()
            ]
        ]
