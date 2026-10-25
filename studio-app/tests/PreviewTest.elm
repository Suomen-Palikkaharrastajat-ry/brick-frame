module PreviewTest exposing (suite)

{-| Tests for the simulation preview and the switch between it and the
workspace.
-}

import Array
import Dict
import Expect
import LDraw.Document as Document
import LDraw.Resolve exposing (PartStatus(..))
import Main exposing (Msg(..))
import Set
import Simulation
import Studio.Preview as Preview
import Studio.Workspace as Workspace
import Test exposing (Test, describe, test)
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


{-| The stub gear specs put an 8T gear at pitch radius 8 and a 16T at 16, so
two gears 24 apart mesh.
-}
gearsFile : String
gearsFile =
    "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n1 4 24 0 0 1 0 0 0 1 0 0 0 1 4019.dat\n1 4 100 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"


triangle : String
triangle =
    "3 16 0 0 0 10 0 0 0 0 10\n"


{-| A studio with the gear model open and every part loaded.
-}
editing : Main.Model
editing =
    Main.init flags
        |> Tuple.first
        |> send (WorkspaceMsg (Workspace.FileTextReceived gearsFile))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3647.dat" (Ok triangle))))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "4019.dat" (Ok triangle))))
        |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3001.dat" (Ok triangle))))


send : Main.Msg -> Main.Model -> Main.Model
send msg model =
    Main.update msg model |> Tuple.first


simulating : Main.Model
simulating =
    send (WorkspaceMsg Workspace.SimulateRequested) editing


withPreview : Main.Model -> (Preview.Model -> Expect.Expectation) -> Expect.Expectation
withPreview model check =
    case model.preview of
        Just preview ->
            check preview

        Nothing ->
            Expect.fail "expected the preview to be open"


suite : Test
suite =
    describe "Studio preview"
        [ test "the workspace has its parts loaded" <|
            \_ ->
                Expect.equal Viewport.Ready editing.workspace.viewport.loadPhase
        , describe "opening"
            [ test "Simulate switches to the preview" <|
                \_ ->
                    Expect.equal True (simulating.preview /= Nothing)
            , test "the preview starts from the workspace's camera" <|
                \_ ->
                    withPreview simulating
                        (\preview ->
                            Expect.all
                                [ \p -> Expect.within (Expect.Absolute 0.000001) editing.workspace.viewport.camera.azimuth p.viewport.camera.azimuth
                                , \p -> Expect.within (Expect.Absolute 0.000001) editing.workspace.viewport.camera.distance p.viewport.camera.distance
                                ]
                                preview
                        )
            , test "parts the workspace already loaded are not fetched again" <|
                \_ ->
                    withPreview simulating
                        (\preview ->
                            Expect.all
                                [ \p -> Expect.equal Viewport.FlatteningGeometry p.viewport.loadPhase
                                , \p -> Expect.equal (Just (Loaded [])) (Dict.get "3647.dat" p.viewport.partCache |> Maybe.map (always (Loaded [])))
                                ]
                                preview
                        )
            , test "gear detection runs on the snapshot" <|
                \_ ->
                    withPreview simulating
                        (\preview ->
                            Expect.all
                                [ \p -> Expect.equal True p.simulation.simulationAvailable
                                , \p -> Expect.equal (Just 2) (Maybe.map (.instances >> Array.length) p.simulation.gearGraph)
                                ]
                                preview
                        )
            , test "the gears are left out of the baked scene's lines" <|
                \_ ->
                    withPreview simulating
                        (\preview ->
                            -- Only the plain brick is flattened; the gears turn on their own.
                            Expect.equal 3 (List.length preview.viewport.topLevelLines)
                        )
            , test "a model without gears says so" <|
                \_ ->
                    let
                        plain =
                            Main.init flags
                                |> Tuple.first
                                |> send (WorkspaceMsg (Workspace.FileTextReceived "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"))
                                |> send (WorkspaceMsg (Workspace.ViewportMsg (Viewport.PartLoaded "3001.dat" (Ok triangle))))
                                |> send (WorkspaceMsg Workspace.SimulateRequested)
                    in
                    withPreview plain
                        (\p ->
                            Expect.all
                                [ \q -> Expect.equal True q.simulation.simulationChecked
                                , \q -> Expect.equal False q.simulation.simulationAvailable
                                ]
                                p
                        )
            ]
        , describe "while simulating"
            [ test "play starts the clock" <|
                \_ ->
                    withPreview (send (PreviewMsg (Preview.SimulationMsg Simulation.Play)) simulating)
                        (\p -> Expect.equal True p.simulation.playback.running)
            , test "the editing workspace is not touched" <|
                \_ ->
                    let
                        selected =
                            send (WorkspaceMsg (Workspace.SelectPlacement 1 False)) editing

                        after =
                            send (WorkspaceMsg Workspace.SimulateRequested) selected
                                |> send (PreviewMsg (Preview.SimulationMsg Simulation.Play))
                    in
                    Expect.all
                        [ \m -> Expect.equal (Set.fromList [ 1 ]) m.workspace.selection
                        , \m -> Expect.equal selected.workspace.document m.workspace.document
                        ]
                        after
            , test "editing messages are ignored while the preview is open" <|
                \_ ->
                    send (WorkspaceMsg Workspace.SelectAll) simulating
                        |> .workspace
                        |> .selection
                        |> Expect.equal Set.empty
            ]
        , describe "leaving"
            [ test "returns to the workspace with selection and document as they were" <|
                \_ ->
                    let
                        selected =
                            send (WorkspaceMsg (Workspace.SelectPlacement 0 False)) editing

                        back =
                            send (WorkspaceMsg Workspace.SimulateRequested) selected
                                |> send (PreviewMsg Preview.Leave)
                    in
                    Expect.all
                        [ \m -> Expect.equal Nothing m.preview
                        , \m -> Expect.equal (Set.fromList [ 0 ]) m.workspace.selection
                        , \m -> Expect.equal (List.length (Document.placements selected.workspace.document)) (List.length (Document.placements m.workspace.document))
                        ]
                        back
            , test "the camera the preview ended on carries back" <|
                \_ ->
                    let
                        moved =
                            simulating
                                |> send (PreviewMsg (Preview.ViewportMsg (Viewport.Wheel 300)))
                                |> send (PreviewMsg Preview.Leave)
                    in
                    Expect.greaterThan editing.workspace.viewport.camera.distance moved.workspace.viewport.camera.distance
            , test "it can be opened again, and detects gears again" <|
                \_ ->
                    let
                        again =
                            simulating
                                |> send (PreviewMsg Preview.Leave)
                                |> send (WorkspaceMsg Workspace.SimulateRequested)
                    in
                    withPreview again (\p -> Expect.equal True p.simulation.simulationAvailable)
            , test "edits made between previews show up in the next one" <|
                \_ ->
                    let
                        edited =
                            simulating
                                |> send (PreviewMsg Preview.Leave)
                                |> send (WorkspaceMsg (Workspace.SelectPlacement 0 False))
                                |> send (WorkspaceMsg Workspace.DeleteSelection)
                                |> send (WorkspaceMsg Workspace.SimulateRequested)
                    in
                    -- The 8T gear is gone, so the 16T has nothing to mesh with.
                    withPreview edited (\p -> Expect.equal (Just 1) (Maybe.map (.instances >> Array.length) p.simulation.gearGraph))
            ]
        ]
