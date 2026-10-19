module WorkspaceTest exposing (suite)

{-| Tests for the studio's workspace page: loading documents into the viewport
and what the page does with the viewport's events.
-}

import Expect
import Http
import LDraw.Document as Document
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Set
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo
import Studio.Shortcuts as Shortcuts exposing (Tool(..))
import Studio.Workspace as Workspace exposing (Msg(..))
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
    }


start : Workspace.Model
start =
    Workspace.init flags |> Tuple.first


{-| A file whose only part is not embedded, so the viewport has to fetch it.
-}
twoParts : String
twoParts =
    "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 14 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"


suite : Test
suite =
    describe "Studio.Workspace"
        [ editingSuite
        , test "starts with an empty document that is ready to show" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.equal [] (Document.placements m.document)
                    , \m -> Expect.equal Viewport.Ready m.viewport.loadPhase
                    ]
                    start
        , test "opening a file replaces the document" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .document
                    |> Document.placements
                    |> List.map .id
                    |> Expect.equal [ 0, 1 ]
        , test "the viewport starts resolving the parts of the new document" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .viewport
                    |> .loadPhase
                    |> Expect.equal Viewport.ResolvingParts
        , test "a new document is empty again" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> Workspace.update NewDocument
                    |> Tuple.first
                    |> .document
                    |> Document.placements
                    |> Expect.equal []
        , test "the document comes back untouched after loading" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .document
                    |> .touched
                    |> Expect.equal (Document.clearTouched Document.empty |> .touched)
        , test "a file error is shown through the viewport" <|
            \_ ->
                Workspace.update (FileFailed "not LDraw") start
                    |> Tuple.first
                    |> .viewport
                    |> .errorMsg
                    |> Expect.equal (Just "not LDraw")
        , test "the help dialog toggles" <|
            \_ ->
                Workspace.update ToggleHelp start
                    |> Tuple.first
                    |> .helpOpen
                    |> Expect.equal True
        ]



-- ── Editing ───────────────────────────────────────────────────────────────────


{-| Two bricks far enough apart that clicking the middle of the screen can only
hit the first.
-}
brickFile : String
brickFile =
    "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 14 300 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"


brickPart : String
brickPart =
    "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10\n4 16 -10 -20 -10 10 -20 -10 10 -20 10 -10 -20 10\n4 16 -10 0 -10 10 0 -10 10 -20 -10 -10 -20 -10\n4 16 -10 0 10 10 0 10 10 -20 10 -10 -20 10\n"


{-| A workspace with the two bricks loaded, parts included.
-}
loaded : Workspace.Model
loaded =
    start
        |> Workspace.update (FileTextReceived brickFile)
        |> Tuple.first
        |> Workspace.update (ViewportMsg (Viewport.PartLoaded "3001.dat" (Ok brickPart)))
        |> Tuple.first
        |> Workspace.update (ViewportMsg (Viewport.WindowResize 800 600))
        |> Tuple.first


step : Msg -> Workspace.Model -> Workspace.Model
step msg model =
    Workspace.update msg model |> Tuple.first


{-| Press and release at a point, as a mouse click.
-}
click : ( Float, Float ) -> Workspace.Model -> Workspace.Model
click ( x, y ) model =
    model
        |> step (ViewportMsg (Viewport.MouseDown x y False))
        |> step (ViewportMsg (Viewport.MouseUp x y))


shiftClick : ( Float, Float ) -> Workspace.Model -> Workspace.Model
shiftClick ( x, y ) model =
    model
        |> step (ViewportMsg (Viewport.MouseDown x y True))
        |> step (ViewportMsg (Viewport.MouseUp x y))


{-| Where a placement's centre is on screen.
-}
screenOf : Int -> Workspace.Model -> ( Float, Float )
screenOf id model =
    Document.getPlacement id model.document
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 -10 0))
        |> Maybe.andThen (Gizmo.toRender >> Gizmo.project model.viewport.camera 800 600)
        |> Maybe.withDefault ( -1, -1 )


positionOf : Int -> Workspace.Model -> Maybe Float
positionOf id model =
    Document.getPlacement id model.document
        |> Maybe.map (\p -> Vec3.getX (Mat4.transform p.matrix (vec3 0 0 0)))


selected : Workspace.Model -> List Int
selected model =
    Set.toList model.selection


editingSuite : Test
editingSuite =
    describe "Studio.Workspace editing"
        [ test "the parts appear once they have loaded" <|
            \_ ->
                loaded.viewport.instancedScene
                    |> Maybe.map (.placements >> List.length)
                    |> Expect.equal (Just 2)
        , describe "selection"
            [ test "clicking a part selects it" <|
                \_ ->
                    click (screenOf 0 loaded) loaded
                        |> selected
                        |> Expect.equal [ 0 ]
            , test "clicking the other part selects that instead" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> click (screenOf 1 loaded)
                        |> selected
                        |> Expect.equal [ 1 ]
            , test "shift-clicking adds to the selection and again removes it" <|
                \_ ->
                    let
                        both =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> shiftClick (screenOf 1 loaded)
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 0, 1 ] (selected m)
                        , \m -> Expect.equal [ 0 ] (selected (shiftClick (screenOf 1 loaded) m))
                        ]
                        both
            , test "clicking empty space clears the selection" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> click ( 5, 5 )
                        |> selected
                        |> Expect.equal []
            , test "a tap selects like a click" <|
                \_ ->
                    let
                        ( x, y ) =
                            screenOf 0 loaded
                    in
                    loaded
                        |> step (ViewportMsg (Viewport.PointerTouchStart { id = 1, x = x, y = y }))
                        |> step (ViewportMsg (Viewport.PointerTouchEnd 1))
                        |> selected
                        |> Expect.equal [ 0 ]
            , test "select all and clear" <|
                \_ ->
                    Expect.all
                        [ \m -> Expect.equal [ 0, 1 ] (selected (step SelectAll m))
                        , \m -> Expect.equal [] (selected (step ClearSelection (step SelectAll m)))
                        ]
                        loaded
            ]
        , describe "delete, duplicate and undo"
            [ test "delete removes the selection from the document and the scene" <|
                \_ ->
                    let
                        after =
                            loaded |> click (screenOf 0 loaded) |> step DeleteSelection
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 1 ] (List.map .id (Document.placements m.document))
                        , \m -> Expect.equal (Just [ 1 ]) (Maybe.map (.placements >> List.map .source) m.viewport.instancedScene)
                        , \m -> Expect.equal [] (selected m)
                        ]
                        after
            , test "undo brings the part back to the document and the scene" <|
                \_ ->
                    let
                        after =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step DeleteSelection
                                |> step Undo
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 0, 1 ] (List.map .id (Document.placements m.document))
                        , \m -> Expect.equal (Just [ 0, 1 ]) (Maybe.map (.placements >> List.map .source >> List.sort) m.viewport.instancedScene)
                        ]
                        after
            , test "redo deletes it again" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> step DeleteSelection
                        |> step Undo
                        |> step Redo
                        |> .document
                        |> Document.placements
                        |> List.map .id
                        |> Expect.equal [ 1 ]
            , test "duplicate selects the copies, offset by the grid step" <|
                \_ ->
                    let
                        after =
                            loaded |> click (screenOf 0 loaded) |> step DuplicateSelection
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 2 ] (selected m)
                        , \m -> Expect.equal (Just 20) (positionOf 2 m)
                        , \m -> Expect.equal 3 (List.length (Document.placements m.document))
                        ]
                        after
            , test "editing with nothing selected does nothing" <|
                \_ ->
                    loaded
                        |> step DeleteSelection
                        |> step (RotateSelection AxisY 1)
                        |> .history
                        |> Edit.canUndo
                        |> Expect.equal False
            ]
        , describe "rotating"
            [ test "a quarter turn about Y moves the part around the selection's centre" <|
                \_ ->
                    let
                        after =
                            loaded
                                |> step SelectAll
                                |> step (RotateSelection AxisY 1)
                    in
                    -- Both bricks turn about their midpoint (x = 150): the one at
                    -- x = 0 ends up on the same X as the one at x = 300.
                    Expect.all
                        [ \m -> Expect.within (Expect.Absolute 0.01) 150 (Maybe.withDefault 0 (positionOf 0 m))
                        , \m -> Expect.within (Expect.Absolute 0.01) 150 (Maybe.withDefault 0 (positionOf 1 m))
                        ]
                        after
            , test "the turn step can be 15°" <|
                \_ ->
                    loaded
                        |> step NextRotateStep
                        |> .rotateStep
                        |> Expect.equal 15
            , test "undo turns it back" <|
                \_ ->
                    loaded
                        |> step SelectAll
                        |> step (RotateSelection AxisY 1)
                        |> step Undo
                        |> positionOf 0
                        |> Maybe.withDefault -1
                        |> Expect.within (Expect.Absolute 0.01) 0
            ]
        , describe "moving with the gizmo"
            [ test "dragging an X handle moves the selection along X, snapped to the grid" <|
                \_ ->
                    let
                        ready =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step (UseTool MoveTool)

                        handle =
                            Edit.selectionCenter [ 0 ] ready.document
                                |> Maybe.map (Gizmo.handles ready.viewport.camera 800 600)
                                |> Maybe.andThen (List.filter (\h -> h.axis == AxisX) >> List.head)

                        dragged =
                            case handle of
                                Just h ->
                                    let
                                        ( ax, ay ) =
                                            h.screenStart

                                        ( bx, by ) =
                                            h.screenEnd

                                        -- Grab the handle halfway along, pull it by one handle length.
                                        ( gx, gy ) =
                                            ( (ax + bx) / 2, (ay + by) / 2 )
                                    in
                                    Just
                                        (ready
                                            |> step (ViewportMsg (Viewport.MouseDown gx gy False))
                                            |> step (ViewportMsg (Viewport.MouseMove (gx + (bx - ax)) (gy + (by - ay))))
                                            |> step (ViewportMsg (Viewport.MouseUp (gx + (bx - ax)) (gy + (by - ay))))
                                        )

                                Nothing ->
                                    Nothing
                    in
                    case dragged of
                        Just m ->
                            Expect.all
                                [ \d -> Expect.within (Expect.Absolute 0.01) 0 (modBy 20 (round (Maybe.withDefault 1 (positionOf 0 d))) |> toFloat)
                                , \d -> Expect.equal True (Maybe.withDefault 0 (positionOf 0 d) > 0)
                                , \d -> Expect.equal (Just 300) (positionOf 1 d)
                                ]
                                m

                        Nothing ->
                            Expect.fail "no X handle"
            , test "the drag is one undo step" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool MoveTool)

                        h =
                            Edit.selectionCenter [ 0 ] ready.document
                                |> Maybe.map (Gizmo.handles ready.viewport.camera 800 600)
                                |> Maybe.andThen (List.filter (\x -> x.axis == AxisX) >> List.head)
                    in
                    case h of
                        Just handle ->
                            let
                                ( ax, ay ) =
                                    handle.screenStart

                                ( bx, by ) =
                                    handle.screenEnd

                                ( gx, gy ) =
                                    ( (ax + bx) / 2, (ay + by) / 2 )

                                dragged =
                                    ready
                                        |> step (ViewportMsg (Viewport.MouseDown gx gy False))
                                        |> step (ViewportMsg (Viewport.MouseMove (gx + (bx - ax)) (gy + (by - ay))))
                                        |> step (ViewportMsg (Viewport.MouseMove (gx + 2 * (bx - ax)) (gy + 2 * (by - ay))))
                                        |> step (ViewportMsg (Viewport.MouseUp (gx + 2 * (bx - ax)) (gy + 2 * (by - ay))))
                            in
                            Expect.all
                                [ \m -> Expect.equal True (Maybe.withDefault 0 (positionOf 0 m) > 0)
                                , \m -> Expect.equal (Just 0) (positionOf 0 (step Undo m))
                                ]
                                dragged

                        Nothing ->
                            Expect.fail "no X handle"
            , test "pressing away from the handles still orbits the camera" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool MoveTool)

                        moved =
                            ready
                                |> step (ViewportMsg (Viewport.MouseDown 100 500 False))
                                |> step (ViewportMsg (Viewport.MouseMove 160 500))
                                |> step (ViewportMsg (Viewport.MouseUp 160 500))
                    in
                    Expect.all
                        [ \m -> Expect.greaterThan 0.001 (abs (ready.viewport.camera.azimuth - m.viewport.camera.azimuth))
                        , \m -> Expect.equal (Just 0) (positionOf 0 m)
                        ]
                        moved
            , test "the move tool shows no handles without a selection" <|
                \_ ->
                    step (UseTool MoveTool) loaded
                        |> .selection
                        |> Set.isEmpty
                        |> Expect.equal True
            ]
        , describe "colour and grid"
            [ test "recolouring is one undoable step" <|
                \_ ->
                    let
                        painted =
                            loaded |> click (screenOf 0 loaded) |> step (SetColorSelection 1)

                        colours m =
                            List.map .color (Document.placements m.document)
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 1, 14 ] (colours m)
                        , \m -> Expect.equal [ 4, 14 ] (colours (step Undo m))
                        ]
                        painted
            , test "the grid step cycles 20, 8, 4, 1" <|
                \_ ->
                    List.range 1 5
                        |> List.foldl (\_ ( m, steps ) -> ( step NextGridStep m, m.gridStep :: steps )) ( loaded, [] )
                        |> Tuple.second
                        |> List.reverse
                        |> Expect.equal [ 20, 8, 4, 1, 20 ]
            ]
        , describe "keyboard"
            [ test "Delete is the delete shortcut" <|
                \_ ->
                    shortcut { key = "Delete", ctrl = False, shift = False, target = "BODY" }
                        |> Expect.equal (Just Shortcuts.DeleteSelection)
            , test "Ctrl+Z is undo and Ctrl+Shift+Z redo" <|
                \_ ->
                    ( shortcut { key = "z", ctrl = True, shift = False, target = "BODY" }
                    , shortcut { key = "z", ctrl = True, shift = True, target = "BODY" }
                    )
                        |> Expect.equal ( Just Shortcuts.Undo, Just Shortcuts.Redo )
            , test "typing in a form field never edits the document" <|
                \_ ->
                    shortcut { key = "Delete", ctrl = False, shift = False, target = "INPUT" }
                        |> Expect.equal Nothing
            , test "browser chords stay with the browser" <|
                \_ ->
                    shortcut { key = "w", ctrl = True, shift = False, target = "BODY" }
                        |> Expect.equal Nothing
            , test "Escape only clears a selection when nothing else claims it" <|
                \_ ->
                    ( shortcutIn { hasSelection = True, helpVisible = False, walking = False } "Escape"
                    , shortcutIn { hasSelection = True, helpVisible = True, walking = False } "Escape"
                    , shortcutIn { hasSelection = False, helpVisible = False, walking = False } "Escape"
                    )
                        |> Expect.equal ( Just Shortcuts.ClearSelection, Nothing, Nothing )
            , test "the viewport keys are not taken" <|
                \_ ->
                    [ "w", "a", "s", "d", "f", "g", "1", "+", "?" ]
                        |> List.map (shortcutIn { hasSelection = True, helpVisible = False, walking = False })
                        |> List.all ((==) Nothing)
                        |> Expect.equal True
            ]
        , describe "files"
            [ test "an opened file starts with a clean history and selection" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> step DeleteSelection
                        |> step (FileTextReceived brickFile)
                        |> Expect.all
                            [ \m -> Expect.equal False (Edit.canUndo m.history)
                            , \m -> Expect.equal [] (selected m)
                            ]
            , test "open, edit, save and reopen gives the edited model" <|
                \_ ->
                    let
                        edited =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step DuplicateSelection
                                |> step (RotateSelection AxisY 1)
                                |> step (SetColorSelection 1)

                        saved =
                            Writer.writeDocument edited.document

                        reopened =
                            start
                                |> step (FileTextReceived saved)
                    in
                    Expect.all
                        [ \m -> Expect.equal saved (Writer.writeDocument m.document)
                        , \m -> Expect.equal 3 (List.length (Document.placements m.document))
                        , \m -> Expect.equal [ 4, 14, 1 ] (List.map .color (Document.placements m.document))
                        ]
                        reopened
            , test "a draft from the last session is opened at start-up" <|
                \_ ->
                    Workspace.init { flags | draft = Just brickFile }
                        |> Tuple.first
                        |> .document
                        |> Document.placements
                        |> List.length
                        |> Expect.equal 2
            , test "a part that fails to load does not break editing the rest" <|
                \_ ->
                    start
                        |> step (FileTextReceived brickFile)
                        |> step (ViewportMsg (Viewport.PartLoaded "3001.dat" (Err Http.NetworkError)))
                        |> step SelectAll
                        |> selected
                        |> Expect.equal [ 0, 1 ]
            ]
        ]


shortcut : { key : String, ctrl : Bool, shift : Bool, target : String } -> Maybe Shortcuts.Action
shortcut { key, ctrl, shift, target } =
    Shortcuts.actionFor { hasSelection = True, helpVisible = False, walking = False }
        { key = key
        , shift = shift
        , ctrl = ctrl
        , meta = False
        , alt = False
        , repeat = False
        , targetTag = target
        , targetEditable = False
        }


shortcutIn : Shortcuts.Context -> String -> Maybe Shortcuts.Action
shortcutIn context key =
    Shortcuts.actionFor context
        { key = key
        , shift = False
        , ctrl = False
        , meta = False
        , alt = False
        , repeat = False
        , targetTag = "BODY"
        , targetEditable = False
        }
