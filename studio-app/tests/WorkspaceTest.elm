module WorkspaceTest exposing (suite)

{-| Tests for the studio's workspace page: loading documents into the viewport
and what the page does with the viewport's events.
-}

import Dict
import Expect
import Http
import LDraw.Connectivity as Connectivity exposing (CollisionMode(..))
import LDraw.Document as Document
import LDraw.Export as Export
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Style as Style
import Set
import Studio.BOM as BOM
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo
import Studio.Layout as Layout
import Studio.Palette as Palette
import Studio.PartIndex as PartIndex
import Studio.Shortcuts as Shortcuts exposing (Tool(..))
import Studio.Transform as Transform
import Studio.Workspace as Workspace exposing (CoordinateFrame(..), Msg(..))
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
        , panelSuite
        , snappingSuite
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
    "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10\n4 16 -10 -20 -10 10 -20 -10 10 -20 10 -10 -20 10\n4 16 -10 0 -10 10 0 -10 10 -20 -10 -10 -20 -10\n4 16 -10 0 10 10 0 10 10 -20 10 -10 -20 10\n4 16 -10 0 -10 -10 0 10 -10 -20 10 -10 -20 -10\n4 16 10 0 -10 10 0 10 10 -20 10 10 -20 -10\n"


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


{-| Browsers report pointer events in window coordinates, so a test aiming at a
canvas point adds the canvas origin the layout gave the viewport, as a real
event would.
-}
mouseDown : Float -> Float -> Bool -> Workspace.Model -> Workspace.Model
mouseDown x y shift model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.MouseDown (x + left) (y + top) shift)) model


mouseMove : Float -> Float -> Workspace.Model -> Workspace.Model
mouseMove x y model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.MouseMove (x + left) (y + top))) model


mouseUp : Float -> Float -> Workspace.Model -> Workspace.Model
mouseUp x y model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.MouseUp (x + left) (y + top))) model


doubleClick : Float -> Float -> Workspace.Model -> Workspace.Model
doubleClick x y model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.DoubleClick (x + left) (y + top))) model


touchStart : Int -> Float -> Float -> Workspace.Model -> Workspace.Model
touchStart id x y model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.PointerTouchStart { id = id, x = x + left, y = y + top })) model


{-| Press and release at a point, as a mouse click.
-}
click : ( Float, Float ) -> Workspace.Model -> Workspace.Model
click ( x, y ) model =
    model
        |> mouseDown x y False
        |> mouseUp x y


shiftClick : ( Float, Float ) -> Workspace.Model -> Workspace.Model
shiftClick ( x, y ) model =
    model
        |> mouseDown x y True
        |> mouseUp x y


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


{-| A placement's position on the ground plane (document X and Z).
-}
groundOf : Int -> Workspace.Model -> Maybe ( Float, Float )
groundOf id model =
    Document.getPlacement id model.document
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
        |> Maybe.map (\v -> ( Vec3.getX v, Vec3.getZ v ))


elevationOf : Int -> Workspace.Model -> Maybe Float
elevationOf id model =
    Document.getPlacement id model.document
        |> Maybe.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))


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
                        |> touchStart 1 x y
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
                                            |> mouseDown gx gy False
                                            |> mouseMove (gx + (bx - ax)) (gy + (by - ay))
                                            |> mouseUp (gx + (bx - ax)) (gy + (by - ay))
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
                                        |> mouseDown gx gy False
                                        |> mouseMove (gx + (bx - ax)) (gy + (by - ay))
                                        |> mouseMove (gx + 2 * (bx - ax)) (gy + 2 * (by - ay))
                                        |> mouseUp (gx + 2 * (bx - ax)) (gy + 2 * (by - ay))
                            in
                            Expect.all
                                [ \m -> Expect.equal True (Maybe.withDefault 0 (positionOf 0 m) > 0)
                                , \m -> Expect.equal (Just 0) (positionOf 0 (step Undo m))
                                ]
                                dragged

                        Nothing ->
                            Expect.fail "no X handle"
            , test "edits during a drag are ignored" <|
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

                                inDrag =
                                    ready
                                        |> mouseDown gx gy False

                                afterDelete =
                                    step DeleteSelection inDrag
                            in
                            Expect.equal (List.length (Document.placements ready.document))
                                (List.length (Document.placements afterDelete.document))

                        Nothing ->
                            Expect.fail "no X handle"
            , test "grabbing an unselected part selects it and slides it over the ground" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        moved =
                            ready
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 0 ] (selected m)
                        , \m -> Expect.notEqual (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.within (Expect.Absolute 1.0e-6) (Maybe.withDefault 0 (elevationOf 0 ready)) (Maybe.withDefault 1 (elevationOf 0 m))
                        , \m -> Expect.within (Expect.Absolute 1.0e-9) ready.viewport.camera.azimuth m.viewport.camera.azimuth
                        ]
                        moved
            , test "a part dragged by its body lands on the move grid, and undoes in one step" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        moved =
                            ready
                                |> mouseDown x y False
                                |> mouseMove (x + 37) (y + 11)
                                |> mouseMove (x + 73) (y + 23)
                                |> mouseUp (x + 73) (y + 23)

                        onGrid v =
                            toFloat (round (v / 20)) * 20 == v
                    in
                    Expect.all
                        [ \m ->
                            groundOf 0 m
                                |> Maybe.map (\( gx, gz ) -> onGrid gx && onGrid gz)
                                |> Expect.equal (Just True)
                        , \m -> Expect.equal (groundOf 0 ready) (groundOf 0 (step Undo m))
                        ]
                        moved
            , test "shift-dragging a part pans the camera instead of moving the part" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        panned =
                            ready
                                |> mouseDown x y True
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)
                    in
                    Expect.all
                        [ \m -> Expect.equal (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.notEqual ready.viewport.camera.target m.viewport.camera.target
                        ]
                        panned
            , test "pressing away from the handles still orbits the camera" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool MoveTool)

                        moved =
                            ready
                                |> mouseDown 100 500 False
                                |> mouseMove 160 500
                                |> mouseUp 160 500
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
            , test "the rotate tool selects RotateTool" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool RotateTool)
                    in
                    Expect.equal True (ready.tool == RotateTool)
            , test "dragging a rotational arc handle rotates the selection and undoes in one step" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool RotateTool)

                        firstArc =
                            Edit.selectionCenter [ 0 ] ready.document
                                |> Maybe.map (Gizmo.arcHandles ready.viewport.camera 800 600)
                                |> Maybe.andThen List.head

                        dragged =
                            case firstArc of
                                Just arc ->
                                    case arc.screenPoints of
                                        ( sx, sy ) :: _ ->
                                            let
                                                ( cx, cy ) =
                                                    arc.screenCenter

                                                -- Rotate 90 degrees counter-clockwise around screenCenter
                                                ( ex, ey ) =
                                                    ( cx + (sy - cy), cy - (sx - cx) )
                                            in
                                            Just
                                                (ready
                                                    |> mouseDown sx sy False
                                                    |> mouseMove ex ey
                                                    |> mouseUp ex ey
                                                )

                                        [] ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        euler m =
                            Document.getPlacement 0 m.document
                                |> Maybe.map (.matrix >> Transform.eulerDegrees)
                    in
                    case dragged of
                        Just m ->
                            Expect.all
                                [ \d -> Expect.notEqual (Just { x = 0, y = 0, z = 0 }) (euler d)
                                , \d -> Expect.equal (Just { x = 0, y = 0, z = 0 }) (euler (step Undo d))
                                , \d -> Expect.equal (euler d) (euler (step Redo (step Undo d)))
                                ]
                                m

                        Nothing ->
                            Expect.fail "expected rotation arc handle"
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
            , test "Escape clears selection before exiting a submodel, and Enter enters a submodel" <|
                \_ ->
                    [ shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False } "Escape"
                    , shortcutIn { hasSelection = True, helpVisible = True, walking = False, inSubmodel = False, menuOpen = False } "Escape"
                    , shortcutIn { hasSelection = False, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False } "Escape"
                    , shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = True, menuOpen = False } "Escape"
                    , shortcutIn { hasSelection = False, helpVisible = False, walking = False, inSubmodel = True, menuOpen = False } "Escape"
                    , shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False } "Enter"
                    ]
                        |> Expect.equal
                            [ Just Shortcuts.ClearSelection
                            , Nothing
                            , Nothing
                            , Just Shortcuts.ClearSelection
                            , Just Shortcuts.ExitSubmodel
                            , Just Shortcuts.EnterSubmodel
                            ]
            , test "Ctrl+G groups and Ctrl+Shift+G explodes" <|
                \_ ->
                    ( shortcut { key = "g", ctrl = True, shift = False, target = "BODY" }
                    , shortcut { key = "g", ctrl = True, shift = True, target = "BODY" }
                    )
                        |> Expect.equal ( Just Shortcuts.GroupSubmodel, Just Shortcuts.ExplodeSubmodel )
            , test "document keys: search, parts list, instructions, simulate, open" <|
                \_ ->
                    [ shortcut { key = "/", ctrl = False, shift = False, target = "BODY" }
                    , shortcut { key = "b", ctrl = False, shift = False, target = "BODY" }
                    , shortcut { key = "i", ctrl = False, shift = False, target = "BODY" }
                    , shortcut { key = "p", ctrl = False, shift = False, target = "BODY" }
                    , shortcut { key = "o", ctrl = True, shift = False, target = "BODY" }
                    ]
                        |> Expect.equal
                            [ Just Shortcuts.FocusSearch
                            , Just Shortcuts.ToggleBOM
                            , Just Shortcuts.OpenInstructions
                            , Just Shortcuts.Simulate
                            , Just Shortcuts.OpenFile
                            ]
            , test "[ makes the move step finer and ] coarser" <|
                \_ ->
                    ( shortcut { key = "[", ctrl = False, shift = False, target = "BODY" }
                    , shortcut { key = "]", ctrl = False, shift = False, target = "BODY" }
                    )
                        |> Expect.equal ( Just Shortcuts.NextGridStep, Just Shortcuts.CoarserGridStep )
            , test "Escape closes an open menu first" <|
                \_ ->
                    shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = True } "Escape"
                        |> Expect.equal (Just Shortcuts.CloseMenu)
            , test "shortcuts still work while a toolbar button has focus" <|
                \_ ->
                    -- Regression: clicking a button focused it, and the studio's
                    -- keys went dead until the canvas was clicked.
                    shortcut { key = "m", ctrl = False, shift = False, target = "BUTTON" }
                        |> Expect.equal (Just (Shortcuts.UseTool MoveTool))
            , test "arrows nudge a selection, Shift nudges finely" <|
                \_ ->
                    [ nudge True "ArrowRight" False
                    , nudge True "ArrowUp" True
                    , nudge True "PageDown" False
                    ]
                        |> Expect.equal
                            [ Just (Shortcuts.Nudge { direction = Shortcuts.Right, fine = False })
                            , Just (Shortcuts.Nudge { direction = Shortcuts.Away, fine = True })
                            , Just (Shortcuts.Nudge { direction = Shortcuts.Down, fine = False })
                            ]
            , test "without a selection the arrows stay with the camera" <|
                \_ ->
                    nudge False "ArrowLeft" False
                        |> Expect.equal Nothing
            , test "arrows inside the tree move focus, not parts" <|
                \_ ->
                    Shortcuts.nudgeFor { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
                        { key = "ArrowDown"
                        , shift = False
                        , ctrl = False
                        , meta = False
                        , alt = False
                        , repeat = False
                        , targetTag = "DIV"
                        , targetEditable = False
                        , targetRole = "treeitem"
                        }
                        |> Expect.equal Nothing
            , test "nudging up raises the selection one plate, and undo puts it back" <|
                \_ ->
                    let
                        heightOf model =
                            Document.getPlacement 0 model.document
                                |> Maybe.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))

                        raised =
                            loaded
                                |> step (SelectPlacement 0 False)
                                |> step (NudgeSelection (Shortcuts.Nudge { direction = Shortcuts.Up, fine = False }))
                    in
                    ( heightOf raised, heightOf (step Undo raised) )
                        |> Expect.equal ( Just -8, Just 0 )
            , test "the viewport keys are not taken" <|
                \_ ->
                    [ "w", "a", "s", "d", "f", "g", "1", "+", "?" ]
                        |> List.map (shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False })
                        |> List.all ((==) Nothing)
                        |> Expect.equal True
            ]
        , describe "submodels"
            [ test "grouping selected parts creates a submodel and undo restores the scene" <|
                \_ ->
                    let
                        grouped =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step GroupSelection

                        undone =
                            step Undo grouped
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ 2 ] (selected grouped)
                        , \_ -> Expect.equal [ "submodel_1.ldr", "3001.dat" ] (List.map .part (Document.mainPlacements grouped.document))
                        , \_ -> Expect.equal (Just [ 1, 2 ]) (Maybe.map (.placements >> List.map .source >> List.sort) grouped.viewport.instancedScene)
                        , \_ -> Expect.equal [ "3001.dat", "3001.dat" ] (List.map .part (Document.mainPlacements undone.document))
                        , \_ -> Expect.equal (Just [ 0, 1 ]) (Maybe.map (.placements >> List.map .source >> List.sort) undone.viewport.instancedScene)
                        ]
                        ()
            , test "entering a submodel opens its child bricks for 3D picking and ghosts parent bricks" <|
                \_ ->
                    let
                        ( subX, subY ) =
                            screenOf 0 loaded

                        entered =
                            loaded
                                |> click ( subX, subY )
                                |> step GroupSelection
                                |> step EnterSubmodel

                        clickedChild =
                            click ( subX, subY ) entered

                        clickedGhostedParent =
                            click (screenOf 1 loaded) entered
                    in
                    Expect.all
                        [ \_ -> Expect.equal "submodel_1.ldr" entered.context.activeSubmodel
                        , \_ -> Expect.equal [ 0 ] (selected clickedChild)
                        , \_ -> Expect.equal [] (selected clickedGhostedParent)
                        ]
                        ()
            , test "double-clicking a submodel reference enters it and ExitSubmodel returns to root" <|
                \_ ->
                    let
                        ( subX, subY ) =
                            screenOf 0 loaded

                        withSubmodel =
                            loaded
                                |> click ( subX, subY )
                                |> step GroupSelection
                                |> step ClearSelection

                        entered =
                            doubleClick subX subY withSubmodel

                        exited =
                            step ExitSubmodel entered
                    in
                    Expect.all
                        [ \_ -> Expect.equal "submodel_1.ldr" entered.context.activeSubmodel
                        , \_ -> Expect.equal 1 (List.length entered.context.path)
                        , \_ -> Expect.equal "main.ldr" exited.context.activeSubmodel
                        , \_ -> Expect.equal [] exited.context.path
                        , \_ -> Expect.equal Nothing exited.viewport.activeSources
                        ]
                        ()
            , test "selecting sibling submodels in the tree switches context without nesting paths" <|
                \_ ->
                    let
                        withTwoSubmodels =
                            loaded
                                |> step (SelectPlacement 0 False)
                                |> step GroupSelection
                                |> step (SelectPlacement 1 False)
                                |> step GroupSelection

                        inFirst =
                            step (SelectSubmodel "submodel_1.ldr") withTwoSubmodels

                        inSecond =
                            step (SelectSubmodel "submodel_2.ldr") inFirst
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ "submodel_1.ldr" ] (List.map .submodelName inFirst.context.path)
                        , \_ -> Expect.equal [ "submodel_2.ldr" ] (List.map .submodelName inSecond.context.path)
                        ]
                        ()
            , test "undoing submodel creation while inside the submodel returns to the root context" <|
                \_ ->
                    let
                        undoneFromInside =
                            loaded
                                |> step (SelectPlacement 0 False)
                                |> step GroupSelection
                                |> step EnterSubmodel
                                |> step Undo
                    in
                    Expect.all
                        [ \_ -> Expect.equal [] undoneFromInside.context.path
                        , \_ -> Expect.equal "" undoneFromInside.context.activeSubmodel
                        , \_ -> Expect.equal Nothing undoneFromInside.viewport.activeSources
                        ]
                        ()
            , test "exploding a submodel inlines its parts and is undoable" <|
                \_ ->
                    let
                        grouped =
                            loaded
                                |> step (SelectPlacement 0 False)
                                |> step GroupSelection

                        exploded =
                            step ExplodeSelection grouped

                        undone =
                            step Undo exploded
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ "3001.dat", "3001.dat" ] (List.map .part (Document.mainPlacements exploded.document))
                        , \_ -> Expect.equal [ "submodel_1.ldr", "3001.dat" ] (List.map .part (Document.mainPlacements undone.document))
                        ]
                        ()
            , test "arc gizmo inside a submodel respects LocalFrame vs WorldFrame orientations and centers" <|
                \_ ->
                    let
                        ( subX, subY ) =
                            screenOf 0 loaded

                        grouped =
                            loaded
                                |> click ( subX, subY )
                                |> step GroupSelection

                        -- Rotate submodel placement (id 2) by 90 degrees around Z
                        ( rotatedDoc, _ ) =
                            Edit.apply
                                (Edit.RotateAroundAxis [ 2 ]
                                    { pivot = vec3 0 0 0
                                    , axis = vec3 0 0 1
                                    , radians = degrees 90
                                    }
                                )
                                grouped.document

                        entered =
                            step EnterSubmodel { grouped | document = rotatedDoc }

                        -- Translate child part (id 0) inside submodel by (10, 0, 0)
                        ( movedDoc, _ ) =
                            Edit.apply (Edit.Translate [ 0 ] (vec3 10 0 0)) entered.document

                        selectedChild =
                            { entered | document = movedDoc }
                                |> step (SelectPlacement 0 False)
                                |> step (UseTool RotateTool)

                        localArcs =
                            Workspace.currentArcHandles selectedChild

                        worldFrameModel =
                            step ToggleCoordinateFrame selectedChild

                        worldArcs =
                            Workspace.currentArcHandles worldFrameModel

                        findKind kind list =
                            List.filter (\a -> a.axisKind == Just kind) list |> List.head
                    in
                    case ( findKind AxisX localArcs, findKind AxisY localArcs, findKind AxisX worldArcs ) of
                        ( Just lx, Just ly, Just wx ) ->
                            Expect.all
                                -- worldCenter: (10, 0, 0) rotated 90 deg around Z becomes (0, 10, 0)
                                [ \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX lx.center)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 10 (Vec3.getY lx.center)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getZ lx.center)

                                -- LocalFrame: local X axis (1, 0, 0) rotated 90 deg around Z is (0, 1, 0)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX lx.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 1 (Vec3.getY lx.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getZ lx.axis)

                                -- LocalFrame: local Y axis (0, 1, 0) rotated 90 deg around Z is (-1, 0, 0)
                                , \_ -> Expect.within (Expect.Absolute 0.01) -1 (Vec3.getX ly.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getY ly.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getZ ly.axis)

                                -- WorldFrame: center is still worldCenter (0, 10, 0)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX wx.center)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 10 (Vec3.getY wx.center)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getZ wx.center)

                                -- WorldFrame: X axis is world cardinal (1, 0, 0)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 1 (Vec3.getX wx.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getY wx.axis)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getZ wx.axis)
                                ]
                                ()

                        _ ->
                            Expect.fail "expected arc handles"
            , test "ArcDrag inside a rotated submodel correctly maps rotation to local space and undoes" <|
                \_ ->
                    let
                        ( subX, subY ) =
                            screenOf 0 loaded

                        grouped =
                            loaded
                                |> click ( subX, subY )
                                |> step GroupSelection

                        -- Rotate submodel reference by 90 degrees around Z
                        ( rotatedDoc, _ ) =
                            Edit.apply
                                (Edit.RotateAroundAxis [ 2 ]
                                    { pivot = vec3 0 0 0
                                    , axis = vec3 0 0 1
                                    , radians = degrees 90
                                    }
                                )
                                grouped.document

                        entered =
                            step EnterSubmodel { grouped | document = rotatedDoc }

                        ready =
                            entered
                                |> step (SelectPlacement 0 False)
                                |> step (UseTool RotateTool)

                        zArc =
                            Workspace.currentArcHandles ready
                                |> List.filter (\a -> a.axisKind == Just AxisZ)
                                |> List.head

                        dragged =
                            case zArc of
                                Just arc ->
                                    case arc.screenPoints of
                                        ( sx, sy ) :: _ ->
                                            let
                                                ( cx, cy ) =
                                                    arc.screenCenter

                                                ( ex, ey ) =
                                                    ( cx + (sy - cy), cy - (sx - cx) )
                                            in
                                            Just
                                                (ready
                                                    |> mouseDown sx sy False
                                                    |> mouseMove ex ey
                                                    |> mouseUp ex ey
                                                )

                                        [] ->
                                            Nothing

                                Nothing ->
                                    Nothing

                        euler m =
                            Document.getPlacement 0 m.document
                                |> Maybe.map (.matrix >> Transform.eulerDegrees)
                    in
                    case dragged of
                        Just m ->
                            Expect.all
                                [ \d -> Expect.notEqual (Just { x = 0, y = 0, z = 0 }) (euler d)
                                , \d -> Expect.equal (Just { x = 0, y = 0, z = 0 }) (euler (step Undo d))
                                , \d -> Expect.equal (euler d) (euler (step Redo (step Undo d)))
                                ]
                                m

                        Nothing ->
                            Expect.fail "expected rotation arc handle"
            , test "ArcDrag with altHeld rotates without angle snapping" <|
                \_ ->
                    let
                        ready =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step (UseTool RotateTool)
                                |> (\m -> { m | altHeld = True, rotateStep = 15 })

                        zArc =
                            Workspace.currentArcHandles ready
                                |> List.filter (\a -> a.axisKind == Just AxisZ)
                                |> List.head

                        dragged =
                            case zArc of
                                Just arc ->
                                    let
                                        ( cx, cy ) =
                                            arc.screenCenter

                                        r =
                                            50.0

                                        -- 3 degrees in radians (less than 15 / 2 = 7.5 deg snapping threshold)
                                        ang =
                                            degrees 3

                                        ( sx, sy ) =
                                            ( cx + r, cy )

                                        ( ex, ey ) =
                                            ( cx + r * cos ang, cy - r * sin ang )
                                    in
                                    Just
                                        (ready
                                            |> mouseDown sx sy False
                                            |> mouseMove ex ey
                                            |> mouseUp ex ey
                                        )

                                Nothing ->
                                    Nothing

                        euler m =
                            Document.getPlacement 0 m.document
                                |> Maybe.map (.matrix >> Transform.eulerDegrees)
                    in
                    case dragged of
                        Just m ->
                            Expect.all
                                [ \d -> Expect.notEqual (Just { x = 0, y = 0, z = 0 }) (euler d)
                                ]
                                m

                        Nothing ->
                            Expect.fail "expected rotation arc handle"
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
    Shortcuts.actionFor { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
        { key = key
        , shift = shift
        , ctrl = ctrl
        , meta = False
        , alt = False
        , repeat = False
        , targetTag = target
        , targetEditable = False
        , targetRole = ""
        }


nudge : Bool -> String -> Bool -> Maybe Shortcuts.Nudge
nudge hasSelection key shift =
    Shortcuts.nudgeFor { hasSelection = hasSelection, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
        { key = key
        , shift = shift
        , ctrl = False
        , meta = False
        , alt = False
        , repeat = False
        , targetTag = "DIV"
        , targetEditable = False
        , targetRole = "application"
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
        , targetRole = ""
        }



-- ── Palette, colours and the inspector ────────────────────────────────────────


catalogue : PartIndex.PartIndex
catalogue =
    { parts =
        [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
        , PartIndex.makePart "3023.dat" "Plate 1 x 2" "Plate" ""
        ]
    , categories = [ ( "Brick", 1 ), ( "Plate", 1 ) ]
    }


{-| An empty workspace whose catalogue has arrived.
-}
withCatalogue : Workspace.Model
withCatalogue =
    start |> step (PartIndexLoaded (Ok catalogue))


part3001 : PartIndex.Part
part3001 =
    PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""


panelSuite : Test
panelSuite =
    describe "palette, colours and inspector"
        [ describe "palette"
            [ test "starts waiting for its catalogue" <|
                \_ ->
                    case start.palette.index of
                        Palette.Loading ->
                            Expect.pass

                        _ ->
                            Expect.fail "expected Loading"
            , test "the catalogue arriving fills the palette" <|
                \_ ->
                    case withCatalogue.palette.index of
                        Palette.Loaded index ->
                            Expect.equal 2 (List.length index.parts)

                        _ ->
                            Expect.fail "expected Loaded"
            , test "a missing catalogue is reported, not fatal" <|
                \_ ->
                    case (step (PartIndexLoaded (Err (Http.BadStatus 404))) start).palette.index of
                        Palette.Failed reason ->
                            Expect.equal "not found" reason

                        _ ->
                            Expect.fail "expected Failed"
            , test "searching and picking a category are remembered" <|
                \_ ->
                    withCatalogue
                        |> step (PaletteQuery "brick")
                        |> step (PaletteCategory (Just "Brick"))
                        |> .palette
                        |> (\p -> ( p.query, p.category ))
                        |> Expect.equal ( "brick", Just "Brick" )
            , test "picking a part previews it" <|
                \_ ->
                    let
                        picked =
                            withCatalogue |> step (ArmPart part3001)
                    in
                    Expect.all
                        [ \m -> Expect.equal (Just "3001.dat") (Maybe.map .file m.palette.armed)
                        , \m -> Expect.equal Viewport.ResolvingParts m.preview.loadPhase
                        ]
                        picked
            , test "adding the picked part puts it at the middle of the view, selected" <|
                \_ ->
                    let
                        added =
                            withCatalogue |> step (ArmPart part3001) |> step AddArmedPart
                    in
                    Expect.all
                        [ \m -> Expect.equal [ "3001.dat" ] (List.map .fileText (Document.placements m.document))
                        , \m -> Expect.equal [ 0 ] (selected m)
                        , \m -> Expect.equal (Just 0) (positionOf 0 m)
                        ]
                        added
            , test "an added part takes the active colour" <|
                \_ ->
                    withCatalogue
                        |> step (PickColour 14)
                        |> step (ArmPart part3001)
                        |> step AddArmedPart
                        |> .document
                        |> Document.placements
                        |> List.map .color
                        |> Expect.equal [ 14 ]
            , test "adding is undoable" <|
                \_ ->
                    withCatalogue
                        |> step (ArmPart part3001)
                        |> step AddArmedPart
                        |> step Undo
                        |> .document
                        |> Document.placements
                        |> Expect.equal []
            , test "adding with nothing picked does nothing" <|
                \_ ->
                    step AddArmedPart withCatalogue
                        |> .document
                        |> Document.placements
                        |> Expect.equal []
            , test "a typed part number is added without the catalogue" <|
                \_ ->
                    start
                        |> step (ManualPart " 3001 ")
                        |> step AddManualPart
                        |> .document
                        |> Document.placements
                        |> List.map .part
                        |> Expect.equal [ "3001.dat" ]
            , test "a typed part number with odd characters is refused" <|
                \_ ->
                    start
                        |> step (ManualPart "../../etc/passwd")
                        |> step AddManualPart
                        |> .document
                        |> Document.placements
                        |> Expect.equal []
            , test "a new part lands on the grid" <|
                \_ ->
                    let
                        camera =
                            start.viewport.camera

                        moved =
                            { start | viewport = (\v -> { v | camera = { camera | target = vec3 13 -11 -31 } }) start.viewport }
                    in
                    moved
                        |> step (ManualPart "3001")
                        |> step AddManualPart
                        |> .document
                        |> Document.placements
                        |> List.head
                        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0) |> Vec3.toRecord)
                        |> Expect.equal (Just { x = 20, y = 20, z = 40 })
            , test "toggling a favorite adds and removes it" <|
                \_ ->
                    let
                        starred =
                            withCatalogue |> step (PaletteToggleFavorite "3001.dat")
                    in
                    Expect.all
                        [ \m -> Expect.equal (Set.singleton "3001.dat") m.palette.favorites
                        , \m -> Expect.equal Set.empty (step (PaletteToggleFavorite "3001.dat") m).palette.favorites
                        ]
                        starred
            , test "adding parts records them in recentParts most-recent-first without duplicates" <|
                \_ ->
                    withCatalogue
                        |> step (ManualPart "3001.dat")
                        |> step AddManualPart
                        |> step (ManualPart "3023.dat")
                        |> step AddManualPart
                        |> step (ManualPart "3001.dat")
                        |> step AddManualPart
                        |> .palette
                        |> .recentParts
                        |> Expect.equal [ "3001.dat", "3023.dat" ]
            , test "inspecting a part arms it for the 3D preview and rotates the turntable" <|
                \_ ->
                    let
                        inspected =
                            withCatalogue
                                |> step (PaletteInspect (Just part3001))
                                |> step (PaletteRotateTurntable 120)
                    in
                    Expect.all
                        [ \m -> Expect.equal (Just part3001) m.palette.inspectingPart
                        , \m -> Expect.equal (Just part3001) m.palette.armed
                        , \m -> Expect.within (Expect.Absolute 0.001) (degrees 120) m.preview.camera.azimuth
                        ]
                        inspected
            , test "thumbnailLoaded stores thumbnails matching the active colour and ignores stale colours" <|
                \_ ->
                    withCatalogue
                        |> step (ThumbnailLoaded { file = "3001.dat", color = 4, url = "data:image/png;base64,red" })
                        |> step (ThumbnailLoaded { file = "3023.dat", color = 1, url = "data:image/png;base64,blue" })
                        |> .thumbnails
                        |> Expect.equal (Dict.singleton "3001.dat" "data:image/png;base64,red")
            ]
        , describe "colours"
            [ test "picking a colour with a selection recolours it, as one undo step" <|
                \_ ->
                    let
                        painted =
                            loaded |> click (screenOf 0 loaded) |> step (PickColour 1)
                    in
                    Expect.all
                        [ \m -> Expect.equal [ 1, 14 ] (List.map .color (Document.placements m.document))
                        , \m -> Expect.equal 1 m.activeColour
                        , \m -> Expect.equal [ 4, 14 ] (List.map .color (Document.placements (step Undo m).document))
                        ]
                        painted
            , test "picking a colour with nothing selected only changes the active colour" <|
                \_ ->
                    loaded
                        |> step (PickColour 2)
                        |> Expect.all
                            [ \m -> Expect.equal [ 4, 14 ] (List.map .color (Document.placements m.document))
                            , \m -> Expect.equal 2 m.activeColour
                            , \m -> Expect.equal False (Edit.canUndo m.history)
                            ]
            ]
        , describe "inspector"
            [ test "typing a position moves the selection there" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> step (SetPosition AxisX 40)
                        |> positionOf 0
                        |> Expect.equal (Just 40)
            , test "moving several parts keeps them together" <|
                \_ ->
                    -- Their centre is x = 150; putting it at 200 moves both by 50.
                    loaded
                        |> step SelectAll
                        |> step (SetPosition AxisX 200)
                        |> (\m -> ( positionOf 0 m, positionOf 1 m ))
                        |> Expect.equal ( Just 50, Just 350 )
            , test "typing a position is one undo step" <|
                \_ ->
                    loaded
                        |> click (screenOf 0 loaded)
                        |> step (SetPosition AxisX 40)
                        |> step Undo
                        |> positionOf 0
                        |> Expect.equal (Just 0)
            , test "typing an angle turns a single part" <|
                \_ ->
                    let
                        turned =
                            loaded |> click (screenOf 0 loaded) |> step (SetRotation AxisY 90)

                        angle m =
                            Document.getPlacement 0 m.document
                                |> Maybe.map (.matrix >> Transform.eulerDegrees)
                    in
                    Expect.all
                        [ \m -> Expect.equal (Just { x = 0, y = 90, z = 0 }) (angle m)
                        , \m -> Expect.equal (Just { x = 0, y = 0, z = 0 }) (angle (step Undo m))
                        ]
                        turned
            , test "typing an angle with several parts selected does nothing" <|
                \_ ->
                    loaded
                        |> step SelectAll
                        |> step (SetRotation AxisY 90)
                        |> .history
                        |> Edit.canUndo
                        |> Expect.equal False
            ]
        , describe "tree"
            [ test "choosing a row selects that part; Shift adds to the selection" <|
                \_ ->
                    Expect.all
                        [ \m -> Expect.equal [ 1 ] (selected (step (SelectPlacement 1 False) m))
                        , \m -> Expect.equal [ 0, 1 ] (selected (step (SelectPlacement 1 True) (step (SelectPlacement 0 False) m)))
                        , \m -> Expect.equal [ 0 ] (selected (step (SelectPlacement 1 True) (step (SelectPlacement 0 True) (step (SelectPlacement 1 False) m))))
                        ]
                        loaded
            ]
        , describe "gear hints"
            [ test "hints are computed once editing pauses, not on every edit" <|
                \_ ->
                    let
                        edited =
                            start
                                |> step (FileTextReceived "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n")
                    in
                    Expect.all
                        [ \m -> Expect.equal 0 m.hints.gears
                        , \m -> Expect.equal 1 (step (HintsDue m.hintGeneration) m).hints.gears
                        ]
                        edited
            , test "a timer from an earlier edit is ignored" <|
                \_ ->
                    start
                        |> step (FileTextReceived "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n")
                        |> step (HintsDue 0)
                        |> .hints
                        |> .gears
                        |> Expect.equal 0
            ]
        , describe "layout"
            [ test "a wide window shrinks the canvas to the space between the panes, below the header" <|
                \_ ->
                    step (WindowSized 1400 900) loaded
                        |> Expect.all
                            [ \m -> Expect.equal Layout.Wide m.layout.mode
                            , \m -> Expect.equal 800 m.viewport.width
                            , \m -> Expect.equal (900 - Layout.headerHeight Layout.Wide) m.viewport.height
                            ]
            , test "a phone gets a full-width canvas between the header and the tab bar" <|
                \_ ->
                    step (WindowSized 390 780) loaded
                        |> Expect.all
                            [ \m -> Expect.equal Layout.Narrow m.layout.mode
                            , \m -> Expect.equal 390 m.viewport.width
                            , \m -> Expect.equal (780 - Layout.tabBarHeight - Layout.headerHeight Layout.Narrow) m.viewport.height
                            ]
            , test "a panel opens and closes" <|
                \_ ->
                    Expect.all
                        [ \m -> Expect.equal (Just Layout.PartsPanel) (step (OpenPanel (Just Layout.PartsPanel)) m).panel
                        , \m -> Expect.equal Nothing (step (OpenPanel Nothing) (step (OpenPanel (Just Layout.PartsPanel)) m)).panel
                        ]
                        loaded
            ]
        , describe "BOM & marketplace integration"
            [ test "ToggleBOM opens and CloseBOM closes the BOM panel" <|
                \_ ->
                    let
                        opened =
                            step ToggleBOM loaded

                        closed =
                            step CloseBOM opened
                    in
                    Expect.all
                        [ \_ -> Expect.equal True opened.bomOpen
                        , \_ -> Expect.equal False closed.bomOpen
                        ]
                        ()
            , test "BOMSelectLot selects matching placements in the viewport" <|
                \_ ->
                    let
                        lotSelected =
                            step (BOMSelectLot "3001" 4) loaded
                    in
                    Expect.equal (Set.singleton 0) lotSelected.selection
            , test "BOMSelectLot selects parent submodel placements for parts nested in MPD submodels" <|
                \_ ->
                    let
                        mpdFile =
                            "0 FILE main.ldr\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 wheel.ldr\n1 1 40 0 0 1 0 0 0 1 0 0 0 1 wheel.ldr\n0 FILE wheel.ldr\n1 0 0 0 0 1 0 0 0 1 0 0 0 1 3023.dat\n"

                        mpdLoaded =
                            start
                                |> step (FileTextReceived mpdFile)
                                |> step (BOMSelectLot "3023" 0)
                    in
                    Expect.equal (Set.fromList [ 0, 1 ]) mpdLoaded.selection
            , test "BOMSort toggles column and order" <|
                \_ ->
                    let
                        sorted =
                            step (BOMSort BOM.SortName) loaded
                    in
                    Expect.all
                        [ \m -> Expect.equal BOM.SortName m.bomState.sortColumn
                        , \m -> Expect.equal False m.bomState.sortAscending
                        ]
                        sorted
            ]
        , describe "rendering quality and exporters"
            [ test "toggling render quality switches between Draft and Realistic and updates viewport style" <|
                \_ ->
                    let
                        realistic =
                            step ToggleRenderQuality loaded

                        backToDraft =
                            step ToggleRenderQuality realistic
                    in
                    Expect.all
                        [ \_ -> Expect.equal Style.Realistic realistic.renderQuality
                        , \_ -> Expect.greaterThan 0.1 realistic.viewport.renderStyle.bevelStrength
                        , \_ -> Expect.greaterThan 0.1 realistic.viewport.renderStyle.studEmbossStrength
                        , \_ -> Expect.equal Style.Draft backToDraft.renderQuality
                        , \_ -> Expect.within (Expect.Absolute 0.0001) 0 backToDraft.viewport.renderStyle.bevelStrength
                        ]
                        ()
            , test "export menu opens and closes when selecting an export format or snapshot" <|
                \_ ->
                    let
                        opened =
                            step ToggleExportMenu loaded
                    in
                    Expect.all
                        [ \_ -> Expect.equal True opened.exportMenuOpen
                        , \_ -> Expect.equal False (step (ExportFormat Export.GlTFBinary) opened).exportMenuOpen
                        , \_ -> Expect.equal False (step (ExportFormat Export.BlenderScript) opened).exportMenuOpen
                        , \_ -> Expect.equal False (step (ExportFormat Export.UsdzArchive) opened).exportMenuOpen
                        , \_ -> Expect.equal False (step CaptureSnapshot opened).exportMenuOpen
                        ]
                        ()
            ]
        ]


snappingSuite : Test
snappingSuite =
    describe "snapping and collision detection"
        [ test "cycling snap distance cycles through predefined steps" <|
            \_ ->
                let
                    s0 =
                        start.snapDistance

                    s1 =
                        (step NextSnapDistance start).snapDistance

                    s2 =
                        (start |> step NextSnapDistance |> step NextSnapDistance).snapDistance

                    s3 =
                        (start |> step NextSnapDistance |> step NextSnapDistance |> step NextSnapDistance).snapDistance

                    sBack =
                        start
                            |> step NextSnapDistance
                            |> step NextSnapDistance
                            |> step NextSnapDistance
                            |> step NextSnapDistance
                in
                Expect.all
                    [ \_ -> Expect.equal 12.0 s0
                    , \_ -> Expect.equal 20.0 s1
                    , \_ -> Expect.equal 8.0 s2
                    , \_ -> Expect.equal 0.0 s3
                    , \_ -> Expect.equal 12.0 sBack.snapDistance
                    ]
                    ()
        , test "cycling collision mode cycles Strict -> Soft -> Off -> Strict" <|
            \_ ->
                let
                    m0 =
                        start

                    m1 =
                        step NextCollisionMode m0

                    m2 =
                        step NextCollisionMode m1

                    m3 =
                        step NextCollisionMode m2
                in
                Expect.all
                    [ \_ -> Expect.equal CollisionStrict m0.collisionMode
                    , \_ -> Expect.equal CollisionSoft m1.collisionMode
                    , \_ -> Expect.equal CollisionOff m2.collisionMode
                    , \_ -> Expect.equal CollisionStrict m3.collisionMode
                    ]
                    ()
        , test "Alt key press and release toggles altHeld" <|
            \_ ->
                let
                    dummyKey key alt =
                        { key = key
                        , shift = False
                        , ctrl = False
                        , meta = False
                        , alt = alt
                        , repeat = False
                        , targetTag = "BODY"
                        , targetEditable = False
                        , targetRole = ""
                        }

                    mAltPressed =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "Alt" True))) start

                    mAltReleased =
                        step (KeyUp "Alt") mAltPressed
                in
                Expect.all
                    [ \_ -> Expect.equal True mAltPressed.altHeld
                    , \_ -> Expect.equal False mAltReleased.altHeld
                    ]
                    ()
        , test "Arrow and 'r' keys cycle snapRotationStep when snapping during drag" <|
            \_ ->
                let
                    dummyKey key alt =
                        { key = key
                        , shift = False
                        , ctrl = False
                        , meta = False
                        , alt = alt
                        , repeat = False
                        , targetTag = "BODY"
                        , targetEditable = False
                        , targetRole = ""
                        }

                    dummyPoint =
                        vec3 0 0 0

                    dummyDir =
                        vec3 0 1 0

                    dummyGuide =
                        Connectivity.StudRingGuide dummyPoint dummyDir False

                    dummyCandidate =
                        { draggedPoint =
                            { id = 0
                            , connType = Connectivity.AntiStud
                            , localPosition = dummyPoint
                            , normal = dummyDir
                            , tangent = vec3 1 0 0
                            }
                        , targetPoint =
                            { placementId = 1
                            , pointId = 0
                            , connType = Connectivity.Stud
                            , worldPosition = dummyPoint
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }
                        , targetPlacementId = 1
                        , targetWorldPosition = dummyPoint
                        , targetWorldNormal = vec3 0 -1 0
                        , snapMatrix = Mat4.identity
                        , distance = 2.0
                        , guide = dummyGuide
                        }

                    readyWithSnap =
                        { start
                            | drag =
                                Just
                                    { kind =
                                        Gizmo.LinearDrag
                                            { axis = AxisX
                                            , origin = dummyPoint
                                            , tip = dummyPoint
                                            , length = 50
                                            , screenStart = ( 100, 100 )
                                            , screenEnd = ( 150, 100 )
                                            }
                                    , ids = []
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 100, 100 )
                                    , original = start.document
                                    , amount = 0
                                    }
                            , activeSnap = Just dummyCandidate
                            , snapRotationStep = 0
                        }

                    stepRight =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "ArrowRight" False))) readyWithSnap

                    stepR =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "r" False))) stepRight

                    stepLeft =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "ArrowLeft" False))) stepR
                in
                Expect.all
                    [ \_ -> Expect.equal 1 stepRight.snapRotationStep
                    , \_ -> Expect.equal 2 stepR.snapRotationStep
                    , \_ -> Expect.equal 1 stepLeft.snapRotationStep
                    ]
                    ()
        , test "CollisionStrict mode reverts document to pre-drag state on endDrag when colliding" <|
            \_ ->
                let
                    origDoc =
                        start.document

                    vp =
                        start.viewport

                    modelWithCollidingDrag =
                        { start
                            | viewport = { vp | captured = True, mousePressed = True }
                            , collisionMode = CollisionStrict
                            , collidingPlacements = Set.fromList [ 1, 2 ]
                            , drag =
                                Just
                                    { kind =
                                        Gizmo.LinearDrag
                                            { axis = AxisX
                                            , origin = vec3 0 0 0
                                            , tip = vec3 0 0 0
                                            , length = 50
                                            , screenStart = ( 100, 100 )
                                            , screenEnd = ( 150, 100 )
                                            }
                                    , ids = [ 1 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 150, 100 )
                                    , original = origDoc
                                    , amount = 50
                                    }
                        }

                    afterEndDrag =
                        mouseUp 150 100 modelWithCollidingDrag
                in
                Expect.all
                    [ \m -> Expect.equal Nothing m.drag
                    , \m -> Expect.equal origDoc m.document
                    , \m -> Expect.equal Set.empty m.collidingPlacements
                    ]
                    afterEndDrag
        , test "CollisionSoft mode allows drop and retains collidingPlacements on endDrag" <|
            \_ ->
                let
                    ( docWithTwo, _ ) =
                        Edit.apply
                            (Edit.Add
                                [ { part = "3001.dat", color = 4, matrix = Mat4.identity }
                                , { part = "3001.dat", color = 14, matrix = Mat4.makeTranslate3 20 0 0 }
                                ]
                            )
                            start.document

                    ( movedDoc, _ ) =
                        Edit.apply
                            (Edit.Translate [ 0 ] (vec3 10 0 0))
                            docWithTwo

                    vp =
                        start.viewport

                    modelWithSoftCollision =
                        { start
                            | viewport = { vp | captured = True, mousePressed = True }
                            , document = movedDoc
                            , collisionMode = CollisionSoft
                            , collidingPlacements = Set.fromList [ 0, 1 ]
                            , drag =
                                Just
                                    { kind =
                                        Gizmo.LinearDrag
                                            { axis = AxisX
                                            , origin = vec3 0 0 0
                                            , tip = vec3 0 0 0
                                            , length = 50
                                            , screenStart = ( 100, 100 )
                                            , screenEnd = ( 150, 100 )
                                            }
                                    , ids = [ 0 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 150, 100 )
                                    , original = docWithTwo
                                    , amount = 10
                                    }
                        }

                    afterEndDrag =
                        mouseUp 150 100 modelWithSoftCollision
                in
                Expect.all
                    [ \m -> Expect.equal Nothing m.drag
                    , \m -> Expect.equal movedDoc m.document
                    , \m -> Expect.equal (Set.fromList [ 0, 1 ]) m.collidingPlacements
                    ]
                    afterEndDrag
        , test "Alt key press during drag immediately moves drag to non-snapped state" <|
            \_ ->
                let
                    dummyKey key alt =
                        { key = key
                        , shift = False
                        , ctrl = False
                        , meta = False
                        , alt = alt
                        , repeat = False
                        , targetTag = "BODY"
                        , targetEditable = False
                        , targetRole = ""
                        }

                    modelDuringDrag =
                        { start
                            | drag =
                                Just
                                    { kind =
                                        Gizmo.LinearDrag
                                            { axis = AxisX
                                            , origin = vec3 0 0 0
                                            , tip = vec3 0 0 0
                                            , length = 50
                                            , screenStart = ( 100, 100 )
                                            , screenEnd = ( 150, 100 )
                                            }
                                    , ids = []
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 120, 100 )
                                    , original = start.document
                                    , amount = 20
                                    }
                            , altHeld = False
                        }

                    mAltPressed =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "Alt" True))) modelDuringDrag

                    mAltReleased =
                        step (KeyUp "Alt") mAltPressed
                in
                Expect.all
                    [ \_ -> Expect.equal True mAltPressed.altHeld
                    , \_ -> Expect.equal False mAltReleased.altHeld
                    ]
                    ()
        ]
