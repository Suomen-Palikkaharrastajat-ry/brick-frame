module WorkspaceTest exposing (suite)

{-| Tests for the studio's workspace page: loading documents into the viewport
and what the page does with the viewport's events.
-}

import Data
import Dict
import Expect
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Types exposing (GearInstance)
import Html.Attributes as HtmlAttr
import Http
import LDraw.Connectivity as Connectivity exposing (CollisionMode(..))
import LDraw.Document as Document
import LDraw.Export as Export
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera
import Render.Style as Style
import Set
import Studio.BOM as BOM
import Studio.DragPlane as DragPlane
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo
import Studio.Layout as Layout
import Studio.Palette as Palette
import Studio.PartIndex as PartIndex
import Studio.Shortcuts as Shortcuts exposing (Tool(..))
import Studio.Transform as Transform
import Studio.Workspace as Workspace exposing (CoordinateFrame(..), Msg(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Viewport
import Viewport.Shortcuts as ViewportShortcuts


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
        , threeAxisMovementSuite
        , viewDragSuite
        , commitSuite
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
    step (ViewportMsg (Viewport.MouseButtonDown (x + left) (y + top) 0 shift False)) model


{-| Press the right button at a point: a right drag orbits the camera.
-}
rightMouseDown : Float -> Float -> Workspace.Model -> Workspace.Model
rightMouseDown x y model =
    let
        ( left, top ) =
            model.viewport.canvasOrigin
    in
    step (ViewportMsg (Viewport.MouseButtonDown (x + left) (y + top) 2 False False)) model


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
            , test "shift-dragging a part moves it finely, on a 1 LDU grid, and keeps the camera" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        fine =
                            ready
                                |> mouseDown x y True
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)

                        coarse =
                            ready
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)
                    in
                    Expect.all
                        [ \m -> Expect.notEqual (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.notEqual (groundOf 0 coarse) (groundOf 0 m)
                        , \m -> Expect.equal ready.viewport.camera.target m.viewport.camera.target
                        ]
                        fine
            , test "shift-dragging empty space pans the camera and moves no part" <|
                \_ ->
                    let
                        panned =
                            loaded
                                |> step (UseTool MoveTool)
                                |> mouseDown 5 5 True
                                |> mouseMove 65 25
                                |> mouseUp 65 25
                    in
                    Expect.all
                        [ \m -> Expect.equal loaded.document m.document
                        , \m -> Expect.notEqual loaded.viewport.camera.target m.viewport.camera.target
                        ]
                        panned
            , test "left-dragging empty space pans the camera, not orbits it" <|
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
                        [ \m -> Expect.within (Expect.Absolute 1.0e-9) ready.viewport.camera.azimuth m.viewport.camera.azimuth
                        , \m -> Expect.notEqual ready.viewport.camera.target m.viewport.camera.target
                        , \m -> Expect.equal (Just 0) (positionOf 0 m)
                        ]
                        moved
            , test "right-dragging empty space orbits the camera" <|
                \_ ->
                    let
                        ready =
                            loaded |> click (screenOf 0 loaded) |> step (UseTool MoveTool)

                        moved =
                            ready
                                |> rightMouseDown 100 500
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
            , test "typing in an input or textarea ignores Ctrl chords so browser text editing works" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (shortcut { key = "a", ctrl = True, shift = False, target = "INPUT" })
                        , \_ -> Expect.equal Nothing (shortcut { key = "z", ctrl = True, shift = False, target = "INPUT" })
                        , \_ -> Expect.equal Nothing (shortcut { key = "y", ctrl = True, shift = False, target = "INPUT" })
                        , \_ -> Expect.equal Nothing (shortcut { key = "d", ctrl = True, shift = False, target = "INPUT" })
                        , \_ -> Expect.equal Nothing (shortcut { key = "a", ctrl = True, shift = False, target = "TEXTAREA" })
                        , \_ -> Expect.equal Nothing (shortcut { key = "z", ctrl = True, shift = False, target = "TEXTAREA" })
                        ]
                        ()
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
            , test "WASD, Q/E and the Page keys move a selection, and Shift makes the move fine" <|
                \_ ->
                    [ editKey True "d" False
                    , editKey True "w" True
                    , editKey True "PageDown" False
                    , editKey True "e" False
                    ]
                        |> Expect.equal
                            [ Just (Shortcuts.MoveBy (Shortcuts.Nudge { direction = Shortcuts.Right, fine = False }))
                            , Just (Shortcuts.MoveBy (Shortcuts.Nudge { direction = Shortcuts.Away, fine = True }))
                            , Just (Shortcuts.MoveBy (Shortcuts.Nudge { direction = Shortcuts.Down, fine = False }))
                            , Just (Shortcuts.MoveBy (Shortcuts.Nudge { direction = Shortcuts.Up, fine = False }))
                            ]
            , test "the arrows turn a selection, and Shift makes the turn 45°" <|
                \_ ->
                    [ editKey True "ArrowRight" False
                    , editKey True "ArrowLeft" True
                    , editKey True "ArrowUp" False
                    , editKey True "ArrowDown" True
                    ]
                        |> Expect.equal
                            [ Just (Shortcuts.RotateBy { direction = Shortcuts.TurnRight, fortyFive = False })
                            , Just (Shortcuts.RotateBy { direction = Shortcuts.TurnLeft, fortyFive = True })
                            , Just (Shortcuts.RotateBy { direction = Shortcuts.TipForward, fortyFive = False })
                            , Just (Shortcuts.RotateBy { direction = Shortcuts.TipBackward, fortyFive = True })
                            ]
            , test "without a selection the arrows and WASD stay with the camera" <|
                \_ ->
                    [ editKey False "ArrowLeft" False, editKey False "d" False ]
                        |> Expect.equal [ Nothing, Nothing ]
            , test "arrows inside the tree move focus, not parts" <|
                \_ ->
                    Shortcuts.editKeyFor { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
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
            , test "Alt, Ctrl and Meta chords are left to the browser, not taken as edits" <|
                \_ ->
                    let
                        chord key =
                            Shortcuts.editKeyFor { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
                                { key = key
                                , shift = False
                                , ctrl = False
                                , meta = False
                                , alt = True
                                , repeat = False
                                , targetTag = "DIV"
                                , targetEditable = False
                                , targetRole = "application"
                                }
                    in
                    List.map chord [ "ArrowUp", "ArrowLeft", "d" ]
                        |> Expect.equal [ Nothing, Nothing, Nothing ]
            , test "the viewport keys are not taken" <|
                \_ ->
                    [ "w", "a", "s", "d", "f", "g", "1", "+", "?" ]
                        |> List.map (shortcutIn { hasSelection = True, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False })
                        |> List.all ((==) Nothing)
                        |> Expect.equal True
            ]
        , describe "editing keys"
            [ test "with a selection, the arrows turn it and the camera stays put" <|
                \_ ->
                    let
                        ready =
                            loaded |> step SelectAll

                        turned =
                            step (ViewportMsg (Viewport.KeyPressed (pressed "ArrowRight" False))) ready
                    in
                    Expect.all
                        [ \m -> Expect.within (Expect.Absolute 0.01) 150 (Maybe.withDefault 0 (positionOf 0 m))
                        , \m -> Expect.equal (cameraView ready) (cameraView m)
                        , \m -> Expect.equal False (Set.member "ArrowRight" m.viewport.heldKeys)
                        ]
                        turned
            , test "with a selection, D moves it and the camera stays put" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (SelectPlacement 0 False)

                        moved =
                            step (ViewportMsg (Viewport.KeyPressed (pressed "d" False))) ready
                    in
                    Expect.all
                        [ \m -> Expect.notEqual (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.equal (cameraView ready) (cameraView m)
                        ]
                        moved
            , test "Escape during a drag puts the parts back and records no undo step" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        dragging =
                            ready
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)

                        cancelled =
                            dragging
                                |> step (ViewportMsg (Viewport.KeyPressed (pressed "Escape" False)))
                                |> mouseUp (x + 60) (y + 20)
                    in
                    Expect.all
                        [ \_ -> Expect.notEqual (groundOf 0 ready) (groundOf 0 dragging)
                        , \m -> Expect.equal (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.equal True (m.drag == Nothing)
                        , \m -> Expect.equal Edit.emptyHistory m.history
                        ]
                        cancelled
            , test "a right press during a drag restores the parts and records no undo step" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( x, y ) =
                            screenOf 0 ready

                        ( left, top ) =
                            ready.viewport.canvasOrigin

                        dragging =
                            ready
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)

                        cancelled =
                            step (ViewportMsg (Viewport.MouseButtonDown (x + left) (y + top) 2 False False)) dragging
                    in
                    Expect.all
                        [ \_ -> Expect.notEqual (groundOf 0 ready) (groundOf 0 dragging)
                        , \m -> Expect.equal (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.equal True (m.drag == Nothing)
                        , \m -> Expect.equal Edit.emptyHistory m.history
                        ]
                        cancelled
            , test "with the help dialog open, Delete, D and the arrows leave the document alone" <|
                \_ ->
                    let
                        ready =
                            loaded |> step SelectAll |> step ToggleHelp

                        pressedAll =
                            [ "Delete", "Backspace", "d", "ArrowRight" ]
                                |> List.foldl (\key m -> step (ViewportMsg (Viewport.KeyPressed (pressed key False))) m) ready
                    in
                    Expect.all
                        [ \m -> Expect.equal ready.document m.document
                        , \m -> Expect.equal Edit.emptyHistory m.history
                        , \m -> Expect.equal True m.helpOpen
                        ]
                        pressedAll
            , test "with the help dialog open, Ctrl Z does not undo" <|
                \_ ->
                    let
                        edited =
                            loaded |> step SelectAll |> step DeleteSelection

                        ready =
                            step ToggleHelp edited

                        undoKey =
                            pressed "z" False

                        afterUndo =
                            step (ViewportMsg (Viewport.KeyPressed { undoKey | ctrl = True })) ready
                    in
                    Expect.equal ready.document afterUndo.document
            , test "Delete during a drag is ignored and records no removal" <|
                \_ ->
                    let
                        ( x, y ) =
                            screenOf 0 loaded

                        dropped =
                            loaded
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)
                                |> step (ViewportMsg (Viewport.KeyPressed (pressed "Delete" False)))
                                |> step (ViewportMsg (Viewport.KeyPressed (pressed "Backspace" False)))
                                |> mouseUp (x + 60) (y + 20)

                        undone =
                            step Undo dropped
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length (Document.placements m.document))
                        , \m -> Expect.notEqual (groundOf 0 loaded) (groundOf 0 m)
                        , \_ -> Expect.equal (groundOf 0 loaded) (groundOf 0 undone)
                        , \_ -> Expect.equal False (Edit.canUndo undone.history)
                        ]
                        dropped
            , test "with the select tool, dragging a part moves it and the camera stays put" <|
                \_ ->
                    let
                        ( x, y ) =
                            screenOf 0 loaded

                        dragged =
                            loaded
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)
                    in
                    Expect.all
                        [ \m -> Expect.equal SelectTool m.tool
                        , \m -> Expect.notEqual (groundOf 0 loaded) (groundOf 0 m)
                        , \m -> Expect.equal (groundOf 1 loaded) (groundOf 1 m)
                        , \m -> Expect.equal (cameraView loaded) (cameraView m)
                        , \m -> Expect.equal [ 0 ] (selected m)
                        , \m -> Expect.equal True (Edit.canUndo m.history)
                        ]
                        dragged
            , test "with the select tool, right-dragging empty space orbits and moves no part" <|
                \_ ->
                    let
                        orbited =
                            loaded
                                |> rightMouseDown 5 5
                                |> mouseMove 65 25
                                |> mouseUp 65 25
                    in
                    Expect.all
                        [ \m -> Expect.equal loaded.document m.document
                        , \m -> Expect.notEqual (cameraView loaded) (cameraView m)
                        ]
                        orbited
            , test "with the select tool, a click on a part of a wider selection selects it alone" <|
                \_ ->
                    loaded
                        |> step SelectAll
                        |> click (screenOf 0 loaded)
                        |> selected
                        |> Expect.equal [ 0 ]
            , test "a click with an armed part places it, as a tap does" <|
                \_ ->
                    let
                        placed =
                            loaded
                                |> step (ArmPart part3001)
                                |> click ( 5, 5 )
                    in
                    Expect.equal 3 (List.length (Document.placements placed.document))
            , test "Space + drag pans the camera and leaves the parts where they are" <|
                \_ ->
                    let
                        ready =
                            loaded

                        ( x, y ) =
                            screenOf 0 ready

                        panned =
                            ready
                                |> step (ViewportMsg (Viewport.KeyPressed (pressed " " False)))
                                |> mouseDown x y False
                                |> mouseMove (x + 60) (y + 20)
                                |> mouseUp (x + 60) (y + 20)
                                |> step (ViewportMsg (Viewport.KeyReleased " "))
                    in
                    Expect.all
                        [ \m -> Expect.equal (groundOf 0 ready) (groundOf 0 m)
                        , \m -> Expect.notEqual ready.viewport.camera.target m.viewport.camera.target
                        , \m -> Expect.equal [] (selected m)
                        ]
                        panned
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


editKey : Bool -> String -> Bool -> Maybe Shortcuts.EditKey
editKey hasSelection key shift =
    Shortcuts.editKeyFor { hasSelection = hasSelection, helpVisible = False, walking = False, inSubmodel = False, menuOpen = False }
        (pressed key shift)


{-| Where the camera looks from and at: unaffected by the scene's size changing.
-}
cameraView : Workspace.Model -> ( Float, Float, Vec3.Vec3 )
cameraView model =
    ( model.viewport.camera.azimuth, model.viewport.camera.elevation, model.viewport.camera.target )


{-| A plain key press on the page body, as the viewport receives it.
-}
pressed : String -> Bool -> ViewportShortcuts.KeyEvent
pressed key shift =
    { key = key
    , shift = shift
    , ctrl = False
    , meta = False
    , alt = False
    , repeat = False
    , targetTag = "BODY"
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
            , test "selecting a group during search retains query and narrows filtered results" <|
                \_ ->
                    let
                        cat =
                            { parts =
                                [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
                                , PartIndex.makePart "3020.dat" "Plate 2 x 4" "Plate" ""
                                ]
                            , categories = [ ( "Brick", 1 ), ( "Plate", 1 ) ]
                            }

                        withCross =
                            start |> step (PartIndexLoaded (Ok cat))

                        searched =
                            withCross |> step (PaletteQuery "2 x 4")

                        narrowed =
                            searched |> step (PaletteGroup (Just "plates"))
                    in
                    Expect.all
                        [ \m -> Expect.equal "2 x 4" m.palette.query
                        , \m -> Expect.equal (Just "plates") m.palette.group
                        , \m -> Expect.equal [ "3020.dat" ] (List.map .file m.palette.filteredParts)
                        ]
                        narrowed
            , test "PaletteGroup moves Favourites and Recent shelves to All shelf" <|
                \_ ->
                    let
                        onFavs =
                            withCatalogue |> step (PaletteSelectShelf Palette.FavoritesShelf)

                        switched =
                            onFavs |> step (PaletteGroup (Just "plates"))
                    in
                    Expect.equal Palette.AllShelf switched.palette.shelf
            , test "PaletteGroupAndFocus updates group and retains query" <|
                \_ ->
                    let
                        searched =
                            withCatalogue |> step (PaletteQuery "brick")

                        focused =
                            searched |> step (PaletteGroupAndFocus (Just "bricks") "palette-group-bricks")
                    in
                    Expect.all
                        [ \m -> Expect.equal "brick" m.palette.query
                        , \m -> Expect.equal (Just "bricks") m.palette.group
                        ]
                        focused
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
                        -- The grid spot nearest the camera target, on the ground: the
                        -- part's origin sits 24 LDU above its bottom (the fallback box).
                        |> Expect.equal (Just { x = 20, y = -24, z = 40 })
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
            , test "focusing and blurring inspector fields manages inspectorFocus" <|
                \_ ->
                    let
                        focused =
                            loaded |> step (FocusInspectorField "position-x")

                        blurred =
                            focused |> step BlurInspectorField
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just "position-x") focused.inspectorFocus
                        , \_ -> Expect.equal Nothing blurred.inspectorFocus
                        ]
                        ()
            , test "committing an inspector field applies the change and clears focus" <|
                \_ ->
                    let
                        modelWithFocus =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step (FocusInspectorField "position-x")

                        committed =
                            modelWithFocus
                                |> step (CommitInspectorField "position-x" (SetPosition AxisX 60))
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just 60) (positionOf 0 committed)
                        , \_ -> Expect.equal Nothing committed.inspectorFocus
                        ]
                        ()
            , test "canceling an inspector field clears focus without altering position" <|
                \_ ->
                    let
                        modelWithFocus =
                            loaded
                                |> click (screenOf 0 loaded)
                                |> step (FocusInspectorField "position-x")

                        canceled =
                            modelWithFocus
                                |> step (CancelInspectorField "position-x")
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just 0) (positionOf 0 canceled)
                        , \_ -> Expect.equal Nothing canceled.inspectorFocus
                        ]
                        ()
            , test "setting position to current value avoids duplicate history entry" <|
                \_ ->
                    let
                        m =
                            loaded |> click (screenOf 0 loaded)

                        historyLenBefore =
                            List.length m.history.undo

                        afterSame =
                            m |> step (SetPosition AxisX 0)
                    in
                    Expect.equal historyLenBefore (List.length afterSame.history.undo)
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
                        , \_ -> Expect.greaterThan 0.1 realistic.viewport.renderStyle.specularStrength
                        , \_ -> Expect.equal Style.Draft backToDraft.renderQuality
                        , \_ -> Expect.within (Expect.Absolute 0.0001) 0 backToDraft.viewport.renderStyle.specularStrength
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
                                    , plane = Nothing
                                    , ids = []
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 100, 100 )
                                    , original = start.document
                                    , amount = 0
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
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
                                    , plane = Nothing
                                    , ids = [ 1 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 150, 100 )
                                    , original = origDoc
                                    , amount = 50
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
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
                                    , plane = Nothing
                                    , ids = [ 0 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 150, 100 )
                                    , original = docWithTwo
                                    , amount = 10
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
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
                                    , plane = Nothing
                                    , ids = []
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 120, 100 )
                                    , original = start.document
                                    , amount = 20
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
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
        , test "dragging a part into collision outlines only the dragged part" <|
            \_ ->
                let
                    ( docWithTwo, _ ) =
                        Edit.apply
                            (Edit.Add
                                [ { part = "3001.dat", color = 4, matrix = Mat4.identity }
                                , { part = "3001.dat", color = 14, matrix = Mat4.identity }
                                ]
                            )
                            start.document

                    vp =
                        start.viewport

                    modelDuringDrag =
                        { start
                            | viewport = { vp | captured = True, mousePressed = True }
                            , document = docWithTwo
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
                                    , plane = Nothing
                                    , ids = [ 0 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 100, 100 )
                                    , original = docWithTwo
                                    , amount = 0
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
                                    }
                            , altHeld = True
                        }

                    moved =
                        mouseMove 100 100 modelDuringDrag
                in
                Expect.equal (Set.fromList [ 0 ]) moved.collidingPlacements
        , test "mated snap connection exempts target placement from collision detection" <|
            \_ ->
                let
                    ( docWithTwo, _ ) =
                        Edit.apply
                            (Edit.Add
                                [ { part = "3705.dat", color = 4, matrix = Mat4.identity }
                                , { part = "3647.dat", color = 14, matrix = Mat4.identity }
                                ]
                            )
                            start.document

                    vp =
                        start.viewport

                    modelDuringDrag =
                        { start
                            | viewport = { vp | captured = True, mousePressed = True }
                            , document = docWithTwo
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
                                    , plane = Nothing
                                    , ids = [ 0 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 100, 100 )
                                    , original = docWithTwo
                                    , amount = 0
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
                                    }
                            , altHeld = False
                            , snapDistance = 15.0
                            , collisionMode = Connectivity.CollisionStrict
                        }

                    moved =
                        mouseMove 100 100 modelDuringDrag
                in
                Expect.all
                    [ \m -> Expect.notEqual Nothing m.activeSnap
                    , \m -> Expect.equal Set.empty m.collidingPlacements
                    ]
                    moved
        , test "Shift key press and release toggles shiftHeld" <|
            \_ ->
                let
                    dummyKeyWithShift key shift =
                        { key = key
                        , shift = shift
                        , ctrl = False
                        , meta = False
                        , alt = False
                        , repeat = False
                        , targetTag = "BODY"
                        , targetEditable = False
                        , targetRole = ""
                        }

                    mShiftPressed =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKeyWithShift "Shift" True))) start

                    mShiftReleased =
                        step (KeyUp "Shift") mShiftPressed
                in
                Expect.all
                    [ \_ -> Expect.equal True mShiftPressed.shiftHeld
                    , \_ -> Expect.equal False mShiftReleased.shiftHeld
                    ]
                    ()
        , test "Arrow, 'r', and bracket keys cycle inFlightRotationStep when not snapped during drag" <|
            \_ ->
                let
                    dummyKey key =
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

                    readyWithoutSnap =
                        { start
                            | drag =
                                Just
                                    { kind = Gizmo.PlaneDrag (vec3 0 0 0)
                                    , plane = Nothing
                                    , ids = [ 0 ]
                                    , start = ( 100, 100 )
                                    , lastPoint = ( 100, 100 )
                                    , original = start.document
                                    , amount = 0
                                    , elevationOffset = 0.0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 100, 100 )
                                    , baseElevation = 0.0
                                    , shiftToggle = Nothing
                                    }
                            , activeSnap = Nothing
                            , inFlightRotationStep = 0
                        }

                    stepRight =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "ArrowRight"))) readyWithoutSnap

                    stepR =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "r"))) stepRight

                    stepBracketClose =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "]"))) stepR

                    stepLeft =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "ArrowLeft"))) stepBracketClose

                    stepBracketOpen =
                        step (ViewportMsg (Viewport.KeyPressed (dummyKey "["))) stepLeft
                in
                Expect.all
                    [ \_ -> Expect.equal 1 stepRight.inFlightRotationStep
                    , \_ -> Expect.equal 2 stepR.inFlightRotationStep
                    , \_ -> Expect.equal 3 stepBracketClose.inFlightRotationStep
                    , \_ -> Expect.equal 2 stepLeft.inFlightRotationStep
                    , \_ -> Expect.equal 1 stepBracketOpen.inFlightRotationStep
                    ]
                    ()
        , test "StartDragPart initiates drag session with temporary placement and selection" <|
            \_ ->
                let
                    testPart =
                        PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                    ( draggedModel, _ ) =
                        Workspace.update (StartDragPart testPart) start
                in
                Expect.all
                    [ \m -> Expect.notEqual Nothing m.drag
                    , \m -> Expect.equal False (Set.isEmpty m.selection)
                    , \m -> Expect.equal (Just testPart) m.palette.armed
                    , \m -> Expect.equal True m.viewport.captured
                    ]
                    draggedModel
        , test "StartDragPart sets viewport.captured to True for gesture capture" <|
            \_ ->
                let
                    testPart =
                        PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                    ( draggedModel, _ ) =
                        Workspace.update (StartDragPart testPart) start
                in
                Expect.equal True draggedModel.viewport.captured
        , test "placeArmedPart in a submodel with LocalFrame correctly transforms coordinates via Mat4.inverse" <|
            \_ ->
                let
                    testPart =
                        PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                    -- Placement 1 in loaded is at (300, 0, 0)
                    grouped =
                        loaded
                            |> step (SelectPlacement 1 False)
                            |> step GroupSelection

                    entered =
                        step EnterSubmodel grouped

                    ( rootPlaced, _ ) =
                        Workspace.placeArmedPart testPart ( 400, 300 ) loaded

                    ( submodelPlaced, _ ) =
                        Workspace.placeArmedPart testPart ( 400, 300 ) entered

                    rootPos =
                        Document.placements rootPlaced.document
                            |> List.filter (\p -> p.id == rootPlaced.document.nextId - 1)
                            |> List.head
                            |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))

                    submodelPos =
                        Document.placementsOf entered.context.activeSubmodel submodelPlaced.document
                            |> List.filter (\p -> p.id == submodelPlaced.document.nextId - 1)
                            |> List.head
                            |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
                in
                case ( rootPos, submodelPos ) of
                    ( Just r, Just s ) ->
                        Expect.all
                            [ \_ -> Expect.equal LocalFrame entered.context.coordinateFrame
                            , \_ -> Expect.equal (Vec3.getX r - 300) (Vec3.getX s)
                            , \_ -> Expect.equal (Vec3.getY r) (Vec3.getY s)
                            , \_ -> Expect.equal (Vec3.getZ r) (Vec3.getZ s)
                            ]
                            ()

                    _ ->
                        Expect.fail "Failed to find placed parts in root or submodel"
        ]


testKey : String -> ViewportShortcuts.KeyEvent
testKey key =
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


threeAxisMovementSuite : Test
threeAxisMovementSuite =
    describe "Three-Axis Mouse Movement & Elevation Control"
        [ describe "Wheel Elevation Stepping"
            [ test "plain wheel up decreases elevationOffset by 8 LDU (1 plate up in LDraw)" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        -- Wheel Up (delta < 0)
                        afterWheelUp =
                            step (ViewportMsg (Viewport.Wheel -100)) dragged
                    in
                    case afterWheelUp.drag of
                        Just d ->
                            Expect.equal -8.0 d.elevationOffset

                        Nothing ->
                            Expect.fail "Expected active drag"
            , test "plain wheel down increases elevationOffset by 8 LDU (1 plate down in LDraw)" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        -- Wheel Down (delta > 0)
                        afterWheelDown =
                            step (ViewportMsg (Viewport.Wheel 100)) dragged
                    in
                    case afterWheelDown.drag of
                        Just d ->
                            Expect.equal 8.0 d.elevationOffset

                        Nothing ->
                            Expect.fail "Expected active drag"
            , test "Shift + wheel steps by 24 LDU (1 brick = 3 plates)" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        withShift =
                            { dragged | shiftHeld = True }

                        afterWheelUp =
                            step (ViewportMsg (Viewport.Wheel -100)) withShift
                    in
                    case afterWheelUp.drag of
                        Just d ->
                            Expect.equal -24.0 d.elevationOffset

                        Nothing ->
                            Expect.fail "Expected active drag"
            , test "Alt + wheel steps by 1 LDU (fine precision)" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        withAlt =
                            { dragged | altHeld = True }

                        afterWheelUp =
                            step (ViewportMsg (Viewport.Wheel -100)) withAlt
                    in
                    case afterWheelUp.drag of
                        Just d ->
                            Expect.equal -1.0 d.elevationOffset

                        Nothing ->
                            Expect.fail "Expected active drag"
            , test "wheel scroll while Shift is held properly steps elevation without jumping" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( sx, sy ) =
                            screenOf 0 ready

                        dragged =
                            ready |> mouseDown sx sy False

                        withShift =
                            { dragged | shiftHeld = True }
                                |> mouseMove sx (sy + 40)

                        elevationBeforeWheel =
                            case withShift.drag of
                                Just d ->
                                    d.elevationOffset

                                Nothing ->
                                    0.0

                        afterWheel =
                            step (ViewportMsg (Viewport.Wheel -100)) withShift
                    in
                    case afterWheel.drag of
                        Just d ->
                            Expect.all
                                [ \drag -> Expect.within (Expect.Absolute 0.01) (elevationBeforeWheel - 24.0) drag.elevationOffset
                                , \drag -> Expect.equal drag.lastPoint drag.modeAnchor
                                , \drag -> Expect.within (Expect.Absolute 0.01) (elevationBeforeWheel - 24.0) drag.baseElevation
                                ]
                                d

                        Nothing ->
                            Expect.fail "Expected active drag"
            ]
        , describe "Axis Lock Keys"
            [ test "tapping X locks to AxisX, tapping X again unlocks" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        keyX =
                            testKey "x"

                        lockedX =
                            step (ViewportMsg (Viewport.KeyPressed keyX)) dragged

                        unlockedX =
                            step (ViewportMsg (Viewport.KeyPressed keyX)) lockedX
                    in
                    Expect.all
                        [ \_ ->
                            case lockedX.drag of
                                Just d ->
                                    Expect.equal (Just AxisX) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after lock"
                        , \_ ->
                            case unlockedX.drag of
                                Just d ->
                                    Expect.equal Nothing d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after unlock"
                        ]
                        ()
            , test "tapping Y locks to AxisY, tapping Z locks to AxisZ" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        lockedY =
                            step (ViewportMsg (Viewport.KeyPressed (testKey "y"))) dragged

                        lockedZ =
                            step (ViewportMsg (Viewport.KeyPressed (testKey "z"))) lockedY
                    in
                    Expect.all
                        [ \_ ->
                            case lockedY.drag of
                                Just d ->
                                    Expect.equal (Just AxisY) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after lock Y"
                        , \_ ->
                            case lockedZ.drag of
                                Just d ->
                                    Expect.equal (Just AxisZ) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after lock Z"
                        ]
                        ()
            , test "key autorepeat does not toggle axis lock" <|
                \_ ->
                    let
                        testPart =
                            PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""

                        dragged =
                            step (StartDragPart testPart) start

                        keyX =
                            testKey "x"

                        lockedX =
                            step (ViewportMsg (Viewport.KeyPressed keyX)) dragged

                        repeatedKeyX =
                            { keyX | repeat = True }

                        stillLockedX =
                            step (ViewportMsg (Viewport.KeyPressed repeatedKeyX)) lockedX

                        repeatedKeyY =
                            { key = "y"
                            , shift = False
                            , ctrl = False
                            , meta = False
                            , alt = False
                            , repeat = True
                            , targetTag = "BODY"
                            , targetEditable = False
                            , targetRole = ""
                            }

                        stillLockedXAfterYRepeat =
                            step (ViewportMsg (Viewport.KeyPressed repeatedKeyY)) lockedX
                    in
                    Expect.all
                        [ \_ ->
                            case lockedX.drag of
                                Just d ->
                                    Expect.equal (Just AxisX) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after lock"
                        , \_ ->
                            case stillLockedX.drag of
                                Just d ->
                                    Expect.equal (Just AxisX) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after repeat X"
                        , \_ ->
                            case stillLockedXAfterYRepeat.drag of
                                Just d ->
                                    Expect.equal (Just AxisX) d.axisLock

                                Nothing ->
                                    Expect.fail "Expected active drag after repeat Y"
                        ]
                        ()
            ]
        , describe "Single Undo Command on Drag Completion"
            [ test "completing a drag with wheel elevation and axis moves records exactly 1 undo entry" <|
                \_ ->
                    let
                        ready =
                            loaded |> step (UseTool MoveTool)

                        ( sx, sy ) =
                            screenOf 0 ready

                        historyBefore =
                            ready.history.undo

                        dragged =
                            ready
                                |> mouseDown sx sy False
                                |> step (ViewportMsg (Viewport.Wheel -100))
                                |> mouseUp sx sy
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length historyBefore + 1) (List.length dragged.history.undo)
                        , \_ -> Expect.equal Nothing dragged.drag
                        ]
                        ()
            ]
        , describe "Send to App URLs and Toast"
            [ test "CopyPlayUrl sets toast and closes export menu" <|
                \_ ->
                    let
                        opened =
                            loaded |> step ToggleExportMenu

                        ( copied, _ ) =
                            Workspace.update CopyPlayUrl opened
                    in
                    Expect.all
                        [ \m -> Expect.equal False m.exportMenuOpen
                        , \m -> Expect.equal (Just "Link copied to clipboard!") (Maybe.map .message m.toast)
                        ]
                        copied
            , test "ClearToast clears active toast when timer matches" <|
                \_ ->
                    let
                        ( copied, _ ) =
                            Workspace.update CopyPlayUrl loaded

                        timerId =
                            copied.toastCounter

                        ( cleared, _ ) =
                            Workspace.update (ClearToast timerId) copied
                    in
                    Expect.equal Nothing cleared.toast
            ]
        ]



-- ── View-relative drag and orbit around the selection ─────────────────────────


{-| The two bricks, with a 1 LDU move step and stud snapping off, so a body
drag shows the raw working-plane movement.
-}
rawDrag : Workspace.Model
rawDrag =
    loaded
        |> step (SetGridStep 1)
        |> (\m -> { m | snapDistance = 0 })


withCamera : (Camera.Camera -> Camera.Camera) -> Workspace.Model -> Workspace.Model
withCamera change model =
    let
        viewport =
            model.viewport
    in
    { model | viewport = { viewport | camera = change viewport.camera } }


{-| Run any camera glide (`Viewport.pivotOn`) to the end.
-}
settle : Workspace.Model -> Workspace.Model
settle model =
    model
        |> step (ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 0)))
        |> step (ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix 1000)))


{-| How far a placement moved, in render space (Y up).
-}
renderMove : Int -> Workspace.Model -> Workspace.Model -> Vec3
renderMove id before after =
    let
        at m =
            Document.getPlacement id m.document
                |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
                |> Maybe.withDefault (vec3 0 0 0)
    in
    Gizmo.toRender (Vec3.sub (at after) (at before))


grabOf : Workspace.Model -> Maybe Vec3
grabOf model =
    case Maybe.map .kind model.drag of
        Just (Gizmo.PlaneDrag grab) ->
            Just grab

        _ ->
            Nothing


{-| A brick shaped like a real one: its origin on top, its bottom 24 LDU below.
-}
tallPart : String
tallPart =
    "4 16 -10 24 -10 10 24 -10 10 24 10 -10 24 10\n4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10\n"


tallRawDrag : Workspace.Model
tallRawDrag =
    rawDrag |> step (ViewportMsg (Viewport.PartLoaded "3005.dat" (Ok tallPart)))


{-| Drag a 3005 brick in from the palette and drop it where the cursor meets
the ground at `ground` (render space).
-}
paletteDragTo : Vec3 -> Workspace.Model -> Workspace.Model
paletteDragTo ground model =
    let
        dragging =
            step (StartDragPart (PartIndex.makePart "3005.dat" "Brick 1 x 1" "Brick" "")) model

        ( x, y ) =
            Gizmo.project dragging.viewport.camera 800 600 ground
                |> Maybe.withDefault ( -1, -1 )
    in
    dragging
        |> mouseMove (x - 5) y
        |> mouseMove x y
        |> mouseUp x y


eyeMoved : Workspace.Model -> Workspace.Model -> Float
eyeMoved before after =
    Vec3.distance (Camera.position before.viewport.camera) (Camera.position after.viewport.camera)


{-| Azimuth, elevation and distance: what a translation glide must not change.
-}
orientation : Workspace.Model -> ( Float, Float, Float )
orientation model =
    ( model.viewport.camera.azimuth, model.viewport.camera.elevation, model.viewport.camera.distance )


viewDragSuite : Test
viewDragSuite =
    describe "view-relative body drag and orbit around the selection"
        [ test "the grabbed point stays under the cursor, with no jump on the first move" <|
            \_ ->
                let
                    ( x, y ) =
                        screenOf 0 rawDrag

                    grabbed =
                        rawDrag |> mouseDown x y False

                    nudged =
                        grabbed |> mouseMove (x + 6) y

                    far =
                        grabbed |> mouseMove (x + 40) (y - 15)
                in
                case grabOf grabbed of
                    Just grab ->
                        Expect.all
                            [ \_ -> Expect.atMost 6 (Vec3.length (renderMove 0 rawDrag nudged))
                            , \_ ->
                                case Gizmo.project far.viewport.camera 800 600 (Vec3.add grab (renderMove 0 rawDrag far)) of
                                    Just ( px, py ) ->
                                        Expect.atMost 2 (abs (px - (x + 40)) + abs (py - (y - 15)))

                                    Nothing ->
                                        Expect.fail "grab point off screen"
                            , \_ -> Expect.greaterThan 10 (Vec3.length (renderMove 0 rawDrag far))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "no body drag"
        , test "dragging screen-left moves the part to the camera's left, however the camera is turned" <|
            \_ ->
                let
                    leftOf azimuth =
                        let
                            turned =
                                rawDrag |> withCamera (\c -> { c | azimuth = azimuth })

                            ( x, y ) =
                                screenOf 0 turned

                            dragged =
                                turned
                                    |> mouseDown x y False
                                    |> mouseMove (x - 60) y
                                    |> mouseUp (x - 60) y

                            moved =
                                renderMove 0 turned dragged

                            right =
                                vec3 (cos azimuth) 0 -(sin azimuth)

                            forward =
                                vec3 -(sin azimuth) 0 -(cos azimuth)
                        in
                        ( Vec3.dot moved right, Vec3.dot moved forward )

                    check azimuth =
                        let
                            ( along, across ) =
                                leftOf azimuth
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan -20 along
                            , \_ -> Expect.atMost (0.2 * abs along) (abs across)
                            ]
                            ()
                in
                Expect.all
                    [ \_ -> check 0.6
                    , \_ -> check 2.4
                    , \_ -> check -1.9
                    ]
                    ()
        , test "with Shift held the part moves finely over the ground, at the same height" <|
            \_ ->
                let
                    ( x, y ) =
                        screenOf 0 rawDrag

                    shiftKey =
                        testKey "Shift"

                    held =
                        rawDrag
                            |> mouseDown x y False
                            |> mouseMove (x + 20) y
                            |> step (ViewportMsg (Viewport.KeyPressed { shiftKey | shift = True }))

                    sideways =
                        held |> mouseMove (x + 80) y
                in
                Expect.all
                    [ \_ -> Expect.equal True sideways.shiftHeld
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) (Maybe.withDefault 0 (elevationOf 0 held)) (Maybe.withDefault 1 (elevationOf 0 sideways))
                    , \_ -> Expect.notEqual (groundOf 0 held) (groundOf 0 sideways)
                    ]
                    ()
        , test "a part dragged in from the palette onto empty ground rests on the ground" <|
            \_ ->
                let
                    dropped =
                        paletteDragTo (vec3 150 0 0) tallRawDrag
                in
                Expect.all
                    [ \m -> Expect.equal (Just ( 150, 0 )) (groundOf 2 m)
                    , \m -> Expect.within (Expect.Absolute 1.0e-6) 0 (Maybe.withDefault 99 (elevationOf 2 m) + 24)
                    ]
                    dropped
        , test "a part dragged in from the palette over a brick rests on its top" <|
            \_ ->
                let
                    dropped =
                        paletteDragTo (vec3 0 0 0) tallRawDrag
                in
                Expect.all
                    [ \m -> Expect.equal (Just ( 0, 0 )) (groundOf 2 m)
                    , \m -> Expect.within (Expect.Absolute 1.0e-6) -20 (Maybe.withDefault 99 (elevationOf 2 m) + 24)
                    ]
                    dropped
        , test "a body drag rests the part on the brick it overlaps, and on the ground off it" <|
            \_ ->
                let
                    -- The test cube's connectivity box is a stock brick's, so
                    -- strict collisions would refuse the drop on top.
                    lenient =
                        { rawDrag | collisionMode = CollisionOff }

                    ( x, y ) =
                        screenOf 1 lenient

                    grabbed =
                        lenient |> mouseDown x y False

                    cursorFor offset =
                        grabOf grabbed
                            |> Maybe.andThen (\grab -> Gizmo.project grabbed.viewport.camera 800 600 (Vec3.add grab offset))
                            |> Maybe.withDefault ( -1, -1 )

                    ( ox, oy ) =
                        cursorFor (vec3 -300 0 0)

                    ( gx, gy ) =
                        cursorFor (vec3 -150 0 0)

                    onTop =
                        grabbed |> mouseMove ox oy

                    droppedOnTop =
                        onTop |> mouseUp ox oy

                    offIt =
                        onTop |> mouseMove gx gy
                in
                Expect.all
                    [ \_ -> Expect.equal (Just ( 0, 0 )) (groundOf 1 onTop)
                    , \_ -> Expect.equal (Just -20) (elevationOf 1 onTop)
                    , \_ -> Expect.equal (Just -20) (elevationOf 1 droppedOnTop)
                    , \_ -> Expect.equal (Just ( 150, 0 )) (groundOf 1 offIt)
                    , \_ -> Expect.equal (Just 0) (elevationOf 1 offIt)
                    ]
                    ()
        , test "clicking a part slides the view onto it without turning or zooming" <|
            \_ ->
                let
                    picked =
                        loaded |> click (screenOf 1 loaded) |> settle
                in
                Expect.all
                    [ \m -> Expect.equal [ 1 ] (selected m)
                    , \m -> Expect.within (Expect.Absolute 1) 300 (Vec3.getX m.viewport.camera.target)
                    , \m -> Expect.equal (orientation loaded) (orientation m)
                    , \m ->
                        Vec3.distance m.viewport.camera.target loaded.viewport.camera.target
                            |> Expect.within (Expect.Absolute 1.0e-6) (eyeMoved loaded m)
                    ]
                    picked
        , test "select all, a keyboard move and undo leave the camera alone" <|
            \_ ->
                let
                    after =
                        loaded
                            |> step SelectAll
                            |> step (NudgeSelection (Shortcuts.Nudge { direction = Shortcuts.Right, fine = False }))
                            |> step Undo
                            |> settle
                in
                Expect.all
                    [ \m -> Expect.equal [ 0, 1 ] (selected m)
                    , \m -> Expect.equal loaded.viewport.camera.target m.viewport.camera.target
                    , \m -> Expect.equal (orientation loaded) (orientation m)
                    ]
                    after
        , test "Fit View recentres the pivot on the model; deselecting keeps it" <|
            \_ ->
                let
                    picked =
                        loaded |> click (screenOf 1 loaded) |> settle

                    cleared =
                        picked |> step ClearSelection |> settle

                    fitted =
                        picked |> step (ViewportMsg Viewport.FitView) |> settle
                in
                Expect.all
                    [ \_ -> Expect.equal picked.viewport.camera.target cleared.viewport.camera.target
                    , \_ -> Expect.equal loaded.viewport.camera.target fitted.viewport.camera.target
                    ]
                    ()
        , test "the pivot holds still during a drag and on the drop" <|
            \_ ->
                let
                    picked =
                        rawDrag |> click (screenOf 0 rawDrag) |> settle

                    ( x, y ) =
                        screenOf 0 picked

                    dragging =
                        picked
                            |> mouseDown x y False
                            |> mouseMove (x + 60) y
                            |> settle

                    dropped =
                        dragging |> mouseUp (x + 60) y |> settle
                in
                Expect.all
                    [ \_ -> Expect.equal picked.viewport.camera.target dragging.viewport.camera.target
                    , \_ -> Expect.notEqual (screenOf 0 picked) (screenOf 0 dropped)
                    , \_ -> Expect.equal picked.viewport.camera.target dropped.viewport.camera.target
                    , \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 (eyeMoved picked dropped)
                    ]
                    ()
        , test "in street-level walk the camera does not re-pivot" <|
            \_ ->
                let
                    walking =
                        loaded |> step (ViewportMsg (Viewport.KeyPressed (testKey "g")))

                    picked =
                        walking |> click (screenOf 1 walking) |> settle
                in
                Expect.all
                    [ \_ -> Expect.equal Camera.Walk walking.viewport.camera.navigation
                    , \_ -> Expect.equal walking.viewport.camera picked.viewport.camera
                    ]
                    ()
        ]



-- ── Committed drags and gear alignment ────────────────────────────────────────


commitSuite : Test
commitSuite =
    describe "Committed drags and gear alignment"
        [ test "a plain left drag of a free brick commits one undo step" <|
            \_ ->
                let
                    ( x, y ) =
                        screenOf 0 rawDrag

                    dragged =
                        rawDrag
                            |> mouseDown x y False
                            |> mouseMove (x + 40) y
                            |> mouseUp (x + 40) y
                in
                Expect.all
                    [ \_ -> Expect.equal True (Edit.canUndo dragged.history)
                    , \_ -> Expect.notEqual (groundOf 0 rawDrag) (groundOf 0 dragged)
                    , \_ -> Expect.equal Nothing dragged.drag
                    ]
                    ()
        , test "a Shift drag of a free brick commits one undo step" <|
            \_ ->
                let
                    ( x, y ) =
                        screenOf 0 rawDrag

                    dragged =
                        rawDrag
                            |> mouseDown x y True
                            |> mouseMove (x + 40) y
                            |> mouseUp (x + 40) y
                in
                Expect.all
                    [ \_ -> Expect.equal True (Edit.canUndo dragged.history)
                    , \_ -> Expect.notEqual (groundOf 0 rawDrag) (groundOf 0 dragged)
                    ]
                    ()
        , test "a drop that would overlap another brick is refused with a message and records no undo step" <|
            \_ ->
                let
                    ( x, y ) =
                        screenOf 0 rawDrag

                    ( tx, ty ) =
                        screenOf 1 rawDrag

                    dropped =
                        rawDrag
                            |> mouseDown x y False
                            |> mouseMove tx ty
                            |> mouseUp tx ty
                in
                Expect.all
                    [ \_ -> Expect.equal False (Edit.canUndo dropped.history)
                    , \_ -> Expect.equal (groundOf 0 rawDrag) (groundOf 0 dropped)
                    , \_ -> Expect.equal (Just True) (Maybe.map (.message >> String.startsWith "Move refused") dropped.toast)
                    ]
                    ()
        , describe "Palette add lands on a free spot"
            [ test "an added part rests on the top of the selected brick when that spot is free" <|
                \_ ->
                    let
                        added =
                            addArmed (selectOnly 0 loaded)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 3 (List.length (Document.placements added.document))
                        , \_ -> Expect.equal (Just -20) (elevationOf 2 added)
                        , \_ -> Expect.equal 0 (overlappingPairs added)
                        ]
                        ()
            , test "adds in a row never overlap a placed part" <|
                \_ ->
                    let
                        added =
                            loaded
                                |> selectOnly 0
                                |> addArmed
                                |> addArmed
                                |> addArmed
                    in
                    Expect.all
                        [ \_ -> Expect.equal 5 (List.length (Document.placements added.document))
                        , \_ -> Expect.equal 0 (overlappingPairs added)
                        ]
                        ()
            ]
        , describe "Gear status"
            [ test "one selected gear shows the mesh status" <|
                \_ ->
                    gearPair
                        |> step (WindowSized 1280 800)
                        |> Workspace.view
                        |> Query.fromHtml
                        |> Query.has [ Selector.attribute (HtmlAttr.attribute "data-gear-feedback" "") ]
            , test "two selected gears show no mesh status" <|
                \_ ->
                    { gearPair | selection = Set.fromList [ 0, 1 ] }
                        |> step (WindowSized 1280 800)
                        |> Workspace.view
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.attribute (HtmlAttr.attribute "data-gear-feedback" "") ]
            ]
        , describe "Gear alignment on a nudge"
            [ test "a nudge that meshes a misaligned gear turns it until the teeth interlock" <|
                \_ ->
                    let
                        back =
                            gearPair |> nudgeBy Shortcuts.Left |> nudgeBy Shortcuts.Right
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 0.1 (gearPairWorst gearPair)
                        , \_ -> Expect.lessThan 0.01 (gearPairWorst back)
                        ]
                        ()
            , test "one undo restores both the nudge and the gear turn" <|
                \_ ->
                    let
                        moved =
                            gearPair |> nudgeBy Shortcuts.Left

                        back =
                            nudgeBy Shortcuts.Right moved

                        undone =
                            step Undo back
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length moved.history.undo + 1) (List.length back.history.undo)
                        , \_ -> Expect.equal moved.document undone.document
                        ]
                        ()
            ]
        ]


{-| Two 24T gears on one line, the second 3 degrees out of mesh, with
collision off so the mesh distance is allowed. Gear 1 is selected.
-}
gearPair : Workspace.Model
gearPair =
    let
        loadedGears =
            start
                |> step (FileTextReceived "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3648.dat\n1 14 60 0 0 0.99863 -0.05234 0 0.05234 0.99863 0 0 0 1 3648.dat\n")
    in
    { loadedGears | collisionMode = CollisionOff, selection = Set.singleton 1 }


nudgeBy : Shortcuts.NudgeDirection -> Workspace.Model -> Workspace.Model
nudgeBy direction model =
    step (NudgeSelection (Shortcuts.Nudge { direction = direction, fine = False })) model


{-| The worst tooth fraction of the mesh between the two gears of a model; 1
when they do not form a pair.
-}
gearPairWorst : Workspace.Model -> Float
gearPairWorst model =
    let
        gears : List GearInstance
        gears =
            Detect.extractGears Data.gearParts (Document.toLines model.document) model.viewport.partCache
    in
    case gears of
        [ a, b ] ->
            let
                ( fixed, moving ) =
                    if Vec3.length (Vec3.sub (Phase.gearFrame a).centre (vec3 0 -24 0)) < 1 then
                        ( a, b )

                    else
                        ( b, a )
            in
            Phase.meshConstraints { fixed = fixed, moving = moving }
                |> Maybe.andThen
                    (\options ->
                        options
                            |> List.map (\c -> max c.fixedFraction (abs c.movingResidual / c.movingPitch))
                            |> List.minimum
                    )
                |> Maybe.withDefault 1

        _ ->
            1


selectOnly : Int -> Workspace.Model -> Workspace.Model
selectOnly id model =
    { model | selection = Set.singleton id }


{-| Arm a 3001 brick from the palette and add it, as the Add button does.
-}
addArmed : Workspace.Model -> Workspace.Model
addArmed model =
    model
        |> step (ArmPart (PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""))
        |> step AddArmedPart


{-| How many pairs of placements have overlapping world boxes, using the
brick's local box from `brickPart`.
-}
overlappingPairs : Workspace.Model -> Int
overlappingPairs model =
    let
        boxes =
            Document.placements model.document
                |> List.map (\p -> DragPlane.worldBox p.matrix { min = vec3 -10 -20 -10, max = vec3 10 0 10 })

        pairs xs =
            case xs of
                [] ->
                    []

                x :: rest ->
                    List.map (\y -> ( x, y )) rest ++ pairs rest
    in
    pairs boxes
        |> List.filter (\( a, b ) -> DragPlane.boxesOverlap a b)
        |> List.length
