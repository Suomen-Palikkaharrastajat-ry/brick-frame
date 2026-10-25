module Studio.CoreTest exposing (suite)

{-| Tests for the Studio.Core TEA component.
-}

import Data
import Dict
import Expect
import Gear.Detect as Detect
import Gear.Phase as Phase
import Gear.Types exposing (GearInstance)
import LDraw.Document as Document
import LDraw.Parser as Parser
import LDraw.Resolve exposing (PartStatus(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera
import Set
import Studio.Core as Core
import Studio.DragPlane as DragPlane
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo exposing (DragHandle(..))
import Studio.Shortcuts as StudioShortcuts exposing (Tool(..))
import Test exposing (Test, describe, test)
import Time
import Viewport
import Viewport.Shortcuts exposing (KeyEvent)


suite : Test
suite =
    describe "Studio.Core"
        [ test "initializes with empty document and default state" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model, _ ) =
                        Core.init config
                in
                Expect.all
                    [ \m -> Expect.equal (Core.getPlacements m) []
                    , \m -> Expect.equal (Core.hasSelection m) False
                    , \m -> Expect.equal (Core.selectionCount m) 0
                    , \m -> Expect.equal (Core.canUndo m) False
                    , \m -> Expect.equal (Core.canRedo m) False
                    , \m -> Expect.equal m.tool MoveTool
                    ]
                    model
        , test "addPart adds placement and records undo" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, events1 ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (Core.getPlacements m)) 1
                    , \m -> Expect.equal (Core.hasSelection m) True
                    , \m -> Expect.equal (Core.canUndo m) True
                    , \_ -> Expect.equal (List.any isDocChanged events1) True
                    ]
                    model1
        , test "undo and redo step through edits" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( modelUndone, _, _ ) =
                        Core.undo model1

                    ( modelRedone, _, _ ) =
                        Core.redo modelUndone
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length (Core.getPlacements modelUndone)) 0
                    , \_ -> Expect.equal (Core.canRedo modelUndone) True
                    , \_ -> Expect.equal (List.length (Core.getPlacements modelRedone)) 1
                    ]
                    ()
        , test "deleteSelection removes part and supports undo" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( modelDeleted, _, _ ) =
                        Core.deleteSelection model1

                    ( modelRestored, _, _ ) =
                        Core.undo modelDeleted
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length (Core.getPlacements modelDeleted)) 0
                    , \_ -> Expect.equal (List.length (Core.getPlacements modelRestored)) 1
                    ]
                    ()
        , test "clearSelection and selectAll work as expected" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    cleared =
                        Core.clearSelection model1

                    selectedAll =
                        Core.selectAll cleared
                in
                Expect.all
                    [ \_ -> Expect.equal (Core.hasSelection cleared) False
                    , \_ -> Expect.equal (Core.selectionCount selectedAll) 1
                    ]
                    ()
        , test "Escape during a drag restores the document and records no history" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( movedDoc, _ ) =
                        Edit.apply (Edit.Translate (Set.toList model1.selection) (vec3 40 0 0)) model1.document

                    dragging =
                        { model1
                            | document = movedDoc
                            , drag =
                                Just
                                    { kind = PlaneDrag (vec3 0 0 0)
                                    , plane = Nothing
                                    , ids = Set.toList model1.selection
                                    , start = ( 0, 0 )
                                    , lastPoint = ( 0, 0 )
                                    , original = model1.document
                                    , amount = 0
                                    , elevationOffset = 0
                                    , axisLock = Nothing
                                    , modeAnchor = ( 0, 0 )
                                    , baseElevation = 0
                                    , shiftToggle = Nothing
                                    }
                        }

                    ( escaped, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed (pressOn "CANVAS" "Escape"))) dragging

                    originalMatrix =
                        placementMatrix model1
                in
                Expect.all
                    [ \m -> Expect.equal m.drag Nothing
                    , \m -> Expect.equal (placementMatrix m) originalMatrix
                    , \m -> Expect.equal m.history model1.history
                    , \m -> Expect.equal (Core.hasSelection m) True
                    ]
                    escaped
        , test "Escape without a drag clears the selection" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( cleared, _, events ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed (pressOn "CANVAS" "Escape"))) model1
                in
                Expect.all
                    [ \m -> Expect.equal (Core.hasSelection m) False
                    , \m -> Expect.equal m.history model1.history
                    , \_ -> Expect.equal [ Core.SelectionChanged Set.empty ] events
                    ]
                    cleared
        , test "Ctrl Z is ignored while typing in an input" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    typing =
                        pressOn "INPUT" "z"

                    ( after, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed { typing | ctrl = True })) model1
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (Core.getPlacements m)) 1
                    , \m -> Expect.equal (Core.canUndo m) True
                    , \m -> Expect.equal m.history model1.history
                    ]
                    after
        , test "an arrow key turns the selection and records one history entry" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( turned, _, events ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed (pressOn "CANVAS" "ArrowRight"))) model1
                in
                Expect.all
                    [ \m -> Expect.notEqual m.history model1.history
                    , \m -> Expect.notEqual (placementMatrix m) (placementMatrix model1)
                    , \_ -> Expect.equal (List.any isDocChanged events) True
                    ]
                    turned
        , test "a right press during a drag cancels it, restoring the document with no history" <|
            \_ ->
                let
                    model1 =
                        addedPart

                    dragging =
                        draggingModel model1

                    ( cancelled, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.MouseButtonDown 0 0 2 False False)) dragging
                in
                Expect.all
                    [ \m -> Expect.equal m.drag Nothing
                    , \m -> Expect.equal (placementMatrix m) (placementMatrix model1)
                    , \m -> Expect.equal m.history model1.history
                    ]
                    cancelled
        , test "Delete is ignored while help is visible" <|
            \_ ->
                let
                    model1 =
                        addedPart

                    helpOpen =
                        Core.setHelpVisible True model1

                    ( afterDelete, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed (pressOn "CANVAS" "Delete"))) helpOpen
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (Core.getPlacements m)) 1
                    , \m -> Expect.equal m.history model1.history
                    ]
                    afterDelete
        , test "Delete is ignored while a drag is in progress" <|
            \_ ->
                let
                    model1 =
                        addedPart

                    ( afterDelete, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.KeyPressed (pressOn "CANVAS" "Delete")))
                            (draggingModel model1)
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (Core.getPlacements m)) 1
                    , \m -> Expect.equal m.history model1.history
                    , \m -> Expect.notEqual m.drag Nothing
                    ]
                    afterDelete
        , bodyDragSuite
        , gearDropSuite
        , pivotSuite
        , placementSuite
        , noGizmoSuite
        , selectionActionSuite
        , emptyDragPanSuite
        , shiftFineSuite
        , restingSuite
        ]


{-| Body drags follow the cursor on the working plane.
-}
bodyDragSuite : Test
bodyDragSuite =
    describe "body drag"
        [ test "the first move keeps the part where it was (no jump)" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -24 0 ]

                    ( moved, _, _ ) =
                        moveTo ( 400, 300 ) model
                in
                firstPosition moved |> Expect.equal (Just (vec3 0 -24 0))
        , test "the grabbed point stays under the cursor" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -24 0 ]

                    target =
                        vec3 37 0 -52

                    ( moved, _, _ ) =
                        moveTo (screenOf model target) model
                in
                case firstPosition moved of
                    Just p ->
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.51) 37 (Vec3.getX p)
                            , \_ -> Expect.within (Expect.Absolute 0.51) 52 (Vec3.getZ p)
                            , \_ -> Expect.within (Expect.Absolute 1.0e-6) -24 (Vec3.getY p)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "the part vanished"
        , test "dragging screen-right moves camera-right" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -24 0 ]

                    cam =
                        model.viewport.camera

                    ( moved, _, _ ) =
                        moveTo ( 480, 300 ) model
                in
                case firstPosition moved of
                    Just p ->
                        Vec3.dot (Gizmo.toRender p) (vec3 (cos cam.azimuth) 0 -(sin cam.azimuth))
                            |> Expect.greaterThan 5

                    Nothing ->
                        Expect.fail "the part vanished"
        , test "passing beside a tall neighbour does not climb it; overlapping it does" <|
            \_ ->
                let
                    -- lead at 0, a two-brick column (top at Y = -52) at x = 100
                    model =
                        grabbedAt [ vec3 0 -24 0, vec3 100 -24 0, vec3 100 -52 0 ]

                    heightAt x =
                        moveTo (screenOf model (vec3 x 0 0)) model
                            |> (\( m, _, _ ) -> firstPosition m)
                            |> Maybe.map (\p -> toFloat (round (Vec3.getY p)))
                in
                Expect.all
                    [ \_ -> Expect.equal (Just -24) (heightAt 75)
                    , \_ -> Expect.equal (Just -76) (heightAt 100)
                    ]
                    ()
        , test "a ray that misses the plane keeps the last position" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -24 0 ]

                    ( moved, _, _ ) =
                        moveTo ( 400, -5000 ) model
                in
                firstPosition moved |> Expect.equal (Just (vec3 0 -24 0))
        ]


{-| `NoGizmo`: no handles, but a press on a part still picks it up.
-}
noGizmoSuite : Test
noGizmoSuite =
    describe "no-gizmo style"
        [ test "shows no arrows and no rings, under either tool" <|
            \_ ->
                let
                    model =
                        Core.setGizmoStyle Core.NoGizmo addedPart
                in
                Expect.all
                    [ \m -> Expect.equal 0 (List.length (Core.currentHandles m))
                    , \m -> Expect.equal 0 (List.length (Core.currentArcHandles m))
                    , \m -> Expect.equal 0 (List.length (Core.currentArcHandles (Core.setTool RotateTool m)))
                    ]
                    model
        , test "the other styles do show handles for the same selection" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.notEqual 0 (List.length (Core.currentHandles m))
                    , \m -> Expect.notEqual 0 (List.length (Core.currentArcHandles (Core.setGizmoStyle Core.MoveAndRotateGizmo m)))
                    ]
                    addedPart
        , test "a press on the part starts a body drag that moves it" <|
            \_ ->
                let
                    model =
                        aimedAt (vec3 0 -24 0) |> Core.setGizmoStyle Core.NoGizmo

                    ( pressed, _, _ ) =
                        Core.update (Core.ViewportMsg (Viewport.MouseButtonDown 400 300 0 False False)) model

                    ( moved, _, _ ) =
                        moveTo ( 480, 300 ) { pressed | altHeld = True }

                    cam =
                        model.viewport.camera
                in
                Expect.all
                    [ \_ ->
                        case Maybe.map .kind pressed.drag of
                            Just (PlaneDrag _) ->
                                Expect.pass

                            _ ->
                                Expect.fail "the press did not pick up the part"
                    , \_ ->
                        case firstPosition moved of
                            Just p ->
                                Vec3.dot (Gizmo.toRender p) (vec3 (cos cam.azimuth) 0 -(sin cam.azimuth))
                                    |> Expect.greaterThan 5

                            Nothing ->
                                Expect.fail "the part vanished"
                    ]
                    ()
        ]


{-| A 24T dragged by its body to mesh distance of a fixed 24T, through the
drag path (press, move, release), is turned so its teeth interlock. The turn is
part of the drop's history step: one undo restores the position and the
rotation together, and redo applies both again.
-}
gearDropSuite : Test
gearDropSuite =
    let
        fixedAt =
            vec3 0 -24 0

        startAt =
            vec3 200 -24 0

        meshAt =
            vec3 60 -24 0

        start =
            gearStart fixedAt startAt

        movingId =
            Core.getPlacements start |> List.drop 1 |> List.head |> Maybe.map .id |> Maybe.withDefault -1

        startMatrix =
            matrixOf 1 start

        grabbed =
            gearGrabbed movingId start

        ( mx, my ) =
            screenOf grabbed (Gizmo.toRender meshAt)

        dropped =
            moveTo ( mx, my ) grabbed
                |> (\( m, _, _ ) -> m)
                |> (\m -> Core.update (Core.ViewportMsg (Viewport.MouseUp mx my)) m)
                |> (\( m, _, _ ) -> m)

        droppedMatrix =
            matrixOf 1 dropped
    in
    describe "dropping a gear at mesh distance (drag path)"
        [ test "the drop turns the gear until its teeth interlock" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal Nothing dropped.drag
                    , \_ -> Expect.equal False (approxGearMatrix droppedMatrix startMatrix)
                    , \_ -> Expect.lessThan 0.01 (gearPairWorst dropped)
                    ]
                    ()
        , test "one undo restores both the position and the turn; redo applies both again" <|
            \_ ->
                let
                    undone =
                        Core.undo dropped |> (\( m, _, _ ) -> m)

                    redone =
                        Core.redo undone |> (\( m, _, _ ) -> m)
                in
                Expect.all
                    [ \_ -> Expect.equal True (approxGearMatrix startMatrix (matrixOf 1 undone))
                    , \_ -> Expect.equal True (approxGearMatrix droppedMatrix (matrixOf 1 redone))
                    , \_ -> Expect.equal 2 (List.length (Core.getPlacements redone))
                    ]
                    ()
        ]


{-| A model with a fixed 24T at `fixedAt` and a second 24T at `startAt`, with
gear alignment on and drags keeping the press-time height (there is no support
to settle on in the test).
-}
gearStart : Vec3 -> Vec3 -> Core.Model
gearStart fixedAt startAt =
    let
        config =
            { ldrawBase = "/ldraw"
            , ldrawFallbackBase = ""
            , defaultModelUrl = ""
            , initialHash = ""
            , supportsPointerEvents = True
            , document = Document.empty
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( model0, _ ) =
            Core.init config

        place pos model =
            Core.addPartAt "3648.dat" 14 (Mat4.makeTranslate pos) model |> (\( m, _, _ ) -> m)
    in
    model0
        |> place fixedAt
        |> place startAt
        |> Core.setDragSettles False
        |> Core.setGearAlignment True


{-| The model in the middle of a body drag of the gear `id`, grabbed at its
centre (as `grabbedAt` does for bricks).
-}
gearGrabbed : Int -> Core.Model -> Core.Model
gearGrabbed id model =
    let
        position =
            Core.getPlacements model
                |> List.filter (\p -> p.id == id)
                |> List.head
                |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
                |> Maybe.withDefault (vec3 0 0 0)

        grab =
            Gizmo.toRender position

        vp =
            model.viewport

        cam =
            vp.camera
    in
    { model
        | viewport = Viewport.captureDrag { vp | camera = { cam | target = grab }, mousePressed = True }
        , selection = Set.singleton id
        , altHeld = True
        , drag =
            Just
                { kind = PlaneDrag grab
                , plane = Nothing
                , ids = [ id ]
                , start = ( 400, 300 )
                , lastPoint = ( 400, 300 )
                , original = model.document
                , amount = 0
                , elevationOffset = 0
                , axisLock = Nothing
                , modeAnchor = ( 400, 300 )
                , baseElevation = 0
                , shiftToggle = Nothing
                }
    }


{-| The worst tooth fraction of the mesh between the two gears of a model.
-}
gearPairWorst : Core.Model -> Float
gearPairWorst model =
    let
        gears : List GearInstance
        gears =
            Detect.extractGears Data.gearParts (Document.toLines model.document) model.viewport.partCache
    in
    case gears of
        [ a, b ] ->
            let
                fixed =
                    if Vec3.length (Vec3.sub (Phase.gearFrame a).centre (vec3 0 -24 0)) < 1 then
                        a

                    else
                        b

                moving =
                    if fixed == a then
                        b

                    else
                        a
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


matrixOf : Int -> Core.Model -> Mat4
matrixOf index model =
    Core.getPlacements model |> List.drop index |> List.head |> Maybe.map .matrix |> Maybe.withDefault Mat4.identity


{-| Two placement matrices are the same map, to within a small tolerance.
-}
approxGearMatrix : Mat4 -> Mat4 -> Bool
approxGearMatrix a b =
    List.all
        (\p -> Vec3.distance (Mat4.transform a p) (Mat4.transform b p) < 1.0e-3)
        [ vec3 1 2 3, vec3 -5 4 7, vec3 0 0 10 ]


{-| The host-button actions on the selection.
-}
selectionActionSuite : Test
selectionActionSuite =
    let
        undoDepth m =
            List.length m.history.undo

        oneEdit name edit check =
            test name <|
                \_ ->
                    let
                        ( after, _, events ) =
                            edit addedPart
                    in
                    Expect.all
                        [ \m -> Expect.equal (undoDepth addedPart + 1) (undoDepth m)
                        , \m -> check (Maybe.map .matrix (List.head (Core.getPlacements m)))
                        , \_ -> Expect.equal True (List.any isDocChanged events)
                        ]
                        after

        before =
            placementMatrix addedPart

        position =
            Maybe.map (\mat -> Mat4.transform mat (vec3 0 0 0))
    in
    describe "selection actions"
        [ oneEdit "turnSelected TurnLeft turns the part and records one entry"
            (Core.turnSelected StudioShortcuts.TurnLeft)
            (\m -> Expect.notEqual before m)
        , oneEdit "turnSelected TurnRight turns the part and records one entry"
            (Core.turnSelected StudioShortcuts.TurnRight)
            (\m -> Expect.notEqual before m)
        , oneEdit "turnSelected TipForward tips the part and records one entry"
            (Core.turnSelected StudioShortcuts.TipForward)
            (\m -> Expect.notEqual before m)
        , oneEdit "raiseSelected lifts the part one step (LDraw up is -Y)"
            Core.raiseSelected
            (\m -> Expect.equal (Just (vec3 0 -32 0)) (position m))
        , oneEdit "lowerSelected lowers the part one step"
            Core.lowerSelected
            (\m -> Expect.equal (Just (vec3 0 -16 0)) (position m))
        , test "left then right turns back to the start" <|
            \_ ->
                addedPart
                    |> Core.turnSelected StudioShortcuts.TurnLeft
                    |> (\( m, _, _ ) -> Core.turnSelected StudioShortcuts.TurnRight m)
                    |> (\( m, _, _ ) -> placementMatrix m)
                    |> Maybe.map Mat4.toRecord
                    |> Maybe.map (\r -> [ r.m11, r.m13, r.m31, r.m33 ] |> List.map (\v -> toFloat (round (v * 1000)) / 1000))
                    |> Expect.equal (Maybe.map (Mat4.toRecord >> (\r -> [ r.m11, r.m13, r.m31, r.m33 ])) before)
        , test "the actions do nothing during a drag" <|
            \_ ->
                let
                    dragging =
                        draggingModel addedPart

                    ( after, _, _ ) =
                        Core.raiseSelected dragging
                in
                Expect.equal dragging.history after.history
        , test "the actions do nothing without a selection" <|
            \_ ->
                let
                    empty =
                        Core.clearSelection addedPart

                    ( after, _, _ ) =
                        Core.turnSelected StudioShortcuts.TurnLeft empty
                in
                Expect.equal empty.history after.history
        ]


{-| One brick at `position`, selected, with the camera aimed at its top so the
centre pixel (400, 300) is on it.
-}
aimedAt : Vec3 -> Core.Model
aimedAt position =
    let
        model =
            loadedAt [ position ]

        firstId =
            Document.placements model.document |> List.head |> Maybe.map .id |> Maybe.withDefault 0

        vp =
            model.viewport

        cam =
            vp.camera
    in
    { model
        | viewport = { vp | camera = { cam | target = Gizmo.toRender position } }
        , selection = Set.singleton firstId
    }


{-| The camera slides onto a part the user clicks, and only then.
-}
pivotSuite : Test
pivotSuite =
    let
        send msg model =
            Core.update (Core.ViewportMsg msg) model |> (\( m, _, _ ) -> m)

        clickAt ( x, y ) model =
            model
                |> send (Viewport.MouseButtonDown x y 0 False False)
                |> send (Viewport.MouseUp x y)
                |> (\m -> settle ( m, Cmd.none, [] ))

        clickThird model =
            clickAt (screenOf model (vec3 80 0 0)) model

        orientation m =
            ( m.viewport.camera.azimuth, m.viewport.camera.elevation, m.viewport.camera.distance )

        unmoved before after =
            Expect.equal ( before.viewport.camera.target, orientation before ) ( after.viewport.camera.target, orientation after )
    in
    describe "the camera follows a clicked part"
        [ test "clicking a part slides the view onto it without turning or zooming" <|
            \_ ->
                let
                    model =
                        loadedBricks

                    clicked =
                        clickThird model
                in
                Expect.all
                    [ \m -> Expect.equal (Set.singleton 2) m.selection
                    , \m -> Expect.within (Expect.Absolute 1.0e-3) 80 (Vec3.getX m.viewport.camera.target)
                    , \m -> Expect.equal (orientation model) (orientation m)
                    , \m ->
                        Vec3.distance
                            (Vec3.add (Camera.position model.viewport.camera) (Vec3.sub m.viewport.camera.target model.viewport.camera.target))
                            (Camera.position m.viewport.camera)
                            |> Expect.within (Expect.Absolute 1.0e-6) 0
                    ]
                    clicked
        , test "deselecting keeps the pivot; Fit View recentres" <|
            \_ ->
                let
                    model =
                        loadedBricks

                    selected =
                        clickThird model

                    ( cleared, _, _ ) =
                        Core.update Core.ClearSelection selected

                    ( fitted, _, _ ) =
                        Core.update Core.FitView selected
                in
                Expect.all
                    [ \_ -> Expect.equal selected.viewport.camera.target cleared.viewport.camera.target
                    , \_ -> Expect.equal model.viewport.camera.target fitted.viewport.camera.target
                    ]
                    ()
        , test "a host selection change does not move the camera" <|
            \_ ->
                Core.update (Core.Select (Set.singleton 2)) loadedBricks
                    |> settle
                    |> unmoved loadedBricks
        , test "selectAll does not move the camera" <|
            \_ ->
                ( Core.selectAll loadedBricks, Cmd.none, [] )
                    |> settle
                    |> unmoved loadedBricks
        , test "adding a part does not move a camera the user has moved" <|
            \_ ->
                let
                    manual =
                        manualCamera loadedBricks
                in
                Core.addPart "3001.dat" 14 (vec3 200 0 0) manual
                    |> settle
                    |> unmoved manual
        , test "adding a part while the camera still auto-fits re-frames the whole model" <|
            \_ ->
                let
                    ( fitted, _ ) =
                        Core.fitView (Core.addPart "3001.dat" 14 (vec3 400 0 0) loadedBricks |> settle)
                in
                Core.addPart "3001.dat" 14 (vec3 400 0 0) loadedBricks
                    |> settle
                    |> Expect.all
                        [ \m -> Expect.equal Viewport.CameraAutoFit m.viewport.cameraMode
                        , \m -> Expect.equal fitted.viewport.camera m.viewport.camera
                        , \m -> Expect.notEqual loadedBricks.viewport.camera.target m.viewport.camera.target
                        ]
        , test "a keyboard move, a turn and undo do not move the camera" <|
            \_ ->
                let
                    model =
                        { loadedBricks | selection = Set.singleton 2 }

                    after =
                        model
                            |> Core.raiseSelected
                            |> (\( m, _, _ ) -> Core.turnSelected StudioShortcuts.TurnLeft m)
                            |> (\( m, _, _ ) -> Core.undo m)
                            |> settle
                in
                Expect.all
                    [ \m -> Expect.notEqual model.history m.history
                    , unmoved model
                    ]
                    after
        , test "no re-pivot during a drag, nor on the drop" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -24 0, vec3 100 -24 0 ]

                    ( moved, _, _ ) =
                        moveTo (screenOf model (vec3 40 0 0)) model

                    dropped =
                        Core.update (Core.ViewportMsg (Viewport.MouseUp 0 0)) moved |> settle
                in
                Expect.all
                    [ \_ -> Expect.equal model.viewport.camera.target moved.viewport.camera.target
                    , \_ -> Expect.equal Nothing dropped.drag
                    , \_ -> Expect.notEqual (firstPosition model) (firstPosition dropped)
                    , \_ -> Expect.equal model.viewport.camera.target dropped.viewport.camera.target
                    ]
                    ()
        ]


{-| Hosts placing parts themselves: explicit-origin adds, drags that keep their
height, and free spots for new parts.
-}
placementSuite : Test
placementSuite =
    let
        added =
            loadedAt []
                |> Core.addPart "3001.dat" 14 (vec3 0 0 0)
                |> (\( m, _, _ ) -> m)

        positions model =
            Core.getPlacements model |> List.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))

        -- The test geometry: a 20 x 24 x 20 box hanging below the origin.
        boxAt p =
            { min = Vec3.add p (vec3 -10 0 -10), max = Vec3.add p (vec3 10 24 10) }

        clashes model =
            let
                boxes =
                    List.map boxAt (positions model)
            in
            List.indexedMap
                (\i a -> List.indexedMap (\j b -> i < j && DragPlane.boxesOverlap a b) boxes)
                boxes
                |> List.concat
                |> List.any identity

        addFree model =
            Core.addPart "3001.dat" 14 (Core.freeSpotFor "3001.dat" model) model |> (\( m, _, _ ) -> m)

        raised =
            Core.addPartAt "3001.dat" 14 (Mat4.makeTranslate3 0 -60 0) (loadedAt [])
                |> (\( m, _, _ ) -> m)
    in
    describe "host placement"
        [ test "addPartAt keeps the given origin (no ground resting) and selects the part" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.equal [ vec3 0 -60 0 ] (positions m)
                    , \m -> Expect.equal 1 (Core.selectionCount m)
                    , \m -> Expect.equal True (Core.canUndo m)
                    ]
                    raised
        , test "addPart still rests on the ground" <|
            \_ -> positions added |> Expect.equal [ vec3 0 -24 0 ]
        , test "with drag settling off, a horizontal body drag keeps the press-time height" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -90 0, vec3 0 -24 0 ] |> Core.setDragSettles False
                in
                model
                    |> moveTo
                        (Gizmo.project model.viewport.camera model.viewport.width model.viewport.height (vec3 40 90 0)
                            |> Maybe.withDefault ( -1, -1 )
                        )
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> ( round (Vec3.getX p), round (Vec3.getY p) ))
                    |> Expect.equal (Just ( 40, -90 ))
        , test "with drag settling on (default), the same drag settles onto the support" <|
            \_ ->
                grabbedAt [ vec3 0 -90 0, vec3 0 -24 0 ]
                    |> moveTo ( 400, 300 )
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> toFloat (round (Vec3.getY p)))
                    |> Expect.equal (Just -48)
        , test "the first free spot in an empty scene is under the camera target, on the ground" <|
            \_ ->
                let
                    model =
                        loadedAt []

                    target =
                        model.viewport.camera.target

                    spot =
                        Core.freeSpotFor "3001.dat" model
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.5) (Vec3.getX target) (Vec3.getX spot)
                    , \_ -> Expect.within (Expect.Absolute 0.5) -(Vec3.getZ target) (Vec3.getZ spot)
                    , \_ -> Expect.equal 0 (Vec3.getY spot)
                    ]
                    ()
        , test "parts added at free spots never overlap, and stay next to each other" <|
            \_ ->
                let
                    model =
                        List.foldl (\_ m -> addFree m) added (List.range 1 6)
                in
                Expect.all
                    [ \m -> Expect.equal 7 (List.length (Core.getPlacements m))
                    , \m -> Expect.equal False (clashes m)
                    , \m ->
                        positions m
                            |> List.map (\p -> max (abs (Vec3.getX p)) (abs (Vec3.getZ p)))
                            |> List.maximum
                            |> Maybe.withDefault 0
                            |> Expect.atMost 200
                    ]
                    model
        , test "the free spot starts next to the selection" <|
            \_ ->
                let
                    model =
                        loadedAt [ vec3 200 -24 0 ] |> (\m -> { m | selection = Set.singleton 0 })

                    spot =
                        Core.freeSpotFor "3001.dat" model
                in
                Vec3.distance spot (vec3 200 0 0) |> Expect.within (Expect.Absolute 0.5) 20
        , test "freeSpotForPlacement keeps the given height" <|
            \_ ->
                Core.freeSpotForPlacement "3001.dat" (Mat4.makeTranslate3 0 -60 0) raised
                    |> (\m -> Mat4.transform m (vec3 0 0 0))
                    |> Expect.all
                        [ \p -> Expect.equal -60 (Vec3.getY p)
                        , \p -> Expect.equal False (DragPlane.boxesOverlap (boxAt p) (boxAt (vec3 0 -60 0)))
                        ]
        ]


{-| The same model with a camera the user has moved (no auto-fit).
-}
manualCamera : Core.Model -> Core.Model
manualCamera model =
    let
        vp =
            model.viewport
    in
    { model | viewport = { vp | cameraMode = Viewport.CameraManual } }


{-| Three 3001 bricks at x = 0, 40, 80 (ids 0..2 in placement order), loaded
with real (tiny) geometry so the viewport has bounds and a scene.
-}
loadedBricks : Core.Model
loadedBricks =
    loadedAt [ vec3 0 -24 0, vec3 40 -24 0, vec3 80 -24 0 ]


loadedAt : List Vec3 -> Core.Model
loadedAt positions =
    let
        doc =
            List.foldl
                (\pos d -> Document.addPlacement "" { part = "3001.dat", color = 4, matrix = Mat4.makeTranslate pos } d |> Tuple.first)
                Document.empty
                positions

        ( model0, _ ) =
            Core.init
                { ldrawBase = "/ldraw"
                , ldrawFallbackBase = ""
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = True
                , document = Document.empty
                , gridStep = Just 1
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }

        geometry =
            Parser.parseFile "4 16 -10 0 -10 10 0 -10 10 0 10 -10 0 10\n4 16 -10 24 -10 10 24 -10 10 24 10 -10 24 10"

        vp =
            model0.viewport

        ( loadedViewport, _, _ ) =
            Viewport.loadDocument doc { vp | partCache = Dict.insert "3001.dat" (Loaded geometry) vp.partCache }
    in
    { model0 | viewport = loadedViewport, document = doc }


{-| Bricks loaded at `positions`, the first one grabbed at its top centre (render
space) as a body drag with the centre pixel as the press point. Alt is held so
the stud solver stays out of the measurements.
-}
grabbedAt : List Vec3 -> Core.Model
grabbedAt positions =
    let
        model =
            loadedAt positions

        firstId =
            Document.placements model.document |> List.head |> Maybe.map .id |> Maybe.withDefault 0

        lead =
            List.head positions |> Maybe.withDefault (vec3 0 0 0)

        grab =
            -- The brick's top face: its origin (geometry spans Y 0..24 below it).
            Gizmo.toRender lead

        -- Aim the camera at the grab point so the centre pixel is on it.
        vp =
            model.viewport

        cam =
            vp.camera
    in
    { model
        | viewport = Viewport.captureDrag { vp | camera = { cam | target = grab }, mousePressed = True }
        , selection = Set.singleton firstId
        , altHeld = True
        , drag =
            Just
                { kind = PlaneDrag grab
                , plane = Nothing
                , ids = [ firstId ]
                , start = ( 400, 300 )
                , lastPoint = ( 400, 300 )
                , original = model.document
                , amount = 0
                , elevationOffset = 0
                , axisLock = Nothing
                , modeAnchor = ( 400, 300 )
                , baseElevation = 0
                , shiftToggle = Nothing
                }
    }


{-| Run any camera glide (`Viewport.pivotOn`) to the end.
-}
settle : ( Core.Model, Cmd Core.Msg, List Core.Event ) -> Core.Model
settle ( model, _, _ ) =
    let
        frame ms m =
            Core.update (Core.ViewportMsg (Viewport.AnimationFrame (Time.millisToPosix ms))) m
                |> (\( next, _, _ ) -> next)
    in
    model |> frame 0 |> frame 1000


moveTo : ( Float, Float ) -> Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )
moveTo ( x, y ) model =
    Core.update (Core.ViewportMsg (Viewport.MouseMove x y)) model


{-| The pixel where a render-space point on the grab plane (Y = 24) appears;
`point`'s own Y is ignored.
-}
screenOf : Core.Model -> Vec3 -> ( Float, Float )
screenOf model point =
    Gizmo.project model.viewport.camera model.viewport.width model.viewport.height (Vec3.setY 24 point)
        |> Maybe.withDefault ( -1, -1 )


firstPosition : Core.Model -> Maybe Vec3
firstPosition model =
    Core.getPlacements model
        |> List.head
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))


{-| A model with one 3001 brick added and selected.
-}
addedPart : Core.Model
addedPart =
    let
        config =
            { ldrawBase = "/ldraw"
            , ldrawFallbackBase = ""
            , defaultModelUrl = ""
            , initialHash = ""
            , supportsPointerEvents = True
            , document = Document.empty
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( model0, _ ) =
            Core.init config

        ( model1, _, _ ) =
            Core.addPart "3001.dat" 14 (vec3 0 0 0) model0
    in
    model1


{-| The model in the middle of a gizmo drag that has moved the selection 40 LDU.
-}
draggingModel : Core.Model -> Core.Model
draggingModel model1 =
    let
        ( movedDoc, _ ) =
            Edit.apply (Edit.Translate (Set.toList model1.selection) (vec3 40 0 0)) model1.document
    in
    { model1
        | document = movedDoc
        , viewport = Viewport.captureDrag model1.viewport
        , drag =
            Just
                { kind = PlaneDrag (vec3 0 0 0)
                , plane = Nothing
                , ids = Set.toList model1.selection
                , start = ( 0, 0 )
                , lastPoint = ( 0, 0 )
                , original = model1.document
                , amount = 0
                , elevationOffset = 0
                , axisLock = Nothing
                , modeAnchor = ( 0, 0 )
                , baseElevation = 0
                , shiftToggle = Nothing
                }
    }


placementMatrix : Core.Model -> Maybe Mat4
placementMatrix model =
    Core.getPlacements model
        |> List.head
        |> Maybe.map .matrix


pressOn : String -> String -> KeyEvent
pressOn tag key =
    { key = key
    , shift = False
    , ctrl = False
    , meta = False
    , alt = False
    , repeat = False
    , targetTag = tag
    , targetEditable = False
    , targetRole = ""
    }


isDocChanged : Core.Event -> Bool
isDocChanged ev =
    case ev of
        Core.DocumentChanged _ ->
            True

        _ ->
            False


{-| `DragPartsOrPan`: a plain left drag on empty canvas pans, like Shift + drag.
-}
emptyDragPanSuite : Test
emptyDragPanSuite =
    let
        withMode mode =
            aimedAt (vec3 0 -24 0) |> Core.setGizmoStyle Core.NoGizmo |> Core.setDragMode mode

        send msg model =
            Core.update (Core.ViewportMsg msg) model |> (\( m, _, _ ) -> m)

        -- (10, 10) is empty canvas: the brick is in the middle of the view.
        dragEmpty mode =
            let
                pressed =
                    send (Viewport.MouseButtonDown 10 10 0 False False) (withMode mode)
            in
            ( pressed, send (Viewport.MouseMove 70 40) pressed )

        touchEmpty mode =
            let
                pressed =
                    send (Viewport.PointerTouchStart { id = 1, x = 10, y = 10 }) (withMode mode)
            in
            ( pressed, send (Viewport.PointerTouchMove { id = 1, x = 70, y = 40 }) pressed )

        pans ( before, after ) =
            Expect.all
                [ \_ -> Expect.notEqual before.viewport.camera.target after.viewport.camera.target
                , \_ -> Expect.within (Expect.Absolute 1.0e-9) before.viewport.camera.azimuth after.viewport.camera.azimuth
                , \_ -> Expect.equal Nothing after.drag
                ]
                ()

        orbits ( before, after ) =
            Expect.greaterThan 1.0e-6 (abs (before.viewport.camera.azimuth - after.viewport.camera.azimuth))
    in
    describe "drag on empty canvas pans (DragPartsOrPan)"
        [ test "the mouse pans" <|
            \_ -> pans (dragEmpty (Core.DragPartsOrPan Core.OneFingerOrbits))
        , test "the default mode still orbits" <|
            \_ -> orbits (dragEmpty Core.DragPartsOrOrbit)
        , test "a click on empty canvas still clears the selection" <|
            \_ ->
                withMode (Core.DragPartsOrPan Core.OneFingerOrbits)
                    |> send (Viewport.MouseButtonDown 10 10 0 False False)
                    |> send (Viewport.MouseUp 10 10)
                    |> Core.hasSelection
                    |> Expect.equal False
        , test "a press on the part still picks it up" <|
            \_ ->
                withMode (Core.DragPartsOrPan Core.OneFingerOrbits)
                    |> send (Viewport.MouseButtonDown 400 300 0 False False)
                    |> .drag
                    |> Maybe.map .kind
                    |> (\kind ->
                            case kind of
                                Just (PlaneDrag _) ->
                                    Expect.pass

                                _ ->
                                    Expect.fail "the press did not pick up the part"
                       )
        , test "OneFingerOrbits: one finger on empty canvas orbits" <|
            \_ -> orbits (touchEmpty (Core.DragPartsOrPan Core.OneFingerOrbits))
        , test "OneFingerPans: one finger on empty canvas pans" <|
            \_ -> pans (touchEmpty (Core.DragPartsOrPan Core.OneFingerPans))
        ]


{-| Shift in `DragPartsOrPan` hosts (Play, Contraptions): a Shift + press on a
part grabs it, holding Shift during a body drag moves on a 1 LDU grid, and a
Shift + press on empty canvas pans. The default mode keeps Shift + drag as a pan.
-}
shiftFineSuite : Test
shiftFineSuite =
    let
        shiftKey =
            let
                keyDown =
                    pressOn "BODY" "Shift"
            in
            { keyDown | shift = True }

        shiftDown model =
            Core.update (Core.ViewportMsg (Viewport.KeyPressed shiftKey)) model
                |> (\( m, _, _ ) -> m)

        shiftUp model =
            Core.update (Core.KeyUp "Shift") model
                |> (\( m, _, _ ) -> m)

        send msg model =
            Core.update (Core.ViewportMsg msg) model |> (\( m, _, _ ) -> m)

        -- The brick at the origin, grabbed at its top centre on a 20 LDU grid. The
        -- cursor aims at X = 37: a 1 LDU step lands there, the 20 LDU grid on 40.
        grabbed mode =
            grabbedAt [ vec3 0 -24 0 ]
                |> Core.setGridStep 20
                |> Core.setDragMode mode

        xAfterMove model =
            moveTo (screenOf model (vec3 37 0 -52)) model
                |> (\( m, _, _ ) -> firstPosition m)
                |> Maybe.map Vec3.getX

        xIs expected model =
            case xAfterMove model of
                Just x ->
                    Expect.within (Expect.Absolute 0.51) expected x

                Nothing ->
                    Expect.fail "the part vanished"

        -- The brick aimed at, selected, in a host's mode.
        aimed mode =
            aimedAt (vec3 0 -24 0)
                |> Core.setGizmoStyle Core.NoGizmo
                |> Core.setDragMode mode

        partsOrPan =
            Core.DragPartsOrPan Core.OneFingerOrbits
    in
    describe "Shift fine moves (DragPartsOrPan)"
        [ test "Shift held before a body drag moves on a 1 LDU grid" <|
            \_ ->
                grabbed partsOrPan
                    |> shiftDown
                    |> xIs 37
        , test "Shift pressed mid-drag applies to the next move" <|
            \_ ->
                grabbed partsOrPan
                    |> shiftDown
                    |> xIs 37
        , test "releasing Shift returns to the 20 LDU grid" <|
            \_ ->
                grabbed partsOrPan
                    |> shiftDown
                    |> shiftUp
                    |> xIs 40
        , test "the default mode keeps the 20 LDU grid with Shift held" <|
            \_ ->
                grabbed Core.DragPartsOrOrbit
                    |> shiftDown
                    |> xIs 40
        , test "a Shift + press on a part grabs it" <|
            \_ ->
                let
                    pressed =
                        aimed partsOrPan
                            |> send (Viewport.MouseButtonDown 400 300 0 True False)
                in
                Expect.all
                    [ \m -> Expect.equal True (m.drag /= Nothing)
                    , \m -> Expect.equal False m.viewport.mousePanDragging
                    ]
                    pressed
        , test "a Shift + press on empty canvas pans" <|
            \_ ->
                let
                    before =
                        aimed partsOrPan

                    after =
                        before
                            |> send (Viewport.MouseButtonDown 10 10 0 True False)
                            |> send (Viewport.MouseMove 70 40)
                in
                Expect.all
                    [ \_ -> Expect.equal Nothing after.drag
                    , \_ -> Expect.notEqual before.viewport.camera.target after.viewport.camera.target
                    ]
                    ()
        , test "the default mode turns a Shift + press into a pan, not a grab" <|
            \_ ->
                aimed Core.DragPartsOrOrbit
                    |> send (Viewport.MouseButtonDown 400 300 0 True False)
                    |> .drag
                    |> Expect.equal Nothing
        , test "a Shift + click on a part toggles it in the selection" <|
            \_ ->
                let
                    base =
                        aimed partsOrPan

                    firstId =
                        Core.getPlacements base |> List.head |> Maybe.map .id |> Maybe.withDefault -1

                    clicked model =
                        model
                            |> send (Viewport.MouseButtonDown 400 300 0 True False)
                            |> send (Viewport.MouseUp 400 300)
                            |> .selection
                in
                Expect.all
                    [ \_ -> Expect.equal (Set.singleton firstId) (clicked { base | selection = Set.empty })
                    , \_ -> Expect.equal Set.empty (clicked base)
                    ]
                    ()
        ]


{-| New parts rest on the ground, and body drags rest parts on their support.
-}
restingSuite : Test
restingSuite =
    let
        added =
            loadedAt []
                |> Core.addPart "3001.dat" 14 (vec3 0 0 0)
                |> (\( m, _, _ ) -> m)

        pressCentre model =
            Core.update (Core.ViewportMsg (Viewport.MouseButtonDown 400 300 0 False False)) model
                |> (\( m, _, _ ) -> { m | altHeld = True })

        aimAt point model =
            let
                vp =
                    model.viewport

                cam =
                    vp.camera
            in
            { model | viewport = { vp | camera = { cam | target = Gizmo.toRender point } } }
                |> Core.setGizmoStyle Core.NoGizmo

        project model renderY point =
            Gizmo.project model.viewport.camera model.viewport.width model.viewport.height (Vec3.setY renderY point)
                |> Maybe.withDefault ( -1, -1 )
    in
    describe "resting on the ground and on supports"
        [ test "addPart puts a brick's bottom on the ground (origin Y = -24)" <|
            \_ -> firstPosition added |> Expect.equal (Just (vec3 0 -24 0))
        , test "addPart's Y is the height the bottom rests at" <|
            \_ ->
                added
                    |> Core.addPart "3001.dat" 14 (vec3 40 -24 0)
                    |> (\( m, _, _ ) -> Core.getPlacements m |> List.reverse |> List.head)
                    |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0))
                    |> Expect.equal (Just (vec3 40 -48 0))
        , test "addPartCentered puts the part under the camera target (render Z is −LDraw Z)" <|
            \_ ->
                let
                    model =
                        loadedAt []

                    vp =
                        model.viewport

                    cam =
                        vp.camera
                in
                { model | viewport = { vp | camera = { cam | target = vec3 40 0 100 } } }
                    |> Core.addPartCentered "3001.dat" 14
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> ( Vec3.getX p, Vec3.getZ p ))
                    |> Expect.equal (Just ( 40, -100 ))
        , test "addPartCentered rests on the ground too" <|
            \_ ->
                loadedAt []
                    |> Core.addPartCentered "3001.dat" 14
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map Vec3.getY
                    |> Expect.equal (Just -24)
        , test "the first drag of a new brick does not move it vertically" <|
            \_ ->
                added
                    |> aimAt (vec3 0 -24 0)
                    |> pressCentre
                    |> moveTo ( 400, 300 )
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> toFloat (round (Vec3.getY p)))
                    |> Expect.equal (Just -24)
        , test "a floating brick settles onto its support on the first move (no remembered gap)" <|
            \_ ->
                grabbedAt [ vec3 0 -90 0, vec3 0 -24 0 ]
                    |> moveTo ( 400, 300 )
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> toFloat (round (Vec3.getY p)))
                    |> Expect.equal (Just -48)
        , test "a stacked brick slid off its support drops to the ground" <|
            \_ ->
                let
                    model =
                        grabbedAt [ vec3 0 -48 0, vec3 0 -24 0 ]
                in
                model
                    |> moveTo (project model 48 (vec3 100 0 0))
                    |> (\( m, _, _ ) -> firstPosition m)
                    |> Maybe.map (\p -> toFloat (round (Vec3.getY p)))
                    |> Expect.equal (Just -24)
        ]
