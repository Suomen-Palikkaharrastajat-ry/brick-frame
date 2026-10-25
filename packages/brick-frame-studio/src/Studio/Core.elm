module Studio.Core exposing
    ( Config
    , Drag
    , DragMode(..)
    , Event(..)
    , GizmoStyle(..)
    , Model
    , Msg(..)
    , OneFinger(..)
    , addPart
    , addPartAt
    , addPartCentered
    , canRedo
    , canUndo
    , cancelDrag
    , clearSelection
    , currentArcHandles
    , currentHandles
    , deleteSelection
    , fitView
    , freeSpotFor
    , freeSpotForPlacement
    , geometryFlattenFailed
    , geometryFlattened
    , getPlacements
    , hasSelection
    , init
    , lowerSelected
    , partBox
    , raiseSelected
    , redo
    , resetDocument
    , rotateSelection
    , selectAll
    , selectionCount
    , setCollisionMode
    , setDragMode
    , setDragSettles
    , setGearAlignment
    , setGizmoStyle
    , setGridStep
    , setHelpVisible
    , setRotateStep
    , setSnapDistance
    , setTool
    , subscriptions
    , turnSelected
    , undo
    , update
    , view
    , viewWithExtra
    )

{-| Reusable 3D studio TEA component.

Encapsulates an LDraw `Document`, orbit/pan/zoom `Viewport`, selection, 3D gizmo
manipulation, stud/hole/axle snapping, undo/redo history, and overlay rendering.
It communicates effects across the boundary via `Event`s returned from `update`.

-}

import Browser.Events
import Dict exposing (Dict)
import EmbeddedParts
import Html exposing (Html, div)
import Html.Attributes as Attr
import Json.Decode as Decode
import LDraw.Connectivity as Connectivity
    exposing
        ( BoundingBox3D
        , CollisionMode(..)
        , SnapCandidate
        , SnapGuide(..)
        )
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Resolve as Resolve
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera
import Render.Pick as Pick
import Render.Style as Style
import Set exposing (Set)
import Studio.DragPlane as DragPlane exposing (WorkingPlane)
import Studio.Edit as Edit exposing (Axis(..), Command(..), History, NewPart)
import Studio.GearAlign as GearAlign
import Studio.Gizmo as Gizmo exposing (ArcHandle, DragHandle(..), Handle)
import Studio.Overlay as Overlay
import Studio.Shortcuts as StudioShortcuts exposing (Tool(..))
import Studio.Transform as Transform
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Shortcuts as ViewportShortcuts
import WebGL exposing (Entity)


{-| Initialization configuration for the Core studio component.
-}
type alias Config =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , defaultModelUrl : String
    , initialHash : String
    , supportsPointerEvents : Bool
    , document : Document
    , gridStep : Maybe Int
    , rotateStep : Maybe Int
    , snapDistance : Maybe Float
    , collisionMode : Maybe CollisionMode
    }


{-| An in-progress gizmo drag or surface grab.

`plane` is the working plane a body drag (`PlaneDrag`) slides on, fixed at press
time (see `Studio.DragPlane`); `Nothing` for handle drags, and a body drag
without one picks it from the camera on its first move.

-}
type alias Drag =
    { kind : DragHandle
    , plane : Maybe WorkingPlane
    , ids : List Int
    , start : ( Float, Float )
    , lastPoint : ( Float, Float )
    , original : Document
    , amount : Float
    , elevationOffset : Float
    , axisLock : Maybe Axis
    , modeAnchor : ( Float, Float )
    , baseElevation : Float
    }


{-| What dragging on the canvas does.

  - `DragPartsOrOrbit` — a part under the pointer is picked up, empty canvas
    orbits the camera. The default.
  - `DragPartsOnly` — a part under the pointer is picked up, empty canvas does
    nothing (a click there clears the selection). The host provides on-screen
    camera controls instead. Shift still pans, and two fingers still pan and
    pinch.
  - `DragPartsOrPan oneFinger` — a part under the pointer is picked up; a
    plain left drag on empty canvas pans the camera exactly like Shift + drag
    (`Viewport.panCurrentGesture`), so the mouse needs no modifier: left-drag a
    part to move it, left-drag empty space to pan, right-drag to orbit, wheel to
    zoom. A click on empty canvas still clears the selection. A single finger
    on empty canvas does what `oneFinger` says (see `OneFinger`); two fingers
    pan and pinch as always.

-}
type DragMode
    = DragPartsOrOrbit
    | DragPartsOnly
    | DragPartsOrPan OneFinger


{-| What one finger dragged on empty canvas does in `DragPartsOrPan` mode.

  - `OneFingerOrbits` — orbit, as in `DragPartsOrOrbit`: touch has no right
    button, so a host without on-screen orbit controls (Brick Play) keeps it.
  - `OneFingerPans` — pan, like the mouse; for a host with its own orbit
    control (Brick Contraptions' camera globe).

A tap without travel clears the selection either way.

-}
type OneFinger
    = OneFingerOrbits
    | OneFingerPans


{-| Which gizmo handles the selection shows (and which a press can grab).

  - `NoGizmo`: no arrows and no rings, neither drawn nor hit-tested. A drag on a
    part still picks it up and moves it on the view-relative working plane
    (`Studio.DragPlane`); turning and raising are left to the keyboard and to
    host buttons (`turnSelected`, `raiseSelected`, `lowerSelected`). The yellow
    selection box is still drawn — it marks the selection, not an axis.
  - `MoveGizmo`: the move tool shows the three axis arrows; the rotate tool the
    rotation rings. The default.
  - `MoveAndRotateGizmo`: the move tool shows the rotation rings around the move
    arrows, so one tool both moves and rotates.

-}
type GizmoStyle
    = NoGizmo
    | MoveGizmo
    | MoveAndRotateGizmo


{-| Core studio component state.

`gizmoStyle` picks the handles drawn on the selection (see `GizmoStyle`).

The camera orbits around the part the user last clicked or tapped: an explicit
click/tap selection of a part slides the view onto it (`Viewport.pivotOn` — a
translation glide, no rotation or zoom). Nothing else moves the pivot: not adding
a part, keyboard or action-bar moves, drag drops, undo/redo or `selectAll`.
Deselecting keeps the last pivot; `fitView` recentres on the model.

`dragSettles` (default `True`, see `setDragSettles`) makes a horizontal-plane
body drag rest the part on whatever supports it; with `False` the part keeps
its press-time height.

`fitAfterAdd` is the part just added (`addPartAt`) while the camera was still
auto-fitting (`Viewport.CameraAutoFit`: the user has not moved it since the start
or the last Fit View): once that part is built into the scene the view re-frames
the whole model, so a model built from the dock stays in view. Any camera move
(or a click on a part, which glides onto it) makes the camera manual and ends this.

-}
type alias Model =
    { viewport : Viewport.Model
    , document : Document
    , history : History
    , selection : Set Int
    , tool : Tool
    , gridStep : Int
    , rotateStep : Int
    , snapDistance : Float
    , collisionMode : CollisionMode
    , activeSnap : Maybe SnapCandidate
    , collidingPlacements : Set Int
    , drag : Maybe Drag
    , altHeld : Bool
    , shiftHeld : Bool
    , inFlightRotationStep : Int
    , snapRotationStep : Int
    , activeColour : Int
    , dragMode : DragMode
    , gizmoStyle : GizmoStyle
    , helpVisible : Bool
    , dragSettles : Bool
    , fitAfterAdd : Maybe Int
    , gearAlignment : Bool
    }


{-| Messages handled by the studio component.
-}
type Msg
    = ViewportMsg Viewport.Msg
    | Select (Set Int)
    | SelectOnly Int
    | ToggleSelect Int
    | ClearSelection
    | SelectAll
    | DeleteSelection
    | Undo
    | Redo
    | RotateSelection Axis Int
    | SetColorSelection Int
    | SetTool Tool
    | SetGridStep Int
    | SetRotateStep Int
    | SetSnapDistance Float
    | SetCollisionMode CollisionMode
    | KeyUp String
    | FitView
    | ResetDocument Document
    | AddPart String Int Vec3
    | AddPartCentered String Int
    | NudgeSelection StudioShortcuts.Nudge
    | GeometryFlattened String
    | GeometryFlattenFailed String


{-| Events returned by `update` to the host application.
-}
type Event
    = FlattenRequested String
    | CameraSettled
    | DocumentChanged Document
    | SelectionChanged (Set Int)
    | KeyAccepted ViewportShortcuts.KeyEvent


grabRadius : Float
grabRadius =
    16


{-| Initialize the studio component.
-}
init : Config -> ( Model, Cmd Msg )
init config =
    let
        viewportConfig =
            { ldrawBase = config.ldrawBase
            , ldrawFallbackBase = config.ldrawFallbackBase
            , defaultModelUrl = config.defaultModelUrl
            , initialHash = config.initialHash
            , supportsPointerEvents = config.supportsPointerEvents
            , useWindowResize = False
            , renderStyle = Style.defaultStyle
            , partCache = EmbeddedParts.cache
            }

        ( initialViewport, viewportCmd ) =
            Viewport.init viewportConfig

        ( loadedViewport, loadCmd, loadEvents ) =
            if List.isEmpty (Document.placements config.document) then
                ( initialViewport, Cmd.none, [] )

            else
                Viewport.loadDocument config.document initialViewport

        initialModel =
            { viewport = loadedViewport
            , document = Document.clearTouched config.document
            , history = Edit.emptyHistory
            , selection = Set.empty
            , tool = MoveTool
            , gridStep = Maybe.withDefault 20 config.gridStep
            , rotateStep = Maybe.withDefault 90 config.rotateStep
            , snapDistance = Maybe.withDefault 24.0 config.snapDistance
            , collisionMode = Maybe.withDefault Connectivity.CollisionSoft config.collisionMode
            , activeSnap = Nothing
            , collidingPlacements = Set.empty
            , drag = Nothing
            , altHeld = False
            , shiftHeld = False
            , inFlightRotationStep = 0
            , snapRotationStep = 0
            , activeColour = 14
            , dragMode = DragPartsOrOrbit
            , helpVisible = False
            , gizmoStyle = MoveGizmo
            , dragSettles = True
            , fitAfterAdd = Nothing
            , gearAlignment = False
            }

        ( finalModel, extraCmds, _ ) =
            handleViewportEvents loadEvents ( initialModel, [], [] )
    in
    ( finalModel
    , Cmd.batch
        [ Cmd.map ViewportMsg viewportCmd
        , Cmd.map ViewportMsg loadCmd
        , Cmd.batch extraCmds
        ]
    )


{-| Query whether undo is available.
-}
canUndo : Model -> Bool
canUndo model =
    Edit.canUndo model.history


{-| Query whether redo is available.
-}
canRedo : Model -> Bool
canRedo model =
    Edit.canRedo model.history


{-| Query whether any placements are selected.
-}
hasSelection : Model -> Bool
hasSelection model =
    not (Set.isEmpty model.selection)


{-| Count how many placements are selected.
-}
selectionCount : Model -> Int
selectionCount model =
    Set.size model.selection


{-| All placements in the document.
-}
getPlacements : Model -> List Placement
getPlacements model =
    Document.placements model.document


{-| Select all placements in the document.
-}
selectAll : Model -> Model
selectAll model =
    let
        allIds =
            List.map .id (Document.placements model.document)
                |> Set.fromList
    in
    { model | selection = allIds }


{-| Clear the current selection.
-}
clearSelection : Model -> Model
clearSelection model =
    { model | selection = Set.empty }


{-| Change the active tool.
-}
setTool : Tool -> Model -> Model
setTool tool model =
    { model | tool = tool, drag = Nothing }


{-| Change what a drag on the canvas does.
-}
setDragMode : DragMode -> Model -> Model
setDragMode mode model =
    { model | dragMode = mode }


{-| Whether a body drag on the horizontal working plane settles the parts onto
their support (the highest part under their footprint, or the ground). On by
default. With `False` the dragged parts keep their press-time height, so a host
can hold parts at a chosen height — Brick Contraptions keeps gear and axle axes
on one line. Drags on the vertical camera-facing plane, the wheel lift and the
raise/lower keys are unaffected.
-}
setDragSettles : Bool -> Model -> Model
setDragSettles settles model =
    { model | dragSettles = settles }


{-| Tell the studio a host dialog (the shortcut list) is open: editing keys pause,
and every key goes to the viewport so the host can close the dialog.
-}
setHelpVisible : Bool -> Model -> Model
setHelpVisible visible model =
    { model | helpVisible = visible }


{-| Turn tooth alignment of newly meshed gears on or off. When on, a committed
move or add that meshes gears turns the rigid group of each moved gear (the
gears and axles on its axle line) so the teeth interlock, in the same undo step
(see `Studio.GearAlign`). Off by default, so a host that does not place gears on
axles (Brick Play) is unchanged. Brick Contraptions turns it on.
-}
setGearAlignment : Bool -> Model -> Model
setGearAlignment on model =
    { model | gearAlignment = on }


{-| Record a committed edit that moves or adds parts as one history entry. With
gear alignment on, the rotations that close the edit's new gear meshes are
recorded in the same `Group`, so one undo restores both the position and the
turn.
-}
recordEdit : Command -> Model -> ( Document, History )
recordEdit command model =
    if not model.gearAlignment then
        Edit.record command model.document model.history

    else
        let
            ( edited, _ ) =
                Edit.apply command model.document

            rotations =
                GearAlign.alignmentCommands model.viewport.partCache
                    { before = model.document, after = edited }
        in
        case rotations of
            [] ->
                Edit.record command model.document model.history

            _ ->
                Edit.record (Group (command :: rotations)) model.document model.history


{-| Choose which gizmo handles the selection shows; see `GizmoStyle`.
-}
setGizmoStyle : GizmoStyle -> Model -> Model
setGizmoStyle style model =
    { model | gizmoStyle = style }


{-| Change grid step (in LDU).
-}
setGridStep : Int -> Model -> Model
setGridStep step model =
    { model | gridStep = max 1 step }


{-| Change rotation step (in degrees).
-}
setRotateStep : Int -> Model -> Model
setRotateStep step model =
    { model | rotateStep = max 1 step }


{-| Change snap distance.
-}
setSnapDistance : Float -> Model -> Model
setSnapDistance dist model =
    { model | snapDistance = max 0 dist }


{-| Change collision mode.
-}
setCollisionMode : CollisionMode -> Model -> Model
setCollisionMode mode model =
    { model | collisionMode = mode }


{-| Reset the document completely.
-}
resetDocument : Document -> Model -> ( Model, Cmd Msg, List Event )
resetDocument doc model =
    let
        ( nextViewport, viewportCmd, events ) =
            Viewport.loadDocument doc model.viewport

        nextModel =
            { model
                | viewport = nextViewport
                , document = Document.clearTouched doc
                , history = Edit.emptyHistory
                , selection = Set.empty
                , drag = Nothing
                , activeSnap = Nothing
                , collidingPlacements = Set.empty
            }

        ( finalModel, extraCmds, emittedEvents ) =
            handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged Set.empty ] )
    in
    ( finalModel
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
    , emittedEvents
    )


{-| Fit view to current document contents.
-}
fitView : Model -> ( Model, Cmd Msg )
fitView model =
    let
        ( nextViewport, cmd, _ ) =
            Viewport.update Viewport.FitView model.viewport
    in
    ( { model | viewport = nextViewport }, Cmd.map ViewportMsg cmd )


{-| Delete currently selected placements.
-}
deleteSelection : Model -> ( Model, Cmd Msg, List Event )
deleteSelection model =
    if Set.isEmpty model.selection then
        ( model, Cmd.none, [] )

    else
        let
            toDelete =
                Set.toList model.selection

            ( newDoc, newHistory ) =
                Edit.record (Remove toDelete) model.document model.history

            ( nextViewport, viewportCmd, events ) =
                Viewport.applyDocument newDoc model.viewport

            nextModel =
                { model
                    | viewport = nextViewport
                    , document = Document.clearTouched newDoc
                    , history = newHistory
                    , selection = Set.empty
                    , drag = Nothing
                    , activeSnap = Nothing
                    , collidingPlacements = Set.empty
                }

            ( finalModel, extraCmds, emittedEvents ) =
                handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged Set.empty ] )
        in
        ( finalModel
        , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
        , emittedEvents
        )


{-| Undo the last edit.
-}
undo : Model -> ( Model, Cmd Msg, List Event )
undo model =
    undoInner model


undoInner : Model -> ( Model, Cmd Msg, List Event )
undoInner model =
    if not (canUndo model) then
        ( model, Cmd.none, [] )

    else
        let
            ( undoneDoc, undoneHistory ) =
                Edit.undo model.document model.history

            presentIds =
                List.map .id (Document.placements undoneDoc)
                    |> Set.fromList

            reconciledSelection =
                Set.filter (\id -> Set.member id presentIds) model.selection

            ( nextViewport, viewportCmd, events ) =
                Viewport.applyDocument undoneDoc model.viewport

            nextModel =
                { model
                    | viewport = nextViewport
                    , document = Document.clearTouched undoneDoc
                    , history = undoneHistory
                    , selection = reconciledSelection
                    , drag = Nothing
                    , activeSnap = Nothing
                    , collidingPlacements = Set.empty
                }

            ( finalModel, extraCmds, emittedEvents ) =
                handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged reconciledSelection ] )
        in
        ( finalModel
        , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
        , emittedEvents
        )


{-| Redo the last undone edit.
-}
redo : Model -> ( Model, Cmd Msg, List Event )
redo model =
    redoInner model


redoInner : Model -> ( Model, Cmd Msg, List Event )
redoInner model =
    if not (canRedo model) then
        ( model, Cmd.none, [] )

    else
        let
            ( redoneDoc, redoneHistory ) =
                Edit.redo model.document model.history

            presentIds =
                List.map .id (Document.placements redoneDoc)
                    |> Set.fromList

            reconciledSelection =
                Set.filter (\id -> Set.member id presentIds) model.selection

            ( nextViewport, viewportCmd, events ) =
                Viewport.applyDocument redoneDoc model.viewport

            nextModel =
                { model
                    | viewport = nextViewport
                    , document = Document.clearTouched redoneDoc
                    , history = redoneHistory
                    , selection = reconciledSelection
                    , drag = Nothing
                    , activeSnap = Nothing
                    , collidingPlacements = Set.empty
                }

            ( finalModel, extraCmds, emittedEvents ) =
                handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged reconciledSelection ] )
        in
        ( finalModel
        , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
        , emittedEvents
        )


{-| Rotate selected placements around their collective center.
-}
rotateSelection : Axis -> Int -> Model -> ( Model, Cmd Msg, List Event )
rotateSelection axis steps model =
    rotateSelectionBy axis (degrees (toFloat (steps * model.rotateStep))) model


{-| Rotate selected placements about their collective center by an angle in radians.
-}
rotateSelectionBy : Axis -> Float -> Model -> ( Model, Cmd Msg, List Event )
rotateSelectionBy axis angle model =
    if Set.isEmpty model.selection then
        ( model, Cmd.none, [] )

    else
        let
            selectedIds =
                Set.toList model.selection

            pivot =
                Edit.selectionCenter selectedIds model.document
                    |> Maybe.withDefault (vec3 0 0 0)

            command =
                Rotate selectedIds { pivot = pivot, axis = axis, radians = angle }

            ( newDoc, newHistory ) =
                recordEdit command model

            ( nextViewport, viewportCmd, events ) =
                Viewport.applyDocument newDoc model.viewport

            nextModel =
                { model
                    | viewport = nextViewport
                    , document = Document.clearTouched newDoc
                    , history = newHistory
                }

            ( finalModel, extraCmds, emittedEvents ) =
                handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document ] )
        in
        ( finalModel
        , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
        , emittedEvents
        )


{-| Add a new part placement resting at the specified position, and select it.

`position` (LDraw) is where the part's **bottom** goes: X and Z are the origin,
Y is the height its underside rests at (0 is the ground). LDraw parts hang down
+Y from their origin, so the origin is lifted by the part's bottom extent — a
brick added at `vec3 x 0 z` gets its origin at Y = −24 and sits on the ground,
exactly where a body drag (`Studio.DragPlane.targetPosition`) would rest it, so
the first drag does not move it vertically.

The bottom extent comes from the same local box the drag's support search uses
(instanced mesh bounds, the connectivity catalog, or the inferred connectivity
envelope). That box does not need the part's geometry to be loaded: catalog
parts carry it, and the inferred envelope's height is fixed (−4 to +24), so the
placement made before the part loads already agrees with the drag once it has.

-}
addPart : String -> Int -> Vec3 -> Model -> ( Model, Cmd Msg, List Event )
addPart partFile color position model =
    let
        bottom =
            Vec3.getY (partLocalBox (resolvedLines model) partFile model).max
    in
    addPartAt partFile color (Mat4.makeTranslate (Vec3.setY (Vec3.getY position - bottom) position)) model


{-| Add a new part with exactly this placement matrix (LDraw: the part's origin
and orientation), and select it. Unlike `addPart` nothing rests it on the
ground — for hosts that put parts at a height of their own (Brick Contraptions
puts gear and axle axes on a common line). Recorded in the history; the camera
does not move.
-}
addPartAt : String -> Int -> Mat4 -> Model -> ( Model, Cmd Msg, List Event )
addPartAt partFile color matrix model =
    let
        newPart =
            { part = partFile
            , color = color
            , matrix = matrix
            }

        ( newDoc, newHistory ) =
            recordEdit (Add [ newPart ]) model

        newPlacement =
            List.head (List.reverse (Document.placements newDoc))

        newSelection =
            case newPlacement of
                Just p ->
                    Set.singleton p.id

                Nothing ->
                    Set.empty

        ( nextViewport, viewportCmd, events ) =
            Viewport.applyDocument newDoc model.viewport

        nextModel =
            { model
                | viewport = nextViewport
                , document = Document.clearTouched newDoc
                , history = newHistory
                , selection = newSelection
                , fitAfterAdd =
                    if model.viewport.cameraMode == Viewport.CameraAutoFit then
                        Maybe.map .id newPlacement

                    else
                        Nothing
            }

        ( finalModel, extraCmds, emittedEvents ) =
            handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged newSelection ] )
    in
    ( fitAddedPart finalModel
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
    , emittedEvents
    )


{-| Re-frame the whole model once the part recorded in `fitAfterAdd` is built
into the scene, if the camera is still auto-fitting (never during a drag: the
view must stay still under the cursor); forget it as soon as the camera is manual.
-}
fitAddedPart : Model -> Model
fitAddedPart model =
    case model.fitAfterAdd of
        Nothing ->
            model

        Just id ->
            if model.viewport.cameraMode /= Viewport.CameraAutoFit then
                { model | fitAfterAdd = Nothing }

            else if
                model.drag
                    == Nothing
                    && (model.viewport.instancedScene
                            |> Maybe.map (.placements >> List.any (\p -> p.source == id))
                            |> Maybe.withDefault False
                       )
            then
                let
                    ( fitted, _, _ ) =
                        Viewport.update Viewport.FitView model.viewport
                in
                { model | viewport = fitted, fitAfterAdd = Nothing }

            else
                model


{-| Add a new part under the camera target, resting on the ground (see `addPart`).
-}
addPartCentered : String -> Int -> Model -> ( Model, Cmd Msg, List Event )
addPartCentered partFile color model =
    let
        camTarget =
            model.viewport.camera.target

        groundPos =
            -- Render space negates Z as well as Y: LDraw Z is −render Z.
            vec3 (Vec3.getX camTarget) 0 -(Vec3.getZ camTarget)
    in
    addPart partFile color groundPos model


{-| Where to add a new `part` so that it rests on the ground without overlapping
anything already placed: pass the result to `addPart`.

The search starts at the selection's centre (so a new piece lands next to the
selected one) or, with nothing selected, under the camera target (so the first
piece lands in the middle of the view), and walks grid positions ring by ring
outward until the part's box clashes with no placement's box
(`Studio.DragPlane.freeSpot`). Y of the result is 0: the part's underside on the
ground. Pure: nothing is added.

-}
freeSpotFor : String -> Model -> Vec3
freeSpotFor part model =
    let
        bottom =
            Vec3.getY (partLocalBox (resolvedLines model) part model).max

        spot =
            Transform.position (freeSpotForPlacement part (Mat4.makeTranslate3 0 -bottom 0) model)
    in
    vec3 (Vec3.getX spot) 0 (Vec3.getZ spot)


{-| `freeSpotFor` for a part placed by matrix (see `addPartAt`): the given
placement's height and orientation are kept and only its X and Z are replaced by
the first free grid position around the selection (or the camera target). Pass
the result to `addPartAt`. Brick Contraptions uses it to put a new gear on the
axle line beside the selected one.
-}
freeSpotForPlacement : String -> Mat4 -> Model -> Mat4
freeSpotForPlacement part matrix model =
    let
        resolveLines =
            resolvedLines model

        origin =
            Transform.position matrix

        atOrigin =
            Mat4.mul (Mat4.makeTranslate3 -(Vec3.getX origin) 0 -(Vec3.getZ origin)) matrix

        start =
            case Edit.selectionCenter (Set.toList model.selection) model.document of
                Just centre ->
                    ( Vec3.getX centre, Vec3.getZ centre )

                Nothing ->
                    -- Render space negates Z as well as Y; LDraw Z is −render Z.
                    ( Vec3.getX model.viewport.camera.target, -(Vec3.getZ model.viewport.camera.target) )

        ( x, z ) =
            DragPlane.freeSpot
                { box = DragPlane.worldBox atOrigin (partLocalBox resolveLines part model)
                , start = start
                , step = toFloat (max 1 model.gridStep)
                , others =
                    Document.placements model.document
                        |> List.map (\p -> DragPlane.worldBox p.matrix (partLocalBox resolveLines p.part model))
                }
    in
    Mat4.mul (Mat4.makeTranslate3 x 0 z) atOrigin


{-| Helper to generate a GeometryFlattened message from external port.
-}
geometryFlattened : String -> Msg
geometryFlattened payload =
    ViewportMsg (Viewport.GeometryFlattened payload)


{-| Helper to generate a GeometryFlattenFailed message from external port.
-}
geometryFlattenFailed : String -> Msg
geometryFlattenFailed payload =
    ViewportMsg (Viewport.GeometryFlattenFailed payload)


{-| The translation handles for the current selection when the move tool is active.

With the combined gizmo they step aside while an arc is being dragged; with
`NoGizmo` there are none.

-}
currentHandles : Model -> List Handle
currentHandles model =
    if model.gizmoStyle /= NoGizmo && model.tool == MoveTool && not (isArcDrag model.drag) then
        case Edit.selectionCenter (Set.toList model.selection) model.document of
            Just localCenter ->
                Gizmo.handlesWithFrame
                    Mat4.identity
                    model.viewport.camera
                    model.viewport.width
                    model.viewport.height
                    localCenter

            Nothing ->
                []

    else
        []


{-| The rotational arc handles for the current selection when the rotate tool is
active, or around the move arrows when the move tool is and the gizmo is combined.
None with `NoGizmo`.
-}
currentArcHandles : Model -> List ArcHandle
currentArcHandles model =
    if model.gizmoStyle == NoGizmo then
        []

    else if model.tool == MoveTool && model.gizmoStyle == MoveAndRotateGizmo then
        case model.drag of
            Just { kind } ->
                case kind of
                    ArcDrag arcHandle ->
                        [ arcHandle ]

                    _ ->
                        []

            Nothing ->
                case Edit.selectionCenter (Set.toList model.selection) model.document of
                    Just center ->
                        Gizmo.outerArcHandles
                            model.viewport.camera
                            model.viewport.width
                            model.viewport.height
                            center

                    Nothing ->
                        []

    else if model.tool == RotateTool then
        case model.drag of
            Just { kind } ->
                case kind of
                    ArcDrag arcHandle ->
                        [ arcHandle ]

                    _ ->
                        []

            Nothing ->
                case Edit.selectionCenter (Set.toList model.selection) model.document of
                    Just localCenter ->
                        Gizmo.arcHandlesWithFrame
                            Mat4.identity
                            model.viewport.camera
                            model.viewport.width
                            model.viewport.height
                            localCenter

                    Nothing ->
                        []

    else
        []


isArcDrag : Maybe Drag -> Bool
isArcDrag drag =
    case Maybe.map .kind drag of
        Just (ArcDrag _) ->
            True

        _ ->
            False


{-| Look a part's lines up in the viewport's part cache, if loaded.
-}
resolvedLines : Model -> String -> Maybe (List LDrawLine)
resolvedLines model name =
    case Dict.get name model.viewport.partCache of
        Just (Resolve.Loaded lines) ->
            Just lines

        _ ->
            Nothing


{-| A part's local bounds (LDraw) as the studio measures them for drags and free
spots (`partLocalBox`): put it under a placement matrix with
`Studio.DragPlane.worldBox`. For hosts with placement rules of their own (Brick
Contraptions keeps a new gear in the selected gear's plane).
-}
partBox : String -> Model -> DragPlane.Box
partBox part model =
    partLocalBox (resolvedLines model) part model


{-| A part's local bounds (LDraw): its mesh bounds once the geometry is built,
else its connectivity envelope, else a 1×1 brick.
-}
partLocalBox : (String -> Maybe (List LDrawLine)) -> String -> Model -> DragPlane.Box
partLocalBox resolveLines part model =
    case model.viewport.instancedScene |> Maybe.andThen (\scene -> Dict.get part scene.parts) of
        Just mesh ->
            { min = mesh.boundsMin, max = mesh.boundsMax }

        Nothing ->
            let
                conn =
                    Connectivity.extractPartConnectivity part resolveLines
            in
            case conn.boundingBoxes of
                box :: _ ->
                    Connectivity.effectiveEnvelope box conn.connections

                [] ->
                    { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


{-| Slide the camera onto the selection (`Viewport.pivotOn`), for an explicit
click or tap that selected a part. Does nothing during a drag or without a
selection.
-}
pivotOnSelection : Model -> Model
pivotOnSelection model =
    if model.drag /= Nothing || Set.isEmpty model.selection then
        model

    else
        case Edit.selectionCenter (Set.toList model.selection) model.document of
            Just centre ->
                { model | viewport = Viewport.pivotOn (selectionPivot model.selection centre model) model.viewport }

            Nothing ->
                model


{-| Where the camera pivots for a selection (render space): the middle of the
selected placements' bounding spheres once they are built, else the average of
their origins (`centre`, LDraw).
-}
selectionPivot : Set Int -> Vec3 -> Model -> Vec3
selectionPivot selection centre model =
    let
        spheres =
            model.viewport.instancedScene
                |> Maybe.map (\scene -> List.filter (\p -> Set.member p.source selection) scene.placements)
                |> Maybe.withDefault []
    in
    case spheres of
        [] ->
            Gizmo.toRender centre

        _ ->
            Vec3.scale (1 / toFloat (List.length spheres)) (List.foldl (\p acc -> Vec3.add p.center acc) (vec3 0 0 0) spheres)


{-| Update the studio component model.
-}
update : Msg -> Model -> ( Model, Cmd Msg, List Event )
update msg model =
    updateInner msg model


updateInner : Msg -> Model -> ( Model, Cmd Msg, List Event )
updateInner msg model =
    case msg of
        ViewportMsg (Viewport.KeyPressed keyEvent) ->
            handleCanvasKey keyEvent model

        ViewportMsg viewportMsg ->
            let
                ( nextViewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( finalModel, extraCmds, emittedEvents ) =
                    handleViewportEvents events ( { model | viewport = nextViewport }, [], [] )
            in
            ( fitAddedPart finalModel
            , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
            , emittedEvents
            )

        Select newSelection ->
            ( { model | selection = newSelection }
            , Cmd.none
            , [ SelectionChanged newSelection ]
            )

        SelectOnly id ->
            let
                newSel =
                    Set.singleton id
            in
            ( { model | selection = newSel }
            , Cmd.none
            , [ SelectionChanged newSel ]
            )

        ToggleSelect id ->
            let
                newSel =
                    if Set.member id model.selection then
                        Set.remove id model.selection

                    else
                        Set.insert id model.selection
            in
            ( { model | selection = newSel }
            , Cmd.none
            , [ SelectionChanged newSel ]
            )

        ClearSelection ->
            ( clearSelection model
            , Cmd.none
            , [ SelectionChanged Set.empty ]
            )

        SelectAll ->
            let
                allModel =
                    selectAll model
            in
            ( allModel
            , Cmd.none
            , [ SelectionChanged allModel.selection ]
            )

        DeleteSelection ->
            deleteSelection model

        Undo ->
            undo model

        Redo ->
            redo model

        RotateSelection axis steps ->
            rotateSelection axis steps model

        SetColorSelection newColor ->
            if Set.isEmpty model.selection then
                ( { model | activeColour = newColor }, Cmd.none, [] )

            else
                let
                    recolorPairs =
                        List.map (\id -> ( id, newColor )) (Set.toList model.selection)

                    ( newDoc, newHistory ) =
                        Edit.record (Recolor recolorPairs) model.document model.history

                    ( nextViewport, viewportCmd, events ) =
                        Viewport.applyDocument newDoc model.viewport

                    nextModel =
                        { model
                            | viewport = nextViewport
                            , document = Document.clearTouched newDoc
                            , history = newHistory
                            , activeColour = newColor
                        }

                    ( finalModel, extraCmds, emittedEvents ) =
                        handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document ] )
                in
                ( finalModel
                , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
                , emittedEvents
                )

        SetTool tool ->
            ( setTool tool model, Cmd.none, [] )

        SetGridStep step ->
            ( setGridStep step model, Cmd.none, [] )

        SetRotateStep step ->
            ( setRotateStep step model, Cmd.none, [] )

        SetSnapDistance dist ->
            ( setSnapDistance dist model, Cmd.none, [] )

        SetCollisionMode mode ->
            ( setCollisionMode mode model, Cmd.none, [] )

        KeyUp key ->
            if key == "Alt" then
                let
                    nextModel =
                        { model | altHeld = False }
                in
                case nextModel.drag of
                    Just d ->
                        let
                            ( moved, cmd, evs ) =
                                moveDrag d d.lastPoint nextModel
                        in
                        ( moved, cmd, evs )

                    Nothing ->
                        ( nextModel, Cmd.none, [] )

            else if key == "Shift" then
                let
                    nextModel =
                        { model | shiftHeld = False }
                in
                case nextModel.drag of
                    Just d ->
                        let
                            updatedDrag =
                                { d | modeAnchor = d.lastPoint, baseElevation = d.elevationOffset }

                            ( moved, cmd, evs ) =
                                moveDrag updatedDrag d.lastPoint { nextModel | drag = Just updatedDrag }
                        in
                        ( moved, cmd, evs )

                    Nothing ->
                        ( nextModel, Cmd.none, [] )

            else
                ( model, Cmd.none, [] )

        FitView ->
            let
                ( m, cmd ) =
                    fitView model
            in
            ( m, cmd, [] )

        ResetDocument doc ->
            resetDocument doc model

        AddPart partFile color pos ->
            addPart partFile color pos model

        AddPartCentered partFile color ->
            addPartCentered partFile color model

        NudgeSelection nudge ->
            nudgeSelected nudge model

        GeometryFlattened payload ->
            updateInner (ViewportMsg (Viewport.GeometryFlattened payload)) model

        GeometryFlattenFailed payload ->
            updateInner (ViewportMsg (Viewport.GeometryFlattenFailed payload)) model


handleCanvasKey : ViewportShortcuts.KeyEvent -> Model -> ( Model, Cmd Msg, List Event )
handleCanvasKey keyEvent model =
    let
        updatedModel =
            { model
                | altHeld = keyEvent.alt || keyEvent.key == "Alt"
                , shiftHeld = keyEvent.shift || keyEvent.key == "Shift"
            }

        context =
            shortcutContext updatedModel

        key =
            ViewportShortcuts.normalizeKey keyEvent.key
    in
    if ViewportShortcuts.isTextEntryTarget keyEvent || ViewportShortcuts.isWidgetKey keyEvent then
        -- Typing into a host form field, or a focused control handles the key
        -- itself: none of the editing keys below may fire.
        ( updatedModel, Cmd.none, [] )

    else if updatedModel.helpVisible then
        passToViewport keyEvent updatedModel

    else if (keyEvent.ctrl || keyEvent.meta) && not keyEvent.alt then
        if key == "z" then
            if keyEvent.shift then
                redo updatedModel

            else
                undo updatedModel

        else if key == "y" then
            redo updatedModel

        else if key == "a" then
            let
                allModel =
                    selectAll updatedModel
            in
            ( allModel, Cmd.none, [ SelectionChanged allModel.selection ] )

        else
            passToViewport keyEvent updatedModel

    else if (key == "Delete" || key == "Backspace") && not (isDragging updatedModel) then
        deleteSelection updatedModel

    else if key == "Escape" && (isDragging updatedModel || (context.hasSelection && not context.walking)) then
        cancelDrag updatedModel

    else
        case StudioShortcuts.editKeyFor context keyEvent of
            Just editKey ->
                if isDragging updatedModel then
                    -- Mid-drag the keys are consumed, not turned into camera movement.
                    ( updatedModel, Cmd.none, [] )

                else
                    case editKey of
                        StudioShortcuts.MoveBy nudge ->
                            nudgeSelected nudge updatedModel

                        StudioShortcuts.RotateBy turn ->
                            turnSelection turn updatedModel

            Nothing ->
                passToViewport keyEvent updatedModel


{-| Give a key to the viewport, which may move the camera or report it as an
accepted shortcut for the host.
-}
passToViewport : ViewportShortcuts.KeyEvent -> Model -> ( Model, Cmd Msg, List Event )
passToViewport keyEvent model =
    let
        ( nextViewport, viewportCmd, events ) =
            Viewport.update (Viewport.KeyPressed keyEvent) model.viewport

        ( finalModel, extraCmds, emittedEvents ) =
            handleViewportEvents events ( { model | viewport = nextViewport }, [], [] )
    in
    ( finalModel
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
    , emittedEvents
    )


isDragging : Model -> Bool
isDragging model =
    case model.drag of
        Just _ ->
            True

        Nothing ->
            False


{-| Cancel the drag in progress, restoring the document as it was when the drag
started. No history entry is recorded, because nothing was committed.

When no drag is running, the selection is cleared instead (the Escape key).

-}
cancelDrag : Model -> ( Model, Cmd Msg, List Event )
cancelDrag model =
    case model.drag of
        Just drag ->
            syncDocumentLive
                { model
                    | document = drag.original
                    , drag = Nothing
                    , activeSnap = Nothing
                    , collidingPlacements = Set.empty
                    , inFlightRotationStep = 0
                    , snapRotationStep = 0
                }

        Nothing ->
            ( clearSelection model
            , Cmd.none
            , [ SelectionChanged Set.empty ]
            )


{-| Turn the selection one rotate step, as the arrow keys do: `TurnLeft` and
`TurnRight` about the vertical axis, `TipForward` and `TipBackward` about the
ground axis most aligned with the screen's horizontal (`StudioShortcuts.turnRotation`).
Recorded in the history; the camera stays where it is. Does
nothing without a selection or during a drag. For host buttons.
-}
turnSelected : StudioShortcuts.TurnDirection -> Model -> ( Model, Cmd Msg, List Event )
turnSelected direction model =
    if isDragging model then
        ( model, Cmd.none, [] )

    else
        turnSelection { direction = direction, fortyFive = False } model


{-| Raise the selection one step, as `E` / Page Up does. Recorded in the history;
the camera stays where it is. Does nothing without a selection or during a drag.
-}
raiseSelected : Model -> ( Model, Cmd Msg, List Event )
raiseSelected =
    stepSelected StudioShortcuts.Up


{-| Lower the selection one step, as `Q` / Page Down does. See `raiseSelected`.
-}
lowerSelected : Model -> ( Model, Cmd Msg, List Event )
lowerSelected =
    stepSelected StudioShortcuts.Down


stepSelected : StudioShortcuts.NudgeDirection -> Model -> ( Model, Cmd Msg, List Event )
stepSelected direction model =
    if isDragging model then
        ( model, Cmd.none, [] )

    else
        nudgeSelected (StudioShortcuts.Nudge { direction = direction, fine = False }) model


shortcutContext : Model -> StudioShortcuts.Context
shortcutContext model =
    { hasSelection = not (Set.isEmpty model.selection)
    , helpVisible = model.helpVisible
    , walking = model.viewport.camera.navigation == Camera.Walk
    , inSubmodel = False
    , menuOpen = False
    }


{-| Turn the selection about its centre, about the axis `StudioShortcuts.turnAxis` picks for
the turn's direction and the current view.
-}
turnSelection : { direction : StudioShortcuts.TurnDirection, fortyFive : Bool } -> Model -> ( Model, Cmd Msg, List Event )
turnSelection turn model =
    let
        ( axis, radians ) =
            StudioShortcuts.turnRotation
                { azimuth = model.viewport.camera.azimuth, rotateStepDegrees = model.rotateStep }
                turn
    in
    rotateSelectionBy axis radians model


nudgeSelected : StudioShortcuts.Nudge -> Model -> ( Model, Cmd Msg, List Event )
nudgeSelected nudge model =
    if Set.isEmpty model.selection then
        ( model, Cmd.none, [] )

    else
        let
            selectedIds =
                Set.toList model.selection

            delta =
                nudgeDelta model nudge

            command =
                Translate selectedIds delta

            ( newDoc, newHistory ) =
                recordEdit command model

            ( nextViewport, viewportCmd, events ) =
                Viewport.applyDocument newDoc model.viewport

            nextModel =
                { model
                    | viewport = nextViewport
                    , document = Document.clearTouched newDoc
                    , history = newHistory
                }

            ( finalModel, extraCmds, emittedEvents ) =
                handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document ] )
        in
        ( finalModel
        , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
        , emittedEvents
        )


nudgeDelta : Model -> StudioShortcuts.Nudge -> Vec3
nudgeDelta model (StudioShortcuts.Nudge nudge) =
    let
        azimuth =
            model.viewport.camera.azimuth

        horizontal =
            if nudge.fine then
                1

            else
                toFloat model.gridStep

        vertical =
            if nudge.fine then
                1

            else
                min 8 (toFloat model.gridStep)

        ( rightX, rightZ ) =
            ( cos azimuth, -(sin azimuth) )

        ( awayX, awayZ ) =
            ( -(sin azimuth), -(cos azimuth) )

        onGround ( x, z ) amount =
            if abs x >= abs z then
                vec3 (amount * sign x) 0 0

            else
                vec3 0 0 (-amount * sign z)

        sign v =
            if v < 0 then
                -1

            else
                1
    in
    case nudge.direction of
        StudioShortcuts.Right ->
            onGround ( rightX, rightZ ) horizontal

        StudioShortcuts.Left ->
            onGround ( rightX, rightZ ) -horizontal

        StudioShortcuts.Away ->
            onGround ( awayX, awayZ ) horizontal

        StudioShortcuts.Toward ->
            onGround ( awayX, awayZ ) -horizontal

        StudioShortcuts.Up ->
            vec3 0 -vertical 0

        StudioShortcuts.Down ->
            vec3 0 vertical 0


handleViewportEvents : List Viewport.Event -> ( Model, List (Cmd Msg), List Event ) -> ( Model, List (Cmd Msg), List Event )
handleViewportEvents events start =
    List.foldl handleViewportEvent start events


handleViewportEvent : Viewport.Event -> ( Model, List (Cmd Msg), List Event ) -> ( Model, List (Cmd Msg), List Event )
handleViewportEvent event ( model, cmds, outEvents ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds, outEvents ++ [ CameraSettled ] )

        Viewport.FlattenRequested payload ->
            ( model, cmds, outEvents ++ [ FlattenRequested payload ] )

        Viewport.KeyAccepted keyEvent ->
            ( model, cmds, outEvents ++ [ KeyAccepted keyEvent ] )

        Viewport.Clicked x y shift ->
            let
                newSel =
                    case ( Viewport.pickSource x y model.viewport, shift ) of
                        ( Just id, True ) ->
                            if Set.member id model.selection then
                                Set.remove id model.selection

                            else
                                Set.insert id model.selection

                        ( Just id, False ) ->
                            Set.singleton id

                        ( Nothing, True ) ->
                            model.selection

                        ( Nothing, False ) ->
                            Set.empty
            in
            ( pivotOnPicked (Viewport.pickSource x y model.viewport) { model | selection = newSel }
            , cmds
            , outEvents ++ [ SelectionChanged newSel ]
            )

        Viewport.DoubleClicked _ _ ->
            ( model, cmds, outEvents )

        Viewport.DragCancelled ->
            -- A right press during a drag puts the parts back, like Escape.
            let
                ( cancelled, cancelCmd, cancelEvents ) =
                    if isDragging model then
                        cancelDrag model

                    else
                        ( model, Cmd.none, [] )
            in
            ( cancelled, cmds ++ [ cancelCmd ], outEvents ++ cancelEvents )

        Viewport.Tapped x y ->
            let
                newSel =
                    case Viewport.pickSource x y model.viewport of
                        Just id ->
                            Set.singleton id

                        Nothing ->
                            Set.empty
            in
            ( pivotOnPicked (Viewport.pickSource x y model.viewport) { model | selection = newSel }
            , cmds
            , outEvents ++ [ SelectionChanged newSel ]
            )

        Viewport.PressStarted x y ->
            case startDrag ( x, y ) model of
                Just drag ->
                    let
                        dragSel =
                            Set.fromList drag.ids
                    in
                    ( { model
                        | viewport = Viewport.captureDrag model.viewport
                        , drag = Just drag
                        , selection = dragSel
                        , inFlightRotationStep = 0
                        , snapRotationStep = 0
                      }
                    , cmds
                    , outEvents ++ [ SelectionChanged dragSel ]
                    )

                Nothing ->
                    if model.dragMode == DragPartsOnly && not model.viewport.mousePanDragging then
                        -- Nothing to pick up and the camera is not ours to move:
                        -- hold the gesture so it does not orbit. The release then
                        -- arrives as `DragEnded`, never `Clicked`, so clear here.
                        let
                            newSel =
                                if model.shiftHeld then
                                    model.selection

                                else
                                    Set.empty
                        in
                        ( { model
                            | viewport = Viewport.captureDrag model.viewport
                            , selection = newSel
                          }
                        , cmds
                        , if newSel == model.selection then
                            outEvents

                          else
                            outEvents ++ [ SelectionChanged newSel ]
                        )

                    else
                        ( { model | viewport = panEmptyPress model.dragMode model.viewport }, cmds, outEvents )

        Viewport.DragMoved x y ->
            case model.drag of
                Just drag ->
                    let
                        ( movedModel, moveCmd, moveEvents ) =
                            moveDrag drag ( x, y ) model
                    in
                    ( movedModel, cmds ++ [ moveCmd ], outEvents ++ moveEvents )

                Nothing ->
                    ( model, cmds, outEvents )

        Viewport.DragEnded _ _ ->
            let
                ( endedModel, endCmd, endEvents ) =
                    endDrag model
            in
            ( endedModel, cmds ++ [ endCmd ], outEvents ++ endEvents )

        Viewport.WheelScrolled delta ->
            case model.drag of
                Just drag ->
                    let
                        stepSize =
                            if model.shiftHeld then
                                24.0

                            else if model.altHeld then
                                1.0

                            else
                                8.0

                        direction =
                            if delta < 0 then
                                -1.0

                            else
                                1.0

                        newOffset =
                            drag.elevationOffset + (direction * stepSize)

                        updatedDrag =
                            { drag | elevationOffset = newOffset }

                        ( movedModel, moveCmd, moveEvents ) =
                            moveDrag updatedDrag updatedDrag.lastPoint { model | drag = Just updatedDrag }
                    in
                    ( movedModel, cmds ++ [ moveCmd ], outEvents ++ moveEvents )

                Nothing ->
                    ( model, cmds, outEvents )

        _ ->
            ( model, cmds, outEvents )


{-| After a click or tap: slide the view onto the selection when it hit a part.
-}
pivotOnPicked : Maybe Int -> Model -> Model
pivotOnPicked picked model =
    case picked of
        Just _ ->
            pivotOnSelection model

        Nothing ->
            model


{-| A press that grabbed nothing: in `DragPartsOrPan` mode the rest of the
gesture pans (the mouse always, one finger when the mode says so).
-}
panEmptyPress : DragMode -> Viewport.Model -> Viewport.Model
panEmptyPress mode viewport =
    case mode of
        DragPartsOrPan oneFinger ->
            if Viewport.isTouchGesture viewport && oneFinger == OneFingerOrbits then
                viewport

            else
                Viewport.panCurrentGesture viewport

        _ ->
            viewport


startDrag : ( Float, Float ) -> Model -> Maybe Drag
startDrag point model =
    let
        handleDrag kind =
            { kind = kind
            , plane = Nothing
            , ids = Set.toList model.selection
            , start = point
            , lastPoint = point
            , original = model.document
            , amount = 0
            , elevationOffset = 0.0
            , axisLock = Nothing
            , modeAnchor = point
            , baseElevation = 0.0
            }

        arcDrag =
            Gizmo.hitArc grabRadius point (currentArcHandles model)
                |> Maybe.map (handleDrag << ArcDrag)
    in
    if model.tool == MoveTool then
        case Gizmo.hit grabRadius point (currentHandles model) of
            Just handle ->
                Just (handleDrag (LinearDrag handle))

            Nothing ->
                case arcDrag of
                    Just drag ->
                        Just drag

                    Nothing ->
                        grabPart point model

    else if model.tool == RotateTool then
        arcDrag

    else
        Nothing


grabPart : ( Float, Float ) -> Model -> Maybe Drag
grabPart ( x, y ) model =
    if model.viewport.mousePanDragging then
        Nothing

    else
        Viewport.pickBodyPoint x y model.viewport
            |> Maybe.map
                (\hit ->
                    { kind = PlaneDrag hit.point
                    , plane = Just (DragPlane.choose model.viewport.camera hit.point)
                    , ids =
                        if Set.member hit.source model.selection then
                            Set.toList model.selection

                        else
                            [ hit.source ]
                    , start = ( x, y )
                    , lastPoint = ( x, y )
                    , original = model.document
                    , amount = 0
                    , elevationOffset = 0.0
                    , axisLock = Nothing
                    , modeAnchor = ( x, y )
                    , baseElevation = 0.0
                    }
                )


moveDrag : Drag -> ( Float, Float ) -> Model -> ( Model, Cmd Msg, List Event )
moveDrag drag point model =
    case drag.kind of
        LinearDrag handle ->
            moveLinearDrag drag handle point model

        PlaneDrag grab ->
            movePlaneDrag drag grab point model

        ArcDrag arcHandle ->
            let
                step =
                    if model.altHeld then
                        0

                    else
                        degrees (toFloat model.rotateStep)

                amount =
                    Gizmo.dragArcAngle step arcHandle drag.start point
            in
            if amount == drag.amount then
                ( model, Cmd.none, [] )

            else
                let
                    rotationCommand =
                        RotateAroundAxis drag.ids
                            { pivot = arcHandle.center
                            , axis = arcHandle.axis
                            , radians = amount
                            }

                    ( movedDoc, _ ) =
                        Edit.apply rotationCommand drag.original
                in
                syncDocumentLive { model | document = movedDoc, drag = Just { drag | amount = amount, lastPoint = point } }


moveLinearDrag : Drag -> Gizmo.Handle -> ( Float, Float ) -> Model -> ( Model, Cmd Msg, List Event )
moveLinearDrag drag handle point model =
    let
        amount =
            Gizmo.dragAmount (toFloat model.gridStep) handle drag.start point

        delta =
            Vec3.scale amount (Edit.axisVector handle.axis)

        ( gizmoMovedDoc, _ ) =
            Edit.apply (Translate drag.ids delta) drag.original
    in
    syncDocumentLive
        { model
            | document = gizmoMovedDoc
            , drag = Just { drag | amount = amount, lastPoint = point }
            , activeSnap = Nothing
        }


movePlaneDrag : Drag -> Vec3 -> ( Float, Float ) -> Model -> ( Model, Cmd Msg, List Event )
movePlaneDrag initialDrag grab ( x, y ) model =
    let
        plane =
            Maybe.withDefault (DragPlane.choose model.viewport.camera grab) initialDrag.plane

        drag =
            { initialDrag | plane = Just plane }

        cursor =
            Pick.screenRay model.viewport.camera model.viewport.width model.viewport.height x y
                |> Maybe.andThen (DragPlane.intersect plane)

        originalPlacements =
            drag.ids
                |> List.filterMap
                    (\id ->
                        case Document.getPlacement id drag.original of
                            Just p ->
                                Just p

                            Nothing ->
                                Document.getPlacement id model.document
                    )
    in
    case ( cursor, originalPlacements ) of
        ( Just cursorPoint, leadP :: _ ) ->
            let
                resolveLines =
                    resolvedLines model

                leadConn =
                    Connectivity.extractPartConnectivity leadP.part resolveLines

                placementBox p =
                    DragPlane.worldBox p.matrix (partLocalBox resolveLines p.part model)

                footprint =
                    originalPlacements
                        |> List.concatMap
                            (\p ->
                                let
                                    b =
                                        placementBox p
                                in
                                [ b.min, b.max ]
                            )
                        |> DragPlane.unionBox
                        |> Maybe.withDefault (placementBox leadP)

                otherBoxes =
                    Document.placements drag.original
                        |> List.filter (\p -> not (List.member p.id drag.ids))
                        |> List.map placementBox

                origPos =
                    Transform.position leadP.matrix

                targetPos =
                    DragPlane.targetPosition
                        { plane = plane
                        , cursor = cursorPoint
                        , original = origPos
                        , gridStep = toFloat (max 1 model.gridStep)
                        , elevationOffset = drag.elevationOffset
                        , footprint = footprint
                        , others = otherBoxes
                        , keepHeight = not model.dragSettles && plane.mode == DragPlane.Horizontal
                        }

                inFlightAngle =
                    toFloat model.inFlightRotationStep * (pi / 2)

                leadSurfaceMatrix =
                    Mat4.mul
                        (Mat4.makeTranslate targetPos)
                        (Mat4.mul
                            (Mat4.makeRotate inFlightAngle (vec3 0 1 0))
                            (Mat4.mul
                                (Mat4.makeTranslate (Vec3.negate origPos))
                                leadP.matrix
                            )
                        )

                deltaMat =
                    Mat4.mul leadSurfaceMatrix (Connectivity.invertRigid leadP.matrix)

                draggedSurfaceUpdates =
                    originalPlacements
                        |> List.map (\p -> ( p.id, Mat4.mul deltaMat p.matrix ))

                ( gizmoMovedDoc, _ ) =
                    Edit.apply (SetMatrix draggedSurfaceUpdates) drag.original

                otherPlacements =
                    Document.placements gizmoMovedDoc
                        |> List.filter (\p -> not (List.member p.id drag.ids))
            in
            if model.altHeld || model.snapDistance <= 0 then
                syncDocumentLive
                    { model
                        | document = gizmoMovedDoc
                        , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                        , activeSnap = Nothing
                        , collidingPlacements = Set.empty
                    }

            else
                let
                    targetWorldPoints =
                        otherPlacements
                            |> List.concatMap
                                (\targetP ->
                                    let
                                        pConn =
                                            Connectivity.extractPartConnectivity targetP.part resolveLines
                                    in
                                    List.map (Connectivity.transformPoint targetP.matrix targetP.id) pConn.connections
                                )

                    maybeSnap =
                        Connectivity.solveSnapWithHysteresis
                            model.snapDistance
                            (model.snapDistance * 1.5)
                            model.activeSnap
                            model.snapRotationStep
                            leadSurfaceMatrix
                            leadConn.connections
                            targetWorldPoints
                in
                case maybeSnap of
                    Just snap ->
                        let
                            snapDeltaMat =
                                Mat4.mul snap.snapMatrix (Connectivity.invertRigid leadP.matrix)

                            draggedSnapUpdates =
                                originalPlacements
                                    |> List.map (\p -> ( p.id, Mat4.mul snapDeltaMat p.matrix ))

                            ( snappedDoc, _ ) =
                                Edit.apply (SetMatrix draggedSnapUpdates) drag.original
                        in
                        syncDocumentLive
                            { model
                                | document = snappedDoc
                                , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                                , activeSnap = Just snap
                                , collidingPlacements = Set.empty
                            }

                    Nothing ->
                        syncDocumentLive
                            { model
                                | document = gizmoMovedDoc
                                , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                                , activeSnap = Nothing
                                , collidingPlacements = Set.empty
                            }

        _ ->
            ( model, Cmd.none, [] )


endDrag : Model -> ( Model, Cmd Msg, List Event )
endDrag model =
    case model.drag of
        Just drag ->
            let
                currentPlacements =
                    drag.ids
                        |> List.filterMap (\id -> Document.getPlacement id model.document)

                originalPlacements =
                    drag.ids
                        |> List.filterMap (\id -> Document.getPlacement id drag.original)

                hasMoved =
                    List.any
                        (\p ->
                            case Document.getPlacement p.id drag.original of
                                Just origP ->
                                    p.matrix /= origP.matrix

                                Nothing ->
                                    True
                        )
                        currentPlacements
            in
            if not hasMoved then
                let
                    released =
                        { model
                            | drag = Nothing
                            , activeSnap = Nothing
                            , inFlightRotationStep = 0
                            , snapRotationStep = 0
                            , collidingPlacements = Set.empty
                        }
                in
                -- A part pressed and released without moving is a click on it
                -- (in the drag modes it never arrives as `Clicked`).
                ( case drag.kind of
                    PlaneDrag _ ->
                        pivotOnSelection released

                    _ ->
                        released
                , Cmd.none
                , []
                )

            else
                let
                    inversePairs =
                        List.map (\p -> ( p.id, p.matrix )) originalPlacements

                    rotations =
                        if model.gearAlignment then
                            GearAlign.alignmentCommands model.viewport.partCache
                                { before = drag.original, after = model.document }

                        else
                            []

                    ( aligned, rotationUndo ) =
                        Edit.apply (Group rotations) model.document

                    -- Undo the turns first (they came last), then put the dragged
                    -- placements back where the drag started.
                    undoCommand =
                        case rotations of
                            [] ->
                                SetMatrix inversePairs

                            _ ->
                                Group [ rotationUndo, SetMatrix inversePairs ]

                    updated =
                        { model
                            | drag = Nothing
                            , activeSnap = Nothing
                            , inFlightRotationStep = 0
                            , snapRotationStep = 0
                            , collidingPlacements = Set.empty
                            , history = Edit.pushApplied undoCommand model.history
                            , document = aligned
                        }
                in
                case rotations of
                    [] ->
                        ( updated, Cmd.none, [ DocumentChanged updated.document ] )

                    _ ->
                        let
                            ( synced, syncCmd, syncEvents ) =
                                syncDocumentLive updated
                        in
                        ( synced, syncCmd, DocumentChanged synced.document :: syncEvents )

        Nothing ->
            ( model, Cmd.none, [] )


syncDocumentLive : Model -> ( Model, Cmd Msg, List Event )
syncDocumentLive model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.applyDocument model.document model.viewport

        synced =
            { model
                | viewport = viewport
                , document = Document.clearTouched model.document
            }

        ( final, extraCmds, emittedEvents ) =
            handleViewportEvents events ( synced, [], [] )
    in
    ( final
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
    , emittedEvents
    )


{-| Subscriptions for the studio component.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ViewportMsg (Viewport.subscriptions model.viewport)
        , Browser.Events.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
        ]


{-| Render the 3D studio viewport with interactive gizmo and snap overlays.
-}
view : { width : Int, height : Int } -> Model -> Html Msg
view dimensions model =
    viewWithExtra
        { width = dimensions.width
        , height = dimensions.height
        , extraEntities = \_ _ -> []
        }
        model


{-| Render the 3D studio viewport with gizmo, snap overlays, and host-supplied extra WebGL entities.
-}
viewWithExtra :
    { width : Int
    , height : Int
    , extraEntities : Camera.Camera -> Float -> List Entity
    }
    -> Model
    -> Html Msg
viewWithExtra config model =
    let
        dragAxisGuide =
            case model.drag of
                Just drag ->
                    case drag.axisLock of
                        Just axis ->
                            case Edit.selectionCenter drag.ids model.document of
                                Just center ->
                                    Just ( axis, center )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    in
    div
        [ Attr.style "width" (String.fromInt config.width ++ "px")
        , Attr.style "height" (String.fromInt config.height ++ "px")
        , Attr.style "position" "relative"
        , Attr.style "overflow" "hidden"
        ]
        [ Html.map ViewportMsg
            (Viewport.view
                { extraEntities =
                    \camera _ aspect ->
                        Overlay.entities camera
                            aspect
                            model.viewport.instancedScene
                            { selection = model.selection
                            , colliding =
                                if model.drag == Nothing then
                                    Set.empty

                                else
                                    model.collidingPlacements
                            , snapGuide = Maybe.map .guide model.activeSnap
                            , axisGuide = dragAxisGuide
                            }
                            (currentHandles model)
                            (currentArcHandles model)
                            ++ config.extraEntities camera aspect
                , coarsenWhileMoving = True
                }
                model.viewport
            )
        ]
