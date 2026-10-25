module Studio.Core exposing
    ( Config
    , Drag
    , Event(..)
    , Model
    , Msg(..)
    , addPart
    , addPartCentered
    , canRedo
    , canUndo
    , clearSelection
    , currentArcHandles
    , currentHandles
    , deleteSelection
    , fitView
    , geometryFlattenFailed
    , geometryFlattened
    , getPlacements
    , hasSelection
    , init
    , redo
    , resetDocument
    , rotateSelection
    , selectAll
    , selectionCount
    , setCollisionMode
    , setGridStep
    , setRotateStep
    , setSnapDistance
    , setTool
    , subscriptions
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
import Render.Style as Style
import Set exposing (Set)
import Studio.Edit as Edit exposing (Axis(..), Command(..), History, NewPart)
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
-}
type alias Drag =
    { kind : DragHandle
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


{-| Core studio component state.
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
    if Set.isEmpty model.selection then
        ( model, Cmd.none, [] )

    else
        let
            selectedIds =
                Set.toList model.selection

            angle =
                degrees (toFloat (steps * model.rotateStep))

            pivot =
                Edit.selectionCenter selectedIds model.document
                    |> Maybe.withDefault (vec3 0 0 0)

            command =
                Rotate selectedIds { pivot = pivot, axis = axis, radians = angle }

            ( newDoc, newHistory ) =
                Edit.record command model.document model.history

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


{-| Add a new part placement at the specified position.
-}
addPart : String -> Int -> Vec3 -> Model -> ( Model, Cmd Msg, List Event )
addPart partFile color position model =
    let
        newPart =
            { part = partFile
            , color = color
            , matrix = Mat4.makeTranslate position
            }

        ( newDoc, newHistory ) =
            Edit.record (Add [ newPart ]) model.document model.history

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
            }

        ( finalModel, extraCmds, emittedEvents ) =
            handleViewportEvents events ( nextModel, [], [ DocumentChanged nextModel.document, SelectionChanged newSelection ] )
    in
    ( finalModel
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
    , emittedEvents
    )


{-| Add a new part centered at the camera target or above the ground.
-}
addPartCentered : String -> Int -> Model -> ( Model, Cmd Msg, List Event )
addPartCentered partFile color model =
    let
        camTarget =
            model.viewport.camera.target

        groundPos =
            vec3 (Vec3.getX camTarget) 0 (Vec3.getZ camTarget)
    in
    addPart partFile color groundPos model


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
-}
currentHandles : Model -> List Handle
currentHandles model =
    if model.tool == MoveTool then
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


{-| The rotational arc handles for the current selection when the rotate tool is active.
-}
currentArcHandles : Model -> List ArcHandle
currentArcHandles model =
    if model.tool == RotateTool then
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


{-| Update the studio component model.
-}
update : Msg -> Model -> ( Model, Cmd Msg, List Event )
update msg model =
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
            ( finalModel
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
            update (ViewportMsg (Viewport.GeometryFlattened payload)) model

        GeometryFlattenFailed payload ->
            update (ViewportMsg (Viewport.GeometryFlattenFailed payload)) model


handleCanvasKey : ViewportShortcuts.KeyEvent -> Model -> ( Model, Cmd Msg, List Event )
handleCanvasKey keyEvent model =
    let
        updatedModel =
            { model
                | altHeld = keyEvent.alt || keyEvent.key == "Alt"
                , shiftHeld = keyEvent.shift || keyEvent.key == "Shift"
            }
    in
    if (keyEvent.ctrl || keyEvent.meta) && not keyEvent.alt then
        let
            lowerKey =
                String.toLower keyEvent.key
        in
        if lowerKey == "z" then
            if keyEvent.shift then
                redo updatedModel

            else
                undo updatedModel

        else if lowerKey == "y" then
            redo updatedModel

        else if lowerKey == "a" then
            let
                allModel =
                    selectAll updatedModel
            in
            ( allModel, Cmd.none, [ SelectionChanged allModel.selection ] )

        else
            update (ViewportMsg (Viewport.KeyPressed keyEvent)) updatedModel

    else if keyEvent.key == "Delete" || keyEvent.key == "Backspace" then
        deleteSelection updatedModel

    else
        case ( updatedModel.drag, StudioShortcuts.nudgeFor (shortcutContext updatedModel) keyEvent ) of
            ( Nothing, Just nudge ) ->
                nudgeSelected nudge updatedModel

            _ ->
                let
                    ( nextViewport, viewportCmd, events ) =
                        Viewport.update (Viewport.KeyPressed keyEvent) updatedModel.viewport

                    ( finalModel, extraCmds, emittedEvents ) =
                        handleViewportEvents events ( { updatedModel | viewport = nextViewport }, [], [] )
                in
                ( finalModel
                , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: extraCmds)
                , emittedEvents
                )


shortcutContext : Model -> StudioShortcuts.Context
shortcutContext model =
    { hasSelection = not (Set.isEmpty model.selection)
    , helpVisible = False
    , walking = False
    , inSubmodel = False
    , menuOpen = False
    }


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
                Edit.record command model.document model.history

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
            ( { model | selection = newSel }
            , cmds
            , outEvents ++ [ SelectionChanged newSel ]
            )

        Viewport.DoubleClicked _ _ ->
            ( model, cmds, outEvents )

        Viewport.Tapped x y ->
            let
                newSel =
                    case Viewport.pickSource x y model.viewport of
                        Just id ->
                            Set.singleton id

                        Nothing ->
                            Set.empty
            in
            ( { model | selection = newSel }
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
                    ( model, cmds, outEvents )

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


startDrag : ( Float, Float ) -> Model -> Maybe Drag
startDrag point model =
    if model.tool == MoveTool then
        case Gizmo.hit grabRadius point (currentHandles model) of
            Just handle ->
                Just
                    { kind = LinearDrag handle
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

            Nothing ->
                grabPart point model

    else if model.tool == RotateTool then
        Gizmo.hitArc grabRadius point (currentArcHandles model)
            |> Maybe.map
                (\arcHandle ->
                    { kind = ArcDrag arcHandle
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
                )

    else
        Nothing


grabPart : ( Float, Float ) -> Model -> Maybe Drag
grabPart ( x, y ) model =
    if model.viewport.mousePanDragging then
        Nothing

    else
        Viewport.pickPoint x y model.viewport
            |> Maybe.map
                (\hit ->
                    { kind = PlaneDrag hit.point
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
movePlaneDrag initialDrag _ ( x, y ) model =
    let
        drag =
            if initialDrag.modeAnchor == ( 0, 0 ) && ( x, y ) /= ( 0, 0 ) then
                { initialDrag | modeAnchor = ( x, y ) }

            else
                initialDrag

        draggedIds =
            Set.fromList drag.ids

        surfaceHit =
            Viewport.pickSurfaceExcluding draggedIds x y model.viewport

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
    case ( surfaceHit, originalPlacements ) of
        ( Just hit, leadP :: _ ) ->
            let
                resolveLines name =
                    case Dict.get name model.viewport.partCache of
                        Just (Resolve.Loaded lines) ->
                            Just lines

                        _ ->
                            Nothing

                leadConn =
                    Connectivity.extractPartConnectivity leadP.part resolveLines

                leadRawBox =
                    List.head leadConn.boundingBoxes
                        |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }

                leadBox =
                    Connectivity.effectiveEnvelope leadRawBox leadConn.connections

                bottomExtent =
                    case model.viewport.instancedScene |> Maybe.andThen (\s -> Dict.get leadP.part s.parts) of
                        Just m ->
                            Vec3.getY m.boundsMax

                        Nothing ->
                            Vec3.getY leadBox.max

                origPos =
                    Transform.position leadP.matrix

                step =
                    toFloat (max 1 model.gridStep)

                docHit =
                    Gizmo.toRender hit.point

                cursorX =
                    toFloat (round (Vec3.getX docHit / step)) * step

                cursorZ =
                    toFloat (round (Vec3.getZ docHit / step)) * step

                cursorBaseY =
                    hit.topY - bottomExtent

                targetPos =
                    vec3 cursorX (cursorBaseY + drag.elevationOffset) cursorZ

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
                ( { model
                    | drag = Nothing
                    , activeSnap = Nothing
                    , inFlightRotationStep = 0
                    , snapRotationStep = 0
                    , collidingPlacements = Set.empty
                  }
                , Cmd.none
                , []
                )

            else
                let
                    inversePairs =
                        List.map (\p -> ( p.id, p.matrix )) originalPlacements

                    newHistory =
                        Edit.pushApplied (SetMatrix inversePairs) model.history

                    updated =
                        { model
                            | drag = Nothing
                            , activeSnap = Nothing
                            , inFlightRotationStep = 0
                            , snapRotationStep = 0
                            , collidingPlacements = Set.empty
                            , history = newHistory
                        }
                in
                ( updated, Cmd.none, [ DocumentChanged updated.document ] )

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
