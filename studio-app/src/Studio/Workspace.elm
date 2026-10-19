module Studio.Workspace exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

{-| The studio's editing page: a document shown in a viewport.

The `Document` is the source of truth; the `Viewport` only displays it. Every
change is an `Edit.Command`, recorded in the undo history and then pushed to the
viewport with `Viewport.applyDocument`, which updates the scene in place.

Input is split by who owns it. The camera keeps orbit, pan, pinch and walk; the
tools are explicit modes. A click selects. In the move tool, pressing on a
gizmo handle takes that gesture away from the camera (`Viewport.captureDrag`)
and turns it into a move along one axis, snapped to the grid.

-}

import EmbeddedParts
import Html exposing (Html, div, span, text)
import Html.Attributes as Attr
import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Ports
import Render.Camera as Camera
import Render.Style as Style
import Set exposing (Set)
import Studio.Edit as Edit exposing (Axis(..), Command(..), History)
import Studio.Gizmo as Gizmo exposing (Handle)
import Studio.Overlay as Overlay
import Studio.Shortcuts as StudioShortcuts exposing (Tool(..))
import UI.Button as Button
import UI.ShortcutHelp as ShortcutHelp
import UI.Status as Status
import UI.Theme as Theme
import UI.ViewPad as ViewPad
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Hash as Hash
import Viewport.Shortcuts as Shortcuts


{-| Start-up values from the JavaScript runtime. The shape is shared with the
viewer (`docs/library-usage.md`); the studio ignores the playback fields.
-}
type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , defaultModelUrl : String
    , initialHash : String
    , maxRpm : Float
    , uiMode : String
    , controlsEnabled : Bool
    , initialMotorIndex : Int
    , initialRpm : Float
    , useWindowResize : Bool
    , ambientStrength : Maybe Float
    , lightStrength : Maybe Float
    , vibrance : Maybe Float
    , edgeWidth : Maybe Float
    , supportsPointerEvents : Bool
    , studioUrl : String
    , draft : Maybe String
    }


{-| The document, how it is being edited, and the viewport showing it.
-}
type alias Model =
    { viewport : Viewport.Model
    , document : Document
    , history : History
    , selection : Set Int
    , tool : Tool
    , gridStep : Int
    , rotateStep : Int
    , drag : Maybe Drag
    , helpOpen : Bool
    }


{-| A handle being dragged: which one, what it moves, where the press began, the
document as it was then (every step is computed from it, so the drag can wander
and come back without drift) and how far it has gone.
-}
type alias Drag =
    { handle : Handle
    , ids : List Int
    , start : ( Float, Float )
    , original : Document
    , amount : Float
    }


type Msg
    = ViewportMsg Viewport.Msg
    | FileTextReceived String
    | FileFailed String
    | RequestFileUpload
    | NewDocument
    | ToggleHelp
    | UseTool Tool
    | Undo
    | Redo
    | DeleteSelection
    | DuplicateSelection
    | SelectAll
    | ClearSelection
    | RotateSelection Axis Int
    | SetColorSelection Int
    | NextGridStep
    | NextRotateStep
    | SetGridStep Int
    | SetRotateStep Int
    | Save
    | NoOp


{-| An empty document in a fresh viewport.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( viewport, viewportCmd ) =
            Viewport.init
                { ldrawBase = flags.ldrawBase
                , ldrawFallbackBase = flags.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = flags.initialHash
                , supportsPointerEvents = flags.supportsPointerEvents
                , useWindowResize = flags.useWindowResize
                , renderStyle = renderStyle flags
                , partCache = EmbeddedParts.cache
                }

        model =
            { viewport = viewport
            , document = Document.empty
            , history = Edit.emptyHistory
            , selection = Set.empty
            , tool = SelectTool
            , gridStep = 20
            , rotateStep = 90
            , drag = Nothing
            , helpOpen = False
            }

        ( opened, openCmd ) =
            openDocument (Maybe.map Document.parse flags.draft |> Maybe.withDefault Document.empty) model
    in
    ( opened
    , Cmd.batch
        [ Cmd.map ViewportMsg viewportCmd
        , openCmd
        , Ports.setUrlHash (Hash.encodeCamera viewport.camera)
        ]
    )


renderStyle : Flags -> Style.Style
renderStyle flags =
    let
        base =
            Style.defaultStyle
    in
    Style.clampStyle
        { base
            | ambientStrength = Maybe.withDefault base.ambientStrength flags.ambientStrength
            , lightStrength = Maybe.withDefault base.lightStrength flags.lightStrength
            , vibrance = Maybe.withDefault base.vibrance flags.vibrance
            , edgeWidth = Maybe.withDefault base.edgeWidth flags.edgeWidth
        }



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportMsg viewportMsg ->
            let
                ( viewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( next, eventCmds ) =
                    handleEvents events ( { model | viewport = viewport }, [] )
            in
            ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )

        FileTextReceived text ->
            openDocument (Document.parse text) model

        FileFailed message ->
            update (ViewportMsg (Viewport.FileLoadError message)) model

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        NewDocument ->
            openDocument Document.empty model

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        UseTool tool ->
            ( { model | tool = tool }, Cmd.none )

        Undo ->
            stepHistory Edit.undo model

        Redo ->
            stepHistory Edit.redo model

        DeleteSelection ->
            whenSelected (\ids -> commit (Remove ids)) model

        DuplicateSelection ->
            whenSelected duplicate model

        SelectAll ->
            ( { model | selection = Set.fromList (List.map .id (Document.mainPlacements model.document)) }
            , Cmd.none
            )

        ClearSelection ->
            ( { model | selection = Set.empty }, Cmd.none )

        RotateSelection axis sign ->
            whenSelected
                (\ids m ->
                    case Edit.selectionCenter ids m.document of
                        Just pivot ->
                            commit
                                (Rotate ids
                                    { pivot = pivot
                                    , axis = axis
                                    , radians = degrees (toFloat (sign * m.rotateStep))
                                    }
                                )
                                m

                        Nothing ->
                            ( m, Cmd.none )
                )
                model

        SetColorSelection colour ->
            whenSelected (\ids -> commit (Recolor (List.map (\id -> ( id, colour )) ids))) model

        NextGridStep ->
            ( { model | gridStep = nextStep gridSteps model.gridStep }, Cmd.none )

        NextRotateStep ->
            ( { model | rotateStep = nextStep rotateSteps model.rotateStep }, Cmd.none )

        SetGridStep step ->
            ( { model | gridStep = step }, Cmd.none )

        SetRotateStep step ->
            ( { model | rotateStep = step }, Cmd.none )

        Save ->
            ( model
            , Ports.downloadFile
                { filename =
                    if Document.isMultiFile model.document then
                        "model.mpd"

                    else
                        "model.ldr"
                , mime = "text/plain"
                , text = Writer.writeDocument model.document
                }
            )

        NoOp ->
            ( model, Cmd.none )


{-| Move steps in LDU, coarsest first: a stud pitch, a plate-ish half step, a
quarter step and a single unit.
-}
gridSteps : List Int
gridSteps =
    [ 20, 8, 4, 1 ]


rotateSteps : List Int
rotateSteps =
    [ 90, 15 ]


{-| The step after `current` in `steps`, wrapping round.
-}
nextStep : List Int -> Int -> Int
nextStep steps current =
    let
        index =
            steps
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, step ) -> step == current)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1
    in
    steps
        |> List.drop (modBy (List.length steps) (index + 1))
        |> List.head
        |> Maybe.withDefault current


{-| Run an edit on the selected placements, or do nothing when none are.
-}
whenSelected : (List Int -> Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
whenSelected edit model =
    case Set.toList model.selection of
        [] ->
            ( model, Cmd.none )

        ids ->
            edit ids model


{-| Do an edit: record it for undo, then bring the viewport and draft up to date.
-}
commit : Command -> Model -> ( Model, Cmd Msg )
commit command model =
    let
        ( document, history ) =
            Edit.record command model.document model.history
    in
    syncDocument { model | document = document, history = history }


{-| Copy the selection, offset by one grid step along X, and select the copies.
-}
duplicate : List Int -> Model -> ( Model, Cmd Msg )
duplicate ids model =
    let
        offset =
            Mat4.makeTranslate3 (toFloat model.gridStep) 0 0

        parts =
            ids
                |> List.filterMap (\id -> Document.getPlacement id model.document)
                |> List.map
                    (\p ->
                        { part = p.fileText
                        , color = p.color
                        , matrix = Mat4.mul offset p.matrix
                        }
                    )

        ( document, history ) =
            Edit.record (Add parts) model.document model.history

        -- Added placements take consecutive ids from the document's counter.
        newIds =
            List.range model.document.nextId (model.document.nextId + List.length parts - 1)
    in
    syncDocument
        { model | document = document, history = history, selection = Set.fromList newIds }


stepHistory : (Document -> History -> ( Document, History )) -> Model -> ( Model, Cmd Msg )
stepHistory step model =
    let
        ( document, history ) =
            step model.document model.history
    in
    syncDocument { model | document = document, history = history }


{-| Push the document to the viewport, forget which placements changed, drop
selected ids that no longer exist and queue the draft.
-}
syncDocument : Model -> ( Model, Cmd Msg )
syncDocument model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.applyDocument model.document model.viewport

        present =
            Document.mainPlacements model.document |> List.map .id |> Set.fromList

        synced =
            { model
                | viewport = viewport
                , document = Document.clearTouched model.document
                , selection = Set.filter (\id -> Set.member id present) model.selection
            }

        ( final, eventCmds ) =
            handleEvents events ( synced, [] )
    in
    ( final
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: Ports.saveDraft (Writer.writeDocument model.document) :: eventCmds)
    )


{-| Replace the document wholesale and load it into the viewport. History and
selection start afresh.
-}
openDocument : Document -> Model -> ( Model, Cmd Msg )
openDocument document model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.loadDocument document model.viewport

        ( next, eventCmds ) =
            handleEvents events
                ( { model
                    | viewport = viewport
                    , document = Document.clearTouched document
                    , history = Edit.emptyHistory
                    , selection = Set.empty
                    , drag = Nothing
                  }
                , []
                )
    in
    ( next
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: Ports.saveDraft (Writer.writeDocument document) :: eventCmds)
    )


handleEvents : List Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvents events start =
    List.foldl handleEvent start events


handleEvent : Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvent event ( model, cmds ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ] )

        Viewport.FlattenRequested payload ->
            ( model, cmds ++ [ Ports.requestGeometryFlatten payload ] )

        Viewport.Clicked x y shift ->
            ( select x y shift model, cmds )

        Viewport.Tapped x y ->
            ( select x y False model, cmds )

        Viewport.PressStarted x y ->
            case startDrag ( x, y ) model of
                Just drag ->
                    ( { model | viewport = Viewport.captureDrag model.viewport, drag = Just drag }, cmds )

                Nothing ->
                    ( model, cmds )

        Viewport.DragMoved x y ->
            case model.drag of
                Just drag ->
                    let
                        ( next, cmd ) =
                            moveDrag drag ( x, y ) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    ( model, cmds )

        Viewport.DragEnded _ _ ->
            ( endDrag model, cmds )

        Viewport.KeyAccepted keyEvent ->
            let
                studioContext =
                    { hasSelection = not (Set.isEmpty model.selection)
                    , helpVisible = model.helpOpen
                    , walking = model.viewport.camera.navigation == Camera.Walk
                    }
            in
            case StudioShortcuts.actionFor studioContext keyEvent of
                Just action ->
                    let
                        ( next, cmd ) =
                            update (msgForStudioAction action) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    case Shortcuts.actionFor (shortcutContext model) keyEvent of
                        Just action ->
                            let
                                ( next, cmd ) =
                                    update (msgForAction action) model
                            in
                            ( next, cmds ++ [ cmd ] )

                        Nothing ->
                            ( model, cmds )

        Viewport.LoadStarted ->
            ( model, cmds )

        Viewport.PartsReady ->
            ( model, cmds )

        Viewport.LargeModel _ ->
            ( model, cmds )

        Viewport.ModelReady ->
            ( model, cmds )

        Viewport.LoadFailed _ ->
            ( model, cmds )


{-| Select what is under a point. Shift toggles it in or out of the selection;
a plain click on nothing clears it.
-}
select : Float -> Float -> Bool -> Model -> Model
select x y shift model =
    case ( Viewport.pickSource x y model.viewport, shift ) of
        ( Just id, True ) ->
            { model
                | selection =
                    if Set.member id model.selection then
                        Set.remove id model.selection

                    else
                        Set.insert id model.selection
            }

        ( Just id, False ) ->
            { model | selection = Set.singleton id }

        ( Nothing, True ) ->
            model

        ( Nothing, False ) ->
            { model | selection = Set.empty }


{-| The handles for the current selection, when the move tool is active.
-}
currentHandles : Model -> List Handle
currentHandles model =
    if model.tool == MoveTool then
        case Edit.selectionCenter (Set.toList model.selection) model.document of
            Just center ->
                Gizmo.handles model.viewport.camera model.viewport.width model.viewport.height center

            Nothing ->
                []

    else
        []


{-| How close, in pixels, a press has to be to a handle to grab it.
-}
grabRadius : Float
grabRadius =
    16


startDrag : ( Float, Float ) -> Model -> Maybe Drag
startDrag point model =
    if model.tool == MoveTool then
        Gizmo.hit grabRadius point (currentHandles model)
            |> Maybe.map
                (\handle ->
                    { handle = handle
                    , ids = Set.toList model.selection
                    , start = point
                    , original = model.document
                    , amount = 0
                    }
                )

    else
        Nothing


{-| Follow the pointer: the document is the drag's original with the snapped
move applied, shown live but not yet recorded for undo.
-}
moveDrag : Drag -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
moveDrag drag point model =
    let
        amount =
            Gizmo.dragAmount (toFloat model.gridStep) drag.handle drag.start point

        delta =
            Vec3.scale amount (Edit.axisVector drag.handle.axis)

        ( moved, _ ) =
            Edit.apply (Translate drag.ids delta) drag.original

        -- `original` was taken after a sync, so its touched set is empty; the
        -- translated copy lists exactly the placements that moved.
        ( next, cmd ) =
            syncDocument { model | document = moved, drag = Just { drag | amount = amount } }
    in
    ( next, cmd )


{-| Finish a drag: the document already shows the move, so only its undo needs
recording.
-}
endDrag : Model -> Model
endDrag model =
    case model.drag of
        Just drag ->
            if drag.amount == 0 then
                { model | drag = Nothing }

            else
                { model
                    | drag = Nothing
                    , history =
                        Edit.pushApplied
                            (Translate drag.ids (Vec3.scale -drag.amount (Edit.axisVector drag.handle.axis)))
                            model.history
                }

        Nothing ->
            model


shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.viewport.camera.navigation == Camera.Walk
    }


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


msgForStudioAction : StudioShortcuts.Action -> Msg
msgForStudioAction action =
    case action of
        StudioShortcuts.Undo ->
            Undo

        StudioShortcuts.Redo ->
            Redo

        StudioShortcuts.DeleteSelection ->
            DeleteSelection

        StudioShortcuts.DuplicateSelection ->
            DuplicateSelection

        StudioShortcuts.SelectAll ->
            SelectAll

        StudioShortcuts.ClearSelection ->
            ClearSelection

        StudioShortcuts.UseTool tool ->
            UseTool tool

        StudioShortcuts.RotateSelection axis sign ->
            RotateSelection axis sign

        StudioShortcuts.NextGridStep ->
            NextGridStep

        StudioShortcuts.NextRotateStep ->
            NextRotateStep


{-| Map a viewport shortcut to the message it stands for. Playback shortcuts have
no meaning while editing.
-}
msgForAction : Shortcuts.Action -> Msg
msgForAction action =
    case action of
        Shortcuts.TogglePlayback ->
            NoOp

        Shortcuts.FitView ->
            ViewportMsg Viewport.FitView

        Shortcuts.ToggleWalk ->
            ViewportMsg Viewport.ToggleWalkMode

        Shortcuts.LeaveWalk ->
            ViewportMsg Viewport.ToggleWalkMode

        Shortcuts.ToggleHelp ->
            ToggleHelp

        Shortcuts.CloseHelp ->
            ToggleHelp

        Shortcuts.ZoomIn ->
            ViewportMsg (Viewport.Wheel -keyboardZoomStep)

        Shortcuts.ZoomOut ->
            ViewportMsg (Viewport.Wheel keyboardZoomStep)

        Shortcuts.PresetFront ->
            ViewportMsg (Viewport.SetViewPreset FrontView)

        Shortcuts.PresetBack ->
            ViewportMsg (Viewport.SetViewPreset BackView)

        Shortcuts.PresetLeft ->
            ViewportMsg (Viewport.SetViewPreset LeftView)

        Shortcuts.PresetRight ->
            ViewportMsg (Viewport.SetViewPreset RightView)

        Shortcuts.PresetTop ->
            ViewportMsg (Viewport.SetViewPreset TopView)

        Shortcuts.PresetIsometric ->
            ViewportMsg (Viewport.SetViewPreset IsometricView)



-- ── Subscriptions ─────────────────────────────────────────────────────────────


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ViewportMsg (Viewport.subscriptions model.viewport)
        , Ports.viewportResized (\size -> ViewportMsg (Viewport.WindowResize size.width size.height))
        , Ports.fileContentReceived FileTextReceived
        , Ports.fileLoadError FileFailed
        , Ports.geometryFlattened (ViewportMsg << Viewport.GeometryFlattened)
        , Ports.geometryFlattenFailed (ViewportMsg << Viewport.GeometryFlattenFailed)
        , Ports.saveRequested (\_ -> Save)
        ]



-- ── View ──────────────────────────────────────────────────────────────────────


view : Model -> Html Msg
view model =
    div
        [ Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" Theme.appBackground
        , Attr.style "position" "relative"
        ]
        [ Html.map ViewportMsg
            (Viewport.view
                { extraEntities =
                    \camera _ aspect ->
                        Overlay.entities camera aspect model.viewport.instancedScene model.selection (currentHandles model)
                , coarsenWhileMoving = True
                }
                model.viewport
            )
        , overlay model
        ]


overlay : Model -> Html Msg
overlay model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "pointer-events" "none"
        ]
        ([ viewStatus model
         , viewTopBar model
         , viewToolbar model
         , viewPad model
         ]
            ++ (if model.helpOpen then
                    [ ShortcutHelp.view
                        { rows = Shortcuts.helpRows ++ StudioShortcuts.helpRows
                        , onClose = ToggleHelp
                        }
                    ]

                else
                    []
               )
        )


viewTopBar : Model -> Html Msg
viewTopBar model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "right" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        , Attr.style "color" Theme.textMuted
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        ]
        [ span [ Attr.style "color" Theme.textPrimary, Attr.style "font-weight" "600" ] [ text "Palikkakehys Studio" ]
        , span [] [ text (documentSummary model) ]
        , div [ Attr.style "flex" "1" ] []
        , Button.secondary NewDocument "New"
        , Button.secondary RequestFileUpload "Open file"
        , Button.secondary Save "Save"
        ]


{-| Tools, edit actions and the snapping steps, down the left edge.
-}
viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        hasSelection =
            not (Set.isEmpty model.selection)

        group =
            div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "4px" ]
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "56px"
        , Attr.style "left" "12px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "12px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        ]
        [ group
            [ toolButton model SelectTool "Select"
            , toolButton model MoveTool "Move"
            , toolButton model RotateTool "Rotate"
            ]
        , group
            ([ Button.toggle { active = False, enabled = Edit.canUndo model.history } Undo "Undo"
             , Button.toggle { active = False, enabled = Edit.canRedo model.history } Redo "Redo"
             , Button.toggle { active = False, enabled = hasSelection } DuplicateSelection "Duplicate"
             , Button.toggle { active = False, enabled = hasSelection } DeleteSelection "Delete"
             ]
                ++ rotateButtons model hasSelection
            )
        , group
            [ Button.secondary NextGridStep ("Move step " ++ String.fromInt model.gridStep)
            , Button.secondary NextRotateStep ("Turn step " ++ String.fromInt model.rotateStep ++ "°")
            ]
        ]


toolButton : Model -> Tool -> String -> Html Msg
toolButton model tool label =
    Button.toggle { active = model.tool == tool, enabled = True } (UseTool tool) label


{-| The rotate tool's buttons: a turn either way about each axis.
-}
rotateButtons : Model -> Bool -> List (Html Msg)
rotateButtons model hasSelection =
    if model.tool == RotateTool then
        [ ( "X", AxisX ), ( "Y", AxisY ), ( "Z", AxisZ ) ]
            |> List.concatMap
                (\( name, axis ) ->
                    [ Button.toggle { active = False, enabled = hasSelection } (RotateSelection axis -1) (name ++ " −")
                    , Button.toggle { active = False, enabled = hasSelection } (RotateSelection axis 1) (name ++ " +")
                    ]
                )

    else
        []


documentSummary : Model -> String
documentSummary model =
    let
        count =
            List.length (Document.mainPlacements model.document)

        selected =
            Set.size model.selection
    in
    String.fromInt count
        ++ (if count == 1 then
                " part"

            else
                " parts"
           )
        ++ (if selected > 0 then
                ", " ++ String.fromInt selected ++ " selected"

            else
                ""
           )


viewStatus : Model -> Html Msg
viewStatus model =
    Status.view
        { error = model.viewport.errorMsg
        , onDismiss = ViewportMsg Viewport.DismissError
        , phase =
            case model.viewport.loadPhase of
                Viewport.Ready ->
                    Status.Done

                Viewport.Idle ->
                    Status.Done

                Viewport.FetchingTopLevel _ ->
                    Status.Fetching

                Viewport.ResolvingParts ->
                    Status.ResolvingParts
                        { loaded = model.viewport.partsLoaded
                        , total = model.viewport.partsTotal
                        , percent = model.viewport.loadingProgressPct
                        }

                Viewport.FlatteningGeometry ->
                    Status.Finalizing model.viewport.loadingProgressPct
        }


viewPad : Model -> Html Msg
viewPad model =
    ViewPad.view
        { walking = model.viewport.camera.navigation == Camera.Walk
        , helpOpen = model.helpOpen
        , play = Nothing
        , onFit = ViewportMsg Viewport.FitView
        , onToggleWalk = ViewportMsg Viewport.ToggleWalkMode
        , onToggleHelp = ToggleHelp
        , onStartRotate = \azimuth elevation -> ViewportMsg (Viewport.StartHoldRotate azimuth elevation)
        , onEndRotate = ViewportMsg Viewport.EndHoldRotate
        }
