module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , accomplishmentBanner
    , init
    , main
    , shareHash
    , subscriptions
    , thumbnailRequests
    , update
    , view
    , viewToast
    )

{-| Main application for Brick Contraptions.

Provides a Technic building playground where gears, beams, axles, and pins
can be assembled with stud/axle snapping and simulated with real-time gear physics.
Building and simulating share one view: Space plays and pauses the machine while
editing stays available. A drag on the canvas picks up and moves parts relative
to the view; there is no axis gizmo — while something is selected, a floating
action bar above the dock turns, tips, raises, lowers or deletes it (the same as
the keyboard). The camera has its own control, the shared camera globe
(`UI.CameraGlobe`) in the bottom-right corner: drag it to fly the camera around
the model, tap it to fly there, and use its zoom slider and reset button. On a
narrow portrait window (a phone) the bottom area shows either the parts dock or
the camera globe, switched by a toggle button.

While the machine plays (and while it is paused part-way) every gear in the
motor's gear train is drawn turning by the shared `Simulation` component, with
the axles and parts threaded on those gears; their static copies are hidden from
the studio's scene (see `Contraption.Animation`).

The dock is a compact picture strip (`UI.PartCard.compactView`) beside a vertical
column of category tabs, exactly as tall as the camera globe panel, with a rendered
image from the shared thumbnail pipeline (through `Ports`); the images of the visible
category are asked for on start and whenever the category changes. The top-right
menu (`UI.HamburgerMenu`) holds undo, redo, delete and clear, beside a visible ?
button that opens the shortcut list and the play button; Escape closes the menu
first.
Angles are a pure function of `currentTime`. Any edit — a drag, a key move, an
added, removed or turned part, undo or redo — stops the machine and rewinds it
to time 0, so the parts are static and editable again; the next Play
re-analyses the machine.
Accomplishment is celebrated when all placed gears are engaged in the gear graph
and rotating as a single machine.

-}

import Array
import Browser
import Browser.Dom
import Browser.Events
import Contraption.Animation as Animation exposing (Animated)
import Contraption.Catalog as Catalog exposing (Category(..), PartItem)
import Contraption.Machine as Machine exposing (MachineState)
import Contraption.Placement as Placement
import Data
import Dict exposing (Dict)
import EmbeddedParts
import FeatherIcons
import Gear.Placements as Placements
import Html exposing (Html, button, div, h1, header, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import LDraw.Colors as Colors
import LDraw.Document as Document
import LDraw.Writer as Writer
import Ports
import Process
import Render.Camera as Camera
import Render.Style as Style
import Set
import Simulation
import Studio.Core as Core
import Studio.GearFeedback as GearFeedback
import Studio.Shortcuts as StudioShortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload
import Task
import UI.Button as Button
import UI.CameraGlobe as CameraGlobe
import UI.Focus as Focus
import UI.HamburgerMenu as HamburgerMenu
import UI.Icon as Icon
import UI.PartCard as PartCard
import UI.ShortcutHelp as ShortcutHelp
import UI.SnapshotModal as SnapshotModal
import Url
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Shortcuts as ViewportShortcuts


{-| Application flags received from JavaScript runtime.
-}
type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , initialHash : String
    , supportsPointerEvents : Bool
    }


{-| Main Contraptions model.
-}
type alias Model =
    { studio : Core.Model
    , machine : MachineState
    , playing : Bool
    , globeDrag : Maybe CameraGlobe.Drag
    , cameraShown : Bool
    , lastTapped : Maybe String
    , helpOpen : Bool
    , menuOpen : Bool
    , activeCategory : Category
    , projectParts : List PartItem
    , currentTime : Float
    , motorRpm : Float
    , simulation : Simulation.Model
    , animated : Animated
    , windowWidth : Int
    , windowHeight : Int
    , thumbnails : Dict String Thumbnail
    , toast : Maybe { message : String, id : Int }
    , toastCounter : Int
    , snapshotModal : Maybe SnapshotModal.Model
    }


{-| A rendered dock image (a data URL) and the part's .dat title (`""` when
unknown), kept under `PartCard.thumbnailKey file color`.
-}
type alias Thumbnail =
    { url : String
    , title : String
    }


{-| Messages handled by the Contraptions application.
-}
type Msg
    = StudioMsg Core.Msg
    | TogglePlay
    | ToggleRaytrace
    | CaptureSnapshot
    | SnapshotReady { dataUrl : String, filename : String }
    | SnapshotSave { dataUrl : String, filename : String }
    | SnapshotClose
    | GlobePressed ( Float, Float )
    | GlobeMoved ( Float, Float )
    | GlobeReleased
    | ZoomTo Float
    | ZoomDone
    | ResetView
    | ToggleCameraPanel
    | ToggleHelp
    | ToggleMenu
    | CloseMenu
    | ShareLink
    | DismissToast Int
    | MenuPicked Msg
    | NoOp
    | SetCategory Category
    | AddPart PartItem
    | Undo
    | Redo
    | Delete
    | TurnSelection TurnDirection
    | RaiseSelection
    | LowerSelection
    | ToggleFine
    | Clear
    | FitView
    | Tick Float
    | WindowResized Int Int
    | ThumbnailLoaded { file : String, color : Int, url : String, title : String }


{-| Initialize the Contraptions application.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        payload =
            UrlPayload.decodeHash flags.initialHash

        initialDoc =
            case payload.modelText of
                Just text ->
                    Document.parse text

                Nothing ->
                    Document.empty

        projectParts =
            List.map Catalog.fromKitItem payload.parts

        activeCategory =
            if List.isEmpty projectParts then
                GearsCategory

            else
                ProjectCategory

        config =
            { ldrawBase = flags.ldrawBase
            , ldrawFallbackBase = flags.ldrawFallbackBase
            , defaultModelUrl = ""
            , initialHash = flags.initialHash
            , supportsPointerEvents = flags.supportsPointerEvents
            , document = initialDoc
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( initialStudio, studioCmd ) =
            Core.init config
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        >> Core.setGearAlignment True
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        -- A drag slides parts at their height, so gear and axle
                        -- axes stay on the axle line; Q/E and the action bar
                        -- still raise and lower them.
                        >> Core.setDragSettles False
                    )

        initialMachine =
            Machine.analyseMachine initialDoc initialStudio.viewport.partCache

        motorRpm =
            30.0

        initialModel =
            { studio = initialStudio
            , machine = initialMachine
            , playing = False
            , globeDrag = Nothing
            , cameraShown = False
            , lastTapped = Nothing
            , helpOpen = False
            , menuOpen = False
            , activeCategory = activeCategory
            , projectParts = projectParts
            , currentTime = 0.0
            , motorRpm = motorRpm
            , simulation =
                Simulation.init
                    { gearSpecs = Data.gearParts
                    , lodCache = EmbeddedParts.lodCache
                    , maxRpm = motorRpm
                    , initialRpm = motorRpm
                    , requestedMotorIndex = Nothing
                    }
            , animated = Animation.none
            , windowWidth = 1280
            , windowHeight = 800
            , thumbnails = Dict.empty
            , toast = Nothing
            , toastCounter = 0
            , snapshotModal = Nothing
            }

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))
    in
    ( initialModel
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , fetchViewportSize
        , requestDockThumbnails initialModel
        ]
    )


{-| Ask for the images of the parts in the dock's current category that have
none yet.
-}
requestDockThumbnails : Model -> Cmd Msg
requestDockThumbnails model =
    case thumbnailRequests (dockParts model) model.thumbnails of
        [] ->
            Cmd.none

        requests ->
            Ports.requestThumbnails requests


{-| The parts the dock lists for its current category.
-}
dockParts : Model -> List PartItem
dockParts model =
    Catalog.partsForCategoryWithProject model.projectParts model.activeCategory


{-| The thumbnails to ask the pipeline for: one per distinct part and colour in
the list that has none yet. `rgb` comes from the colour table the 3D view uses,
so the pictures match the parts on the canvas.
-}
thumbnailRequests : List PartItem -> Dict String Thumbnail -> List { file : String, color : Int, rgb : List Float }
thumbnailRequests items thumbnails =
    let
        addRequest item ( seen, acc ) =
            let
                key =
                    PartCard.thumbnailKey item.file item.color
            in
            if Set.member key seen || Dict.member key thumbnails then
                ( seen, acc )

            else
                ( Set.insert key seen
                , { file = item.file
                  , color = item.color
                  , rgb =
                        Colors.resolveColor item.color 16
                            |> (\c -> [ c.r, c.g, c.b ])
                  }
                    :: acc
                )
    in
    List.foldl addRequest ( Set.empty, [] ) items
        |> Tuple.second
        |> List.reverse


{-| Update the application model. Whatever changed the document, the machine
stops and rewinds (`stopForEdit`).
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            step msg model
    in
    -- Compared without `touched`, which the studio clears on its own.
    if next.studio.document.submodels /= model.studio.document.submodels then
        ( stopForEdit next, cmd )

    else
        ( next, cmd )


{-| An edit puts the machine back at rest: paused, at time 0, nothing animated,
so every part is drawn by the static scene again.
-}
stopForEdit : Model -> Model
stopForEdit model =
    { model
        | playing = False
        , currentTime = 0
        , simulation = Simulation.setTime 0 model.simulation
        , animated = Animation.none
    }


{-| Play from rest analyses the current document for the turning gears and
their meshes; resuming a pause keeps the analysis and the clock.
-}
startPlaying : Model -> Model
startPlaying model =
    if model.currentTime > 0 then
        { model | playing = True }

    else
        let
            ( simulation, _ ) =
                Simulation.analyse
                    { lines = Document.toLines model.studio.document
                    , cache = model.studio.viewport.partCache
                    }
                    (Simulation.setTime 0 model.simulation)

            gears =
                simulation.gearGraph
                    |> Maybe.map (.instances >> Array.toList)
                    |> Maybe.withDefault []
        in
        { model
            | playing = True
            , simulation = Simulation.setTime 0 simulation
            , animated =
                let
                    turning =
                        Simulation.rotatingGearIds simulation
                in
                Animation.rotating turning
                    (Placements.gearSources model.studio.document gears)
                    (Placements.partSources model.studio.document (Simulation.drivenPartsFor turning simulation))
        }


{-| Encode current document and project parts into a shareable URL hash.
-}
shareHash : Model -> String
shareHash model =
    let
        doc =
            model.studio.document

        modelStr =
            Writer.writeDocument doc

        partsStr =
            model.projectParts
                |> List.map (\p -> { part = p.file, color = p.color, count = 1 })
                |> UrlPayload.encodeParts

        hasModel =
            not (List.isEmpty (Document.mainPlacements doc))

        hasParts =
            not (String.isEmpty (String.trim partsStr))
    in
    case ( hasModel, hasParts ) of
        ( True, True ) ->
            "model=" ++ Url.percentEncode modelStr ++ "&parts=" ++ Url.percentEncode partsStr

        ( True, False ) ->
            "model=" ++ Url.percentEncode modelStr

        ( False, True ) ->
            "parts=" ++ Url.percentEncode partsStr

        ( False, False ) ->
            ""


{-| Update URL hash if the given core events contain a document change.
-}
syncDocHash : Model -> List Core.Event -> Cmd Msg
syncDocHash model events =
    let
        hasDocChange ev =
            case ev of
                Core.DocumentChanged _ ->
                    True

                _ ->
                    False
    in
    if List.any hasDocChange events then
        Ports.setUrlHash (shareHash model)

    else
        Cmd.none


step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        StudioMsg studioMsg ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.update studioMsg model.studio

                newMachine =
                    case List.filterMap extractDocChanged events of
                        updatedDoc :: _ ->
                            Machine.analyseMachine updatedDoc nextStudio.viewport.partCache

                        [] ->
                            model.machine

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine }

                syncCmd =
                    syncDocHash nextModel events
            in
            List.foldl applyShortcut
                ( nextModel
                , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncCmd ]
                )
                (List.filterMap extractKey events)

        TogglePlay ->
            if model.playing then
                ( { model | playing = False }, Cmd.none )

            else
                ( startPlaying model, Cmd.none )

        GlobePressed point ->
            ( { model | globeDrag = Just (CameraGlobe.startDrag model.studio.viewport.camera point) }
            , Cmd.none
            )

        GlobeMoved point ->
            case model.globeDrag of
                Just drag ->
                    let
                        ( ( azimuthDelta, elevationDelta ), nextDrag ) =
                            CameraGlobe.moveDrag drag point
                    in
                    update (viewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta))
                        { model | globeDrag = Just nextDrag }

                Nothing ->
                    ( model, Cmd.none )

        GlobeReleased ->
            let
                released =
                    { model | globeDrag = Nothing }
            in
            case model.globeDrag |> Maybe.andThen (CameraGlobe.releaseDrag model.studio.viewport.camera) of
                Just ( azimuthDelta, elevationDelta ) ->
                    update (viewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta)) released

                Nothing ->
                    ( released, Cmd.none )

        ZoomTo level ->
            update (viewportMsg (Viewport.Wheel (CameraGlobe.wheelDeltaForZoom model.studio.viewport.camera level))) model

        ZoomDone ->
            ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.zoomSliderId) )

        ResetView ->
            let
                ( fitted, fitCmd ) =
                    update FitView model
            in
            ( fitted
            , Cmd.batch [ fitCmd, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.resetButtonId) ]
            )

        ToggleCameraPanel ->
            ( { model | cameraShown = not model.cameraShown }, Cmd.none )

        ToggleHelp ->
            -- The studio pauses its editing keys while the dialog is open, so
            -- Esc and ? reach this app to close it.
            let
                open =
                    not model.helpOpen
            in
            ( syncKeyPause { model | helpOpen = open, menuOpen = False }, Cmd.none )

        ToggleMenu ->
            ( syncKeyPause { model | menuOpen = not model.menuOpen && not model.helpOpen }, Cmd.none )

        CloseMenu ->
            ( syncKeyPause { model | menuOpen = False }, Cmd.none )

        ShareLink ->
            let
                hash =
                    shareHash model

                nextId =
                    model.toastCounter + 1
            in
            ( { model
                | menuOpen = False
                , toast = Just { message = "Link copied to clipboard!", id = nextId }
                , toastCounter = nextId
              }
            , Cmd.batch
                [ Ports.setUrlHash hash
                , Ports.copyToClipboard ("#" ++ hash)
                , Process.sleep 3000 |> Task.perform (\_ -> DismissToast nextId)
                ]
            )

        DismissToast id ->
            if Maybe.map .id model.toast == Just id then
                ( { model | toast = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        MenuPicked action ->
            -- The menu closes, then the chosen message runs as if its button were pressed.
            let
                ( next, cmd ) =
                    update action (syncKeyPause { model | menuOpen = False })
            in
            ( syncKeyPause next, cmd )

        NoOp ->
            ( model, Cmd.none )

        SetCategory category ->
            let
                switched =
                    { model | activeCategory = category }
            in
            ( switched, requestDockThumbnails switched )

        ThumbnailLoaded { file, color, url, title } ->
            ( { model
                | thumbnails =
                    Dict.insert (PartCard.thumbnailKey file color) { url = url, title = title } model.thumbnails
              }
            , Cmd.none
            )

        AddPart item ->
            let
                -- Technic parts go on the common axle line, each on a free
                -- spot next to the selection; see `Contraption.Placement`.
                ( nextStudio, studioCmd, events ) =
                    case Placement.targetFor item.file model.studio of
                        Placement.AtPlacement matrix ->
                            Core.addPartAt item.file item.color matrix model.studio

                        Placement.OnGround spot ->
                            Core.addPart item.file item.color spot model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine, lastTapped = Just (dockName model.thumbnails item) }
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        Undo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.undo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine }
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        Redo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.redo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine }
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        Delete ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.deleteSelection model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine }
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        TurnSelection direction ->
            runCore (Core.turnSelected direction) model

        RaiseSelection ->
            runCore Core.raiseSelected model

        LowerSelection ->
            runCore Core.lowerSelected model

        ToggleFine ->
            ( { model | studio = Core.setFineMove (not (Core.fineMove model.studio)) model.studio }
            , Cmd.none
            )

        ToggleRaytrace ->
            let
                nextQuality =
                    case Core.renderQuality model.studio of
                        Style.Draft ->
                            Style.Realistic

                        Style.Realistic ->
                            Style.Draft
            in
            ( { model | studio = Core.setRenderQuality nextQuality model.studio }
            , Cmd.none
            )

        CaptureSnapshot ->
            ( { model | snapshotModal = Just (SnapshotModal.init { filename = "brick-contraptions-render.png" }) }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = False
                , filename = "brick-contraptions-render.png"
                }
            )

        SnapshotReady payload ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setReady payload) model.snapshotModal }
            , Cmd.none
            )

        SnapshotSave payload ->
            ( { model | snapshotModal = Maybe.map SnapshotModal.markSaved model.snapshotModal }
            , Ports.saveSnapshot payload
            )

        SnapshotClose ->
            ( { model | snapshotModal = Nothing }
            , Cmd.none
            )

        Clear ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.resetDocument Document.empty model.studio

                newMachine =
                    Machine.analyseMachine Document.empty nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio, machine = newMachine, currentTime = 0.0 }
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        FitView ->
            let
                ( nextStudio, studioCmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = nextStudio }, Cmd.map StudioMsg studioCmd )

        Tick delta ->
            if model.playing then
                let
                    dt =
                        delta / 1000.0

                    newTime =
                        model.currentTime + dt
                in
                ( { model
                    | currentTime = newTime
                    , simulation = Simulation.setTime newTime model.simulation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        WindowResized w h ->
            -- The viewport projects picks and gizmo handles through its own
            -- size, so it must track the canvas, not keep its 800×600 default.
            update (viewportMsg (Viewport.WindowResize w h)) { model | windowWidth = w, windowHeight = h }


{-| The studio pauses its editing keys while a layer is open (the help dialog or
the menu), so Escape reaches the app to close that layer first.
-}
syncKeyPause : Model -> Model
syncKeyPause model =
    { model | studio = Core.setHelpVisible (model.helpOpen || model.menuOpen) model.studio }


{-| Run a studio edit, re-analyse the machine and forward the effects.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore edit model =
    let
        ( nextStudio, studioCmd, events ) =
            edit model.studio

        nextModel =
            { model
                | studio = nextStudio
                , machine = Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache
            }
    in
    ( nextModel
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events, syncDocHash nextModel events ]
    )


extractKey : Core.Event -> Maybe ViewportShortcuts.KeyEvent
extractKey event =
    case event of
        Core.KeyAccepted keyEvent ->
            Just keyEvent

        _ ->
            Nothing


{-| Run the shortcut a key press stands for, after the studio has had its turn.
-}
applyShortcut : ViewportShortcuts.KeyEvent -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
applyShortcut keyEvent ( model, cmd ) =
    if model.menuOpen && closesMenu keyEvent then
        let
            ( next, actionCmd ) =
                update CloseMenu model
        in
        ( next, Cmd.batch [ cmd, actionCmd ] )

    else
        case ViewportShortcuts.actionFor (shortcutContext model) keyEvent of
            Just action ->
                let
                    ( next, actionCmd ) =
                        update (msgForAction action) model
                in
                ( next, Cmd.batch [ cmd, actionCmd ] )

            Nothing ->
                ( model, cmd )


{-| Escape closes an open menu first. Asked of the studio's own shortcut table,
so the key is still matched in one place.
-}
closesMenu : ViewportShortcuts.KeyEvent -> Bool
closesMenu keyEvent =
    StudioShortcuts.actionFor
        { hasSelection = False
        , helpVisible = True
        , walking = False
        , inSubmodel = False
        , menuOpen = True
        }
        keyEvent
        == Just StudioShortcuts.CloseMenu


{-| What `Escape` and `?` need to know to pick between their meanings.
-}
shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.studio.viewport.camera.navigation == Camera.Walk
    }


{-| Map a viewport shortcut to the message it stands for.
-}
msgForAction : ViewportShortcuts.Action -> Msg
msgForAction action =
    case action of
        ViewportShortcuts.TogglePlayback ->
            TogglePlay

        ViewportShortcuts.FitView ->
            FitView

        ViewportShortcuts.ToggleWalk ->
            viewportMsg Viewport.ToggleWalkMode

        ViewportShortcuts.LeaveWalk ->
            viewportMsg Viewport.ToggleWalkMode

        ViewportShortcuts.ToggleHelp ->
            ToggleHelp

        ViewportShortcuts.CloseHelp ->
            ToggleHelp

        ViewportShortcuts.ZoomIn ->
            viewportMsg (Viewport.Wheel -keyboardZoomStep)

        ViewportShortcuts.ZoomOut ->
            viewportMsg (Viewport.Wheel keyboardZoomStep)

        ViewportShortcuts.PresetFront ->
            viewportMsg (Viewport.SetViewPreset FrontView)

        ViewportShortcuts.PresetBack ->
            viewportMsg (Viewport.SetViewPreset BackView)

        ViewportShortcuts.PresetLeft ->
            viewportMsg (Viewport.SetViewPreset LeftView)

        ViewportShortcuts.PresetRight ->
            viewportMsg (Viewport.SetViewPreset RightView)

        ViewportShortcuts.PresetTop ->
            viewportMsg (Viewport.SetViewPreset TopView)

        ViewportShortcuts.PresetBottom ->
            viewportMsg (Viewport.SetViewPreset BottomView)

        ViewportShortcuts.PresetIsometric ->
            viewportMsg (Viewport.SetViewPreset IsometricView)


viewportMsg : Viewport.Msg -> Msg
viewportMsg =
    StudioMsg << Core.ViewportMsg


{-| How far `+` and `-` zoom, in wheel units.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


extractDocChanged : Core.Event -> Maybe Document.Document
extractDocChanged event =
    case event of
        Core.DocumentChanged doc ->
            Just doc

        _ ->
            Nothing


handleCoreEvents : List Core.Event -> Cmd Msg
handleCoreEvents events =
    let
        toCmd ev =
            case ev of
                Core.FlattenRequested payload ->
                    Ports.requestGeometryFlatten payload

                _ ->
                    Cmd.none
    in
    Cmd.batch (List.map toCmd events)


{-| Global subscriptions for Contraptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , Ports.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , Ports.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Ports.thumbnailLoaded ThumbnailLoaded
        , Ports.snapshotReady SnapshotReady
        , Browser.Events.onResize WindowResized
        , if model.playing then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]


{-| Render the Contraptions application.
-}
view : Model -> Html Msg
view model =
    let
        accomplished =
            Machine.isAccomplished model.machine

        gearCount =
            List.length model.machine.gears
    in
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- One row across the top: the accomplishment banner sits in the middle,
          -- the help, play and menu buttons on the right. The banner shares the
          -- buttons' height and top edge, and shrinks before it reaches them.
          header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "1rem"
            , Attr.style "left" "1rem"
            , Attr.style "right" "1rem"
            , Attr.style "height" "2.25rem"
            , Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "1fr minmax(0, auto) 1fr"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.75rem"
            , Attr.style "z-index" "20"
            , Attr.style "pointer-events" "none"
            , keepFocusOnCanvas
            ]
            [ h1 [ Attr.class "sr-only" ] [ text "Palikkakehys Contraptions" ]
            , div [] []
            , if accomplished then
                accomplishmentBanner gearCount

              else
                div [ Attr.style "display" "none" ] []
            , div
                [ Attr.style "justify-self" "end"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "pointer-events" "auto"
                ]
                [ helpButton
                , playButton model.playing
                , HamburgerMenu.view (menuConfig model)
                ]
            ]

        -- 3D WebGL Canvas
        , Html.map StudioMsg (viewCanvas model)

        -- Camera control: the canvas drag edits, so the camera lives here. On a
        -- narrow window it shares the bottom row with the dock instead.
        , if isNarrow model then
            text ""

          else
            div
                [ Attr.style "position" "absolute"
                , Attr.style "right" "0.75rem"
                , Attr.style "bottom" "0.75rem"
                , Attr.style "z-index" "10"
                ]
                [ globeControl model ]
        , if model.helpOpen then
            -- Above the header, dock and camera pad (all z-index 10), which
            -- would otherwise cover the top and bottom of the dialog.
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" "0"
                , Attr.style "z-index" "30"
                , Attr.style "pointer-events" "none"
                ]
                [ ShortcutHelp.view { rows = helpRows, onClose = ToggleHelp } ]

          else
            text ""

        -- Bottom: the selection action bar floats above the Technic dock, whose
        -- bottom edge lines up with the camera globe's
        , bottomArea model
        , case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        , case model.snapshotModal of
            Just modal ->
                SnapshotModal.view
                    { onSave = SnapshotSave
                    , onClose = SnapshotClose
                    }
                    modal

            Nothing ->
                text ""
        ]


{-| Toast notification overlay.
-}
viewToast : String -> Html Msg
viewToast message =
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.style "position" "fixed"
        , Attr.style "top" "1rem"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "padding" "0.625rem 1.125rem"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "box-shadow" "0 4px 16px rgba(0, 0, 0, 0.5)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "500"
        , Attr.style "z-index" "60"
        , Attr.style "pointer-events" "none"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        ]
        [ Icon.featherSized 16 "check"
        , text message
        ]


{-| Below this window width (CSS px, a portrait phone) the bottom area shows
either the dock or the camera globe, never both.
-}
narrowWidth : Int
narrowWidth =
    600


isNarrow : Model -> Bool
isNarrow model =
    model.windowWidth < narrowWidth


{-| The bottom area: the gear feedback pill, the selection action bar, then the
dock. On a narrow window the dock row also carries the camera toggle, and shows
the camera globe instead of the dock while the toggle is on.
-}
bottomArea : Model -> Html Msg
bottomArea model =
    div
        ([ Attr.style "position" "absolute"
         , Attr.style "bottom" "0.75rem"
         , Attr.style "display" "flex"
         , Attr.style "flex-direction" "column"
         , Attr.style "gap" "0.5rem"
         , Attr.style "z-index" "10"
         , Attr.style "pointer-events" "none"
         ]
            ++ dockPlacement model.windowWidth
        )
        [ -- Truncated to the row rather than pushing the bar off a phone
          div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "max-width" "100%"
            , Attr.style "min-width" "0"
            , Attr.style "overflow" "hidden"
            , Attr.style "white-space" "nowrap"
            ]
            [ gearPill model ]
        , selectionBar model
        , touchLabel model
        , if isNarrow model then
            narrowRow model

          else
            viewDock model
        ]


{-| The narrow dock row: the dock (or the camera globe) and the toggle between
them, at a finger-sized width.
-}
narrowRow : Model -> Html Msg
narrowRow model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "width" "100%"
        , Attr.style "min-width" "0"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "flex" "1 1 auto"
            , Attr.style "min-width" "0"
            , Attr.style "pointer-events" "auto"
            ]
            [ if model.cameraShown then
                globeControl model

              else
                viewDock model
            ]
        , cameraToggle model
        ]


{-| The toggle between the dock and the camera globe on a narrow window. Its icon
shows what is on screen now; `aria-pressed` is on while the camera is shown.
-}
cameraToggle : Model -> Html Msg
cameraToggle model =
    let
        icon =
            if model.cameraShown then
                FeatherIcons.camera

            else
                FeatherIcons.box
    in
    button
        [ onClick ToggleCameraPanel
        , Attr.attribute "aria-label" "Camera controls"
        , Attr.attribute "aria-pressed"
            (if model.cameraShown then
                "true"

             else
                "false"
            )
        , Attr.title "Camera controls"
        , Attr.class "touch-target"
        , Attr.style "display" "flex"
        , Attr.style "flex-shrink" "0"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.75rem"
        , Attr.style "height" "2.75rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.75rem"
        , Attr.style "background"
            (if model.cameraShown then
                "var(--color-brand-yellow)"

             else
                "rgba(5, 19, 29, 0.9)"
            )
        , Attr.style "color"
            (if model.cameraShown then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        [ icon
            |> FeatherIcons.withSize 20
            |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]
        ]


{-| The shared camera globe, with the zoom slider and the reset button.
-}
globeControl : Model -> Html Msg
globeControl model =
    CameraGlobe.view
        { camera = model.studio.viewport.camera
        , onPress = GlobePressed
        , onMove = GlobeMoved
        , onRelease = GlobeReleased
        , onZoom = ZoomTo
        , onZoomDone = ZoomDone
        , onReset = ResetView
        , extras =
            [ div
                [ keepFocusOnCanvas
                , Attr.style "display" "contents"
                ]
                [ Button.icon
                    { icon = Icon.feather "sun"
                    , label = "Realistic rendering"
                    , shortcut = Nothing
                    , pressed = Just (Core.renderQuality model.studio == Style.Realistic)
                    , enabled = True
                    , showLabel = False
                    }
                    ToggleRaytrace
                , Button.icon
                    { icon = Icon.feather "camera"
                    , label = "Take snapshot"
                    , shortcut = Nothing
                    , pressed = Nothing
                    , enabled = True
                    , showLabel = False
                    }
                    CaptureSnapshot
                ]
            ]
        }


{-| On touch screens, where tooltips never show: the name of the dock part last
tapped, above the dock. Hidden while the camera globe takes the bottom row.
-}
touchLabel : Model -> Html Msg
touchLabel model =
    case model.lastTapped of
        Just name ->
            if isNarrow model && model.cameraShown then
                text ""

            else
                div
                    [ Attr.class "touch-only"
                    , Attr.style "align-self" "center"
                    , Attr.style "max-width" "100%"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "text-overflow" "ellipsis"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "padding" "0.125rem 0.75rem"
                    , Attr.style "border-radius" "9999px"
                    , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                    , Attr.style "color" "var(--color-text-on-dark)"
                    , Attr.style "font-size" "0.8125rem"
                    , Attr.style "font-weight" "600"
                    , Attr.style "pointer-events" "auto"
                    ]
                    [ Keyed.node "span" [] [ ( name, span [] [ text name ] ) ] ]

        Nothing ->
            text ""


{-| The gear feedback pill, keyed by its text so a changed status replaces it.
-}
gearPill : Model -> Html Msg
gearPill model =
    case GearFeedback.gearFeedback (Set.toList model.studio.selection) model.studio.document of
        Just feedback ->
            let
                label =
                    GearFeedback.feedbackText feedback
            in
            Keyed.node "span" [ Attr.style "display" "contents" ] [ ( label, GearFeedback.pill feedback ) ]

        Nothing ->
            text ""


{-| The name of the selection for the action bar: the part's name when one part
is selected, else how many parts are.
-}
selectionName : Model -> String
selectionName model =
    case Set.toList model.studio.selection of
        [ id ] ->
            case List.filter (\p -> p.id == id) (Core.getPlacements model.studio) of
                placed :: _ ->
                    case List.filter (\item -> item.file == placed.part) Catalog.curatedParts of
                        item :: _ ->
                            item.name

                        [] ->
                            Dict.get (PartCard.thumbnailKey placed.part placed.color) model.thumbnails
                                |> Maybe.map .title
                                |> Maybe.withDefault ""
                                |> String.trim
                                |> (\title ->
                                        if String.isEmpty title then
                                            String.replace ".dat" "" placed.part

                                        else
                                            title
                                   )

                [] ->
                    ""

        ids ->
            String.fromInt (List.length ids) ++ " parts"


{-| The top-right menu: undo, redo, delete, clear and the shortcut list. Each
item does what its keyboard shortcut does.
-}
menuConfig : Model -> HamburgerMenu.Config Msg
menuConfig model =
    { id = menuId
    , open = model.menuOpen
    , onToggle = ToggleMenu
    , onClose = CloseMenu
    , items =
        [ { label = "Share link", shortcut = "", enabled = True, onSelect = MenuPicked ShareLink }
        , { label = "Undo", shortcut = "Ctrl+Z", enabled = Core.canUndo model.studio, onSelect = MenuPicked Undo }
        , { label = "Redo", shortcut = "Ctrl+Shift+Z", enabled = Core.canRedo model.studio, onSelect = MenuPicked Redo }
        , { label = "Delete", shortcut = "Del", enabled = Core.hasSelection model.studio, onSelect = MenuPicked Delete }
        , { label = "Clear", shortcut = "", enabled = List.length (Core.getPlacements model.studio) > 0, onSelect = MenuPicked Clear }
        ]
    }


{-| The visible shortcut-reference button: it toggles the help overlay, as the
`?` key does.
-}
helpButton : Html Msg
helpButton =
    button
        [ onClick ToggleHelp
        , Attr.attribute "aria-label" "Keyboard shortcuts (?)"
        , Attr.title "Keyboard shortcuts (?)"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "2.25rem"
        , Attr.style "height" "2.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18 "help-circle" ]


{-| The DOM id of the menu panel, for `aria-controls`.
-}
menuId : String
menuId =
    "contraptions-menu"


{-| The accomplishment banner: a polite live status line, in the green of the
LDraw palette's Green (the theme has no green token). It is one line, the height
of the top buttons, and ellipsizes on a narrow window.
-}
accomplishmentBanner : Int -> Html Msg
accomplishmentBanner gearCount =
    let
        green =
            Colors.resolveColor 2 16

        channel value =
            String.fromInt (round (value * 255))
    in
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.style "height" "2.25rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "min-width" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "padding" "0 1.25rem"
        , Attr.style "background" ("rgba(" ++ channel green.r ++ ", " ++ channel green.g ++ ", " ++ channel green.b ++ ", 0.95)")
        , Attr.style "border" "2px solid var(--color-brand-yellow)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "box-shadow" "0 10px 25px rgba(0, 0, 0, 0.6)"
        ]
        [ -- Keyed: a text change replaces the element, so a browser that has
          -- translated the old text cannot leave stale copies behind.
          let
            label =
                "All " ++ String.fromInt gearCount ++ " gears are rotating together!"
          in
          Keyed.node "span" [] [ ( label, span [] [ text label ] ) ]
        ]


{-| The studio canvas. While the machine is animated the turning gears are
drawn by the simulation (with the axles and parts riding on them) and their
static placements are hidden.
-}
viewCanvas : Model -> Html Core.Msg
viewCanvas model =
    let
        animated =
            if Animation.isAnimating { playing = model.playing, currentTime = model.currentTime } then
                model.animated

            else
                Animation.none

        studio =
            model.studio

        shown =
            if Set.isEmpty animated.sources then
                studio

            else
                { studio | viewport = Viewport.setHiddenSources animated.sources studio.viewport }
    in
    Core.viewWithExtra
        { width = model.windowWidth
        , height = model.windowHeight
        , extraEntities =
            \camera aspect ->
                if Set.isEmpty animated.gears then
                    []

                else
                    Simulation.gearEntitiesFor animated.gears camera studio.viewport.renderStyle aspect model.simulation
                        ++ Simulation.drivenEntitiesFor animated.gears camera studio.viewport.renderStyle aspect model.simulation
        }
        shown


{-| Buttons for the selection, the touch- and mouse-friendly stand-in for an
axis gizmo. Each does what its keyboard shortcut does. The Fine toggle is always
last, so it can be set before anything is selected; without a selection the bar
shows only that toggle.
-}
selectionBar : Model -> Html Msg
selectionBar model =
    let
        selected =
            Core.hasSelection model.studio

        label =
            selectionName model

        actions =
            if selected then
                [ span
                    [ Attr.class "touch-only"
                    , Attr.style "max-width" "9rem"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "text-overflow" "ellipsis"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "padding" "0 0.5rem"
                    , Attr.style "color" "var(--color-brand-yellow)"
                    , Attr.style "font-size" "0.8125rem"
                    , Attr.style "font-weight" "700"
                    ]
                    [ Keyed.node "span" [] [ ( label, span [] [ text label ] ) ] ]
                , actionButton (TurnSelection TurnLeft) "rotate-ccw" "Turn left" "←"
                , actionButton (TurnSelection TurnRight) "rotate-cw" "Turn right" "→"
                , actionButton (TurnSelection TipForward) "corner-right-down" "Tip" "↑"
                , actionButton RaiseSelection "arrow-up" "Raise" "E"
                , actionButton LowerSelection "arrow-down" "Lower" "Q"
                , actionButton Delete "trash-2" "Delete" "Delete"
                ]

            else
                []
    in
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.attribute "aria-label" "Selected parts"
        , Attr.style "align-self" "center"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "max-width" "100%"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "padding" "0.375rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "1.5rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        (actions ++ [ fineButton (Core.fineMove model.studio) ])


{-| The Fine toggle: 1 LDU moves for body drags and nudges, the touch stand-in
for holding Shift. `aria-pressed` shows whether it is on; it persists across
selection changes.
-}
fineButton : Bool -> Html Msg
fineButton on =
    button
        [ onClick ToggleFine
        , Attr.attribute "aria-pressed" (boolText on)
        , Attr.attribute "aria-label" "Fine moves"
        , Attr.title "Fine moves: 1 LDU steps (Shift)"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "0.125rem"
        , Attr.style "min-width" "3rem"
        , Attr.style "min-height" "2.75rem"
        , Attr.style "padding" "0.25rem 0.375rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" (fineBackground on)
        , Attr.style "color" (fineColour on)
        , Attr.style "font-size" "0.6875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18 "crosshair"
        , span [ Attr.attribute "aria-hidden" "true" ] [ text "Fine" ]
        ]


fineBackground : Bool -> String
fineBackground on =
    if on then
        "var(--color-brand-yellow)"

    else
        "rgba(255, 255, 255, 0.1)"


fineColour : Bool -> String
fineColour on =
    if on then
        "var(--color-brand)"

    else
        "var(--color-text-on-dark)"


boolText : Bool -> String
boolText on =
    if on then
        "true"

    else
        "false"


actionButton : Msg -> String -> String -> String -> Html Msg
actionButton msg icon label shortcut =
    button
        [ onClick msg
        , Attr.attribute "aria-label" label
        , Attr.title (label ++ " (" ++ shortcut ++ ")")
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "0.125rem"
        , Attr.style "min-width" "3rem"
        , Attr.style "min-height" "2.75rem"
        , Attr.style "padding" "0.25rem 0.375rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" "rgba(255, 255, 255, 0.1)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-size" "0.6875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18 icon
        , span [ Attr.attribute "aria-hidden" "true" ] [ text label ]
        ]


viewDock : Model -> Html Msg
viewDock model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "stretch"
        , Attr.style "gap" "0.5rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "height" PartCard.compactPanelHeight
        , Attr.style "min-width" "0"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.92)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        [ categoryTabs model
        , div
            [ Attr.attribute "role" "group"
            , Attr.attribute "aria-label" "Parts"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "flex" "1 1 auto"
            , Attr.style "min-width" "0"
            , Attr.style "overflow-x" "auto"
            , Attr.style "padding" "0.125rem"
            ]
            (List.map (partCard model.thumbnails) (dockParts model))
        ]


{-| The category tabs: a narrow column on the left of the dock.
-}
categoryTabs : Model -> Html Msg
categoryTabs model =
    let
        categories =
            if List.isEmpty model.projectParts then
                [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]

            else
                [ ProjectCategory, GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
    in
    div
        [ Attr.attribute "role" "tablist"
        , Attr.attribute "aria-orientation" "vertical"
        , Attr.attribute "aria-label" "Part categories"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "space-between"
        , Attr.style "flex-shrink" "0"
        , Attr.style "width" "4.5rem"
        , Attr.style "min-height" "0"
        ]
        (List.map (categoryTab model.activeCategory) categories)


categoryTab : Category -> Category -> Html Msg
categoryTab active cat =
    let
        isSelected =
            active == cat

        label =
            case cat of
                ProjectCategory ->
                    "Project"

                PinsCategory ->
                    "Pins"

                _ ->
                    Catalog.categoryLabel cat
    in
    button
        [ onClick (SetCategory cat)
        , Attr.attribute "role" "tab"
        , Attr.attribute "aria-selected"
            (if isSelected then
                "true"

             else
                "false"
            )
        , Attr.title (Catalog.categoryLabel cat)
        , Attr.style "flex-shrink" "0"
        , Attr.style "width" "100%"
        , Attr.style "padding" "0.1875rem 0.25rem"
        , Attr.style "font-size" "0.6875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "line-height" "1.2"
        , Attr.style "white-space" "nowrap"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if isSelected then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.08)"
            )
        , Attr.style "color"
            (if isSelected then
                "var(--color-brand)"

             else
                "var(--color-text-subtle)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ text label ]


{-| One dock part as a compact picture card. Its name, part number and
description are in the tooltip and the accessible name.
-}
partCard : Dict String Thumbnail -> PartItem -> Html Msg
partCard thumbnails item =
    let
        thumb =
            Dict.get (PartCard.thumbnailKey item.file item.color) thumbnails

        partNumber =
            String.replace ".dat" "" item.file

        name =
            dockName thumbnails item
    in
    PartCard.compactView
        { imageUrl = Maybe.map .url thumb
        , tooltip = name ++ " (" ++ item.file ++ ")" ++ descriptionSuffix item.description
        , ariaLabel = "Add " ++ name ++ " (" ++ partNumber ++ ")"
        , badge = Nothing
        , onClick = Just (AddPart item)
        , attributes = [ keepFocusOnCanvas ]
        }


{-| The readable name of a dock part: its catalogue name, or for a project part
the title from the thumbnail pipeline when it has one.
-}
dockName : Dict String Thumbnail -> PartItem -> String
dockName thumbnails item =
    case ( item.category, Dict.get (PartCard.thumbnailKey item.file item.color) thumbnails ) of
        ( ProjectCategory, Just t ) ->
            if String.isEmpty (String.trim t.title) then
                item.name

            else
                String.trim t.title

        _ ->
            item.name


descriptionSuffix : String -> String
descriptionSuffix description =
    if String.isEmpty description then
        ""

    else
        " — " ++ description


{-| Where the bottom area sits. Centred when there is room, leaving the camera
globe its corner on the right; on a narrow window it spans the width.
-}
dockPlacement : Int -> List (Html.Attribute Msg)
dockPlacement windowWidth =
    if windowWidth < narrowWidth then
        [ Attr.style "left" "0.75rem"
        , Attr.style "right" "0.75rem"
        ]

    else
        [ Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "max-width" "calc(100vw - 24rem)"
        ]


{-| Clicking a toolbar, dock or pad button must not leave it focused: a focused
button takes Space for itself, so Space would undo or add a part instead of
playing and pausing.
-}
keepFocusOnCanvas : Html.Attribute Msg
keepFocusOnCanvas =
    Focus.keepFocusOnCanvas NoOp


{-| The shortcut list for this app. A left drag (or one finger) on empty canvas
pans, a right drag orbits, and the globe orbits by touch, so the whole camera is
reachable with a plain mouse or a single finger.
-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Build"
      , [ ( "Menu, top right", "Undo, redo, delete, clear, and this list" )
        , ( "Add from the dock", "Gears, axles, beams and pins go on one axle line, next to the selection" )
        , ( "Drag a part", "Slide it at its height, as seen from the camera; it snaps to studs and axles. Grab a gear by its axle hole too" )
        , ( "Fine (action bar) or Shift", "1 LDU moves for drags and W A S D nudges" )
        , ( "Gears turn to mesh automatically", "A gear that meets another turns its axle line so the teeth interlock" )
        , ( "Action bar (above the dock)", "Turn left / right, tip, raise, lower or remove the selection; Fine toggles 1 LDU moves" )
        , ( "Wheel while dragging", "Raise / lower (Shift: coarser, Alt: finer)" )
        , ( "W A S D", "Move the selected part one grid step (Shift: 1 LDU)" )
        , ( "Q / PgDn  ·  E / PgUp", "Lower / raise the selected part" )
        , ( "← →", "Turn about the vertical axis (Shift: 45°)" )
        , ( "↑ ↓", "Tip forward / back (Shift: 45°)" )
        , ( "Delete", "Remove the selection" )
        , ( "Ctrl Z", "Undo" )
        , ( "Ctrl Shift Z / Ctrl Y", "Redo" )
        , ( "Ctrl A", "Select all" )
        , ( "Ctrl / Shift + click", "Add to or remove from the selection" )
        , ( "Esc", "Cancel a drag, or clear the selection" )
        , ( "Right click mid-drag", "Cancel the drag" )
        ]
      )
    , ( "Camera"
      , [ ( "Drag the globe", "Orbit the camera (also by touch)" )
        , ( "Tap the globe", "Fly to that side of the model" )
        , ( "Zoom slider", "Zoom in and out" )
        , ( "Reset view, or F", "Fit the whole model in view" )
        , ( "Drag empty space", "Pan" )
        , ( "Right-drag", "Orbit" )
        , ( "Middle drag", "Pan" )
        , ( "One finger on empty space", "Pan (two fingers: pan and pinch-zoom; the globe orbits)" )
        , ( "Wheel", "Zoom" )
        , ( "W A S D", "Move the camera, when nothing is selected" )
        , ( "Arrows", "Turn the camera, when nothing is selected" )
        , ( "1 2 3 4 5 6", "Front, back, left, right, top, bottom" )
        , ( "`", "Isometric view" )
        ]
      )
    , ( "Machine"
      , [ ( "Space", "Play / pause the machine (an edit stops it and rewinds)" )
        , ( "?  /", "Show or hide this list" )
        ]
      )
    ]


{-| Play or pause the machine. Highlighted while it runs, like the old
simulate toggle, so the state is visible at a glance.
-}
playButton : Bool -> Html Msg
playButton playing =
    button
        [ onClick TogglePlay
        , Attr.title "Play / pause (Space)"
        , Attr.attribute "aria-pressed"
            (if playing then
                "true"

             else
                "false"
            )
        , Attr.style "padding" "0.375rem 0.875rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if playing then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if playing then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.375rem"
        , Attr.style "white-space" "nowrap"
        , Attr.class "touch-target"
        ]
        (if playing then
            [ Icon.feather "pause", text "Pause" ]

         else
            [ Icon.feather "play", text "Play" ]
        )


{-| Main application program.
-}
main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
