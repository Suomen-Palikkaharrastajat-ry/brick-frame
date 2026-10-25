module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , main
    , subscriptions
    , thumbnailRequests
    , update
    , view
    )

{-| Main application for Brick Contraptions.

Provides a Technic building playground where gears, beams, axles, and pins
can be assembled with stud/axle snapping and simulated with real-time gear physics.
Building and simulating share one view: Space plays and pauses the machine while
editing stays available. A drag on the canvas picks up and moves parts relative
to the view; there is no axis gizmo — while something is selected, a floating
action bar above the dock turns, tips, raises, lowers or deletes it (the same as
the keyboard). The camera has its own control in the bottom-right corner, a globe to fly it around the model and a zoom slider.

While the machine plays (and while it is paused part-way) every gear in the
motor's gear train is drawn turning by the shared `Simulation` component, with
the axles and parts threaded on those gears; their static copies are hidden from
the studio's scene (see `Contraption.Animation`).

The dock shows each part as a visual card (`UI.PartCard`) with a rendered image
from the shared thumbnail pipeline (through `Ports`); the images of the visible
category are asked for on start and whenever the category changes.
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
import Contraption.CameraGlobe as CameraGlobe
import Contraption.Catalog as Catalog exposing (Category(..), PartItem)
import Contraption.Machine as Machine exposing (MachineState)
import Contraption.Placement as Placement
import Data
import Dict exposing (Dict)
import EmbeddedParts
import Gear.Placements as Placements
import Html exposing (Html, button, div, header, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as Decode
import LDraw.Colors as Colors
import LDraw.Document as Document
import Ports
import Render.Camera as Camera
import Set
import Simulation
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload
import Task
import UI.Icon as Icon
import UI.PartCard as PartCard
import UI.ShortcutHelp as ShortcutHelp
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
    , globeDrag : Maybe GlobeDrag
    , helpOpen : Bool
    , activeCategory : Category
    , projectParts : List PartItem
    , currentTime : Float
    , motorRpm : Float
    , simulation : Simulation.Model
    , animated : Animated
    , windowWidth : Int
    , windowHeight : Int
    , thumbnails : Dict String Thumbnail
    }


{-| A rendered dock image (a data URL) and the part's .dat title (`""` when
unknown), kept under `PartCard.thumbnailKey file color`.
-}
type alias Thumbnail =
    { url : String
    , title : String
    }


{-| A press on the camera globe: where it started and last was (relative to the
globe's centre), how far it has travelled, and which way a horizontal drag turns
the camera (fixed for the whole drag, so crossing the globe's edge does not
reverse it).
-}
type alias GlobeDrag =
    { start : ( Float, Float )
    , last : ( Float, Float )
    , travel : Float
    , azimuthSign : Float
    }


{-| Messages handled by the Contraptions application.
-}
type Msg
    = StudioMsg Core.Msg
    | TogglePlay
    | GlobePressed ( Float, Float )
    | GlobeMoved ( Float, Float )
    | GlobeReleased
    | ZoomTo Float
    | ZoomDone
    | ResetView
    | ToggleHelp
    | NoOp
    | SetCategory Category
    | AddPart PartItem
    | Undo
    | Redo
    | Delete
    | TurnSelection TurnDirection
    | RaiseSelection
    | LowerSelection
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
            , helpOpen = False
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
            in
            List.foldl applyShortcut
                ( { model | studio = nextStudio, machine = newMachine }
                , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
                )
                (List.filterMap extractKey events)

        TogglePlay ->
            if model.playing then
                ( { model | playing = False }, Cmd.none )

            else
                ( startPlaying model, Cmd.none )

        GlobePressed point ->
            ( { model
                | globeDrag =
                    Just
                        { start = point
                        , last = point
                        , travel = 0
                        , azimuthSign = CameraGlobe.facingSign model.studio.viewport.camera
                        }
              }
            , Cmd.none
            )

        GlobeMoved ( x, y ) ->
            case model.globeDrag of
                Just drag ->
                    let
                        ( lx, ly ) =
                            drag.last

                        ( azimuthDelta, elevationDelta ) =
                            CameraGlobe.dragOrbit drag.azimuthSign ( x - lx, y - ly )
                    in
                    update (viewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta))
                        { model
                            | globeDrag =
                                Just
                                    { drag
                                        | last = ( x, y )
                                        , travel = drag.travel + sqrt ((x - lx) ^ 2 + (y - ly) ^ 2)
                                    }
                        }

                Nothing ->
                    ( model, Cmd.none )

        GlobeReleased ->
            let
                released =
                    { model | globeDrag = Nothing }
            in
            case model.globeDrag |> Maybe.andThen (tapTarget model) of
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

        ToggleHelp ->
            -- The studio pauses its editing keys while the dialog is open, so
            -- Esc and ? reach this app to close it.
            let
                open =
                    not model.helpOpen
            in
            ( { model
                | helpOpen = open
                , studio = Core.setHelpVisible open model.studio
              }
            , Cmd.none
            )

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
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Undo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.undo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Redo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.redo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Delete ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.deleteSelection model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        TurnSelection direction ->
            runCore (Core.turnSelected direction) model

        RaiseSelection ->
            runCore Core.raiseSelected model

        LowerSelection ->
            runCore Core.lowerSelected model

        Clear ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.resetDocument Document.empty model.studio

                newMachine =
                    Machine.analyseMachine Document.empty nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine, currentTime = 0.0 }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
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


{-| Run a studio edit, re-analyse the machine and forward the effects.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore edit model =
    let
        ( nextStudio, studioCmd, events ) =
            edit model.studio
    in
    ( { model
        | studio = nextStudio
        , machine = Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache
      }
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
    )


{-| A press that barely moved is a tap: fly the camera to that point of the globe.
-}
tapTarget : Model -> GlobeDrag -> Maybe ( Float, Float )
tapTarget model drag =
    if drag.travel < 3 then
        CameraGlobe.flyToDelta model.studio.viewport.camera drag.start

    else
        Nothing


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
    case ViewportShortcuts.actionFor (shortcutContext model) keyEvent of
        Just action ->
            let
                ( next, actionCmd ) =
                    update (msgForAction action) model
            in
            ( next, Cmd.batch [ cmd, actionCmd ] )

        Nothing ->
            ( model, cmd )


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

        placedCount =
            List.length (Core.getPlacements model.studio)

        hasUndo =
            Core.canUndo model.studio

        hasRedo =
            Core.canRedo model.studio

        hasSel =
            Core.hasSelection model.studio
    in
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- Top toolbar & header
          header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "1rem"
            , Attr.style "left" "1rem"
            , Attr.style "right" "1rem"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "z-index" "10"
            , Attr.style "pointer-events" "none"
            , keepFocusOnCanvas
            ]
            [ -- App Title badge
              div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.75rem"
                , Attr.style "padding" "0.5rem 1rem"
                , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "border-radius" "9999px"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "color" "var(--color-text-on-dark)"
                , Attr.style "pointer-events" "auto"
                ]
                [ span
                    [ Attr.style "font-weight" "700"
                    , Attr.style "letter-spacing" "-0.01em"
                    ]
                    [ text "Palikkakehys Contraptions" ]
                , span
                    [ Attr.style "font-size" "0.875rem"
                    , Attr.style "color" "var(--color-brand-yellow)"
                    ]
                    [ text (String.fromInt gearCount ++ " gears") ]
                ]

            -- Action Controls Toolbar
            , div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "padding" "0.375rem"
                , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "border-radius" "0.75rem"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "pointer-events" "auto"
                ]
                [ playButton model.playing
                , toolbarButton Undo hasUndo "Undo"
                , toolbarButton Redo hasRedo "Redo"
                , toolbarButton Delete hasSel "Delete"
                , toolbarButtonWith [ Attr.title "Clear — remove all parts", Attr.attribute "aria-label" "Clear — remove all parts" ] Clear (placedCount > 0) "Clear"
                , toolbarButton ToggleHelp True "?"
                ]
            ]

        -- Celebration Accomplishment Banner
        , if accomplished then
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "4.5rem"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "1rem"
                , Attr.style "padding" "0.75rem 1.5rem"
                , Attr.style "background" "rgba(201, 26, 9, 0.95)"
                , Attr.style "backdrop-filter" "blur(12px)"
                , Attr.style "border-radius" "9999px"
                , Attr.style "border" "2px solid var(--color-brand-yellow)"
                , Attr.style "color" "var(--color-text-on-dark)"
                , Attr.style "box-shadow" "0 10px 25px rgba(0, 0, 0, 0.6)"
                , Attr.style "z-index" "20"
                , Attr.style "animation" "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite"
                ]
                [ span
                    [ Attr.style "font-size" "1.25rem" ]
                    [ text "⚙️" ]
                , div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    ]
                    [ span
                        [ Attr.style "font-weight" "800"
                        , Attr.style "font-size" "1.125rem"
                        , Attr.style "letter-spacing" "-0.01em"
                        ]
                        [ text "Contraption Accomplished!" ]
                    , span
                        [ Attr.style "font-size" "0.875rem"
                        , Attr.style "color" "var(--color-brand-yellow)"
                        ]
                        [ text ("All " ++ String.fromInt gearCount ++ " gears are rotating together!") ]
                    ]
                ]

          else
            text ""

        -- 3D WebGL Canvas
        , Html.map StudioMsg (viewCanvas model)

        -- Camera control: the canvas drag edits, so the camera lives here
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "right" "0.75rem"
            , Attr.style "bottom" "0.75rem"
            , Attr.style "z-index" "10"
            ]
            [ CameraGlobe.view
                { camera = model.studio.viewport.camera
                , onPress = GlobePressed
                , onMove = GlobeMoved
                , onRelease = GlobeReleased
                , onZoom = ZoomTo
                , onZoomDone = ZoomDone
                , onReset = ResetView
                }
            ]
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

        -- Bottom: the selection action bar floats above the Technic dock
        , div
            ([ Attr.style "position" "absolute"
             , Attr.style "bottom" "1.5rem"
             , Attr.style "display" "flex"
             , Attr.style "flex-direction" "column"
             , Attr.style "gap" "0.5rem"
             , Attr.style "z-index" "10"
             , Attr.style "pointer-events" "none"
             ]
                ++ dockPlacement model.windowWidth
            )
            [ Machine.gearFeedback (Set.toList model.studio.selection) model.studio.document
                |> Maybe.map gearFeedbackPill
                |> Maybe.withDefault (text "")
            , if hasSel then
                selectionBar

              else
                text ""
            , viewDock model
            ]
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


{-| Status pill telling how the one selected gear sits against its nearest
other gear (see `Machine.gearFeedback`). Yellow border once it meshes.
-}
gearFeedbackPill : Machine.GearFeedback -> Html Msg
gearFeedbackPill feedback =
    let
        meshes =
            feedback.relation == Machine.Meshes
    in
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.attribute "data-gear-feedback" ""
        , Attr.style "align-self" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.3125rem 0.875rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border"
            (if meshes then
                "1px solid var(--color-brand-yellow)"

             else
                "1px solid rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if meshes then
                "var(--color-brand-yellow)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "600"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "font-variant-numeric" "tabular-nums"
        ]
        [ text (Machine.feedbackText feedback) ]


{-| Buttons for the selection, the touch- and mouse-friendly stand-in for an
axis gizmo. Each does what its keyboard shortcut does.
-}
selectionBar : Html Msg
selectionBar =
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.attribute "aria-label" "Selected parts"
        , Attr.style "align-self" "center"
        , Attr.style "display" "flex"
        , Attr.style "gap" "0.25rem"
        , Attr.style "padding" "0.375rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        [ actionButton (TurnSelection TurnLeft) "rotate-ccw" "Turn left" "←"
        , actionButton (TurnSelection TurnRight) "rotate-cw" "Turn right" "→"
        , actionButton (TurnSelection TipForward) "corner-right-down" "Tip" "↑"
        , actionButton RaiseSelection "arrow-up" "Raise" "E"
        , actionButton LowerSelection "arrow-down" "Lower" "Q"
        , actionButton Delete "trash-2" "Delete" "Delete"
        ]


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
        ]
        [ Icon.featherSized 18 icon
        , span [ Attr.attribute "aria-hidden" "true" ] [ text label ]
        ]


viewDock : Model -> Html Msg
viewDock model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "padding" "0.75rem 1rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.92)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border-radius" "1.25rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , Attr.style "min-width" "0"
        , keepFocusOnCanvas
        ]
        [ -- Category Tabs
          div
            [ Attr.style "display" "flex"
            , Attr.style "gap" "0.375rem"
            , Attr.style "margin-bottom" "0.25rem"
            ]
            (let
                categories =
                    if List.isEmpty model.projectParts then
                        [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]

                    else
                        [ ProjectCategory, GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
             in
             List.map (categoryTab model.activeCategory) categories
            )

        -- Parts Row
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "overflow-x" "auto"
            , Attr.style "max-width" "100%"
            , Attr.style "padding" "0.25rem"
            ]
            (List.map (partCard model.thumbnails) (dockParts model))
        ]


categoryTab : Category -> Category -> Html Msg
categoryTab active cat =
    let
        isSelected =
            active == cat
    in
    button
        [ onClick (SetCategory cat)
        , Attr.style "padding" "0.375rem 0.75rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
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
        ]
        [ text (Catalog.categoryLabel cat) ]


{-| One dock part as a visual card: its rendered image, its name and its part
number. Project kit parts are named by their .dat title once it is known.
-}
partCard : Dict String Thumbnail -> PartItem -> Html Msg
partCard thumbnails item =
    let
        thumb =
            Dict.get (PartCard.thumbnailKey item.file item.color) thumbnails

        partNumber =
            String.replace ".dat" "" item.file

        name =
            case ( item.category, thumb ) of
                ( ProjectCategory, Just t ) ->
                    if String.isEmpty (String.trim t.title) then
                        item.name

                    else
                        String.trim t.title

                _ ->
                    item.name

        secondary =
            if name == partNumber then
                Catalog.categoryLabel item.category

            else
                partNumber
    in
    PartCard.view
        { imageUrl = Maybe.map .url thumb
        , name = name
        , secondary = secondary
        , tooltip = name ++ " (" ++ item.file ++ ")" ++ descriptionSuffix item.description
        , ariaLabel = "Add " ++ name
        , onClick = Just (AddPart item)
        , attributes = [ keepFocusOnCanvas ]
        }


descriptionSuffix : String -> String
descriptionSuffix description =
    if String.isEmpty description then
        ""

    else
        " — " ++ description


{-| Where the parts dock sits. Centred when there is room; on a narrow window it
hugs the left edge so it never slides under the camera pad on the right.
-}
dockPlacement : Int -> List (Html.Attribute Msg)
dockPlacement windowWidth =
    if windowWidth < 900 then
        [ Attr.style "left" "0.75rem"
        , Attr.style "right" "11.5rem"
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
    Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))


{-| The shortcut list for this app. A left drag (or one finger) on empty canvas
pans, a right drag orbits, and the globe orbits by touch, so the whole camera is
reachable with a plain mouse or a single finger.
-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Build"
      , [ ( "Add from the dock", "Gears, axles, beams and pins go on one axle line, next to the selection" )
        , ( "Drag a part", "Slide it at its height, as seen from the camera; it snaps to studs and axles. Grab a gear by its axle hole too" )
        , ( "Shift + drag a part", "Fine move: 1 LDU steps (Shift held during a drag works too)" )
        , ( "Gears turn to mesh automatically", "A gear that meets another turns its axle line so the teeth interlock" )
        , ( "Action bar (above the dock)", "Turn left / right, tip, raise, lower or remove the selection" )
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
        ]
        (if playing then
            [ Icon.feather "pause", text "Pause" ]

         else
            [ Icon.feather "play", text "Play" ]
        )


toolbarButton : Msg -> Bool -> String -> Html Msg
toolbarButton =
    toolbarButtonWith []


{-| A toolbar button with extra attributes, such as a longer `title` and
`aria-label` than its visible label.
-}
toolbarButtonWith : List (Html.Attribute Msg) -> Msg -> Bool -> String -> Html Msg
toolbarButtonWith extra msg enabled label =
    button
        (extra
            ++ [ onClick msg
               , Attr.disabled (not enabled)
               , Attr.style "padding" "0.375rem 0.75rem"
               , Attr.style "font-size" "0.875rem"
               , Attr.style "font-weight" "500"
               , Attr.style "border" "none"
               , Attr.style "border-radius" "0.375rem"
               , Attr.style "background"
                    (if enabled then
                        "rgba(255, 255, 255, 0.1)"

                     else
                        "transparent"
                    )
               , Attr.style "color"
                    (if enabled then
                        "var(--color-text-on-dark)"

                     else
                        "rgba(255, 255, 255, 0.3)"
                    )
               , Attr.style "cursor"
                    (if enabled then
                        "pointer"

                     else
                        "not-allowed"
                    )
               , Attr.style "white-space" "nowrap"
               , Attr.style "transition" "all 150ms ease"
               ]
        )
        [ text label ]


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
