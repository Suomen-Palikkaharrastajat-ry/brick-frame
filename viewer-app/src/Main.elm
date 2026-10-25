module Main exposing (Flags, Model, Msg, init, main, needsAnimationFrame)

{-| Brick Frame (Palikkakehys) viewer and simulator: the thin shell.

The 3D canvas is `Viewport` (camera, input, model loading) and the gear train is
`Simulation` (detection, playback, motor); both are shared packages. This module
owns what only this application has:

  - the Elm ports and the runtime events that wrappers listen to,
  - mirroring the camera to the URL hash,
  - the controls panel, shortcut help and loading/error overlays,
  - the seam between the two components (what a click on a gear does, which
    lines the viewport bakes, when the simulation resets).

Both components return events instead of performing effects; `handleViewportEvent`
is where those become commands.

-}

import Array
import Browser
import Browser.Events
import Data
import Dict exposing (Dict)
import EmbeddedParts
import FeatherIcons
import Gear.Components as Components
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId, GearInstance)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..))
import Ports
import Render.Camera as Camera
import Render.Style as Style
import Simulation
import UI.Button as Button
import UI.FileUpload as FileUpload
import UI.ShortcutHelp as ShortcutHelp
import UI.Status as Status
import UI.Theme as Theme
import UI.Touch as Touch
import UI.ViewPad as ViewPad
import Url
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Hash as Hash
import Viewport.Shortcuts as Shortcuts


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
    }



-- ── Model ─────────────────────────────────────────────────────────────────────


type UiMode
    = ViewerMode
    | SimulatorMode


{-| The two components plus this application's own UI state.
-}
type alias Model =
    { viewport : Viewport.Model
    , simulation : Simulation.Model
    , controlsCollapsed : Bool
    , uiMode : UiMode
    , viewerControlsEnabled : Bool
    , showShortcutHelp : Bool
    , studioUrl : String
    , currentModelUrl : Maybe String
    }



-- ── Init ──────────────────────────────────────────────────────────────────────


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        trimmedModelUrl =
            String.trim flags.defaultModelUrl

        currentModelUrl =
            if String.isEmpty trimmedModelUrl then
                Nothing

            else
                Just trimmedModelUrl

        ( viewport, viewportCmd ) =
            Viewport.init
                { ldrawBase = flags.ldrawBase
                , ldrawFallbackBase = flags.ldrawFallbackBase
                , defaultModelUrl = flags.defaultModelUrl
                , initialHash = flags.initialHash
                , supportsPointerEvents = flags.supportsPointerEvents
                , useWindowResize = flags.useWindowResize
                , renderStyle = buildRenderStyle flags.ambientStrength flags.lightStrength flags.vibrance flags.edgeWidth
                , partCache = EmbeddedParts.cache
                }
    in
    ( { viewport = viewport
      , simulation =
            Simulation.init
                { gearSpecs = Data.gearParts
                , lodCache = EmbeddedParts.lodCache
                , maxRpm = flags.maxRpm
                , initialRpm = flags.initialRpm
                , requestedMotorIndex =
                    if flags.initialMotorIndex < 0 then
                        Nothing

                    else
                        Just flags.initialMotorIndex
                }
      , controlsCollapsed = False
      , uiMode = parseUiMode flags.uiMode
      , viewerControlsEnabled = flags.controlsEnabled
      , showShortcutHelp = False
      , studioUrl = String.trim flags.studioUrl
      , currentModelUrl = currentModelUrl
      }
    , Cmd.batch
        [ Cmd.map ViewportMsg viewportCmd
        , Ports.setUrlHash (Hash.encodeCamera viewport.camera)
        ]
    )


buildRenderStyle : Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Style.Style
buildRenderStyle maybeAmbient maybeLightStrength maybeVibrance maybeEdgeWidth =
    let
        baseStyle =
            Style.defaultStyle

        ambient =
            Maybe.withDefault baseStyle.ambientStrength maybeAmbient

        lightStrength =
            Maybe.withDefault baseStyle.lightStrength maybeLightStrength

        vibrance =
            Maybe.withDefault baseStyle.vibrance maybeVibrance

        edgeWidth =
            Maybe.withDefault baseStyle.edgeWidth maybeEdgeWidth
    in
    Style.clampStyle
        { baseStyle
            | ambientStrength = ambient
            , lightStrength = lightStrength
            , vibrance = vibrance
            , edgeWidth = edgeWidth
        }



-- ── Msg ───────────────────────────────────────────────────────────────────────


{-| The viewport and simulation keep their own messages; the rest is this
application's UI.
-}
type Msg
    = ViewportMsg Viewport.Msg
    | SimulationMsg Simulation.Msg
    | RequestFileUpload
    | ToggleControlsPanel
    | ToggleShortcutHelp


runtimeEventCmd : String -> List ( String, Encode.Value ) -> Cmd Msg
runtimeEventCmd eventType fields =
    Ports.runtimeEvent <|
        Encode.encode 0 <|
            Encode.object
                (( "type", Encode.string eventType ) :: fields)


modelLoadedEventCmd : Model -> Cmd Msg
modelLoadedEventCmd model =
    runtimeEventCmd "model-loaded"
        [ ( "mode", Encode.string (uiModeToString model.uiMode) )
        , ( "simulationAvailable", Encode.bool model.simulation.simulationAvailable )
        ]


modelErrorEventCmd : String -> Cmd Msg
modelErrorEventCmd message =
    runtimeEventCmd "model-error"
        [ ( "message", Encode.string message ) ]


playStateEventCmd : Bool -> Float -> Cmd Msg
playStateEventCmd running currentTime =
    runtimeEventCmd "play-state-changed"
        [ ( "running", Encode.bool running )
        , ( "time", Encode.float currentTime )
        ]


cameraChangedEventCmd : Camera.Camera -> Cmd Msg
cameraChangedEventCmd camera =
    runtimeEventCmd "camera-changed" (Viewport.cameraChangedPayload camera)


cameraChangedIfNeededCmd : Camera.Camera -> Camera.Camera -> Cmd Msg
cameraChangedIfNeededCmd before after =
    if Viewport.cameraMoved before after then
        cameraChangedEventCmd after

    else
        Cmd.none



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            updateInner msg model

        -- One choke point for everything derived from the camera: the gears'
        -- level of detail (so any camera or mesh change is reflected before the
        -- next render, with hysteresis carried over from the previous frame) and
        -- the `camera-changed` runtime event.
        withLods =
            { newModel
                | simulation = Simulation.refreshLods newModel.viewport.camera newModel.simulation
            }
    in
    ( withLods
    , Cmd.batch
        [ cmd
        , cameraChangedIfNeededCmd model.viewport.camera newModel.viewport.camera
        ]
    )


updateInner : Msg -> Model -> ( Model, Cmd Msg )
updateInner msg model =
    case msg of
        ViewportMsg viewportMsg ->
            let
                ( viewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( next, eventCmds ) =
                    handleViewportEvents events ( { model | viewport = viewport }, [] )

                updatedModelUrl =
                    case viewportMsg of
                        Viewport.FileContentReceived _ ->
                            Nothing

                        _ ->
                            next.currentModelUrl
            in
            ( { next | currentModelUrl = updatedModelUrl }
            , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds)
            )

        SimulationMsg simulationMsg ->
            let
                ( simulation, events ) =
                    Simulation.update simulationMsg model.simulation

                stopCmd =
                    -- Stopping rewinds to the start; the hash records the view
                    -- the rewound scene is seen from.
                    if simulationMsg == Simulation.Stop then
                        [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ]

                    else
                        []
            in
            ( { model | simulation = simulation }
            , Cmd.batch (stopCmd ++ List.map simulationEventCmd events)
            )

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        ToggleControlsPanel ->
            ( { model | controlsCollapsed = not model.controlsCollapsed }, Cmd.none )

        ToggleShortcutHelp ->
            ( { model | showShortcutHelp = not model.showShortcutHelp }, Cmd.none )


{-| Turn what the viewport reported into model changes and commands, in order.
-}
handleViewportEvents : List Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleViewportEvents events start =
    List.foldl handleViewportEvent start events


handleViewportEvent : Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleViewportEvent event ( model, cmds ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ] )

        Viewport.Clicked x y _ ->
            let
                viewport =
                    model.viewport
            in
            case Simulation.pickGear viewport.camera viewport.width viewport.height x y model.simulation of
                Just gearId ->
                    -- Clicking a gear makes it the motor; that is not a camera
                    -- change, so the hash is left alone.
                    let
                        ( simulation, _ ) =
                            Simulation.update (Simulation.SetMotorGear gearId) model.simulation
                    in
                    ( { model | simulation = simulation }, cmds )

                Nothing ->
                    ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera viewport.camera) ] )

        Viewport.DoubleClicked _ _ ->
            ( model, cmds )

        Viewport.Tapped _ _ ->
            ( model, cmds )

        Viewport.PressStarted _ _ ->
            ( model, cmds )

        Viewport.DragMoved _ _ ->
            ( model, cmds )

        Viewport.DragEnded _ _ ->
            ( model, cmds )

        Viewport.DragCancelled ->
            ( model, cmds )

        Viewport.WheelScrolled _ ->
            ( model, cmds )

        Viewport.LoadStarted ->
            ( { model | simulation = Simulation.reset model.simulation }, cmds )

        Viewport.PartsReady ->
            let
                ( simulation, analysis ) =
                    Simulation.analyse
                        { lines = model.viewport.topLevelLines, cache = model.viewport.partCache }
                        model.simulation

                ( viewport, flattenEvents ) =
                    Viewport.beginFlatten
                        { lines = analysis.staticLines, fallbackLines = analysis.fallbackLines }
                        model.viewport

                simulationEvent =
                    if simulation.simulationAvailable then
                        runtimeEventCmd "simulation-ready"
                            [ ( "gearCount", Encode.int analysis.gearCount ) ]

                    else
                        runtimeEventCmd "simulation-unavailable"
                            [ ( "reason", Encode.string "No supported gear train detected" ) ]
            in
            handleViewportEvents flattenEvents
                ( { model | simulation = simulation, viewport = viewport }
                , cmds ++ [ simulationEvent ]
                )

        Viewport.LargeModel partCount ->
            ( { model | simulation = Simulation.markUnavailable model.simulation }
            , cmds
                ++ [ runtimeEventCmd "simulation-unavailable"
                        [ ( "reason", Encode.string "Model too large for gear detection" )
                        , ( "partCount", Encode.int partCount )
                        ]
                   ]
            )

        Viewport.FlattenRequested payload ->
            ( model, cmds ++ [ Ports.requestGeometryFlatten payload ] )

        Viewport.ModelReady ->
            ( model, cmds ++ [ modelLoadedEventCmd model ] )

        Viewport.LoadFailed message ->
            ( model, cmds ++ [ modelErrorEventCmd message ] )

        Viewport.KeyAccepted keyEvent ->
            case Shortcuts.actionFor (shortcutContext model) keyEvent of
                Just action ->
                    let
                        ( next, cmd ) =
                            updateInner (msgForAction action) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    ( model, cmds )


simulationEventCmd : Simulation.Event -> Cmd Msg
simulationEventCmd event =
    case event of
        Simulation.PlayStateChanged running time ->
            playStateEventCmd running time


{-| What `Escape` and `?` need to know to pick between their meanings.
-}
shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.showShortcutHelp
    , walking = model.viewport.camera.navigation == Camera.Walk
    }


{-| Map a shortcut to the message it stands for.

`Viewport.Shortcuts` names its own actions so it stays free of `Msg`; this is the
seam.

-}
msgForAction : Shortcuts.Action -> Msg
msgForAction action =
    case action of
        Shortcuts.TogglePlayback ->
            SimulationMsg Simulation.ToggleMotor

        Shortcuts.FitView ->
            ViewportMsg Viewport.FitView

        Shortcuts.ToggleWalk ->
            ViewportMsg Viewport.ToggleWalkMode

        Shortcuts.LeaveWalk ->
            ViewportMsg Viewport.ToggleWalkMode

        Shortcuts.ToggleHelp ->
            ToggleShortcutHelp

        Shortcuts.CloseHelp ->
            ToggleShortcutHelp

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

        Shortcuts.PresetBottom ->
            ViewportMsg (Viewport.SetViewPreset BottomView)

        Shortcuts.PresetIsometric ->
            ViewportMsg (Viewport.SetViewPreset IsometricView)


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0



-- ── Subscriptions ─────────────────────────────────────────────────────────────


{-| Whether anything is doing per-frame work. Exposed for tests; the
subscriptions below follow the same rule through each component.
-}
needsAnimationFrame : Model -> Bool
needsAnimationFrame model =
    Viewport.needsAnimationFrame model.viewport || Simulation.needsAnimationFrame model.simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ViewportMsg (Viewport.subscriptions model.viewport)
        , Sub.map SimulationMsg (Simulation.subscriptions model.simulation)
        , Ports.viewportResized (\size -> ViewportMsg (Viewport.WindowResize size.width size.height))
        , Ports.fileContentReceived (ViewportMsg << Viewport.FileContentReceived)
        , Ports.fileLoadError (ViewportMsg << Viewport.FileLoadError)
        , Ports.geometryFlattened (ViewportMsg << Viewport.GeometryFlattened)
        , Ports.geometryFlattenFailed (ViewportMsg << Viewport.GeometryFlattenFailed)
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
                    \camera style aspect -> Simulation.entities camera style aspect model.simulation
                , coarsenWhileMoving = not model.simulation.simulationAvailable
                }
                model.viewport
            )
        , viewOverlay model
        ]


viewShortcutHelp : Html Msg
viewShortcutHelp =
    ShortcutHelp.view { rows = Shortcuts.helpRows, onClose = ToggleShortcutHelp }


viewPad : Model -> Html Msg
viewPad model =
    ViewPad.view
        { walking = model.viewport.camera.navigation == Camera.Walk
        , helpOpen = model.showShortcutHelp
        , play =
            if model.simulation.simulationAvailable then
                Just
                    { running = model.simulation.playback.running
                    , onToggle = SimulationMsg Simulation.ToggleMotor
                    }

            else
                Nothing
        , onFit = ViewportMsg Viewport.FitView
        , onToggleWalk = ViewportMsg Viewport.ToggleWalkMode
        , onToggleHelp = ToggleShortcutHelp
        , onStartRotate = \azimuth elevation -> ViewportMsg (Viewport.StartHoldRotate azimuth elevation)
        , onEndRotate = ViewportMsg Viewport.EndHoldRotate
        }


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


viewOverlay : Model -> Html Msg
viewOverlay model =
    let
        modeOverlays =
            case model.uiMode of
                ViewerMode ->
                    if model.viewerControlsEnabled then
                        [ viewPad model ]

                    else
                        []

                SimulatorMode ->
                    [ viewDebug model
                    , viewGearPanel model
                    , viewToolbar model
                    ]
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "pointer-events" "none"
        ]
        ([ viewStatus model ]
            ++ modeOverlays
            -- Shortcuts work in both UI modes, so the reference does too.
            ++ (if model.showShortcutHelp then
                    [ viewShortcutHelp ]

                else
                    []
               )
        )


viewDebug : Model -> Html Msg
viewDebug model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "color" Theme.textMuted
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "line-height" "1.7"
        ]
        [ div [] [ text "Palikkakehys" ]
        , div []
            [ text
                ("az "
                    ++ String.fromInt (round (model.viewport.camera.azimuth * 180 / pi))
                    ++ "°  el "
                    ++ String.fromInt (round (model.viewport.camera.elevation * 180 / pi))
                    ++ "°  dist "
                    ++ String.fromInt (round model.viewport.camera.distance)
                )
            ]
        , div [] [ text "Drag or right-drag to orbit · Middle-drag or Shift+drag to pan · Scroll to zoom" ]
        , div [] [ text "Touch: 1-finger orbit · 2-finger pan/pinch zoom" ]
        ]


{-| Gear info panel — top-right corner. Shows detected gears, connections,
play/pause control, and per-gear "Set as Motor" buttons.
-}
viewGearPanel : Model -> Html Msg
viewGearPanel model =
    case model.uiMode of
        ViewerMode ->
            text ""

        SimulatorMode ->
            if model.simulation.simulationChecked && not model.simulation.simulationAvailable then
                viewSimulationUnavailablePanel

            else
                case model.simulation.gearGraph of
                    Nothing ->
                        text ""

                    Just graph ->
                        let
                            instances =
                                Array.toList graph.instances

                            axleCount =
                                model.simulation.components
                                    |> List.filter (\c -> c.kind == Components.AxleLike)
                                    |> List.length

                            beamCount =
                                model.simulation.components
                                    |> List.filter (\c -> c.kind == Components.Beam)
                                    |> List.length

                            ratios =
                                case model.simulation.motor.drivingGearId of
                                    Just motorId ->
                                        Physics.propagate graph motorId 1.0

                                    Nothing ->
                                        Dict.empty
                        in
                        if List.isEmpty instances then
                            text ""

                        else
                            div
                                [ Attr.style "position" "absolute"
                                , Attr.style "top" "12px"
                                , Attr.style "right" "12px"
                                , Attr.style "background" Theme.panelBackground
                                , Attr.style "color" Theme.textPrimary
                                , Attr.style "font-family" "monospace"
                                , Attr.style "font-size" "12px"
                                , Attr.style "border-radius" "8px"
                                , Attr.style "padding" "12px 16px"
                                , Attr.style "width" "280px"
                                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                                , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 8%, transparent)"
                                , Attr.style "box-sizing" "border-box"
                                , Attr.style "overflow-x" "hidden"
                                , Attr.style "pointer-events" "auto"
                                , Attr.style "touch-action" "none"
                                ]
                                ([ div
                                    [ Attr.style "display" "flex"
                                    , Attr.style "align-items" "center"
                                    , Attr.style "justify-content" "space-between"
                                    ]
                                    [ div
                                        [ Attr.style "color" Theme.brandYellow ]
                                        [ text
                                            (String.fromInt (List.length instances)
                                                ++ " gear"
                                                ++ (if List.length instances == 1 then
                                                        ""

                                                    else
                                                        "s"
                                                   )
                                                ++ " detected"
                                            )
                                        ]
                                    , button
                                        [ Html.Events.onClick ToggleControlsPanel
                                        , Touch.onTap ToggleControlsPanel
                                        , Attr.style "padding" "2px 8px"
                                        , Attr.style "background" Theme.panelSubtleBackground
                                        , Attr.style "color" Theme.textPrimary
                                        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                                        , Attr.style "border-radius" "3px"
                                        , Attr.style "cursor" "pointer"
                                        , Attr.style "font-family" "monospace"
                                        , Attr.style "font-size" "11px"
                                        ]
                                        [ text
                                            (if model.controlsCollapsed then
                                                "Maximize"

                                             else
                                                "Minimize"
                                            )
                                        ]
                                    ]
                                 ]
                                    ++ (if model.controlsCollapsed then
                                            []

                                        else
                                            [ viewComponentSummary axleCount beamCount ]
                                                ++ List.map (viewGearRow model graph ratios) instances
                                                ++ [ viewMotorControls model ]
                                       )
                                )


viewSimulationUnavailablePanel : Html Msg
viewSimulationUnavailablePanel =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "right" "12px"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "border-radius" "8px"
        , Attr.style "padding" "12px 16px"
        , Attr.style "width" "280px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 8%, transparent)"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "pointer-events" "auto"
        ]
        [ div [ Attr.style "color" Theme.brandYellow ] [ text "Simulation unavailable" ]
        , div
            [ Attr.style "margin-top" "6px"
            , Attr.style "font-size" "11px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text "No supported gear train detected in this model." ]
        ]


viewGearRow : Model -> GearGraph -> Dict GearId Float -> GearInstance -> Html Msg
viewGearRow model graph ratios inst =
    let
        isMotor =
            model.simulation.motor.drivingGearId == Just inst.id

        angle =
            Dict.get inst.id model.simulation.gearAngles |> Maybe.withDefault 0.0

        angleDeg =
            angle * 180 / pi |> truncateAngle

        ratio =
            Dict.get inst.id ratios |> Maybe.withDefault 0.0

        neighbours =
            Dict.get inst.id graph.connections |> Maybe.withDefault []

        connectionStr =
            if List.isEmpty neighbours then
                "isolated"

            else
                "→ "
                    ++ String.join ", "
                        (List.map
                            (\nid ->
                                case Array.get nid graph.instances of
                                    Just n ->
                                        String.fromInt n.spec.teeth ++ "T"

                                    Nothing ->
                                        "?"
                            )
                            neighbours
                        )
    in
    div
        [ Attr.style "margin-bottom" "6px"
        , Attr.style "padding" "4px 6px"
        , Attr.style "border-radius" "4px"
        , Attr.style "background"
            (if isMotor then
                "color-mix(in srgb, var(--color-brand-yellow) 20%, transparent)"

             else
                "transparent"
            )
        ]
        [ div []
            [ text
                ((if isMotor then
                    "⚙ "

                  else
                    "  "
                 )
                    ++ String.fromInt inst.spec.teeth
                    ++ "T"
                )
            ]
        , div
            [ Attr.style "color" Theme.textMuted
            , Attr.style "font-size" "11px"
            ]
            [ text
                ("ratio "
                    ++ format1dp ratio
                    ++ "x, angle "
                    ++ format1dp angleDeg
                    ++ "°"
                )
            ]
        , div
            [ Attr.style "color" Theme.textMuted
            , Attr.style "font-size" "11px"
            ]
            [ text connectionStr ]
        , if not isMotor then
            button
                [ Html.Events.onClick (SimulationMsg (Simulation.SetMotorGear inst.id))
                , Touch.onTap (SimulationMsg (Simulation.SetMotorGear inst.id))
                , Attr.style "margin-top" "3px"
                , Attr.style "padding" "2px 8px"
                , Attr.style "background" Theme.panelSubtleBackground
                , Attr.style "color" Theme.textPrimary
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "3px"
                , Attr.style "cursor" "pointer"
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "11px"
                ]
                [ text "Set as motor" ]

          else
            text ""
        ]


viewComponentSummary : Int -> Int -> Html Msg
viewComponentSummary axleCount beamCount =
    div
        [ Attr.style "margin-bottom" "8px"
        , Attr.style "padding" "6px"
        , Attr.style "border-radius" "4px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        ]
        [ div [] [ text ("axles/pins " ++ String.fromInt axleCount) ]
        , div [ Attr.style "font-size" "11px", Attr.style "color" Theme.textMuted ] [ text ("beams " ++ String.fromInt beamCount) ]
        ]


viewMotorControls : Model -> Html Msg
viewMotorControls model =
    div
        [ Attr.style "margin-top" "10px"
        , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "padding-top" "8px"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "6px"
            ]
            [ button
                [ Html.Events.onClick (SimulationMsg Simulation.ToggleMotor)
                , Touch.onTap (SimulationMsg Simulation.ToggleMotor)
                , Attr.style "flex" "1"
                , Attr.style "padding" "5px 0"
                , Attr.style "background"
                    (if model.simulation.playback.running then
                        Theme.brandYellow

                     else
                        Theme.panelSubtleBackground
                    )
                , Attr.style "color"
                    (if model.simulation.playback.running then
                        Theme.brand

                     else
                        Theme.textPrimary
                    )
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "4px"
                , Attr.style "cursor" "pointer"
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "12px"
                ]
                [ text
                    (if model.simulation.playback.running then
                        "⏸ Pause"

                     else
                        "▶ Play"
                    )
                ]
            , button
                [ Html.Events.onClick (SimulationMsg Simulation.Stop)
                , Touch.onTap (SimulationMsg Simulation.Stop)
                , Attr.style "padding" "5px 8px"
                , Attr.style "background" Theme.panelSubtleBackground
                , Attr.style "color" Theme.textPrimary
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "4px"
                , Attr.style "cursor" "pointer"
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "12px"
                ]
                [ text "■ Stop" ]
            ]
        , div
            [ Attr.style "margin-top" "6px"
            , Attr.style "font-size" "11px"
            , Attr.style "color" Theme.textMuted
            ]
            [ text
                (formatTime model.simulation.playback.currentTime
                    ++ (if model.simulation.playback.running then
                            " PLAYING"

                        else
                            " PAUSED"
                       )
                )
            ]
        , div
            [ Attr.style "margin-top" "6px"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "color" Theme.textMuted
            ]
            [ div [ Attr.style "font-size" "11px" ] [ text "RPM" ]
            , div [ Attr.style "font-size" "11px" ] [ text (String.fromInt (round (model.simulation.motor.speedRadPerSec * 60 / (2 * pi))) ++ " / ±" ++ String.fromInt (round model.simulation.maxRpm)) ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.min (String.fromFloat -model.simulation.maxRpm)
            , Attr.max (String.fromFloat model.simulation.maxRpm)
            , Attr.step "1"
            , Attr.value (String.fromFloat (model.simulation.motor.speedRadPerSec * 60 / (2 * pi)))
            , Html.Events.onInput
                (\raw ->
                    case String.toFloat raw of
                        Just val ->
                            SimulationMsg (Simulation.SetMotorSpeed val)

                        Nothing ->
                            SimulationMsg (Simulation.SetMotorSpeed (model.simulation.motor.speedRadPerSec * 60 / (2 * pi)))
                )
            , Attr.style "width" "100%"
            ]
            []
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "16px"
        , Attr.style "right" "16px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        ]
        (FileUpload.view
            { onRequestFileUpload = RequestFileUpload }
            ++ studioLink model
        )


{-| "Edit in studio", carrying the model and the current camera in the URL hash.
Only offered when the host configured a studio URL and the current model came
from a URL (local/uploaded files have no URL to pass).
-}
studioLink : Model -> List (Html Msg)
studioLink model =
    if model.studioUrl == "" then
        []

    else
        case model.currentModelUrl of
            Just url ->
                let
                    cameraHash =
                        Hash.encodeCamera model.viewport.camera

                    href =
                        model.studioUrl
                            ++ "#src="
                            ++ Url.percentEncode url
                            ++ (if String.isEmpty cameraHash then
                                    ""

                                else
                                    "&" ++ cameraHash
                               )
                in
                [ Button.link href "Edit in studio" ]

            Nothing ->
                []



-- ── Helpers ───────────────────────────────────────────────────────────────────


parseUiMode : String -> UiMode
parseUiMode rawMode =
    case String.toLower (String.trim rawMode) of
        "viewer" ->
            ViewerMode

        _ ->
            SimulatorMode


uiModeToString : UiMode -> String
uiModeToString uiMode =
    case uiMode of
        ViewerMode ->
            "viewer"

        SimulatorMode ->
            "simulator"


{-| Wrap angle to [−180, 180) degrees for display.
-}
truncateAngle : Float -> Float
truncateAngle deg =
    let
        n =
            deg - 360 * toFloat (floor (deg / 360))
    in
    if n >= 180 then
        n - 360

    else
        n


format1dp : Float -> String
format1dp value =
    let
        rounded =
            toFloat (round (value * 10)) / 10

        whole =
            truncate rounded

        fractional =
            abs (round ((rounded - toFloat whole) * 10))
    in
    String.fromInt whole ++ "." ++ String.fromInt fractional


formatTime : Float -> String
formatTime secondsRaw =
    let
        seconds =
            max 0 secondsRaw

        minutesPart =
            floor (seconds / 60)

        secondsPart =
            floor (seconds - toFloat (minutesPart * 60))

        tenths =
            remainderBy 10 (floor (seconds * 10))

        mm =
            if minutesPart < 10 then
                "0" ++ String.fromInt minutesPart

            else
                String.fromInt minutesPart

        ss =
            if secondsPart < 10 then
                "0" ++ String.fromInt secondsPart

            else
                String.fromInt secondsPart
    in
    mm ++ ":" ++ ss ++ "." ++ String.fromInt tenths



-- ── Entry point ───────────────────────────────────────────────────────────────


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
