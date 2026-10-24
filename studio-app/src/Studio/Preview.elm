module Studio.Preview exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

{-| The simulation preview: the document as it is now, with its gears turning.

Opening the preview takes a snapshot of the document and gives it to a fresh
`Viewport` and `Simulation` — the same pair the viewer is built from — so what
runs here is exactly what the viewer would show for the saved file. Gear
detection runs every time the preview opens, on the snapshot; nothing in the
editing workspace is touched, so closing the preview returns to the same
selection, history and document, with the camera wherever it was left.

The preview starts from the workspace's camera and its already loaded parts, so
it normally opens without fetching anything.

-}

import Array
import Browser.Events
import Data
import Dict
import EmbeddedParts
import FeatherIcons
import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attr
import Html.Events
import LDraw.Document exposing (Document)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Writer as Writer
import Ports
import Render.Camera as Camera exposing (Camera)
import Render.Style as Style
import Simulation
import Task
import UI.Button as Button
import UI.ShortcutHelp as ShortcutHelp
import UI.Status as Status
import UI.Theme as Theme
import UI.ViewPad as ViewPad
import Viewport
import Viewport.Framing exposing (ViewPreset(..))
import Viewport.Hash as Hash
import Viewport.Shortcuts as Shortcuts


{-| What the preview needs to open.
-}
type alias Config =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , renderStyle : Style.Style
    , maxRpm : Float
    , initialRpm : Float
    , snapshot : Document
    , camera : Camera
    , loadedParts : PartCache
    , window : ( Int, Int )
    }


type alias Model =
    { viewport : Viewport.Model
    , simulation : Simulation.Model
    , helpOpen : Bool
    }


type Msg
    = ViewportMsg Viewport.Msg
    | SimulationMsg Simulation.Msg
    | ToggleHelp
    | WindowSized Int Int
    | Leave
    | NoOp


{-| Open the preview on a snapshot of the document.
-}
init : Config -> ( Model, Cmd Msg )
init config =
    let
        ( width, height ) =
            config.window

        ( viewport, _ ) =
            Viewport.init
                { ldrawBase = config.ldrawBase
                , ldrawFallbackBase = config.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = config.supportsPointerEvents
                , useWindowResize = False
                , renderStyle = config.renderStyle
                , partCache = Dict.union (onlyLoaded config.loadedParts) EmbeddedParts.cache
                }

        placed =
            { viewport
                | camera = config.camera
                , cameraMode = Viewport.CameraManual
            }

        model =
            { viewport = placed
            , simulation =
                Simulation.init
                    { gearSpecs = Data.gearParts
                    , lodCache = EmbeddedParts.lodCache
                    , maxRpm = config.maxRpm
                    , initialRpm = config.initialRpm
                    , requestedMotorIndex = Nothing
                    }
            , helpOpen = False
            }
    in
    update (WindowSized width height) model
        |> andThen (update (ViewportMsg (Viewport.FileContentReceived (Writer.writeDocument config.snapshot))))


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen next ( model, cmd ) =
    let
        ( final, nextCmd ) =
            next model
    in
    ( final, Cmd.batch [ cmd, nextCmd ] )


{-| Parts the workspace has finished loading; entries still in flight or failed
are left for the preview to fetch itself.
-}
onlyLoaded : PartCache -> PartCache
onlyLoaded cache =
    Dict.filter
        (\_ status ->
            case status of
                Loaded _ ->
                    True

                _ ->
                    False
        )
        cache



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            updateInner msg model
    in
    -- The gears' level of detail follows the camera, whatever moved it.
    ( { next | simulation = Simulation.refreshLods next.viewport.camera next.simulation }, cmd )


updateInner : Msg -> Model -> ( Model, Cmd Msg )
updateInner msg model =
    case msg of
        ViewportMsg viewportMsg ->
            let
                ( viewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( next, eventCmds ) =
                    List.foldl handleEvent ( { model | viewport = viewport }, [] ) events
            in
            ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )

        SimulationMsg simulationMsg ->
            let
                ( simulation, _ ) =
                    Simulation.update simulationMsg model.simulation
            in
            ( { model | simulation = simulation }, Cmd.none )

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        WindowSized width height ->
            updateInner (ViewportMsg (Viewport.WindowResize width height)) model

        Leave ->
            -- Leaving is the page switcher's business; see `Main`.
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleEvent : Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvent event ( model, cmds ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ] )

        Viewport.LoadStarted ->
            ( { model | simulation = Simulation.reset model.simulation }, cmds )

        Viewport.PartsReady ->
            -- Gear detection: every time the preview opens, on its snapshot.
            let
                ( simulation, analysis ) =
                    Simulation.analyse
                        { lines = model.viewport.topLevelLines, cache = model.viewport.partCache }
                        model.simulation

                ( viewport, flattenEvents ) =
                    Viewport.beginFlatten
                        { lines = analysis.staticLines, fallbackLines = analysis.fallbackLines }
                        model.viewport
            in
            List.foldl handleEvent
                ( { model | simulation = simulation, viewport = viewport }, cmds )
                flattenEvents

        Viewport.LargeModel _ ->
            ( { model | simulation = Simulation.markUnavailable model.simulation }, cmds )

        Viewport.FlattenRequested payload ->
            ( model, cmds ++ [ Ports.requestGeometryFlatten payload ] )

        Viewport.Clicked x y _ ->
            ( pick x y model, cmds )

        Viewport.DoubleClicked _ _ ->
            ( model, cmds )

        Viewport.Tapped x y ->
            ( pick x y model, cmds )

        Viewport.KeyAccepted keyEvent ->
            case Shortcuts.actionFor { helpVisible = model.helpOpen, walking = model.viewport.camera.navigation == Camera.Walk } keyEvent of
                Just action ->
                    let
                        ( next, cmd ) =
                            updateInner (msgForAction action) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    if keyEvent.key == "Escape" then
                        -- `Main` switches pages, and it only sees messages that
                        -- come back from outside: re-send `Leave` as a command.
                        ( model, cmds ++ [ Task.perform identity (Task.succeed Leave) ] )

                    else
                        ( model, cmds )

        Viewport.PressStarted _ _ ->
            ( model, cmds )

        Viewport.DragMoved _ _ ->
            ( model, cmds )

        Viewport.DragEnded _ _ ->
            ( model, cmds )

        Viewport.WheelScrolled _ ->
            ( model, cmds )

        Viewport.ModelReady ->
            ( model, cmds )

        Viewport.LoadFailed _ ->
            ( model, cmds )


{-| Make the gear under a point the motor.
-}
pick : Float -> Float -> Model -> Model
pick x y model =
    case Simulation.pickGear model.viewport.camera model.viewport.width model.viewport.height x y model.simulation of
        Just gearId ->
            { model | simulation = Simulation.update (Simulation.SetMotorGear gearId) model.simulation |> Tuple.first }

        Nothing ->
            model


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


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
        , Sub.map SimulationMsg (Simulation.subscriptions model.simulation)
        , -- Window size, not the runtime's port: Elm replaces the mount node, so the
          -- runtime's ResizeObserver watches a detached element and never fires.
          Browser.Events.onResize WindowSized
        , Ports.geometryFlattened (ViewportMsg << Viewport.GeometryFlattened)
        , Ports.geometryFlattenFailed (ViewportMsg << Viewport.GeometryFlattenFailed)
        ]



-- ── View ──────────────────────────────────────────────────────────────────────


view : Model -> Html Msg
view model =
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" Theme.appBackground
        ]
        [ Html.map ViewportMsg
            (Viewport.view
                { extraEntities = \camera style aspect -> Simulation.entities camera style aspect model.simulation
                , coarsenWhileMoving = not model.simulation.simulationAvailable
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
         , viewControls model
         , viewPad model
         ]
            ++ (if model.helpOpen then
                    [ ShortcutHelp.view
                        { rows = Shortcuts.helpRows ++ [ ( "Preview", [ ( "Esc", "Back to editing" ) ] ) ]
                        , onClose = ToggleHelp
                        }
                    ]

                else
                    []
               )
        )


viewTopBar : Model -> Html Msg
viewTopBar model =
    Html.header
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "max-width" "calc(100% - 24px)"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "12px"
        , Attr.style "padding" "6px 14px 6px 6px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "10px"
        , Attr.style "box-shadow" floatingShadow
        , Attr.style "color" Theme.textMuted
        , Attr.style "font-size" "13px"
        , Attr.style "white-space" "nowrap"
        ]
        [ Button.icon
            { icon = icon FeatherIcons.arrowLeft
            , label = "Back to editing"
            , shortcut = Just "Esc"
            , pressed = Nothing
            , enabled = True
            , showLabel = True
            }
            Leave
        , Html.h1 [ Attr.style "margin" "0", Attr.style "font-size" "15px", Attr.style "font-weight" "700", Attr.style "color" Theme.textPrimary ] [ text "Simulation" ]
        , span [ Attr.attribute "role" "status", Attr.style "overflow" "hidden", Attr.style "text-overflow" "ellipsis" ] [ text (summary model) ]
        ]


icon : FeatherIcons.Icon -> Html msg
icon featherIcon =
    featherIcon
        |> FeatherIcons.withSize 18
        |> FeatherIcons.withStrokeWidth 1.75
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true", Attr.attribute "focusable" "false" ]


floatingShadow : String
floatingShadow =
    "0 6px 20px color-mix(in srgb, var(--color-brand) 12%, transparent), 0 1px 2px color-mix(in srgb, var(--color-brand) 8%, transparent)"


summary : Model -> String
summary model =
    case ( model.simulation.simulationAvailable, model.simulation.gearGraph ) of
        ( True, Just graph ) ->
            let
                count =
                    Array.length graph.instances
            in
            String.fromInt count
                ++ (if count == 1 then
                        " gear detected"

                    else
                        " gears detected"
                   )

        _ ->
            if model.simulation.simulationChecked then
                "No supported gear train detected in this model."

            else
                ""


{-| Play, stop and the motor speed, along the bottom edge.
-}
viewControls : Model -> Html Msg
viewControls model =
    if not model.simulation.simulationAvailable then
        text ""

    else
        let
            rpm =
                model.simulation.motor.speedRadPerSec * 60 / (2 * pi)
        in
        div
            [ Attr.style "position" "absolute"
            , Attr.style "left" "12px"
            , Attr.style "bottom" "12px"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "8px"
            , Attr.style "padding" "6px 14px 6px 6px"
            , Attr.style "pointer-events" "auto"
            , Attr.style "touch-action" "none"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "10px"
            , Attr.style "box-shadow" floatingShadow
            , Attr.style "font-size" "13px"
            , Attr.style "color" Theme.textMuted
            , Attr.attribute "role" "group"
            , Attr.attribute "aria-label" "Motor"
            ]
            [ Button.icon
                { icon =
                    icon
                        (if model.simulation.playback.running then
                            FeatherIcons.pause

                         else
                            FeatherIcons.play
                        )
                , label =
                    if model.simulation.playback.running then
                        "Pause"

                    else
                        "Play"
                , shortcut = Just "Space"
                , pressed = Nothing
                , enabled = True
                , showLabel = True
                }
                (SimulationMsg Simulation.ToggleMotor)
            , Button.icon
                { icon = icon FeatherIcons.square
                , label = "Stop"
                , shortcut = Nothing
                , pressed = Nothing
                , enabled = True
                , showLabel = True
                }
                (SimulationMsg Simulation.Stop)
            , Html.label [ Attr.for "motor-speed", Attr.style "margin-left" "6px" ] [ text "Speed" ]
            , input
                [ Attr.type_ "range"
                , Attr.id "motor-speed"
                , Attr.attribute "aria-valuetext" (String.fromInt (round rpm) ++ " RPM")
                , Attr.min (String.fromFloat -model.simulation.maxRpm)
                , Attr.max (String.fromFloat model.simulation.maxRpm)
                , Attr.step "1"
                , Attr.value (String.fromFloat rpm)
                , Html.Events.onInput
                    (\raw ->
                        case String.toFloat raw of
                            Just value ->
                                SimulationMsg (Simulation.SetMotorSpeed value)

                            Nothing ->
                                NoOp
                    )
                , Attr.style "width" "140px"
                ]
                []
            , span [ Attr.attribute "aria-hidden" "true", Attr.style "min-width" "64px", Attr.style "text-align" "right", Attr.style "color" Theme.textPrimary, Attr.style "font-variant-numeric" "tabular-nums" ] [ text (String.fromInt (round rpm) ++ " RPM") ]
            ]


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
