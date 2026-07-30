module Main exposing (Flags, HeldControl(..), LoadPhase(..), Model, init, main, needsAnimationFrame)

{-| Main application entrypoint for Brick Frame (Palikkakehys).
-}

import Array
import Browser
import Browser.Dom
import Browser.Events
import Data
import Dict exposing (Dict)
import FeatherIcons
import Gear.Animate as Animate exposing (MotorState)
import Gear.Components as Components
import Gear.Detect as Detect
import Gear.Lod as Lod exposing (Lod)
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId, GearInstance)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..), ResolverConfig)
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4
import Ports
import Render.Camera as Camera exposing (Camera)
import Render.EdgeShader as EdgeShader exposing (EdgeVertex)
import Render.GuideShader as GuideShader
import Render.Instanced as Instanced exposing (InstancedScene)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)
import Render.Shader as Shader
import Render.Style as Style
import Set exposing (Set)
import Task
import Time
import UI.FileUpload as FileUpload
import UI.Shortcuts as Shortcuts
import UI.Theme as Theme
import WebGL
import WebGL.Settings.DepthTest as DepthTest


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
    }



-- ── Model ─────────────────────────────────────────────────────────────────────


type LoadPhase
    = Idle
    | FetchingTopLevel String
    | ResolvingParts
    | FlatteningGeometry
    | Ready


type alias PlaybackState =
    { running : Bool
    , currentTime : Float
    , motorGearId : Maybe GearId
    , motorSpeedRadPerSec : Float
    }


type CameraMode
    = CameraAutoFit
    | CameraManual


{-| A canonical viewpoint, reachable from the number keys.

Each frames the whole model from a fixed direction; `IsometricView` is the
three-quarter view auto-fit uses.

-}
type ViewPreset
    = FrontView
    | BackView
    | LeftView
    | RightView
    | TopView
    | IsometricView


{-| Azimuth and elevation for a preset, in radians.

`TopView` stops just short of straight down, matching the camera's own elevation
clamp.

-}
viewPresetAngles : ViewPreset -> ( Float, Float )
viewPresetAngles preset =
    case preset of
        FrontView ->
            ( 0, 0 )

        BackView ->
            ( pi, 0 )

        LeftView ->
            ( -pi / 2, 0 )

        RightView ->
            ( pi / 2, 0 )

        TopView ->
            ( 0, pi / 2 - 0.01 )

        IsometricView ->
            ( 0.75, 0.6 )


type UiMode
    = ViewerMode
    | SimulatorMode


type alias TouchPoint =
    { id : Int
    , x : Float
    , y : Float
    }


type TouchGesture
    = NoTouchGesture
    | SingleTouchGesture Int
    | PinchGesture PinchState


{-| Two-finger gesture state.

`startDistance` and `zoomEngaged` implement the zoom dead-zone: a two-finger
drag is treated as pure pan until the fingers have deliberately changed
separation, after which zoom stays engaged for the rest of the gesture. Without
it, ordinary finger jitter during a pan bled into the zoom level.

-}
type alias PinchState =
    { idA : Int
    , idB : Int
    , distance : Float
    , midX : Float
    , midY : Float
    , startDistance : Float
    , zoomEngaged : Bool
    }


{-| How far the fingers must change separation, in pixels, before a two-finger
gesture starts zooming as well as panning.
-}
pinchZoomDeadZone : Float
pinchZoomDeadZone =
    12.0


type HeldControl
    = NoHeldControl
    | HoldRotate Float Float


type alias Model =
    { camera : Camera
    , width : Int
    , height : Int
    , loadPhase : LoadPhase
    , topLevelLines : List LDrawLine
    , partCache : PartCache
    , resolverConfig : ResolverConfig
    , partsLoaded : Int
    , partsTotal : Int
    , scene : Maybe Scene
    , instancedScene : Maybe InstancedScene
    , modelBounds : Maybe ModelBounds
    , submodelNames : Set String
    , refineFrames : Int
    , errorMsg : Maybe String
    , gearGraph : Maybe GearGraph
    , gearMeshes : Dict GearId GearRender
    , gearLods : Dict GearId Lod
    , components : List Components.ComponentInstance
    , componentMeshes : List ComponentMeshRender
    , componentRenders : List ComponentRender
    , motor : MotorState
    , playback : PlaybackState
    , maxRpm : Float
    , gearAngles : Dict GearId Float
    , clickStart : Maybe ( Float, Float )
    , dragTravel : Float
    , lastFrameTime : Maybe Time.Posix
    , cameraMode : CameraMode
    , touchGesture : TouchGesture
    , activeTouches : Dict Int TouchPoint
    , controlsCollapsed : Bool
    , mousePanDragging : Bool
    , uiMode : UiMode
    , simulationChecked : Bool
    , simulationAvailable : Bool
    , loadingProgressPct : Float
    , loadingProgressTick : Maybe Time.Posix
    , viewerControlsEnabled : Bool
    , supportsPointerEvents : Bool
    , requestedMotorIndex : Maybe Int
    , heldControl : HeldControl
    , heldControlTick : Maybe Time.Posix
    , heldKeys : Set String
    , heldKeysTick : Maybe Time.Posix
    , showShortcutHelp : Bool
    , useWindowResize : Bool
    , renderStyle : Style.Style
    }


type alias GearRender =
    { mesh : WebGL.Mesh Vertex
    , lodMesh : WebGL.Mesh Vertex
    , center : Vec3.Vec3
    , axis : Vec3.Vec3
    , pitchRadius : Float
    }


type alias ComponentRender =
    { mesh : WebGL.Mesh EdgeVertex
    , center : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : Maybe GearId
    }


type alias ComponentMeshRender =
    { mesh : WebGL.Mesh Vertex
    , center : Vec3.Vec3
    , rotationCenter : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : Maybe GearId
    }



-- ── Init ──────────────────────────────────────────────────────────────────────


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        uiMode =
            parseUiMode flags.uiMode

        fallbackBase =
            if String.trim flags.ldrawFallbackBase == "" then
                Nothing

            else
                Just flags.ldrawFallbackBase

        resolver =
            Resolve.resolverConfig flags.ldrawBase fallbackBase

        initialHash =
            decodeHash flags.initialHash

        initialModelUrl =
            String.trim flags.defaultModelUrl

        shouldLoadInitialModel =
            not (String.isEmpty initialModelUrl)

        baseCamera =
            Camera.init

        initialCamera =
            { baseCamera
                | azimuth = Maybe.withDefault baseCamera.azimuth initialHash.azimuth
                , elevation = Maybe.withDefault baseCamera.elevation initialHash.elevation
                , distance = Maybe.withDefault baseCamera.distance initialHash.distance
                , target =
                    Vec3.vec3
                        (Maybe.withDefault (Vec3.getX baseCamera.target) initialHash.targetX)
                        (Maybe.withDefault (Vec3.getY baseCamera.target) initialHash.targetY)
                        (Maybe.withDefault (Vec3.getZ baseCamera.target) initialHash.targetZ)
                , navigation = Maybe.withDefault baseCamera.navigation initialHash.navigation
            }

        maxRpmValue =
            max 1 flags.maxRpm

        initialMotorRpm =
            clamp -maxRpmValue maxRpmValue flags.initialRpm

        defaultMotor =
            Animate.defaultMotor

        initialCameraMode =
            if hasExplicitHashCamera initialHash then
                CameraManual

            else
                CameraAutoFit

        initialLoadPhase =
            if shouldLoadInitialModel then
                FetchingTopLevel initialModelUrl

            else
                Idle
    in
    ( { camera = initialCamera
      , width = 800
      , height = 600
      , loadPhase = initialLoadPhase
      , topLevelLines = []
      , partCache = embeddedPartCache
      , resolverConfig = resolver
      , partsLoaded = 0
      , partsTotal = 0
      , scene = Nothing
      , instancedScene = Nothing
      , modelBounds = Nothing
      , submodelNames = Set.empty
      , refineFrames = 0
      , errorMsg = Nothing
      , gearGraph = Nothing
      , gearMeshes = Dict.empty
      , gearLods = Dict.empty
      , components = []
      , componentMeshes = []
      , componentRenders = []
      , motor = { defaultMotor | speedRadPerSec = initialMotorRpm * 2 * pi / 60 }
      , playback =
            { running = False
            , currentTime = 0.0
            , motorGearId = Nothing
            , motorSpeedRadPerSec = initialMotorRpm * 2 * pi / 60
            }
      , maxRpm = maxRpmValue
      , gearAngles = Dict.empty
      , clickStart = Nothing
      , dragTravel = 0.0
      , lastFrameTime = Nothing
      , cameraMode = initialCameraMode
      , touchGesture = NoTouchGesture
      , activeTouches = Dict.empty
      , controlsCollapsed = False
      , mousePanDragging = False
      , uiMode = uiMode
      , simulationChecked = False
      , simulationAvailable = False
      , loadingProgressPct = 0.0
      , loadingProgressTick = Nothing
      , viewerControlsEnabled = flags.controlsEnabled
      , supportsPointerEvents = flags.supportsPointerEvents
      , requestedMotorIndex =
            if flags.initialMotorIndex < 0 then
                Nothing

            else
                Just flags.initialMotorIndex
      , heldControl = NoHeldControl
      , heldControlTick = Nothing
      , heldKeys = Set.empty
      , heldKeysTick = Nothing
      , showShortcutHelp = False
      , useWindowResize = flags.useWindowResize
      , renderStyle = buildRenderStyle flags.ambientStrength flags.lightStrength flags.vibrance flags.edgeWidth
      }
    , Cmd.batch <|
        (if flags.useWindowResize then
            [ Browser.Dom.getViewport
                |> Task.perform
                    (\vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height))
            ]

         else
            []
        )
            ++ [ Ports.setUrlHash (encodeHashString initialCamera) ]
            ++ (if shouldLoadInitialModel then
                    [ fetchTopLevel initialModelUrl ]

                else
                    []
               )
    )


fetchTopLevel : String -> Cmd Msg
fetchTopLevel url =
    Http.get
        { url = url
        , expect = Http.expectString (TopLevelLoaded url)
        }


embeddedPartCache : PartCache
embeddedPartCache =
    Dict.foldl
        (\name text acc ->
            Dict.insert name (Loaded (Parser.parseFile text)) acc
        )
        Resolve.initCache
        Data.embeddedParts


{-| Simplified (decimated) versions of the embedded gear parts, keyed by the
same part-file names as `embeddedPartCache`. Used to build the low-detail gear
meshes selected at a distance (see `Gear.Lod`).
-}
embeddedLodCache : PartCache
embeddedLodCache =
    Dict.foldl
        (\name text acc ->
            Dict.insert name (Loaded (Parser.parseFile text)) acc
        )
        Resolve.initCache
        Data.lodParts


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


type Msg
    = WindowResize Int Int
    | MouseDown Float Float Bool
    | MouseMove Float Float
    | MouseUp Float Float
    | Wheel Float
    | TouchStart (List TouchPoint)
    | TouchMove (List TouchPoint)
    | TouchEnd (List TouchPoint)
    | PointerTouchStart TouchPoint
    | PointerTouchMove TouchPoint
    | PointerTouchEnd Int
    | TopLevelLoaded String (Result Http.Error String)
    | PartLoaded String (Result Http.Error String)
    | RequestFileUpload
    | FileContentReceived String
    | FileLoadError String
    | GeometryFlattened String
    | GeometryFlattenFailed String
    | DismissError
    | AnimationFrame Time.Posix
    | Play
    | Pause
    | Stop
    | ToggleMotor
    | SetMotorGear GearId
    | SetMotorSpeed Float
    | RotateCameraBy Float Float
    | StartHoldRotate Float Float
    | EndHoldRotate
    | FitView
    | ToggleWalkMode
    | SetViewPreset ViewPreset
    | ToggleControlsPanel
    | KeyPressed Shortcuts.KeyEvent
    | KeyReleased String
    | AllKeysReleased
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
        , ( "simulationAvailable", Encode.bool model.simulationAvailable )
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


cameraChangedEventCmd : Camera -> Cmd Msg
cameraChangedEventCmd camera =
    runtimeEventCmd "camera-changed"
        [ ( "azimuth", Encode.float camera.azimuth )
        , ( "elevation", Encode.float camera.elevation )
        , ( "azimuthDeg", Encode.float (camera.azimuth * 180 / pi) )
        , ( "elevationDeg", Encode.float (camera.elevation * 180 / pi) )
        , ( "distance", Encode.float camera.distance )
        , ( "targetX", Encode.float (Vec3.getX camera.target) )
        , ( "targetY", Encode.float (Vec3.getY camera.target) )
        , ( "targetZ", Encode.float (Vec3.getZ camera.target) )
        , ( "navigation", Encode.string (navigationToString camera.navigation) )
        ]


cameraChangedIfNeededCmd : Camera -> Camera -> Cmd Msg
cameraChangedIfNeededCmd before after =
    if cameraMoved before after then
        cameraChangedEventCmd after

    else
        Cmd.none


{-| Whether the viewpoint actually changed, ignoring drag bookkeeping fields.
-}
cameraMoved : Camera -> Camera -> Bool
cameraMoved before after =
    before.azimuth
        /= after.azimuth
        || before.elevation
        /= after.elevation
        || before.distance
        /= after.distance
        || Vec3.getX before.target
        /= Vec3.getX after.target
        || Vec3.getY before.target
        /= Vec3.getY after.target
        || Vec3.getZ before.target
        /= Vec3.getZ after.target
        || before.navigation
        /= after.navigation


partsProgressPct : Int -> Int -> Float
partsProgressPct loaded total =
    if total <= 0 then
        90

    else
        clamp 0 90 (toFloat loaded / toFloat total * 90)



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmd ) =
            updateInner msg model
    in
    -- Refresh per-gear LOD after every update so that any camera or gear-mesh
    -- change is reflected before the next render. `chooseLod` carries the
    -- previous LOD, giving hysteresis across frames. This single choke point
    -- avoids threading the refresh through every camera-mutating branch, and
    -- is also where the instanced renderer's settle countdown is maintained.
    ( { newModel
        | gearLods = refreshGearLods newModel
        , refineFrames = nextRefineFrames msg model newModel
      }
    , cmd
    )


{-| Countdown of frames still to be drawn at reduced detail.

Any camera movement refills it; each animation frame that does not move the
camera spends one. When it reaches zero the next frame is drawn at full detail.
Counting frames rather than milliseconds keeps this in step with the
`onAnimationFrame` subscription that `needsAnimationFrame` gates, so the
refinement frame is guaranteed to actually be rendered before the subscription
goes quiet again.

-}
nextRefineFrames : Msg -> Model -> Model -> Int
nextRefineFrames msg before after =
    if cameraMoved before.camera after.camera then
        coarseFrameCount

    else
        case msg of
            AnimationFrame _ ->
                max 0 (after.refineFrames - 1)

            _ ->
                after.refineFrames


{-| How many frames to keep coarsening after the last camera movement.
At 60 fps this is a little over 100 ms — long enough to cover the gap between
consecutive wheel events, short enough to feel immediate on release.
-}
coarseFrameCount : Int
coarseFrameCount =
    8


{-| Whether the next frame should be drawn one detail band coarser.

Only static scenes get this treatment. A model with a running simulation is
redrawing continuously and has no settled state to refine towards, so trading
detail for latency there would simply make it permanently coarse.

-}
cameraIsMoving : Model -> Bool
cameraIsMoving model =
    not model.simulationAvailable && model.refineFrames > 0


{-| Recompute each gear's level of detail from its distance to the camera,
starting from its previous LOD so the switch has hysteresis (see `Gear.Lod`).
-}
refreshGearLods : Model -> Dict GearId Lod
refreshGearLods model =
    let
        viewPos =
            Camera.position model.camera
    in
    Dict.map
        (\id rendered ->
            let
                distance =
                    Vec3.distance viewPos rendered.center

                previous =
                    Dict.get id model.gearLods |> Maybe.withDefault Lod.Full
            in
            Lod.chooseLod previous distance rendered.pitchRadius
        )
        model.gearMeshes


updateInner : Msg -> Model -> ( Model, Cmd Msg )
updateInner msg model =
    case msg of
        WindowResize w h ->
            ( { model
                | width = w
                , height = h

                -- Orbit and pan sensitivity are relative to canvas height.
                , camera = Camera.setViewportHeight (toFloat h) model.camera
              }
            , Cmd.none
            )

        MouseDown x y shiftHeld ->
            if touchInputActive model then
                ( model, Cmd.none )

            else
                ( { model
                    | camera = Camera.onMouseDown x y model.camera
                    , clickStart = Just ( x, y )
                    , dragTravel = 0.0
                    , cameraMode = CameraManual
                    , mousePanDragging = shiftHeld
                  }
                , Cmd.none
                )

        MouseMove x y ->
            if touchInputActive model then
                ( model, Cmd.none )

            else
                let
                    stepDist =
                        case model.camera.lastMousePos of
                            Just ( lx, ly ) ->
                                sqrt (((x - lx) * (x - lx)) + ((y - ly) * (y - ly)))

                            Nothing ->
                                0.0

                    nextCamera =
                        if model.mousePanDragging then
                            case model.camera.lastMousePos of
                                Just ( lx, ly ) ->
                                    Camera.onPan (x - lx) (y - ly) model.camera
                                        |> (\cam -> { cam | lastMousePos = Just ( x, y ) })

                                Nothing ->
                                    model.camera

                        else
                            Camera.onMouseMove x y model.camera

                    nextModel =
                        { model
                            | camera = nextCamera
                            , dragTravel = model.dragTravel + stepDist
                            , cameraMode = CameraManual
                        }
                in
                ( nextModel
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                )

        MouseUp x y ->
            if touchInputActive model then
                -- A real mouseup while touch state is live means that state is
                -- stale: a touchend or pointerup went missing (backgrounded tab,
                -- a touchcancel that still listed fingers). Clearing it here is
                -- the only way out — the reset below is unreachable in exactly
                -- this case, which used to leave mouse input dead for the rest
                -- of the session.
                ( { model
                    | touchGesture = NoTouchGesture
                    , activeTouches = Dict.empty
                    , camera = Camera.onMouseUp model.camera
                  }
                , Cmd.none
                )

            else
                let
                    clickedGear =
                        if model.dragTravel < 3.0 then
                            pickGearByScreenPoint x y model

                        else
                            Nothing

                    releasedModel =
                        { model
                            | camera = Camera.onMouseUp model.camera
                            , clickStart = Nothing
                            , dragTravel = 0.0
                            , touchGesture = NoTouchGesture
                            , activeTouches = Dict.empty
                            , mousePanDragging = False
                        }
                in
                case clickedGear of
                    Just gearId ->
                        setMotorGear gearId releasedModel

                    Nothing ->
                        ( releasedModel, Ports.setUrlHash (encodeHash releasedModel) )

        Wheel delta ->
            let
                nextModel =
                    { model
                        | camera = applyWheel delta model.camera
                        , cameraMode = CameraManual
                    }
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        TouchStart touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    beginTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel
            , cameraChangedIfNeededCmd model.camera nextModel.camera
            )

        TouchMove touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    advanceTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel
            , cameraChangedIfNeededCmd model.camera nextModel.camera
            )

        TouchEnd touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    endTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        PointerTouchStart touchPoint ->
            let
                nextTouches =
                    Dict.insert touchPoint.id touchPoint model.activeTouches

                nextModel =
                    beginTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel
            , cameraChangedIfNeededCmd model.camera nextModel.camera
            )

        PointerTouchMove touchPoint ->
            if Dict.member touchPoint.id model.activeTouches then
                let
                    nextTouches =
                        Dict.insert touchPoint.id touchPoint model.activeTouches

                    nextModel =
                        advanceTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
                in
                ( nextModel
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                )

            else
                ( model, Cmd.none )

        PointerTouchEnd touchId ->
            -- Guarded like PointerTouchMove: iOS Safari fires both Touch and
            -- Pointer events, and the dict is keyed by whichever family opened
            -- the gesture. Removing an unknown id used to be a no-op that still
            -- ran endTouchGesture over the remaining fingers, restarting a
            -- single-touch drag from stale coordinates.
            if Dict.member touchId model.activeTouches then
                let
                    nextTouches =
                        Dict.remove touchId model.activeTouches

                    nextModel =
                        endTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
                in
                ( nextModel
                , Cmd.batch
                    [ Ports.setUrlHash (encodeHash nextModel)
                    , cameraChangedIfNeededCmd model.camera nextModel.camera
                    ]
                )

            else
                ( model, Cmd.none )

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        FileContentReceived text ->
            handleTopLevelText text model

        FileLoadError err ->
            ( { model | errorMsg = Just err, loadingProgressPct = 0, loadingProgressTick = Nothing }, modelErrorEventCmd err )

        GeometryFlattened payload ->
            case Decode.decodeString flattenResultDecoder payload of
                Ok result ->
                    let
                        scene =
                            Scene.buildScene
                                { triangles = result.triangles
                                , lines = result.lines
                                , conditionalLines = result.conditionalLines
                                }
                                result.bfcCertified

                        updatedModel =
                            { model
                                | scene = Just scene
                                , loadPhase = Ready
                                , loadingProgressPct = 100
                                , loadingProgressTick = Nothing
                            }
                    in
                    ( updatedModel, modelLoadedEventCmd updatedModel )

                Err err ->
                    let
                        fallback =
                            buildSceneFallback model
                    in
                    ( { fallback
                        | errorMsg =
                            Just
                                ("Geometry worker decode failed; used local flatten. "
                                    ++ Decode.errorToString err
                                )
                      }
                    , modelLoadedEventCmd fallback
                    )

        GeometryFlattenFailed err ->
            let
                fallback =
                    buildSceneFallback model
            in
            ( { fallback | errorMsg = Just ("Geometry worker failed; used local flatten. " ++ err) }
            , modelLoadedEventCmd fallback
            )

        DismissError ->
            ( { model | errorMsg = Nothing }, Cmd.none )

        TopLevelLoaded _ (Err err) ->
            let
                message =
                    "Failed to load model: " ++ httpErrString err
            in
            ( { model
                | errorMsg = Just message
                , loadPhase = Idle
                , loadingProgressPct = 0
                , loadingProgressTick = Nothing
              }
            , modelErrorEventCmd message
            )

        TopLevelLoaded _ (Ok text) ->
            if looksLikeHtmlResponse text then
                let
                    message =
                        "Top-level URL returned HTML instead of LDraw text. Run `make sync-ldraw` or use a direct LDraw URL."
                in
                ( { model
                    | errorMsg = Just message
                    , loadPhase = Idle
                    , loadingProgressPct = 0
                    , loadingProgressTick = Nothing
                  }
                , modelErrorEventCmd message
                )

            else
                handleTopLevelText text model

        PartLoaded name result ->
            handlePartResult name result model

        AnimationFrame now ->
            let
                ( progressModel, progressCmd ) =
                    if model.loadPhase == FlatteningGeometry then
                        let
                            dtSeconds =
                                case model.loadingProgressTick of
                                    Just prev ->
                                        max 0 (toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0)

                                    Nothing ->
                                        0.016

                            nextPct =
                                min 99.5 (max 90 model.loadingProgressPct + (dtSeconds * 4))
                        in
                        ( { model
                            | loadingProgressPct = nextPct
                            , loadingProgressTick = Just now
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | loadingProgressTick = Nothing }, Cmd.none )

                ( buttonModel, buttonCmd ) =
                    applyHeldControl now progressModel

                ( heldModel, keyCmd ) =
                    applyHeldKeys now buttonModel

                heldCmd =
                    Cmd.batch [ buttonCmd, keyCmd ]
            in
            if heldModel.playback.running then
                let
                    dtSeconds =
                        case heldModel.lastFrameTime of
                            Just prev ->
                                toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0

                            Nothing ->
                                0.0

                    newTime =
                        heldModel.playback.currentTime + dtSeconds

                    newAngles =
                        case heldModel.gearGraph of
                            Just graph ->
                                Animate.gearAngles heldModel.motor graph newTime

                            Nothing ->
                                Dict.empty

                    playbackState =
                        heldModel.playback
                in
                ( { heldModel
                    | playback =
                        { playbackState
                            | currentTime = newTime
                        }
                    , gearAngles = newAngles
                    , lastFrameTime = Just now
                  }
                , Cmd.batch [ progressCmd, heldCmd ]
                )

            else
                ( { heldModel | lastFrameTime = Nothing }, Cmd.batch [ progressCmd, heldCmd ] )

        Play ->
            let
                playbackState =
                    model.playback

                motorState =
                    model.motor

                resolvedMotorGearId =
                    resolveMotorGearId model

                newMotor =
                    { motorState
                        | running = True
                        , drivingGearId = resolvedMotorGearId
                    }

                newAngles =
                    case model.gearGraph of
                        Just graph ->
                            Animate.gearAngles newMotor graph model.playback.currentTime

                        Nothing ->
                            Dict.empty
            in
            ( { model
                | playback =
                    { playbackState
                        | running = True
                        , motorGearId = resolvedMotorGearId
                    }
                , motor = newMotor
                , gearAngles = newAngles
                , lastFrameTime = Nothing
              }
            , playStateEventCmd True model.playback.currentTime
            )

        Pause ->
            let
                playbackState =
                    model.playback

                motorState =
                    model.motor
            in
            ( { model
                | playback = { playbackState | running = False }
                , motor = { motorState | running = False }
              }
            , playStateEventCmd False model.playback.currentTime
            )

        Stop ->
            let
                playbackState =
                    model.playback

                motorState =
                    model.motor

                resetAngles =
                    case model.gearGraph of
                        Just graph ->
                            Animate.gearAngles model.motor graph 0.0

                        Nothing ->
                            Dict.empty
            in
            ( { model
                | playback = { playbackState | running = False, currentTime = 0.0 }
                , motor = { motorState | running = False }
                , gearAngles = resetAngles
                , lastFrameTime = Nothing
              }
            , Cmd.batch
                [ Ports.setUrlHash (encodeHashString model.camera)
                , playStateEventCmd False 0.0
                ]
            )

        ToggleMotor ->
            if model.playback.running then
                update Pause model

            else
                update Play model

        SetMotorGear id ->
            setMotorGear id model

        SetMotorSpeed rpm ->
            let
                motor =
                    model.motor

                playbackState =
                    model.playback

                clampedRpm =
                    clamp -model.maxRpm model.maxRpm rpm

                newMotor =
                    { motor | speedRadPerSec = clampedRpm * 2 * pi / 60 }
            in
            ( { model
                | motor = newMotor
                , playback = { playbackState | motorSpeedRadPerSec = newMotor.speedRadPerSec }
              }
            , Cmd.none
            )

        RotateCameraBy azimuthDelta elevationDelta ->
            let
                baseCamera =
                    model.camera

                nextCamera =
                    applyLook azimuthDelta elevationDelta baseCamera

                nextModel =
                    { model | camera = nextCamera, cameraMode = CameraManual }
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        StartHoldRotate azimuthStep elevationStep ->
            ( { model
                | heldControl = HoldRotate azimuthStep elevationStep
                , heldControlTick = Nothing
              }
            , Cmd.none
            )

        EndHoldRotate ->
            ( { model
                | heldControl = NoHeldControl
                , heldControlTick = Nothing
              }
            , Cmd.none
            )

        FitView ->
            let
                nextModel =
                    frameModelBounds model
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        ToggleWalkMode ->
            let
                nextModel =
                    toggleWalkMode model
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        SetViewPreset preset ->
            let
                nextModel =
                    applyViewPreset preset model
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )

        ToggleControlsPanel ->
            ( { model | controlsCollapsed = not model.controlsCollapsed }, Cmd.none )

        KeyPressed event ->
            if Shortcuts.isTextEntryTarget event then
                -- Someone is typing into a host page's form field. The keydown
                -- subscription is document-global, so without this a space in a
                -- URL box toggles playback in the embedded viewer.
                ( model, Cmd.none )

            else
                let
                    heldModel =
                        { model | heldKeys = Set.insert (Shortcuts.normalizeKey event.key) model.heldKeys }
                in
                -- A held key repeats at the OS rate; movement is integrated per
                -- frame instead, so only the first press does discrete work.
                if event.repeat then
                    ( heldModel, Cmd.none )

                else
                    case Shortcuts.actionFor (shortcutContext model) event of
                        Just action ->
                            updateInner (msgForAction action) heldModel

                        Nothing ->
                            ( heldModel, Cmd.none )

        KeyReleased key ->
            ( { model | heldKeys = Set.remove (Shortcuts.normalizeKey key) model.heldKeys }
            , Cmd.none
            )

        AllKeysReleased ->
            -- A key-up lost to a window blur or tab switch would otherwise leave
            -- the camera walking forever.
            ( { model | heldKeys = Set.empty }, Cmd.none )

        ToggleShortcutHelp ->
            ( { model | showShortcutHelp = not model.showShortcutHelp }, Cmd.none )


{-| What `Escape` and `?` need to know to pick between their meanings.
-}
shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.showShortcutHelp
    , walking = model.camera.navigation == Camera.Walk
    }


{-| Map a shortcut to the message it stands for.

`UI.Shortcuts` names its own actions so it stays free of `Msg`; this is the seam.

-}
msgForAction : Shortcuts.Action -> Msg
msgForAction action =
    case action of
        Shortcuts.TogglePlayback ->
            ToggleMotor

        Shortcuts.FitView ->
            FitView

        Shortcuts.ToggleWalk ->
            ToggleWalkMode

        Shortcuts.LeaveWalk ->
            ToggleWalkMode

        Shortcuts.ToggleHelp ->
            ToggleShortcutHelp

        Shortcuts.CloseHelp ->
            ToggleShortcutHelp

        Shortcuts.ZoomIn ->
            Wheel -keyboardZoomStep

        Shortcuts.ZoomOut ->
            Wheel keyboardZoomStep

        Shortcuts.PresetFront ->
            SetViewPreset FrontView

        Shortcuts.PresetBack ->
            SetViewPreset BackView

        Shortcuts.PresetLeft ->
            SetViewPreset LeftView

        Shortcuts.PresetRight ->
            SetViewPreset RightView

        Shortcuts.PresetTop ->
            SetViewPreset TopView

        Shortcuts.PresetIsometric ->
            SetViewPreset IsometricView


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


{-| Move and look according to the keys currently held down.

Shares `heldControlTick` with the on-screen rotate buttons so there is a single
notion of frame time, and integrates over it rather than counting key repeats —
so movement speed is independent of the OS repeat rate.

-}
applyHeldKeys : Time.Posix -> Model -> ( Model, Cmd Msg )
applyHeldKeys now model =
    if not (Shortcuts.hasContinuousInput model.heldKeys) then
        ( { model | heldKeysTick = Nothing }, Cmd.none )

    else
        let
            dtSeconds =
                case model.heldKeysTick of
                    Just prev ->
                        -- Capped: flattening geometry blocks the main thread, and
                        -- an uncapped delta would spend the whole stall as one
                        -- stride and teleport you across the model.
                        clamp 0 maxHeldKeyStep (toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0)

                    Nothing ->
                        0.016
        in
        if dtSeconds <= 0 then
            ( { model | heldKeysTick = Just now }, Cmd.none )

        else
            let
                movement =
                    Shortcuts.movementFor model.heldKeys

                look =
                    Shortcuts.lookFor model.heldKeys

                stride =
                    Camera.walkSpeed model.camera
                        * Shortcuts.speedMultiplier model.heldKeys
                        * dtSeconds

                turn =
                    keyboardTurnRate * dtSeconds

                nextCamera =
                    model.camera
                        |> Camera.moveBy
                            { forward = movement.forward * stride
                            , right = movement.right * stride
                            , up = movement.up * stride
                            }
                        |> applyLook (look.azimuth * turn) (look.elevation * turn)

                nextModel =
                    { model
                        | camera = nextCamera
                        , cameraMode = CameraManual
                        , heldKeysTick = Just now
                    }
            in
            ( nextModel
            , Cmd.batch
                [ Ports.setUrlHash (encodeHash nextModel)
                , cameraChangedIfNeededCmd model.camera nextModel.camera
                ]
            )


{-| Rotate the way a drag would in the active navigation mode.

Every rotation in this module goes through here — the arrow keys, the on-screen
chevron buttons and `RotateCameraBy` — so this is the only `Camera.orbitBy` call
site. Reaching for `orbitBy` directly is what made the chevron buttons swing the
eye in a small circle instead of turning the head while walking, which on a touch
device left dragging as the only way to turn.

-}
applyLook : Float -> Float -> Camera -> Camera
applyLook azimuthDelta elevationDelta camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.orbitBy azimuthDelta elevationDelta camera

        Camera.Walk ->
            Camera.lookAround azimuthDelta elevationDelta camera


{-| Arrow-key turn rate, in radians per second.
-}
keyboardTurnRate : Float
keyboardTurnRate =
    1.2


{-| Longest frame time a single held-key step may integrate over, in seconds.
-}
maxHeldKeyStep : Float
maxHeldKeyStep =
    0.1


applyHeldControl : Time.Posix -> Model -> ( Model, Cmd Msg )
applyHeldControl now model =
    case model.heldControl of
        HoldRotate azimuthStep elevationStep ->
            let
                dtSeconds =
                    case model.heldControlTick of
                        Just prev ->
                            max 0 (toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0)

                        Nothing ->
                            0.016

                speedFactor =
                    6.0
            in
            if dtSeconds <= 0 then
                ( { model | heldControlTick = Just now }, Cmd.none )

            else
                let
                    baseCamera =
                        model.camera

                    nextCamera =
                        applyLook
                            (azimuthStep * speedFactor * dtSeconds)
                            (elevationStep * speedFactor * dtSeconds)
                            baseCamera

                    nextModel =
                        { model
                            | camera = nextCamera
                            , cameraMode = CameraManual
                            , heldControlTick = Just now
                        }
                in
                ( nextModel
                , Cmd.batch
                    [ Ports.setUrlHash (encodeHash nextModel)
                    , cameraChangedIfNeededCmd model.camera nextModel.camera
                    ]
                )

        NoHeldControl ->
            ( { model | heldControlTick = Nothing }, Cmd.none )


handlePartResult : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
handlePartResult name result model =
    if model.loadPhase /= ResolvingParts then
        ( { model
            | partCache =
                Resolve.updateCache name result Parser.parseFile model.partCache
          }
        , Cmd.none
        )

    else
        let
            newCache =
                Resolve.updateCache name result Parser.parseFile model.partCache

            newLines =
                case result of
                    Ok text ->
                        Parser.parseFile text

                    Err _ ->
                        []

            additionalPending =
                Resolve.pendingParts newLines newCache

            allPending =
                Resolve.pendingParts model.topLevelLines newCache
                    ++ additionalPending
                    |> List.filter (\n -> not (Dict.member n newCache))
                    |> deduplicate

            newLoaded =
                model.partsLoaded + 1

            newTotal =
                max model.partsTotal (newLoaded + List.length allPending)

            cacheWithLoading =
                Resolve.markLoading allPending newCache

            updatedModel =
                { model
                    | partCache = cacheWithLoading
                    , partsLoaded = newLoaded
                    , partsTotal = newTotal
                    , loadingProgressPct = partsProgressPct newLoaded newTotal
                }
        in
        if List.isEmpty allPending && not (hasLoadingParts updatedModel.partCache) then
            finishLoading updatedModel

        else if List.isEmpty allPending then
            ( updatedModel, Cmd.none )

        else
            ( updatedModel, fetchPending model.resolverConfig allPending )


{-| Reset model fields that depend on the loaded file, keeping the part cache.
-}
resetForLoad : String -> Model -> Model
resetForLoad url m =
    { m
        | loadPhase = FetchingTopLevel url
        , scene = Nothing
        , instancedScene = Nothing
        , modelBounds = Nothing
        , submodelNames = Set.empty
        , errorMsg = Nothing
        , topLevelLines = []
        , partsLoaded = 0
        , partsTotal = 0
        , gearGraph = Nothing
        , gearMeshes = Dict.empty
        , gearLods = Dict.empty
        , components = []
        , componentMeshes = []
        , componentRenders = []
        , motor = Animate.defaultMotor
        , playback =
            { running = False
            , currentTime = 0.0
            , motorGearId = Nothing
            , motorSpeedRadPerSec = 1.0
            }
        , gearAngles = Dict.empty
        , clickStart = Nothing
        , dragTravel = 0.0
        , lastFrameTime = Nothing
        , cameraMode = CameraAutoFit
        , touchGesture = NoTouchGesture
        , activeTouches = Dict.empty
        , mousePanDragging = False
        , simulationChecked = False
        , simulationAvailable = False
        , loadingProgressPct = 0.0
        , loadingProgressTick = Nothing
        , heldControl = NoHeldControl
        , heldControlTick = Nothing
    }


beginTouchGesture : List TouchPoint -> Model -> Model
beginTouchGesture touches model =
    case touches of
        p1 :: p2 :: _ ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture (beginPinch p1 p2)
                , cameraMode = CameraManual
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        p1 :: [] ->
            { model
                | camera = Camera.onMouseDown p1.x p1.y model.camera
                , touchGesture = SingleTouchGesture p1.id
                , cameraMode = CameraManual
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        [] ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = NoTouchGesture
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }


advanceTouchGesture : List TouchPoint -> Model -> Model
advanceTouchGesture touches model =
    case model.touchGesture of
        SingleTouchGesture touchId ->
            case findTouch touchId touches of
                Just point ->
                    { model
                        | camera = Camera.onMouseMove point.x point.y model.camera
                        , cameraMode = CameraManual
                    }

                Nothing ->
                    beginTouchGesture touches { model | camera = Camera.onMouseUp model.camera }

        PinchGesture pinch ->
            case ( findTouch pinch.idA touches, findTouch pinch.idB touches ) of
                ( Just a, Just b ) ->
                    let
                        newDistance =
                            touchDistance a b

                        ( midX, midY ) =
                            touchMidpoint a b

                        zoomEngaged =
                            pinch.zoomEngaged
                                || abs (newDistance - pinch.startDistance)
                                > pinchZoomDeadZone

                        -- Pan first, so the pixels-to-world scale is taken at a
                        -- single distance for the whole frame.
                        cameraAfterPan =
                            Camera.onPan (midX - pinch.midX) (midY - pinch.midY) model.camera

                        cameraAfterZoom =
                            if zoomEngaged && newDistance > 0 && pinch.distance > 0 then
                                applyPinchZoom (pinch.distance / newDistance) cameraAfterPan

                            else
                                cameraAfterPan
                    in
                    { model
                        | camera = cameraAfterZoom
                        , touchGesture =
                            PinchGesture
                                { pinch
                                    | distance = newDistance
                                    , midX = midX
                                    , midY = midY
                                    , zoomEngaged = zoomEngaged
                                }
                        , cameraMode = CameraManual
                    }

                _ ->
                    endTouchGesture touches model

        NoTouchGesture ->
            beginTouchGesture touches model


endTouchGesture : List TouchPoint -> Model -> Model
endTouchGesture remainingTouches model =
    case remainingTouches of
        [] ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = NoTouchGesture
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        [ p1 ] ->
            { model
                | camera = Camera.onMouseDown p1.x p1.y (Camera.onMouseUp model.camera)
                , touchGesture = SingleTouchGesture p1.id
                , cameraMode = CameraManual
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        p1 :: p2 :: _ ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture (beginPinch p1 p2)
                , cameraMode = CameraManual
                , clickStart = Nothing
                , dragTravel = 0.0
                , mousePanDragging = False
            }


findTouch : Int -> List TouchPoint -> Maybe TouchPoint
findTouch touchId touches =
    touches |> List.filter (\p -> p.id == touchId) |> List.head


touchDictFromList : List TouchPoint -> Dict Int TouchPoint
touchDictFromList touches =
    touches
        |> List.foldl (\touch acc -> Dict.insert touch.id touch acc) Dict.empty


touchesFromDict : Dict Int TouchPoint -> List TouchPoint
touchesFromDict touches =
    touches
        |> Dict.values
        |> List.sortBy .id


touchInputActive : Model -> Bool
touchInputActive model =
    not (Dict.isEmpty model.activeTouches)
        || (case model.touchGesture of
                NoTouchGesture ->
                    False

                _ ->
                    True
           )


resolveMotorGearId : Model -> Maybe GearId
resolveMotorGearId model =
    case model.motor.drivingGearId of
        Just gearId ->
            Just gearId

        Nothing ->
            case model.playback.motorGearId of
                Just gearId ->
                    Just gearId

                Nothing ->
                    case model.gearGraph of
                        Just graph ->
                            graph.instances
                                |> Array.get 0
                                |> Maybe.map .id

                        Nothing ->
                            Nothing


touchDistance : TouchPoint -> TouchPoint -> Float
touchDistance p1 p2 =
    let
        dx =
            p2.x - p1.x

        dy =
            p2.y - p1.y
    in
    sqrt ((dx * dx) + (dy * dy))


touchMidpoint : TouchPoint -> TouchPoint -> ( Float, Float )
touchMidpoint p1 p2 =
    ( (p1.x + p2.x) / 2
    , (p1.y + p2.y) / 2
    )


{-| Start a two-finger gesture as pure pan; zoom engages once the fingers move
past `pinchZoomDeadZone`.
-}
beginPinch : TouchPoint -> TouchPoint -> PinchState
beginPinch p1 p2 =
    let
        distance =
            touchDistance p1 p2

        ( midX, midY ) =
            touchMidpoint p1 p2
    in
    { idA = p1.id
    , idB = p2.id
    , distance = distance
    , midX = midX
    , midY = midY
    , startDistance = distance
    , zoomEngaged = False
    }


{-| A model's extents, kept so the view can be re-framed on demand without
re-flattening the geometry.

`minCorner`/`maxCorner` are the axis-aligned box; `groundLevel` reads the floor
off it for street-level positioning. `center`/`radius` are the bounding sphere
used for framing and for the far plane.

-}
type alias ModelBounds =
    { minCorner : Vec3.Vec3
    , maxCorner : Vec3.Vec3
    , center : Vec3.Vec3
    , radius : Float
    }


{-| The model's lowest point — where the ground is, in render space.
-}
groundLevel : ModelBounds -> Float
groundLevel bounds =
    Vec3.getY bounds.minCorner


{-| Wheel zoom, interpreted for the active navigation mode.

Orbiting pulls the eye towards the pivot. Walking has no pivot to pull towards —
shrinking the orbit distance would slide you out of the street rather than along
it — so the wheel moves you forwards and backwards instead.

-}
applyWheel : Float -> Camera -> Camera
applyWheel delta camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.onWheel delta camera

        Camera.Walk ->
            Camera.dollyBy (-delta * 0.01 * Camera.walkSpeed camera * walkWheelSeconds) camera


{-| Pinch zoom, interpreted for the active navigation mode.

`ratio` is the same figure `Camera.zoomByRatio` takes: below 1 means zoom in.

-}
applyPinchZoom : Float -> Camera -> Camera
applyPinchZoom ratio camera =
    case camera.navigation of
        Camera.Orbit ->
            Camera.zoomByRatio ratio camera

        Camera.Walk ->
            -- A ratio is scale-free, so turn it into a stride: pinching out by a
            -- factor of two walks one second forwards.
            Camera.dollyBy ((1 - ratio) * Camera.walkSpeed camera * walkWheelSeconds) camera


{-| How many seconds of walking one full wheel notch is worth.

A whole second of travel per notch overshoots badly at street level — on a
city-scale model that is fourteen studs from one flick of the finger.

-}
walkWheelSeconds : Float
walkWheelSeconds =
    0.25


{-| Bounding sphere of the baked geometry path.
-}
modelBoundsFromLines : List LDrawLine -> PartCache -> Maybe ModelBounds
modelBoundsFromLines lines cache =
    let
        geom =
            Geometry.flatten lines cache 15 Mat4.identity

        points =
            (geom.triangles
                |> List.concatMap (\( a, b, c ) -> [ a.position, b.position, c.position ])
            )
                ++ (geom.lines |> List.concatMap (\( p1, p2 ) -> [ p1, p2 ]))
                ++ (geom.conditionalLines |> List.concatMap (\e -> [ e.p1, e.p2, e.c1, e.c2 ]))
    in
    case points of
        [] ->
            Nothing

        p0 :: rest ->
            let
                bounds =
                    List.foldl
                        (\p acc ->
                            { minX = min acc.minX (Vec3.getX p)
                            , maxX = max acc.maxX (Vec3.getX p)
                            , minY = min acc.minY (Vec3.getY p)
                            , maxY = max acc.maxY (Vec3.getY p)
                            , minZ = min acc.minZ (Vec3.getZ p)
                            , maxZ = max acc.maxZ (Vec3.getZ p)
                            }
                        )
                        { minX = Vec3.getX p0
                        , maxX = Vec3.getX p0
                        , minY = Vec3.getY p0
                        , maxY = Vec3.getY p0
                        , minZ = Vec3.getZ p0
                        , maxZ = Vec3.getZ p0
                        }
                        rest

                center =
                    Vec3.vec3
                        ((bounds.minX + bounds.maxX) / 2)
                        ((bounds.minY + bounds.maxY) / 2)
                        ((bounds.minZ + bounds.maxZ) / 2)

                extentX =
                    bounds.maxX - bounds.minX

                extentY =
                    bounds.maxY - bounds.minY

                extentZ =
                    bounds.maxZ - bounds.minZ

                radius =
                    max 1 (sqrt (extentX * extentX + extentY * extentY + extentZ * extentZ) / 2)
            in
            Just
                { minCorner = Vec3.vec3 bounds.minX bounds.minY bounds.minZ
                , maxCorner = Vec3.vec3 bounds.maxX bounds.maxY bounds.maxZ
                , center = center
                , radius = radius
                }


{-| Bounding sphere of the instanced path, from the placement spheres that
`Instanced.build` has already measured.
-}
modelBoundsFromInstanced : InstancedScene -> ModelBounds
modelBoundsFromInstanced instanced =
    { minCorner = instanced.boundsMin
    , maxCorner = instanced.boundsMax
    , center = Vec3.scale 0.5 (Vec3.add instanced.boundsMin instanced.boundsMax)
    , radius = max 1 (0.5 * Vec3.length (Vec3.sub instanced.boundsMax instanced.boundsMin))
    }


{-| Re-frame the stored model bounds, as the fit-view control does.

Without this the camera is a one-way door: the first drag switches to
`CameraManual` and auto-fit never runs again, so a user who loses a large model
off-screen has to reload the page to get it back.

-}
frameModelBounds : Model -> Model
frameModelBounds model =
    case model.modelBounds of
        Just bounds ->
            { model
                | camera =
                    cameraForBounds model.width model.height bounds.center bounds.radius model.camera
                        -- Framing the whole model is an outside-in view; walking
                        -- with a 7500 LDU pivot in front of you is not.
                        |> Camera.setNavigation Camera.Orbit
                , cameraMode = CameraAutoFit
            }

        Nothing ->
            model


{-| Drop to street level, or step back out to orbit.

Needs the model bounds to know where the ground is, so it is a no-op before a
model has loaded.

-}
toggleWalkMode : Model -> Model
toggleWalkMode model =
    case ( model.camera.navigation, model.modelBounds ) of
        ( Camera.Orbit, Just bounds ) ->
            { model
                | camera = Camera.enterWalk (groundLevel bounds) model.camera
                , cameraMode = CameraManual
            }

        ( Camera.Walk, _ ) ->
            -- Leaving walk mode keeps the viewpoint; only the drag pivot changes.
            { model
                | camera = Camera.setNavigation Camera.Orbit model.camera
                , cameraMode = CameraManual
            }

        ( Camera.Orbit, Nothing ) ->
            model


{-| Frame the model from a canonical direction.

Reuses `cameraForBounds` for the target and distance — those come from the
bounding _sphere_ and so are independent of the viewing angle — then overrides
the angles.

-}
applyViewPreset : ViewPreset -> Model -> Model
applyViewPreset preset model =
    case model.modelBounds of
        Just bounds ->
            let
                ( azimuth, elevation ) =
                    viewPresetAngles preset

                framed =
                    cameraForBounds model.width model.height bounds.center bounds.radius model.camera
            in
            { model
                | camera =
                    { framed | azimuth = azimuth, elevation = elevation }
                        |> Camera.setNavigation Camera.Orbit
                , cameraMode = CameraManual
            }

        Nothing ->
            model


{-| Frame a bounding sphere: orbit far enough back that it fits the narrower of
the two field-of-view angles, from the default three-quarter viewpoint.

Split out of `autoFitCamera` so the instanced path can reuse it with bounds
derived from placement spheres, instead of flattening the whole model just to
measure it.

-}
cameraForBounds : Int -> Int -> Vec3.Vec3 -> Float -> Camera -> Camera
cameraForBounds width height center radius currentCamera =
    { currentCamera
        | target = center
        , distance = framedDistance width height radius
        , azimuth = 0.75
        , elevation = 0.6
    }
        |> applyZoomRange width height radius


{-| Orbit distance at which a bounding sphere of `radius` fills the viewport.
-}
framedDistance : Int -> Int -> Float -> Float
framedDistance width height radius =
    let
        aspect =
            toFloat width / max 1 (toFloat height)

        fovY =
            degrees Camera.fovYDegrees

        fovX =
            2 * atan (tan (fovY / 2) * aspect)

        limitingHalfFov =
            max 0.15 (min (fovX / 2) (fovY / 2))
    in
    clamp 8 maxAutoFitDistance (radius / sin limitingHalfFov * 1.25)


{-| Size the interactive zoom range to the model, and record the canvas height
and model radius.

Applied whether or not the camera is being re-framed, because all three are
properties of the model rather than of the current view: a shared `#d=` link
restores a manual camera, and it still needs a range that reaches it. A fixed
ceiling used to snap a city-scale model from its framed distance down to the
limit on the very first wheel tick, with no way back out.

The scene radius is what keeps the far plane honest once the camera is _inside_
the model — see `Render.Camera.farPlane`. This is the only place it is set, so
walk mode inherits it for free.

-}
applyZoomRange : Int -> Int -> Float -> Camera -> Camera
applyZoomRange width height radius currentCamera =
    let
        framed =
            framedDistance width height radius
    in
    currentCamera
        |> Camera.setViewportHeight (toFloat height)
        |> Camera.setSceneRadius radius
        |> Camera.setZoomRange 0.5 (max 2000 (framed * zoomOutHeadroom))


{-| Upper bound on the auto-fit orbit distance, in LDU.

A city-scale model needs to be viewed from several thousand LDU out; the clip
planes now scale with orbit distance (`Render.Camera.farPlane`), so there is no
longer a fixed far plane for this to stay under.

-}
maxAutoFitDistance : Float
maxAutoFitDistance =
    200000


{-| How far past its framed distance a model can be zoomed out, as a multiple.

Enough to pull well back from a city-scale layout and see it in context, while
still stopping short of the point where the model is a speck.

-}
zoomOutHeadroom : Float
zoomOutHeadroom =
    8


{-| Shared handler for HTTP-fetched and file-uploaded LDraw text.
-}
handleTopLevelText : String -> Model -> ( Model, Cmd Msg )
handleTopLevelText text model =
    let
        isMpd =
            String.contains "0 FILE " text

        -- The names of the model's own embedded sub-models, as opposed to the
        -- library parts fetched over HTTP. `Render.Instanced` recurses through
        -- these and stops at everything else, so one placement comes out per
        -- physical brick.
        submodelNames =
            if isMpd then
                Parser.splitMpd text |> Dict.keys |> Set.fromList

            else
                Set.empty

        ( seededCache, lines ) =
            if isMpd then
                let
                    ( newCache, mainFile ) =
                        Resolve.seedFromMpd text model.partCache

                    mainLines =
                        case mainFile of
                            Just name ->
                                case Dict.get name newCache of
                                    Just (Loaded ls) ->
                                        ls

                                    _ ->
                                        Parser.parseFile text

                            Nothing ->
                                Parser.parseFile text
                in
                ( newCache, mainLines )

            else
                ( model.partCache, Parser.parseFile text )

        -- Also scan embedded submodels (seeded from MPD) for their own dependencies,
        -- since those lines are never passed through handlePartResult's additionalPending.
        seededSubmodelLines =
            seededCache
                |> Dict.values
                |> List.filterMap
                    (\status ->
                        case status of
                            Loaded ls ->
                                Just ls

                            _ ->
                                Nothing
                    )
                |> List.concat

        pending =
            Resolve.pendingParts (lines ++ seededSubmodelLines) seededCache

        cacheWithLoading =
            Resolve.markLoading pending seededCache

        loadingModel =
            { model
                | topLevelLines = lines
                , partCache = cacheWithLoading
                , submodelNames = submodelNames
                , loadPhase = ResolvingParts
                , partsTotal = List.length pending
                , partsLoaded = 0
                , scene = Nothing
                , instancedScene = Nothing
                , modelBounds = Nothing
                , errorMsg = Nothing
                , gearGraph = Nothing
                , gearMeshes = Dict.empty
                , gearLods = Dict.empty
                , components = []
                , componentMeshes = []
                , componentRenders = []
                , motor = Animate.defaultMotor
                , playback =
                    { running = False
                    , currentTime = 0.0
                    , motorGearId = Nothing
                    , motorSpeedRadPerSec = 1.0
                    }
                , gearAngles = Dict.empty
                , clickStart = Nothing
                , dragTravel = 0.0
                , lastFrameTime = Nothing
                , simulationChecked = False
                , simulationAvailable = False
                , loadingProgressPct = partsProgressPct 0 (List.length pending)
                , loadingProgressTick = Nothing
                , heldControl = NoHeldControl
                , heldControlTick = Nothing
            }
    in
    if List.isEmpty pending then
        finishLoading loadingModel

    else
        ( loadingModel, fetchPending model.resolverConfig pending )


{-| Called when all parts have been resolved. Build the scene and gear graph.

Model size is measured first, because every step below — gear detection, the
auto-fit flatten, and the geometry worker — walks the fully expanded part tree
and none of them are viable at city scale. Above `Instanced` threshold the model
takes the instanced path instead; see `finishLoadingInstanced`.

-}
finishLoading : Model -> ( Model, Cmd Msg )
finishLoading model =
    let
        placements =
            Instanced.extractPlacements model.submodelNames model.topLevelLines model.partCache rootColorCode
    in
    if Instanced.triangleBudgetExceeded (List.length placements) then
        finishLoadingInstanced placements model

    else
        finishLoadingBaked model


{-| The root inherited colour: LDraw code 15 (white), matching the value passed
to `Geometry.flatten` everywhere else.
-}
rootColorCode : Int
rootColorCode =
    15


{-| Large-model path: build per-part meshes and per-placement transforms.

Gear detection is deliberately skipped. `Detect.extractGears` recurses through
every sub-file down to the primitives, which on a model this size is millions of
nodes for a walk that is almost certainly going to come back empty — a model of
this scale is a display piece, not a mechanism. The runtime event says so
explicitly rather than leaving the missing simulation unexplained.

-}
finishLoadingInstanced : List Instanced.RawPlacement -> Model -> ( Model, Cmd Msg )
finishLoadingInstanced placements model =
    let
        instanced =
            Instanced.build model.partCache placements

        bounds =
            modelBoundsFromInstanced instanced

        nextCamera =
            case model.cameraMode of
                CameraAutoFit ->
                    cameraForBounds model.width model.height bounds.center bounds.radius model.camera

                CameraManual ->
                    applyZoomRange model.width model.height bounds.radius model.camera

        updatedModel =
            { model
                | camera = nextCamera
                , scene = Nothing
                , instancedScene = Just instanced
                , modelBounds = Just bounds
                , loadPhase = Ready
                , gearGraph = Nothing
                , gearMeshes = Dict.empty
                , gearLods = Dict.empty
                , components = []
                , componentMeshes = []
                , componentRenders = []
                , gearAngles = Dict.empty
                , simulationChecked = True
                , simulationAvailable = False
                , loadingProgressPct = 100
                , loadingProgressTick = Nothing
            }
    in
    ( updatedModel
    , Cmd.batch
        [ runtimeEventCmd "simulation-unavailable"
            [ ( "reason", Encode.string "Model too large for gear detection" )
            , ( "partCount", Encode.int (List.length placements) )
            ]
        , modelLoadedEventCmd updatedModel
        , cameraChangedIfNeededCmd model.camera nextCamera
        ]
    )


{-| Original path: detect gears, then bake world-space geometry in the worker.
-}
finishLoadingBaked : Model -> ( Model, Cmd Msg )
finishLoadingBaked model =
    let
        bounds =
            modelBoundsFromLines model.topLevelLines model.partCache

        nextCamera =
            case ( model.cameraMode, bounds ) of
                ( CameraAutoFit, Just b ) ->
                    cameraForBounds model.width model.height b.center b.radius model.camera

                ( CameraManual, Just b ) ->
                    applyZoomRange model.width model.height b.radius model.camera

                _ ->
                    model.camera

        gearInstances =
            Detect.extractGears Data.gearParts model.topLevelLines model.partCache

        components =
            topLevelComponents model.topLevelLines

        drivenAxles =
            buildDrivenAxles model.partCache components gearInstances

        coaxialGearByLine =
            List.map (\line -> topLevelCoaxialGear model.partCache drivenAxles line gearInstances) model.topLevelLines

        nonGearTopLevelLines =
            List.map2 Tuple.pair model.topLevelLines coaxialGearByLine
                |> List.filter
                    (\( line, coaxial ) ->
                        not (isTopLevelGearRef line)
                            && not (isTopLevelAxleRef line)
                            && coaxial
                            == Nothing
                    )
                |> List.map Tuple.first

        coaxialPartData =
            List.map2 Tuple.pair model.topLevelLines coaxialGearByLine
                |> List.filterMap
                    (\( line, coaxial ) ->
                        case ( line, coaxial ) of
                            ( SubFileRef ref, Just gearId ) ->
                                if not (isTopLevelGearRef line) && not (isTopLevelAxleRef line) then
                                    Just
                                        { file = ref.file
                                        , color = resolveRootColor ref.color
                                        , transform = ref.transform
                                        , drivingGearId = gearId
                                        }

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        gearMeshes =
            buildGearMeshes model.partCache gearInstances

        graph =
            Detect.buildGearGraph gearInstances

        firstGearId =
            List.head gearInstances |> Maybe.map .id

        selectedMotorGearId =
            case model.requestedMotorIndex of
                Just idx ->
                    case gearInstances |> List.drop idx |> List.head |> Maybe.map .id of
                        Just gid ->
                            Just gid

                        Nothing ->
                            firstGearId

                Nothing ->
                    firstGearId

        playbackState =
            model.playback

        baseMotor =
            model.motor

        motor =
            { baseMotor
                | drivingGearId = selectedMotorGearId
                , running = False
            }

        startTime =
            max 0 model.playback.currentTime

        initAngles =
            Animate.gearAngles motor graph startTime

        componentRenders =
            buildComponentRenders model.partCache components gearInstances

        componentMeshes =
            buildComponentMeshRenders model.partCache components gearInstances
                ++ buildCoaxialMeshRenders model.partCache gearInstances coaxialPartData

        simulationAvailable =
            not (List.isEmpty gearInstances)

        simulationEvent =
            if simulationAvailable then
                runtimeEventCmd "simulation-ready"
                    [ ( "gearCount", Encode.int (List.length gearInstances) ) ]

            else
                runtimeEventCmd "simulation-unavailable"
                    [ ( "reason", Encode.string "No supported gear train detected" ) ]
    in
    ( { model
        | camera = nextCamera
        , modelBounds = bounds
        , scene = Nothing
        , loadPhase = FlatteningGeometry
        , gearGraph = Just graph
        , gearMeshes = gearMeshes
        , components = components
        , componentMeshes = componentMeshes
        , componentRenders = componentRenders
        , motor = motor
        , playback =
            { playbackState
                | running = False
                , currentTime = startTime
                , motorGearId = selectedMotorGearId
                , motorSpeedRadPerSec = motor.speedRadPerSec
            }
        , gearAngles = initAngles
        , simulationChecked = True
        , simulationAvailable = simulationAvailable
        , loadingProgressPct = 90
        , loadingProgressTick = Nothing
      }
    , Cmd.batch
        [ requestGeometryFlatten nonGearTopLevelLines model.partCache
        , simulationEvent
        , cameraChangedIfNeededCmd model.camera nextCamera
        ]
    )


fetchPending : ResolverConfig -> List String -> Cmd Msg
fetchPending resolver names =
    names
        |> List.map (\name -> Resolve.fetchPart resolver name PartLoaded)
        |> Cmd.batch


requestGeometryFlatten : List LDrawLine -> PartCache -> Cmd Msg
requestGeometryFlatten lines cache =
    let
        payload =
            Encode.object
                [ ( "lines", Encode.list encodeLDrawLine lines )
                , ( "cache", encodeLoadedCache cache )
                , ( "parentColor", Encode.int 15 )
                , ( "colorTable", encodeColorTable Data.ldrawColors )
                ]
                |> Encode.encode 0
    in
    Ports.requestGeometryFlatten payload


buildSceneFallback : Model -> Model
buildSceneFallback model =
    let
        staticLines =
            List.filter (not << isTopLevelGearRef) model.topLevelLines

        geom =
            Geometry.flatten staticLines model.partCache 15 Mat4.identity

        scene =
            Scene.buildScene
                { triangles = geom.triangles
                , lines = geom.lines
                , conditionalLines = geom.conditionalLines
                }
                geom.bfcCertified
    in
    { model
        | scene = Just scene
        , loadPhase = Ready
        , loadingProgressPct = 100
        , loadingProgressTick = Nothing
    }


deduplicate : List String -> List String
deduplicate =
    List.foldl
        (\x ( seen, acc ) ->
            if List.member x seen then
                ( seen, acc )

            else
                ( x :: seen, x :: acc )
        )
        ( [], [] )
        >> Tuple.second
        >> List.reverse


hasLoadingParts : PartCache -> Bool
hasLoadingParts cache =
    cache
        |> Dict.values
        |> List.any
            (\status ->
                case status of
                    Loading ->
                        True

                    _ ->
                        False
            )


isTopLevelGearRef : LDrawLine -> Bool
isTopLevelGearRef line =
    case line of
        SubFileRef ref ->
            List.any (\spec -> spec.partFile == ref.file) Data.gearParts

        _ ->
            False


isTopLevelAxleRef : LDrawLine -> Bool
isTopLevelAxleRef line =
    case line of
        SubFileRef ref ->
            List.any (\spec -> spec.partFile == ref.file && spec.kind == Components.AxleLike) Components.defaultSpecs

        _ ->
            False


topLevelComponents : List LDrawLine -> List Components.ComponentInstance
topLevelComponents lines =
    lines
        |> List.filterMap
            (\line ->
                case line of
                    SubFileRef ref ->
                        case List.filter (\spec -> spec.partFile == ref.file) Components.defaultSpecs |> List.head of
                            Just spec ->
                                let
                                    origin =
                                        Mat4.transform ref.transform (Vec3.vec3 0 0 0)

                                    axisEnd =
                                        Mat4.transform ref.transform (Vec3.vec3 0 0 1)

                                    rawAxis =
                                        Vec3.sub axisEnd origin

                                    axisLen =
                                        Vec3.length rawAxis

                                    axis =
                                        if axisLen < 1.0e-6 then
                                            Vec3.vec3 0 0 1

                                        else
                                            Vec3.scale (1 / axisLen) rawAxis
                                in
                                Just
                                    { kind = spec.kind
                                    , partFile = spec.partFile
                                    , color = resolveRootColor ref.color
                                    , worldPosition = origin
                                    , worldAxis = axis
                                    , worldMatrix = ref.transform
                                    }

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing
            )


resolveRootColor : Int -> Int
resolveRootColor colorCode =
    if colorCode == 16 || colorCode == -1 then
        15

    else
        colorCode


encodeLDrawLine : LDrawLine -> Encode.Value
encodeLDrawLine line =
    case line of
        Comment textValue ->
            Encode.object
                [ ( "k", Encode.string "comment" )
                , ( "text", Encode.string textValue )
                ]

        SubFileRef ref ->
            Encode.object
                [ ( "k", Encode.string "subfile" )
                , ( "file", Encode.string ref.file )
                , ( "color", Encode.int ref.color )
                , ( "transform", encodeAffine ref.transform )
                ]

        Triangle tri ->
            Encode.object
                [ ( "k", Encode.string "tri" )
                , ( "color", Encode.int tri.color )
                , ( "p1", encodeVec3 tri.p1 )
                , ( "p2", encodeVec3 tri.p2 )
                , ( "p3", encodeVec3 tri.p3 )
                ]

        Quad quad ->
            Encode.object
                [ ( "k", Encode.string "quad" )
                , ( "color", Encode.int quad.color )
                , ( "p1", encodeVec3 quad.p1 )
                , ( "p2", encodeVec3 quad.p2 )
                , ( "p3", encodeVec3 quad.p3 )
                , ( "p4", encodeVec3 quad.p4 )
                ]

        LineSegment seg ->
            Encode.object
                [ ( "k", Encode.string "line" )
                , ( "color", Encode.int seg.color )
                , ( "p1", encodeVec3 seg.p1 )
                , ( "p2", encodeVec3 seg.p2 )
                ]

        ConditionalLine cond ->
            Encode.object
                [ ( "k", Encode.string "cond" )
                , ( "color", Encode.int cond.color )
                , ( "p1", encodeVec3 cond.p1 )
                , ( "p2", encodeVec3 cond.p2 )
                , ( "c1", encodeVec3 cond.c1 )
                , ( "c2", encodeVec3 cond.c2 )
                ]


encodeAffine : Mat4.Mat4 -> Encode.Value
encodeAffine mat =
    let
        origin =
            Mat4.transform mat (Vec3.vec3 0 0 0)

        xAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 1 0 0)) origin

        yAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 0 1 0)) origin

        zAxis =
            Vec3.sub (Mat4.transform mat (Vec3.vec3 0 0 1)) origin
    in
    Encode.object
        [ ( "o", encodeVec3 origin )
        , ( "x", encodeVec3 xAxis )
        , ( "y", encodeVec3 yAxis )
        , ( "z", encodeVec3 zAxis )
        ]


encodeLoadedCache : PartCache -> Encode.Value
encodeLoadedCache cache =
    cache
        |> Dict.toList
        |> List.filterMap
            (\( name, status ) ->
                case status of
                    Loaded lines ->
                        Just ( name, Encode.list encodeLDrawLine lines )

                    _ ->
                        Nothing
            )
        |> Encode.object


encodeColorTable : Dict Int { r : Float, g : Float, b : Float, alpha : Float } -> Encode.Value
encodeColorTable table =
    table
        |> Dict.toList
        |> List.map
            (\( code, c ) ->
                ( String.fromInt code
                , Encode.object
                    [ ( "r", Encode.float c.r )
                    , ( "g", Encode.float c.g )
                    , ( "b", Encode.float c.b )
                    , ( "alpha", Encode.float c.alpha )
                    ]
                )
            )
        |> Encode.object


encodeVec3 : Vec3.Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]


flattenResultDecoder :
    Decode.Decoder
        { triangles : List ( Vertex, Vertex, Vertex )
        , lines : List ( Vec3.Vec3, Vec3.Vec3 )
        , conditionalLines : List Geometry.ConditionalEdge
        , bfcCertified : Bool
        }
flattenResultDecoder =
    Decode.map4
        (\tris lines conds bfc ->
            { triangles = tris
            , lines = lines
            , conditionalLines = conds
            , bfcCertified = bfc
            }
        )
        (Decode.field "triangles" (Decode.list triangleDecoder))
        (Decode.field "lines" (Decode.list linePairDecoder))
        (Decode.field "conditionalLines" (Decode.list conditionalEdgeDecoder))
        (Decode.field "bfcCertified" Decode.bool)


triangleDecoder : Decode.Decoder ( Vertex, Vertex, Vertex )
triangleDecoder =
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.index 0 vertexDecoder)
        (Decode.index 1 vertexDecoder)
        (Decode.index 2 vertexDecoder)


vertexDecoder : Decode.Decoder Vertex
vertexDecoder =
    -- The worker bakes world-space geometry with colours already resolved, so
    -- nothing it produces inherits from a placement.
    Decode.map3
        (\p n c -> { position = p, normal = n, color = c, inherit = 0 })
        (Decode.field "position" vec3Decoder)
        (Decode.field "normal" vec3Decoder)
        (Decode.field "color" vec4Decoder)


linePairDecoder : Decode.Decoder ( Vec3.Vec3, Vec3.Vec3 )
linePairDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 vec3Decoder)
        (Decode.index 1 vec3Decoder)


conditionalEdgeDecoder : Decode.Decoder Geometry.ConditionalEdge
conditionalEdgeDecoder =
    Decode.map4
        (\p1 p2 c1 c2 -> { p1 = p1, p2 = p2, c1 = c1, c2 = c2 })
        (Decode.field "p1" vec3Decoder)
        (Decode.field "p2" vec3Decoder)
        (Decode.field "c1" vec3Decoder)
        (Decode.field "c2" vec3Decoder)


vec3Decoder : Decode.Decoder Vec3.Vec3
vec3Decoder =
    Decode.map3 Vec3.vec3
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)


vec4Decoder : Decode.Decoder Vec4.Vec4
vec4Decoder =
    Decode.map4 Vec4.vec4
        (Decode.index 0 Decode.float)
        (Decode.index 1 Decode.float)
        (Decode.index 2 Decode.float)
        (Decode.index 3 Decode.float)


buildGearMeshes : PartCache -> List GearInstance -> Dict GearId GearRender
buildGearMeshes cache instances =
    instances
        |> List.foldl
            (\inst acc ->
                let
                    file =
                        inst.spec.partFile
                in
                case Dict.get file cache of
                    Just (Loaded lines) ->
                        let
                            geom =
                                Geometry.flatten lines cache inst.color inst.worldMatrix

                            -- Low-detail mesh from the decimated part text, falling
                            -- back to the full geometry when no LOD variant exists.
                            -- Sub-file refs are still resolved against the full cache.
                            lodTriangles =
                                case Dict.get file embeddedLodCache of
                                    Just (Loaded lodLines) ->
                                        (Geometry.flatten lodLines cache inst.color inst.worldMatrix).triangles

                                    _ ->
                                        geom.triangles

                            center =
                                toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0))

                            axisEnd =
                                toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 1))

                            axisRaw =
                                Vec3.sub axisEnd center

                            axisLen =
                                Vec3.length axisRaw

                            axis =
                                if axisLen < 1.0e-6 then
                                    Vec3.vec3 0 1 0

                                else
                                    canonicalizeAxis (Vec3.scale (1 / axisLen) axisRaw)
                        in
                        Dict.insert
                            inst.id
                            { mesh = WebGL.triangles geom.triangles
                            , lodMesh = WebGL.triangles lodTriangles
                            , center = center
                            , axis = axis
                            , pitchRadius = inst.spec.pitchRadius
                            }
                            acc

                    _ ->
                        acc
            )
            Dict.empty


toYUpPoint : Vec3.Vec3 -> Vec3.Vec3
toYUpPoint p =
    Vec3.vec3 (Vec3.getX p) -(Vec3.getY p) -(Vec3.getZ p)


toYUpAxis : Vec3.Vec3 -> Vec3.Vec3
toYUpAxis axis =
    let
        raw =
            Vec3.vec3 (Vec3.getX axis) -(Vec3.getY axis) -(Vec3.getZ axis)

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        Vec3.vec3 0 1 0

    else
        canonicalizeAxis (Vec3.scale (1 / len) raw)


canonicalizeAxis : Vec3.Vec3 -> Vec3.Vec3
canonicalizeAxis axis =
    let
        ax =
            abs (Vec3.getX axis)

        ay =
            abs (Vec3.getY axis)

        az =
            abs (Vec3.getZ axis)

        signToKeep =
            if ax >= ay && ax >= az then
                Vec3.getX axis

            else if ay >= az then
                Vec3.getY axis

            else
                Vec3.getZ axis
    in
    if signToKeep < 0 then
        Vec3.scale -1 axis

    else
        axis


rotationAround : Float -> Vec3.Vec3 -> Vec3.Vec3 -> Mat4.Mat4
rotationAround angle axis pivot =
    let
        toOrigin =
            Mat4.makeTranslate (Vec3.scale -1 pivot)

        rotate =
            Mat4.makeRotate angle axis

        back =
            Mat4.makeTranslate pivot
    in
    Mat4.mul back (Mat4.mul rotate toOrigin)


renderGearEntities : Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
renderGearEntities camera styleInput aspect model =
    case model.gearGraph of
        Nothing ->
            []

        Just graph ->
            let
                style =
                    Style.clampStyle styleInput

                viewMat =
                    Camera.viewMatrix camera

                projMat =
                    Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

                viewPos =
                    Camera.position camera
            in
            graph.instances
                |> Array.toList
                |> List.filterMap
                    (\inst ->
                        case Dict.get inst.id model.gearMeshes of
                            Nothing ->
                                Nothing

                            Just rendered ->
                                if isSphereVisible camera aspect rendered.center inst.spec.pitchRadius then
                                    let
                                        angle =
                                            Dict.get inst.id model.gearAngles
                                                |> Maybe.withDefault 0.0

                                        modelMat =
                                            rotationAround angle rendered.axis rendered.center

                                        meshForLod =
                                            case Dict.get inst.id model.gearLods of
                                                Just Lod.Simplified ->
                                                    rendered.lodMesh

                                                _ ->
                                                    rendered.mesh

                                        uniforms =
                                            { modelMatrix = modelMat
                                            , viewMatrix = viewMat
                                            , projectionMatrix = projMat
                                            , viewPosition = viewPos
                                            , lightDirection = style.lightDirection
                                            , instanceColor = Shader.noInstanceColor
                                            , ambientStrength = style.ambientStrength
                                            , lightStrength = style.lightStrength
                                            , specularStrength = style.specularStrength
                                            , specularPower = style.specularPower
                                            , rimStrength = style.rimStrength
                                            , rimPower = style.rimPower
                                            , vibrance = style.vibrance
                                            }
                                    in
                                    Just
                                        (WebGL.entityWith
                                            [ DepthTest.default ]
                                            Shader.vertexShader
                                            Shader.fragmentShader
                                            meshForLod
                                            uniforms
                                        )

                                else
                                    Nothing
                    )


buildComponentRenders : PartCache -> List Components.ComponentInstance -> List GearInstance -> List ComponentRender
buildComponentRenders cache components gears =
    components
        |> List.filter (\c -> c.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                let
                    axis =
                        inferComponentAxis cache component

                    lines =
                        componentArrowLines component axis
                            |> List.map lineToEdgeVertices
                in
                if List.isEmpty lines then
                    Nothing

                else
                    Just
                        { mesh = WebGL.lines lines
                        , center = toYUpPoint component.worldPosition
                        , axis = axis
                        , drivingGearId = componentDrivingGear cache component gears
                        }
            )


buildComponentMeshRenders : PartCache -> List Components.ComponentInstance -> List GearInstance -> List ComponentMeshRender
buildComponentMeshRenders cache components gears =
    components
        |> List.filter (\c -> c.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                case Dict.get component.partFile cache of
                    Just (Loaded lines) ->
                        let
                            geom =
                                Geometry.flatten lines cache component.color component.worldMatrix
                        in
                        Just
                            { mesh = WebGL.triangles geom.triangles
                            , center = toYUpPoint component.worldPosition
                            , rotationCenter = toYUpPoint component.worldPosition
                            , axis = inferComponentAxis cache component
                            , drivingGearId = componentDrivingGear cache component gears
                            }

                    _ ->
                        Nothing
            )


componentArrowLines : Components.ComponentInstance -> Vec3.Vec3 -> List ( Vec3.Vec3, Vec3.Vec3 )
componentArrowLines component axis =
    let
        center =
            toYUpPoint component.worldPosition

        shaftHalfLength =
            10.0

        headLength =
            4.0

        headWidth =
            2.5

        start =
            Vec3.sub center (Vec3.scale shaftHalfLength axis)

        tip =
            Vec3.add center (Vec3.scale shaftHalfLength axis)

        worldUp =
            Vec3.vec3 0 1 0

        perpRawA =
            Vec3.cross axis worldUp

        perpRawB =
            Vec3.cross axis (Vec3.vec3 1 0 0)

        perpRaw =
            if Vec3.length perpRawA < 1.0e-6 then
                perpRawB

            else
                perpRawA

        perpLen =
            Vec3.length perpRaw

        perp =
            if perpLen < 1.0e-6 then
                Vec3.vec3 0 0 1

            else
                Vec3.scale (1 / perpLen) perpRaw

        headBase =
            Vec3.sub tip (Vec3.scale headLength axis)

        headLeft =
            Vec3.add headBase (Vec3.scale headWidth perp)

        headRight =
            Vec3.sub headBase (Vec3.scale headWidth perp)
    in
    [ ( start, tip )
    , ( tip, headLeft )
    , ( tip, headRight )
    ]


lineToEdgeVertices : ( Vec3.Vec3, Vec3.Vec3 ) -> ( EdgeVertex, EdgeVertex )
lineToEdgeVertices ( p1, p2 ) =
    -- GuideShader only reads `position`; `other` and `side` are unused but required by the type.
    ( { position = p1, other = p2, side = -1.0 }
    , { position = p2, other = p1, side = 1.0 }
    )


componentDrivingGear : PartCache -> Components.ComponentInstance -> List GearInstance -> Maybe GearId
componentDrivingGear cache component gears =
    let
        compCenter =
            toYUpPoint component.worldPosition

        compAxis =
            inferComponentAxis cache component

        candidates =
            gears
                |> List.filterMap
                    (\gear ->
                        let
                            gearCenter =
                                toYUpPoint gear.worldPosition

                            gearAxisEnd =
                                toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                            gearAxisRaw =
                                Vec3.sub gearAxisEnd gearCenter

                            gearAxisLen =
                                Vec3.length gearAxisRaw

                            gearAxis =
                                if gearAxisLen < 1.0e-6 then
                                    Vec3.vec3 0 1 0

                                else
                                    Vec3.scale (1 / gearAxisLen) gearAxisRaw

                            axisDot =
                                abs (Vec3.dot compAxis gearAxis)

                            lineDist =
                                pointToLineDistance compCenter gearCenter gearAxis
                        in
                        if axisDot >= 0.94 && lineDist <= 2.5 then
                            Just ( lineDist, gear.id )

                        else
                            Nothing
                    )
    in
    candidates
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


{-| Determine whether a top-level LDraw line is co-axial with any detected
gear. Returns the id of the closest qualifying gear, or Nothing.

Used in `finishLoading` to decide which non-gear, non-component parts should
rotate with a gear rather than being baked into the static scene.

-}
topLevelCoaxialGear : PartCache -> List DrivenAxle -> LDrawLine -> List GearInstance -> Maybe GearId
topLevelCoaxialGear cache drivenAxles line gears =
    case line of
        SubFileRef ref ->
            let
                pos =
                    toYUpPoint (Mat4.transform ref.transform (Vec3.vec3 0 0 0))

                connectorPoints =
                    connectorLocalPoints cache ref.file
                        |> List.map (\local -> toYUpPoint (Mat4.transform ref.transform local))

                samplePoints =
                    pos :: connectorPoints

                axleMatch =
                    drivenAxles
                        |> List.filterMap
                            (\axle ->
                                bestLineMatchForPoints samplePoints axle.center axle.axis
                                    |> Maybe.andThen
                                        (\( lineDist, axialOffset ) ->
                                            if lineDist <= 6 && axialOffset <= 240 then
                                                Just ( lineDist, axialOffset, axle.drivingGearId )

                                            else
                                                Nothing
                                        )
                            )
                        |> List.sortBy (\( lineDist, axialOffset, _ ) -> ( lineDist, axialOffset ))
                        |> List.head
                        |> Maybe.map (\( _, _, gearId ) -> gearId)

                gearMatch =
                    gears
                        |> List.filterMap
                            (\gear ->
                                let
                                    gearCenter =
                                        toYUpPoint gear.worldPosition

                                    gearAxisEnd =
                                        toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                                    gearAxisRaw =
                                        Vec3.sub gearAxisEnd gearCenter

                                    gearAxisLen =
                                        Vec3.length gearAxisRaw

                                    gearAxis =
                                        if gearAxisLen < 1.0e-6 then
                                            Vec3.vec3 0 1 0

                                        else
                                            Vec3.scale (1 / gearAxisLen) gearAxisRaw
                                in
                                bestLineMatchForPoints samplePoints gearCenter gearAxis
                                    |> Maybe.andThen
                                        (\( lineDist, axialOffset ) ->
                                            if lineDist <= 2.5 && axialOffset <= 120 then
                                                Just ( lineDist, axialOffset, gear.id )

                                            else
                                                Nothing
                                        )
                            )
                        |> List.sortBy (\( lineDist, axialOffset, _ ) -> ( lineDist, axialOffset ))
                        |> List.head
                        |> Maybe.map (\( _, _, gearId ) -> gearId)
            in
            case axleMatch of
                Just gearId ->
                    Just gearId

                Nothing ->
                    gearMatch

        _ ->
            Nothing


bestLineMatchForPoints : List Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Maybe ( Float, Float )
bestLineMatchForPoints points lineOrigin lineDir =
    points
        |> List.map
            (\point ->
                let
                    lineDist =
                        pointToLineDistance point lineOrigin lineDir

                    axialOffset =
                        abs (Vec3.dot (Vec3.sub point lineOrigin) lineDir)
                in
                ( lineDist, axialOffset )
            )
        |> List.sortBy (\( lineDist, axialOffset ) -> ( lineDist, axialOffset ))
        |> List.head


{-| Build rotating `ComponentMeshRender` entries for parts that are co-axial
with a detected gear but are not in `Components.defaultSpecs`.

These parts are excluded from the static scene geometry worker and rendered
with the same rotation angle as their driving gear.

-}
buildCoaxialMeshRenders :
    PartCache
    -> List GearInstance
    -> List { file : String, color : Int, transform : Mat4.Mat4, drivingGearId : GearId }
    -> List ComponentMeshRender
buildCoaxialMeshRenders cache gears parts =
    List.filterMap
        (\part ->
            case Dict.get part.file cache of
                Just (Loaded lines) ->
                    let
                        geom =
                            Geometry.flatten lines cache part.color part.transform

                        center =
                            toYUpPoint (Mat4.transform part.transform (Vec3.vec3 0 0 0))

                        axisEnd =
                            toYUpPoint (Mat4.transform part.transform (Vec3.vec3 0 0 1))

                        axisRaw =
                            Vec3.sub axisEnd center

                        axisLen =
                            Vec3.length axisRaw

                        localAxis =
                            if axisLen < 1.0e-6 then
                                Vec3.vec3 0 1 0

                            else
                                canonicalizeAxis (Vec3.scale (1 / axisLen) axisRaw)

                        ( rotationCenter, axis ) =
                            case gearAxisInfoById part.drivingGearId gears of
                                Just info ->
                                    ( projectPointToLine center info.center info.axis
                                    , info.axis
                                    )

                                Nothing ->
                                    ( center, localAxis )
                    in
                    Just
                        { mesh = WebGL.triangles geom.triangles
                        , center = center
                        , rotationCenter = rotationCenter
                        , axis = axis
                        , drivingGearId = Just part.drivingGearId
                        }

                _ ->
                    Nothing
        )
        parts


pointToLineDistance : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Float
pointToLineDistance point lineOrigin lineDir =
    let
        offset =
            Vec3.sub point lineOrigin

        projectionLen =
            Vec3.dot offset lineDir

        projected =
            Vec3.scale projectionLen lineDir

        rejection =
            Vec3.sub offset projected
    in
    Vec3.length rejection


projectPointToLine : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3
projectPointToLine point lineOrigin lineDir =
    let
        offset =
            Vec3.sub point lineOrigin

        projectionLen =
            Vec3.dot offset lineDir
    in
    Vec3.add lineOrigin (Vec3.scale projectionLen lineDir)


type alias GearAxisInfo =
    { center : Vec3.Vec3
    , axis : Vec3.Vec3
    }


type alias DrivenAxle =
    { center : Vec3.Vec3
    , axis : Vec3.Vec3
    , drivingGearId : GearId
    }


buildDrivenAxles : PartCache -> List Components.ComponentInstance -> List GearInstance -> List DrivenAxle
buildDrivenAxles cache components gears =
    components
        |> List.filter (\component -> component.kind == Components.AxleLike)
        |> List.filterMap
            (\component ->
                componentDrivingGear cache component gears
                    |> Maybe.map
                        (\gearId ->
                            { center = toYUpPoint component.worldPosition
                            , axis = inferComponentAxis cache component
                            , drivingGearId = gearId
                            }
                        )
            )


connectorLocalPoints : PartCache -> String -> List Vec3.Vec3
connectorLocalPoints cache partFile =
    case Dict.get partFile cache of
        Just (Loaded lines) ->
            lines
                |> List.filterMap
                    (\line ->
                        case line of
                            SubFileRef subRef ->
                                if isRotationalConnectorReference subRef.file then
                                    Just (Mat4.transform subRef.transform (Vec3.vec3 0 0 0))

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        _ ->
            []


isRotationalConnectorReference : String -> Bool
isRotationalConnectorReference file =
    let
        lower =
            String.toLower file
    in
    List.any
        (\needle -> String.contains needle lower)
        [ "axlehol"
        , "axleend"
        , "axle.dat"
        ]


gearAxisInfoById : GearId -> List GearInstance -> Maybe GearAxisInfo
gearAxisInfoById targetId gears =
    gears
        |> List.filter (\gear -> gear.id == targetId)
        |> List.head
        |> Maybe.map
            (\gear ->
                let
                    center =
                        toYUpPoint gear.worldPosition

                    axisEnd =
                        toYUpPoint (Mat4.transform gear.worldMatrix (Vec3.vec3 0 0 1))

                    rawAxis =
                        Vec3.sub axisEnd center

                    axisLen =
                        Vec3.length rawAxis

                    axis =
                        if axisLen < 1.0e-6 then
                            Vec3.vec3 0 1 0

                        else
                            canonicalizeAxis (Vec3.scale (1 / axisLen) rawAxis)
                in
                { center = center, axis = axis }
            )


inferComponentAxis : PartCache -> Components.ComponentInstance -> Vec3.Vec3
inferComponentAxis cache component =
    let
        localAxis =
            inferPartLocalAxis cache component.partFile

        origin =
            Mat4.transform component.worldMatrix (Vec3.vec3 0 0 0)

        axisEnd =
            Mat4.transform component.worldMatrix localAxis

        raw =
            Vec3.sub axisEnd origin

        len =
            Vec3.length raw
    in
    if len < 1.0e-6 then
        Vec3.vec3 0 1 0

    else
        canonicalizeAxis (Vec3.scale (1 / len) raw)


inferPartLocalAxis : PartCache -> String -> Vec3.Vec3
inferPartLocalAxis cache partFile =
    case Dict.get partFile cache of
        Just (Loaded lines) ->
            let
                geom =
                    Geometry.flatten lines cache 15 Mat4.identity

                points =
                    (geom.triangles
                        |> List.concatMap (\( a, b, c ) -> [ a.position, b.position, c.position ])
                    )
                        ++ (geom.lines
                                |> List.concatMap (\( p1, p2 ) -> [ p1, p2 ])
                           )

                initialBounds =
                    { minX = 1 / 0
                    , maxX = -(1 / 0)
                    , minY = 1 / 0
                    , maxY = -(1 / 0)
                    , minZ = 1 / 0
                    , maxZ = -(1 / 0)
                    }

                bounds =
                    List.foldl
                        (\p acc ->
                            { minX = min acc.minX (Vec3.getX p)
                            , maxX = max acc.maxX (Vec3.getX p)
                            , minY = min acc.minY (Vec3.getY p)
                            , maxY = max acc.maxY (Vec3.getY p)
                            , minZ = min acc.minZ (Vec3.getZ p)
                            , maxZ = max acc.maxZ (Vec3.getZ p)
                            }
                        )
                        initialBounds
                        points

                extentX =
                    bounds.maxX - bounds.minX

                extentY =
                    bounds.maxY - bounds.minY

                extentZ =
                    bounds.maxZ - bounds.minZ
            in
            if extentX >= extentY && extentX >= extentZ then
                Vec3.vec3 1 0 0

            else if extentY >= extentZ then
                Vec3.vec3 0 1 0

            else
                Vec3.vec3 0 0 1

        _ ->
            Vec3.vec3 0 0 1


renderComponentEntities : Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
renderComponentEntities camera styleInput aspect model =
    let
        style =
            Style.clampStyle styleInput

        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        viewPos =
            Camera.position camera
    in
    model.componentMeshes
        |> List.filter (\c -> isSphereVisible camera aspect c.center 10.0)
        |> List.map
            (\component ->
                let
                    angle =
                        case component.drivingGearId of
                            Just gid ->
                                Dict.get gid model.gearAngles |> Maybe.withDefault 0.0

                            Nothing ->
                                0.0

                    modelMat =
                        rotationAround angle component.axis component.rotationCenter

                    uniforms =
                        { modelMatrix = modelMat
                        , viewMatrix = viewMat
                        , projectionMatrix = projMat
                        , viewPosition = viewPos
                        , lightDirection = style.lightDirection
                        , instanceColor = Shader.noInstanceColor
                        , ambientStrength = style.ambientStrength
                        , lightStrength = style.lightStrength
                        , specularStrength = style.specularStrength
                        , specularPower = style.specularPower
                        , rimStrength = style.rimStrength
                        , rimPower = style.rimPower
                        , vibrance = style.vibrance
                        }
                in
                WebGL.entityWith
                    [ DepthTest.default ]
                    Shader.vertexShader
                    Shader.fragmentShader
                    component.mesh
                    uniforms
            )


renderComponentArrows : Camera -> Float -> Model -> List WebGL.Entity
renderComponentArrows camera aspect model =
    let
        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
    in
    model.componentRenders
        |> List.filter (\c -> isSphereVisible camera aspect c.center 14.0)
        |> List.map
            (\component ->
                let
                    angle =
                        case component.drivingGearId of
                            Just gid ->
                                Dict.get gid model.gearAngles |> Maybe.withDefault 0.0

                            Nothing ->
                                0.0

                    modelMat =
                        rotationAround angle component.axis component.center

                    uniforms =
                        { modelMatrix = modelMat
                        , viewMatrix = viewMat
                        , projectionMatrix = projMat
                        }
                in
                WebGL.entityWith
                    [ DepthTest.default ]
                    GuideShader.vertexShader
                    GuideShader.fragmentShader
                    component.mesh
                    uniforms
            )


isSphereVisible : Camera -> Float -> Vec3.Vec3 -> Float -> Bool
isSphereVisible camera aspect center radius =
    let
        eye =
            Camera.position camera

        forwardRaw =
            Vec3.sub camera.target eye

        forwardLen =
            Vec3.length forwardRaw
    in
    if forwardLen < 1.0e-6 then
        True

    else
        let
            forward =
                Vec3.scale (1 / forwardLen) forwardRaw

            worldUp =
                Vec3.vec3 0 1 0

            rightRaw =
                Vec3.cross forward worldUp

            rightLen =
                Vec3.length rightRaw
        in
        if rightLen < 1.0e-6 then
            True

        else
            let
                right =
                    Vec3.scale (1 / rightLen) rightRaw

                upRaw =
                    Vec3.cross right forward

                upLen =
                    Vec3.length upRaw

                up =
                    if upLen < 1.0e-6 then
                        worldUp

                    else
                        Vec3.scale (1 / upLen) upRaw

                toCenter =
                    Vec3.sub center eye

                z =
                    Vec3.dot toCenter forward

                x =
                    Vec3.dot toCenter right

                y =
                    Vec3.dot toCenter up

                nearPlane =
                    Camera.nearPlane camera

                farPlane =
                    Camera.farPlane camera

                tanHalfFov =
                    tan (degrees Camera.fovYDegrees / 2)

                maxX =
                    z * tanHalfFov * aspect + radius

                maxY =
                    z * tanHalfFov + radius
            in
            z
                >= nearPlane
                - radius
                && z
                <= farPlane
                + radius
                && abs x
                <= maxX
                && abs y
                <= maxY


setMotorGear : GearId -> Model -> ( Model, Cmd Msg )
setMotorGear id model =
    let
        motor =
            model.motor

        playbackState =
            model.playback

        newMotor =
            { motor | drivingGearId = Just id }

        newAngles =
            case model.gearGraph of
                Just graph ->
                    Animate.gearAngles newMotor graph model.playback.currentTime

                Nothing ->
                    Dict.empty
    in
    ( { model
        | motor = newMotor
        , playback = { playbackState | motorGearId = Just id }
        , gearAngles = newAngles
      }
    , Cmd.none
    )


pickGearByScreenPoint : Float -> Float -> Model -> Maybe GearId
pickGearByScreenPoint mouseX mouseY model =
    case model.gearGraph of
        Nothing ->
            Nothing

        Just graph ->
            let
                maybeRay =
                    screenRayFromMouse mouseX mouseY model
            in
            case maybeRay of
                Nothing ->
                    Nothing

                Just ( origin, dir ) ->
                    graph.instances
                        |> Array.toList
                        |> List.filterMap
                            (\inst ->
                                let
                                    center =
                                        toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0))
                                in
                                raySphereHit origin dir center inst.spec.pitchRadius
                                    |> Maybe.map (\t -> ( t, inst.id ))
                            )
                        |> List.foldl
                            (\( t, gid ) best ->
                                case best of
                                    Nothing ->
                                        Just ( t, gid )

                                    Just ( bestT, bestId ) ->
                                        if t < bestT then
                                            Just ( t, gid )

                                        else
                                            Just ( bestT, bestId )
                            )
                            Nothing
                        |> Maybe.map Tuple.second


screenRayFromMouse : Float -> Float -> Model -> Maybe ( Vec3.Vec3, Vec3.Vec3 )
screenRayFromMouse mouseX mouseY model =
    if model.width <= 0 || model.height <= 0 then
        Nothing

    else
        let
            widthF =
                toFloat model.width

            heightF =
                toFloat model.height

            ndcX =
                (2 * mouseX / widthF) - 1

            ndcY =
                1 - (2 * mouseY / heightF)

            eye =
                Camera.position model.camera

            forwardRaw =
                Vec3.sub model.camera.target eye

            forwardLen =
                Vec3.length forwardRaw
        in
        if forwardLen < 1.0e-6 then
            Nothing

        else
            let
                forward =
                    Vec3.scale (1 / forwardLen) forwardRaw

                worldUp =
                    Vec3.vec3 0 1 0

                rightRaw =
                    Vec3.cross forward worldUp

                rightLen =
                    Vec3.length rightRaw
            in
            if rightLen < 1.0e-6 then
                Nothing

            else
                let
                    right =
                        Vec3.scale (1 / rightLen) rightRaw

                    upRaw =
                        Vec3.cross right forward

                    upLen =
                        Vec3.length upRaw

                    up =
                        if upLen < 1.0e-6 then
                            worldUp

                        else
                            Vec3.scale (1 / upLen) upRaw

                    aspect =
                        widthF / heightF

                    tanHalfFov =
                        tan (degrees Camera.fovYDegrees / 2)

                    dirRaw =
                        Vec3.add
                            (Vec3.add forward (Vec3.scale (ndcX * aspect * tanHalfFov) right))
                            (Vec3.scale (ndcY * tanHalfFov) up)

                    dirLen =
                        Vec3.length dirRaw
                in
                if dirLen < 1.0e-6 then
                    Nothing

                else
                    Just ( eye, Vec3.scale (1 / dirLen) dirRaw )


raySphereHit : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Float -> Maybe Float
raySphereHit rayOrigin rayDir center radius =
    let
        oc =
            Vec3.sub rayOrigin center

        b =
            2 * Vec3.dot oc rayDir

        c =
            Vec3.dot oc oc - (radius * radius)

        disc =
            b * b - 4 * c
    in
    if disc < 0 then
        Nothing

    else
        let
            sqrtDisc =
                sqrt disc

            t1 =
                (-b - sqrtDisc) / 2

            t2 =
                (-b + sqrtDisc) / 2
        in
        if t1 > 0 then
            Just t1

        else if t2 > 0 then
            Just t2

        else
            Nothing



-- ── Subscriptions ─────────────────────────────────────────────────────────────


{-| Whether the scene is doing per-frame work and therefore needs animation
frames. When this is `False` we drop the `onAnimationFrame` subscription entirely
so a static model stops forcing a full update/view/diff at 60 fps.

The cases mirror the only branches of the `AnimationFrame` handler that do real
work: driving the flattening progress bar, applying a held rotate button,
advancing the gear playback clock, and spending down the instanced renderer's
settle countdown so the refined frame gets drawn after the camera stops.

-}
needsAnimationFrame : Model -> Bool
needsAnimationFrame model =
    model.playback.running
        || (model.loadPhase == FlatteningGeometry)
        || model.refineFrames
        > 0
        || Shortcuts.hasContinuousInput model.heldKeys
        || (case model.heldControl of
                NoHeldControl ->
                    False

                HoldRotate _ _ ->
                    True
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseMove
            (Decode.map2 MouseMove
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        , Browser.Events.onMouseUp
            (Decode.map2 MouseUp
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        , Browser.Events.onKeyDown (Decode.map KeyPressed keyEventDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyReleased (Decode.field "key" Decode.string))

        -- Losing focus swallows the matching keyup, which would leave a held
        -- movement key stuck down for the rest of the session.
        , Browser.Events.onVisibilityChange (\_ -> AllKeysReleased)
        , if model.useWindowResize then
            Browser.Events.onResize WindowResize

          else
            Sub.none
        , Ports.viewportResized (\size -> WindowResize size.width size.height)
        , Ports.fileContentReceived FileContentReceived
        , Ports.fileLoadError FileLoadError
        , Ports.geometryFlattened GeometryFlattened
        , Ports.geometryFlattenFailed GeometryFlattenFailed
        , if needsAnimationFrame model then
            Browser.Events.onAnimationFrame AnimationFrame

          else
            Sub.none
        ]


{-| Wheel `deltaY`, normalised to pixel units.

Browsers report scroll amounts in three different units, and only Chrome uses
pixels: Firefox reports lines (~3 per notch) and some setups report pages. Left
unnormalised, the same gesture zoomed roughly 30x slower in Firefox. `ctrlKey`
marks a trackpad pinch, which arrives as very small deltas.

Scale factors match three.js `OrbitControls`.

-}
wheelDeltaDecoder : Decode.Decoder Float
wheelDeltaDecoder =
    Decode.map3
        (\delta mode ctrlKey ->
            let
                unitScale =
                    case mode of
                        1 ->
                            16.0

                        2 ->
                            100.0

                        _ ->
                            1.0

                pinchScale =
                    if ctrlKey then
                        10.0

                    else
                        1.0
            in
            delta * unitScale * pinchScale
        )
        (Decode.field "deltaY" Decode.float)
        (Decode.oneOf [ Decode.field "deltaMode" Decode.int, Decode.succeed 0 ])
        (Decode.oneOf [ Decode.field "ctrlKey" Decode.bool, Decode.succeed False ])


{-| Everything `UI.Shortcuts` needs from a keyboard event.

`target.tagName` and `target.isContentEditable` drive the typing guard. Both are
read defensively — a synthetic event, or one whose target is the document rather
than an element, has neither.

-}
keyEventDecoder : Decode.Decoder Shortcuts.KeyEvent
keyEventDecoder =
    Decode.map8
        (\key shift ctrl meta alt repeat targetTag targetEditable ->
            { key = key
            , shift = shift
            , ctrl = ctrl
            , meta = meta
            , alt = alt
            , repeat = repeat
            , targetTag = targetTag
            , targetEditable = targetEditable
            }
        )
        (Decode.field "key" Decode.string)
        (optionalFlag "shiftKey")
        (optionalFlag "ctrlKey")
        (optionalFlag "metaKey")
        (optionalFlag "altKey")
        (optionalFlag "repeat")
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.at [ "target", "isContentEditable" ] Decode.bool, Decode.succeed False ])


{-| A boolean event field, defaulting to `False` when absent.
-}
optionalFlag : String -> Decode.Decoder Bool
optionalFlag field =
    Decode.oneOf [ Decode.field field Decode.bool, Decode.succeed False ]


{-| Fingers currently touching **this canvas**.

`targetTouches` rather than `touches`: the latter counts every finger on the
screen, so resting a thumb on the controls panel forced the canvas into a pinch
gesture.

-}
touchesDecoder : Decode.Decoder (List TouchPoint)
touchesDecoder =
    Decode.field "targetTouches" (Decode.list touchPointDecoder)


touchPointDecoder : Decode.Decoder TouchPoint
touchPointDecoder =
    Decode.map3
        (\identifier x y ->
            { id = identifier
            , x = x
            , y = y
            }
        )
        (Decode.field "identifier" intLikeDecoder)
        (coordinateDecoder [ "clientX", "pageX", "screenX" ])
        (coordinateDecoder [ "clientY", "pageY", "screenY" ])


pointerTouchPointDecoder : Decode.Decoder TouchPoint
pointerTouchPointDecoder =
    Decode.field "pointerType" Decode.string
        |> Decode.andThen
            (\pointerType ->
                if pointerType == "touch" then
                    Decode.map3
                        (\pointerId x y ->
                            { id = pointerId
                            , x = x
                            , y = y
                            }
                        )
                        (Decode.field "pointerId" intLikeDecoder)
                        (coordinateDecoder [ "clientX", "pageX", "screenX" ])
                        (coordinateDecoder [ "clientY", "pageY", "screenY" ])

                else
                    Decode.fail "Non-touch pointer"
            )


pointerTouchIdDecoder : Decode.Decoder Int
pointerTouchIdDecoder =
    Decode.field "pointerType" Decode.string
        |> Decode.andThen
            (\pointerType ->
                if pointerType == "touch" then
                    Decode.field "pointerId" intLikeDecoder

                else
                    Decode.fail "Non-touch pointer"
            )


intLikeDecoder : Decode.Decoder Int
intLikeDecoder =
    Decode.oneOf
        [ Decode.int
        , Decode.float |> Decode.map round
        ]


floatLikeDecoder : Decode.Decoder Float
floatLikeDecoder =
    Decode.oneOf
        [ Decode.float
        , Decode.int |> Decode.map toFloat
        ]


coordinateDecoder : List String -> Decode.Decoder Float
coordinateDecoder keys =
    keys
        |> List.map (\key -> Decode.field key floatLikeDecoder)
        |> Decode.oneOf



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
        [ viewCanvas model
        , viewOverlay model
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    let
        aspect =
            toFloat model.width / toFloat model.height

        entities =
            case ( model.scene, model.instancedScene ) of
                ( Just scene, _ ) ->
                    Scene.renderSceneWithStyle scene model.camera model.renderStyle model.width model.height
                        ++ renderGearEntities model.camera model.renderStyle aspect model
                        ++ renderComponentEntities model.camera model.renderStyle aspect model
                        ++ renderComponentArrows model.camera aspect model

                ( Nothing, Just instanced ) ->
                    Instanced.render
                        { camera = model.camera
                        , style = model.renderStyle
                        , width = model.width
                        , height = model.height
                        , coarsen = cameraIsMoving model
                        }
                        instanced

                ( Nothing, Nothing ) ->
                    []
    in
    WebGL.toHtml
        ([ Attr.width model.width
         , Attr.height model.height
         , Attr.style "display" "block"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "touch-action" "none"
         , Html.Events.on "mousedown"
            (Decode.map3 MouseDown
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                (Decode.field "shiftKey" Decode.bool)
            )
         , Html.Events.preventDefaultOn "wheel"
            (Decode.map (\delta -> ( Wheel delta, True )) wheelDeltaDecoder)
         ]
            ++ touchListeners model
        )
        entities


{-| Canvas listeners for finger input — one family, never both.

iOS Safari dispatches Touch **and** Pointer events for the same finger. With
both bound, `activeTouches` was keyed by touch `identifier` from one path and
`pointerId` from the other, and the two sets of bookkeeping fought each other.

-}
touchListeners : Model -> List (Html.Attribute Msg)
touchListeners model =
    if model.supportsPointerEvents then
        [ Html.Events.preventDefaultOn "pointerdown"
            (Decode.map (\touch -> ( PointerTouchStart touch, True )) pointerTouchPointDecoder)
        , Html.Events.preventDefaultOn "pointermove"
            (Decode.map (\touch -> ( PointerTouchMove touch, True )) pointerTouchPointDecoder)
        , Html.Events.preventDefaultOn "pointerup"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        , Html.Events.preventDefaultOn "pointercancel"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        ]

    else
        [ Html.Events.preventDefaultOn "touchstart"
            (Decode.map (\touches -> ( TouchStart touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchmove"
            (Decode.map (\touches -> ( TouchMove touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchend"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchcancel"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    let
        modeOverlays =
            case model.uiMode of
                ViewerMode ->
                    if model.viewerControlsEnabled then
                        [ viewViewerControls model ]

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


{-| The keyboard shortcut reference.

Content comes from `UI.Shortcuts.helpRows` so it cannot drift away from the
bindings it documents.

-}
viewShortcutHelp : Html Msg
viewShortcutHelp =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "50%"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translate(-50%, -50%)"
        , Attr.style "max-width" "min(560px, calc(100% - 24px))"
        , Attr.style "max-height" "calc(100% - 24px)"
        , Attr.style "overflow" "auto"
        , Attr.style "pointer-events" "auto"
        , Attr.style "padding" "16px 20px"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 16%, transparent)"
        , Attr.style "font-size" "12px"
        , Attr.attribute "role" "dialog"
        , Attr.attribute "aria-label" "Keyboard shortcuts"
        ]
        (div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "space-between"
            , Attr.style "gap" "12px"
            , Attr.style "margin-bottom" "12px"
            ]
            [ div
                [ Attr.style "font-weight" "600"
                , Attr.style "font-size" "13px"
                ]
                [ text "Keyboard shortcuts" ]
            , button
                [ Html.Events.onClick ToggleShortcutHelp
                , onTouchTap ToggleShortcutHelp
                , Attr.title "Close"
                , Attr.attribute "aria-label" "Close"
                , Attr.style "padding" "2px 4px"
                , Attr.style "background" "transparent"
                , Attr.style "color" Theme.textMuted
                , Attr.style "border" "none"
                , Attr.style "cursor" "pointer"
                , Attr.style "display" "flex"
                ]
                [ featherIcon "x" ]
            ]
            :: List.map viewShortcutGroup Shortcuts.helpRows
            ++ [ div
                    [ Attr.style "margin-top" "12px"
                    , Attr.style "color" Theme.textMuted
                    ]
                    [ text "Street level flies through walls — there is no collision." ]
               ]
        )


{-| One headed group of the shortcut reference.
-}
viewShortcutGroup : ( String, List ( String, String ) ) -> Html Msg
viewShortcutGroup ( heading, rows ) =
    div [ Attr.style "margin-bottom" "10px" ]
        (div
            [ Attr.style "color" Theme.textMuted
            , Attr.style "text-transform" "uppercase"
            , Attr.style "letter-spacing" "0.06em"
            , Attr.style "font-size" "10px"
            , Attr.style "margin-bottom" "4px"
            ]
            [ text heading ]
            :: List.map viewShortcutRow rows
        )


{-| One key/description line of the shortcut reference.
-}
viewShortcutRow : ( String, String ) -> Html Msg
viewShortcutRow ( keys, description ) =
    div
        [ Attr.style "display" "flex"
        , Attr.style "gap" "12px"
        , Attr.style "line-height" "1.8"
        ]
        [ div
            [ Attr.style "flex" "0 0 110px"
            , Attr.style "font-family" "monospace"
            ]
            [ text keys ]
        , div [] [ text description ]
        ]


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
                    ++ String.fromInt (round (model.camera.azimuth * 180 / pi))
                    ++ "°  el "
                    ++ String.fromInt (round (model.camera.elevation * 180 / pi))
                    ++ "°  dist "
                    ++ String.fromInt (round model.camera.distance)
                )
            ]
        , div [] [ text "Drag to orbit · Shift+Drag to pan · Scroll to zoom" ]
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
            if model.simulationChecked && not model.simulationAvailable then
                viewSimulationUnavailablePanel

            else
                case model.gearGraph of
                    Nothing ->
                        text ""

                    Just graph ->
                        let
                            instances =
                                Array.toList graph.instances

                            axleCount =
                                model.components
                                    |> List.filter (\c -> c.kind == Components.AxleLike)
                                    |> List.length

                            beamCount =
                                model.components
                                    |> List.filter (\c -> c.kind == Components.Beam)
                                    |> List.length

                            ratios =
                                case model.motor.drivingGearId of
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
                                        , onTouchTap ToggleControlsPanel
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
            model.motor.drivingGearId == Just inst.id

        angle =
            Dict.get inst.id model.gearAngles |> Maybe.withDefault 0.0

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
                [ Html.Events.onClick (SetMotorGear inst.id)
                , onTouchTap (SetMotorGear inst.id)
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
                [ Html.Events.onClick ToggleMotor
                , onTouchTap ToggleMotor
                , Attr.style "flex" "1"
                , Attr.style "padding" "5px 0"
                , Attr.style "background"
                    (if model.playback.running then
                        Theme.brandYellow

                     else
                        Theme.panelSubtleBackground
                    )
                , Attr.style "color"
                    (if model.playback.running then
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
                    (if model.playback.running then
                        "⏸ Pause"

                     else
                        "▶ Play"
                    )
                ]
            , button
                [ Html.Events.onClick Stop
                , onTouchTap Stop
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
                (formatTime model.playback.currentTime
                    ++ (if model.playback.running then
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
            , div [ Attr.style "font-size" "11px" ] [ text (String.fromInt (round (model.motor.speedRadPerSec * 60 / (2 * pi))) ++ " / ±" ++ String.fromInt (round model.maxRpm)) ]
            ]
        , input
            [ Attr.type_ "range"
            , Attr.min (String.fromFloat -model.maxRpm)
            , Attr.max (String.fromFloat model.maxRpm)
            , Attr.step "1"
            , Attr.value (String.fromFloat (model.motor.speedRadPerSec * 60 / (2 * pi)))
            , Html.Events.onInput
                (\raw ->
                    case String.toFloat raw of
                        Just val ->
                            SetMotorSpeed val

                        Nothing ->
                            SetMotorSpeed (model.motor.speedRadPerSec * 60 / (2 * pi))
                )
            , Attr.style "width" "100%"
            ]
            []
        ]


onTouchTap : msg -> Html.Attribute msg
onTouchTap msg =
    Html.Events.preventDefaultOn "touchstart" (Decode.succeed ( msg, True ))


viewViewerControls : Model -> Html Msg
viewViewerControls model =
    let
        step =
            0.08
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "right" "12px"
        , Attr.style "bottom" "12px"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(3, 36px)"
        , Attr.style "grid-template-rows" "repeat(3, 32px)"
        , Attr.style "gap" "4px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        , Attr.style "padding" "8px"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "box-shadow" "0 10px 32px color-mix(in srgb, var(--color-brand) 8%, transparent)"
        ]
        [ viewerFitViewButton
        , viewerRotateButton (featherIcon "chevron-up") (StartHoldRotate 0 -step)
        , viewerWalkButton model
        , viewerRotateButton (featherIcon "chevron-left") (StartHoldRotate step 0)
        , if model.simulationAvailable then
            viewerCenterPlayButton model

          else
            div [] []
        , viewerRotateButton (featherIcon "chevron-right") (StartHoldRotate -step 0)
        , div [] []
        , viewerRotateButton (featherIcon "chevron-down") (StartHoldRotate 0 step)
        , viewerHelpButton model
        ]


{-| Drop to street level, or step back out.

Shown as engaged while walking, following the play button's precedent, because
the same drag means two different things depending on the mode.

-}
viewerWalkButton : Model -> Html Msg
viewerWalkButton model =
    let
        walking =
            model.camera.navigation == Camera.Walk

        label =
            if walking then
                "Leave street level"

            else
                "View from street level"
    in
    button
        (Html.Events.onClick ToggleWalkMode
            :: onTouchTap ToggleWalkMode
            :: Attr.title label
            :: Attr.attribute "aria-label" label
            :: Attr.attribute "aria-pressed"
                (if walking then
                    "true"

                 else
                    "false"
                )
            :: viewerButtonStyles walking
        )
        [ featherIcon "user" ]


{-| Show or hide the keyboard shortcut list.
-}
viewerHelpButton : Model -> Html Msg
viewerHelpButton model =
    button
        (Html.Events.onClick ToggleShortcutHelp
            :: onTouchTap ToggleShortcutHelp
            :: Attr.title "Keyboard shortcuts"
            :: Attr.attribute "aria-label" "Keyboard shortcuts"
            :: viewerButtonStyles model.showShortcutHelp
        )
        [ featherIcon "help-circle" ]


{-| Shared styling for the viewer pad's toggle buttons.

`active` swaps in the brand colours, so a mode that changes what the mouse does
is visible at a glance.

-}
viewerButtonStyles : Bool -> List (Html.Attribute Msg)
viewerButtonStyles active =
    [ Attr.style "width" "36px"
    , Attr.style "height" "32px"
    , Attr.style "padding" "0"
    , Attr.style "background"
        (if active then
            Theme.brandYellow

         else
            Theme.panelSubtleBackground
        )
    , Attr.style "color"
        (if active then
            Theme.brand

         else
            Theme.textPrimary
        )
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "4px"
    , Attr.style "cursor" "pointer"
    , Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    ]


{-| Re-frame the whole model. The only way back to the auto-fit view once the
user has orbited away from it.
-}
viewerFitViewButton : Html Msg
viewerFitViewButton =
    button
        [ Html.Events.onClick FitView
        , onTouchTap FitView
        , Attr.title "Fit model to view"
        , Attr.attribute "aria-label" "Fit model to view"
        , Attr.style "width" "36px"
        , Attr.style "height" "32px"
        , Attr.style "padding" "0"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ featherIcon "maximize" ]


viewerRotateButton : Html Msg -> Msg -> Html Msg
viewerRotateButton icon msg =
    button
        [ Html.Events.on "pointerdown" (Decode.succeed msg)
        , Html.Events.on "pointerup" (Decode.succeed EndHoldRotate)
        , Html.Events.on "pointercancel" (Decode.succeed EndHoldRotate)
        , Html.Events.on "pointerleave" (Decode.succeed EndHoldRotate)
        , Html.Events.on "mouseup" (Decode.succeed EndHoldRotate)
        , Html.Events.preventDefaultOn "touchstart" (Decode.succeed ( msg, True ))
        , Html.Events.preventDefaultOn "touchend" (Decode.succeed ( EndHoldRotate, True ))
        , Html.Events.preventDefaultOn "touchcancel" (Decode.succeed ( EndHoldRotate, True ))
        , Attr.style "width" "36px"
        , Attr.style "height" "32px"
        , Attr.style "padding" "0"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ icon ]


viewerCenterPlayButton : Model -> Html Msg
viewerCenterPlayButton model =
    button
        [ Html.Events.onClick ToggleMotor
        , onTouchTap ToggleMotor
        , Attr.style "width" "36px"
        , Attr.style "height" "32px"
        , Attr.style "padding" "0"
        , Attr.style "background"
            (if model.playback.running then
                Theme.brandYellow

             else
                Theme.panelSubtleBackground
            )
        , Attr.style "color"
            (if model.playback.running then
                Theme.brand

             else
                Theme.textPrimary
            )
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ featherIcon
            (if model.playback.running then
                "pause"

             else
                "play"
            )
        ]


featherIcon : String -> Html Msg
featherIcon name =
    let
        icon =
            case name of
                "chevron-up" ->
                    FeatherIcons.chevronUp

                "chevron-down" ->
                    FeatherIcons.chevronDown

                "chevron-left" ->
                    FeatherIcons.chevronLeft

                "chevron-right" ->
                    FeatherIcons.chevronRight

                "pause" ->
                    FeatherIcons.pause

                "maximize" ->
                    FeatherIcons.maximize

                "user" ->
                    FeatherIcons.user

                "help-circle" ->
                    FeatherIcons.helpCircle

                "x" ->
                    FeatherIcons.x

                _ ->
                    FeatherIcons.play
    in
    icon
        |> FeatherIcons.withSize 14
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true" ]


viewToolbar : Model -> Html Msg
viewToolbar _ =
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
        )


viewStatus : Model -> Html Msg
viewStatus model =
    case model.errorMsg of
        Just err ->
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "50%"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translate(-50%, -50%)"
                , Attr.style "color" Theme.brandRed
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "14px"
                , Attr.style "text-align" "center"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "padding" "16px 24px"
                , Attr.style "border-radius" "8px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "box-shadow" "0 16px 48px color-mix(in srgb, var(--color-brand) 12%, transparent)"
                , Attr.style "pointer-events" "auto"
                ]
                [ div [] [ text err ]
                , button
                    [ Html.Events.onClick DismissError
                    , Attr.style "margin-top" "10px"
                    , Attr.style "padding" "4px 12px"
                    , Attr.style "background" Theme.panelSubtleBackground
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "4px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "12px"
                    ]
                    [ text "Dismiss" ]
                ]

        Nothing ->
            case model.loadPhase of
                Ready ->
                    text ""

                Idle ->
                    text ""

                FetchingTopLevel _ ->
                    viewLoadingBox "Fetching model…" Nothing

                ResolvingParts ->
                    viewLoadingBox
                        ("Preparing model… "
                            ++ String.fromInt model.partsLoaded
                            ++ " / "
                            ++ String.fromInt model.partsTotal
                        )
                        (Just (round model.loadingProgressPct))

                FlatteningGeometry ->
                    viewLoadingBox
                        "Preparing model… finalizing geometry"
                        (Just (round model.loadingProgressPct))


viewLoadingBox : String -> Maybe Int -> Html Msg
viewLoadingBox label maybePct =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "50%"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translate(-50%, -50%)"
        , Attr.style "color" Theme.textPrimary
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "13px"
        , Attr.style "text-align" "center"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "padding" "20px 28px"
        , Attr.style "border-radius" "8px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "box-shadow" "0 16px 48px color-mix(in srgb, var(--color-brand) 10%, transparent)"
        , Attr.style "min-width" "260px"
        ]
        (div [] [ text label ]
            :: (case maybePct of
                    Just pct ->
                        [ div
                            [ Attr.style "margin-top" "10px"
                            , Attr.style "height" "4px"
                            , Attr.style "background" Theme.panelSubtleBackground
                            , Attr.style "border-radius" "2px"
                            , Attr.style "overflow" "hidden"
                            ]
                            [ div
                                [ Attr.style "height" "100%"
                                , Attr.style "width" (String.fromInt pct ++ "%")
                                , Attr.style "background" Theme.brandYellow
                                , Attr.style "border-radius" "2px"
                                , Attr.style "transition" "width 0.2s"
                                ]
                                []
                            ]
                        ]

                    Nothing ->
                        []
               )
        )



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


type alias HashState =
    { azimuth : Maybe Float
    , elevation : Maybe Float
    , distance : Maybe Float
    , targetX : Maybe Float
    , targetY : Maybe Float
    , targetZ : Maybe Float
    , navigation : Maybe Camera.Navigation
    }


decodeHash : String -> HashState
decodeHash rawHash =
    let
        withoutPrefix =
            if String.startsWith "#" rawHash then
                String.dropLeft 1 rawHash

            else
                rawHash

        entries =
            withoutPrefix
                |> String.split "&"
                |> List.filter (\chunk -> String.trim chunk /= "")
                |> List.filterMap
                    (\chunk ->
                        case String.split "=" chunk of
                            key :: rest ->
                                Just ( key, String.join "=" rest )

                            [] ->
                                Nothing
                    )
                |> Dict.fromList

        getFloat key =
            Dict.get key entries |> Maybe.andThen String.toFloat
    in
    { azimuth = getFloat "az"
    , elevation = getFloat "el"
    , distance = getFloat "d"
    , targetX = getFloat "tx"
    , targetY = getFloat "ty"
    , targetZ = getFloat "tz"
    , navigation = Dict.get "nav" entries |> Maybe.andThen navigationFromString
    }


hasExplicitHashCamera : HashState -> Bool
hasExplicitHashCamera state =
    state.azimuth
        /= Nothing
        || state.elevation
        /= Nothing
        || state.distance
        /= Nothing
        || state.targetX
        /= Nothing
        || state.targetY
        /= Nothing
        || state.targetZ
        /= Nothing
        || state.navigation
        /= Nothing


encodeHash : Model -> String
encodeHash model =
    encodeHashString model.camera


encodeHashString : Camera -> String
encodeHashString camera =
    "az="
        ++ String.fromFloat camera.azimuth
        ++ "&el="
        ++ String.fromFloat camera.elevation
        ++ "&d="
        ++ String.fromFloat camera.distance
        ++ "&tx="
        ++ String.fromFloat (Vec3.getX camera.target)
        ++ "&ty="
        ++ String.fromFloat (Vec3.getY camera.target)
        ++ "&tz="
        ++ String.fromFloat (Vec3.getZ camera.target)
        -- Orbit is the default, so only walk needs saying. Keeps the common
        -- case's URLs as short as they were.
        ++ (case camera.navigation of
                Camera.Orbit ->
                    ""

                Camera.Walk ->
                    "&nav=walk"
           )


{-| Navigation mode name used in the URL hash, the `camera-changed` payload and
the `camera-navigation` attribute.
-}
navigationToString : Camera.Navigation -> String
navigationToString navigation =
    case navigation of
        Camera.Orbit ->
            "orbit"

        Camera.Walk ->
            "walk"


{-| Parse a navigation mode name, ignoring case. Unknown values are `Nothing`, so
a typo falls back to auto-fit orbit rather than silently walking.
-}
navigationFromString : String -> Maybe Camera.Navigation
navigationFromString raw =
    case String.toLower (String.trim raw) of
        "walk" ->
            Just Camera.Walk

        "orbit" ->
            Just Camera.Orbit

        _ ->
            Nothing


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


httpErrString : Http.Error -> String
httpErrString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "HTTP " ++ String.fromInt code

        Http.BadBody msg ->
            "Bad body: " ++ msg


looksLikeHtmlResponse : String -> Bool
looksLikeHtmlResponse text =
    let
        firstNonEmpty =
            text
                |> String.lines
                |> List.filter (\line -> String.trim line /= "")
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim
                |> String.toLower
    in
    String.startsWith "<!doctype html" firstNonEmpty
        || String.startsWith "<html" firstNonEmpty



-- ── Entry point ───────────────────────────────────────────────────────────────


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
