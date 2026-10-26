module Viewport exposing
    ( CameraMode(..)
    , Config
    , Event(..)
    , HeldControl(..)
    , LoadPhase(..)
    , Model
    , Msg(..)
    , ViewConfig
    , applyDocument
    , beginFlatten
    , cameraChangedPayload
    , cameraMoved
    , captureDrag
    , groundIntersection
    , init
    , isTouchGesture
    , loadDocument
    , needsAnimationFrame
    , panCurrentGesture
    , pickBodyPoint
    , pickPoint
    , pickSource
    , pickSurfaceExcluding
    , pivotOn
    , pressedWithShift
    , renderQuality
    , setActivePath
    , setActiveSources
    , setCanvasOrigin
    , setHiddenSources
    , setRenderQuality
    , setShiftPressGrabs
    , setSpacePan
    , snapshotRequest
    , subscriptions
    , update
    , view
    )

{-| A 3D viewport as a stateful component: camera, input and model loading.

The component owns everything that makes a canvas show an LDraw model and lets
the user move around it — orbit/pan/zoom/walk, mouse, touch and pointer input,
held keys, view presets, fit-to-view, and the load pipeline (fetch → resolve
parts → geometry worker → scene).

It has no ports and no knowledge of gears. Whatever must cross that boundary
comes back as an `Event` from `update`, and the host decides what to do:

  - `FlattenRequested` carries the JSON for the geometry worker; the host sends
    it through its own port and feeds the answer back as `GeometryFlattened` /
    `GeometryFlattenFailed`.
  - `PartsReady` means every part is loaded and the model is waiting for the host
    to say which lines are static; answer with `beginFlatten`.
  - `Clicked`, `CameraSettled`, `KeyAccepted` and the load notifications let the
    host pick things, mirror the camera to the URL hash and emit runtime events.

See `docs/library-usage.md` for the full port contract.

-}

import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import LDraw.Document as Document exposing (Document)
import LDraw.Geometry as Geometry
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartCache, PartStatus(..), ResolverConfig)
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Render.Camera as Camera exposing (Camera)
import Render.Instanced as Instanced exposing (InstancedScene)
import Render.Pick as Pick
import Render.Scene as Scene exposing (Scene)
import Render.Style as Style
import Set exposing (Set)
import Task
import Time
import Viewport.Decode as ViewportDecode exposing (TouchPoint, pointerTouchIdDecoder, pointerTouchPointDecoder, touchesDecoder)
import Viewport.Framing as Framing exposing (ModelBounds, ViewPreset(..))
import Viewport.Hash as Hash
import Viewport.Navigation as Navigation
import Viewport.Shortcuts as Shortcuts
import Viewport.Snapshot as Snapshot
import Viewport.Sync as Sync
import Viewport.Wire as Wire
import WebGL



-- ── Model ─────────────────────────────────────────────────────────────────────


{-| Where the load pipeline is.
-}
type LoadPhase
    = Idle
    | FetchingTopLevel String
    | ResolvingParts
    | FlatteningGeometry
    | Ready


{-| Whether the camera follows the model's bounds (`CameraAutoFit`) or has been
moved by the user (`CameraManual`).
-}
type CameraMode
    = CameraAutoFit
    | CameraManual


{-| A pivot change in progress (`pivotOn`): the orbit target glides from `from`
to `to` over `pivotTweenMillis`, the eye sliding along with it (azimuth,
elevation and distance held), so the view pans smoothly onto the new pivot
instead of snapping. `last` is the camera the tween
last produced; any other change to the camera (the user orbiting, Fit View, a
part drag capturing the pointer) abandons the tween where it is.
-}
type alias PivotTween =
    { from : Vec3.Vec3
    , to : Vec3.Vec3
    , base : Camera
    , last : Camera
    , startedAt : Maybe Time.Posix
    }


pivotTweenMillis : Float
pivotTweenMillis =
    280


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


{-| Everything the viewport knows. Hosts read fields (camera, size, load state)
but change them only through `update`.
-}
type alias Model =
    { camera : Camera
    , pivotTween : Maybe PivotTween
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
    , fallbackLines : List LDrawLine
    , document : Maybe Document
    , sync : Maybe Sync.State
    , dragTravel : Float
    , captured : Bool
    , mousePressed : Bool
    , lastPointer : ( Float, Float )
    , cameraMode : CameraMode
    , touchGesture : TouchGesture
    , activeTouches : Dict Int TouchPoint
    , mousePanDragging : Bool
    , pressButton : Int
    , pressToggle : Bool
    , pressShift : Bool
    , shiftPressGrabs : Bool
    , spacePan : Bool
    , loadingProgressPct : Float
    , loadingProgressTick : Maybe Time.Posix
    , heldControl : HeldControl
    , heldControlTick : Maybe Time.Posix
    , heldKeys : Set String
    , heldKeysTick : Maybe Time.Posix
    , useWindowResize : Bool
    , supportsPointerEvents : Bool
    , renderStyle : Style.Style
    , renderQuality : Style.Quality
    , activeSources : Maybe (Set Int)
    , hiddenSources : Set Int
    , activePath : List Int
    , canvasOrigin : ( Float, Float )
    }


{-| What `init` needs from the host.

`initialHash` is a URL hash such as `#az=0.75&el=0.6`; an explicit camera in it
switches off auto-fit. `partCache` seeds the part cache, typically with the
parts the host embeds in its bundle.

-}
type alias Config =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , defaultModelUrl : String
    , initialHash : String
    , supportsPointerEvents : Bool
    , useWindowResize : Bool
    , renderStyle : Style.Style
    , partCache : PartCache
    }



-- ── Init ──────────────────────────────────────────────────────────────────────


{-| Create a viewport, and start fetching `defaultModelUrl` when it is not empty.
-}
init : Config -> ( Model, Cmd Msg )
init config =
    let
        fallbackBase =
            if String.trim config.ldrawFallbackBase == "" then
                Nothing

            else
                Just config.ldrawFallbackBase

        resolver =
            Resolve.resolverConfig config.ldrawBase fallbackBase

        initialHash =
            Hash.decode config.initialHash

        initialModelUrl =
            String.trim config.defaultModelUrl

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

        initialCameraMode =
            if Hash.hasExplicitCamera initialHash then
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
      , pivotTween = Nothing
      , width = 800
      , height = 600
      , loadPhase = initialLoadPhase
      , topLevelLines = []
      , partCache = config.partCache
      , resolverConfig = resolver
      , partsLoaded = 0
      , partsTotal = 0
      , scene = Nothing
      , instancedScene = Nothing
      , modelBounds = Nothing
      , submodelNames = Set.empty
      , refineFrames = 0
      , errorMsg = Nothing
      , fallbackLines = []
      , document = Nothing
      , sync = Nothing
      , dragTravel = 0.0
      , captured = False
      , mousePressed = False
      , lastPointer = ( 0, 0 )
      , cameraMode = initialCameraMode
      , touchGesture = NoTouchGesture
      , activeTouches = Dict.empty
      , mousePanDragging = False
      , pressButton = 0
      , pressToggle = False
      , pressShift = False
      , shiftPressGrabs = False
      , spacePan = False
      , loadingProgressPct = 0.0
      , loadingProgressTick = Nothing
      , heldControl = NoHeldControl
      , heldControlTick = Nothing
      , heldKeys = Set.empty
      , heldKeysTick = Nothing
      , useWindowResize = config.useWindowResize
      , supportsPointerEvents = config.supportsPointerEvents
      , renderStyle = config.renderStyle
      , renderQuality = Style.Draft
      , activeSources = Nothing
      , hiddenSources = Set.empty
      , activePath = []
      , canvasOrigin = ( 0, 0 )
      }
    , Cmd.batch <|
        (if config.useWindowResize then
            [ Browser.Dom.getViewport
                |> Task.perform
                    (\vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height))
            ]

         else
            []
        )
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



-- ── Msg and events ────────────────────────────────────────────────────────────


{-| Everything the viewport reacts to. Hosts forward their port subscriptions
as `FileContentReceived`, `FileLoadError`, `GeometryFlattened` and
`GeometryFlattenFailed`.

Pointer messages carry window (client) coordinates, as the DOM reports them;
`update` turns them into canvas pixels using `setCanvasOrigin`.

-}
type Msg
    = WindowResize Int Int
    | CanvasPressed ( Float, Float ) Msg
    | MouseButtonDown Float Float Int Bool Bool
    | MouseMove Float Float
    | MouseUp Float Float
    | DoubleClick Float Float
    | Wheel Float
    | TouchStart (List TouchPoint)
    | TouchMove (List TouchPoint)
    | TouchEnd (List TouchPoint)
    | PointerTouchStart TouchPoint
    | PointerTouchMove TouchPoint
    | PointerTouchEnd Int
    | TopLevelLoaded String (Result Http.Error String)
    | PartLoaded String (Result Http.Error String)
    | FileContentReceived String
    | FileLoadError String
    | GeometryFlattened String
    | GeometryFlattenFailed String
    | ContextMenuOpened
    | DismissError
    | AnimationFrame Time.Posix
    | RotateCameraBy Float Float
    | StartHoldRotate Float Float
    | EndHoldRotate
    | FitView
    | ToggleWalkMode
    | SetViewPreset ViewPreset
    | SetRenderQuality Style.Quality
    | KeyPressed Shortcuts.KeyEvent
    | KeyReleased String
    | AllKeysReleased


{-| What the host may want to react to after an `update`.

  - `CameraSettled` — the camera came to rest (or moved discretely); a good time
    to write the URL hash. Not sent on every drag step.
  - `Clicked` — a left-button press-release that did not drag, in canvas pixels
    (see `setCanvasOrigin`). The Bool is the toggle modifier: Shift, Ctrl or Cmd
    was held. Right and middle clicks emit nothing. The host decides what is
    under it.
  - `Tapped` — the same for a one-finger touch.
  - `PressStarted` — a plain left-button press (no Shift, Ctrl or Cmd, and not
    Space-panning), a Shift + left press when `setShiftPressGrabs` is on, or a
    single finger went down at this point. The camera has started to follow it;
    call `captureDrag` to take the gesture for the host instead (to drag a
    handle, say), or `panCurrentGesture` to pan. Right, middle and other
    modified presses never emit it, so hosts cannot grab parts with them.
  - `DragMoved` / `DragEnded` — while a gesture is captured, where it is now and
    where it ended. The camera does not move.
  - `DragCancelled` — a right-button press arrived while a gesture was captured.
    The capture has been released (the host should abandon the drag, restoring
    anything it changed mid-drag), and the right drag orbits the camera.
  - `WheelScrolled` — the mouse wheel scrolled while a gesture was captured
    (see `captureDrag`). The camera did not move.
  - `LoadStarted` — a new model replaces the old one; drop anything derived from
    the previous one.
  - `PartsReady` — all parts are loaded and the model is small enough to bake.
    The host answers with `beginFlatten`.
  - `LargeModel` — the model took the instanced path with this many placements;
    there is nothing for the host to analyse.
  - `FlattenRequested` — send this JSON to the geometry worker.
  - `ModelReady` — a scene is now showing (from the worker or the local fallback).
  - `LoadFailed` — fetching or reading the model failed.
  - `KeyAccepted` — a key went down that is not for a text field and is not an
    auto-repeat; map it with `Viewport.Shortcuts.actionFor`.

-}
type Event
    = CameraSettled
    | Clicked Float Float Bool
    | DoubleClicked Float Float
    | Tapped Float Float
    | PressStarted Float Float
    | DragMoved Float Float
    | DragEnded Float Float
    | DragCancelled
    | WheelScrolled Float
    | LoadStarted
    | PartsReady
    | LargeModel Int
    | FlattenRequested String
    | ModelReady
    | LoadFailed String
    | KeyAccepted Shortcuts.KeyEvent


{-| Fields of the `camera-changed` runtime event, for hosts that report it.
-}
cameraChangedPayload : Camera -> List ( String, Encode.Value )
cameraChangedPayload camera =
    [ ( "azimuth", Encode.float camera.azimuth )
    , ( "elevation", Encode.float camera.elevation )
    , ( "azimuthDeg", Encode.float (camera.azimuth * 180 / pi) )
    , ( "elevationDeg", Encode.float (camera.elevation * 180 / pi) )
    , ( "distance", Encode.float camera.distance )
    , ( "targetX", Encode.float (Vec3.getX camera.target) )
    , ( "targetY", Encode.float (Vec3.getY camera.target) )
    , ( "targetZ", Encode.float (Vec3.getZ camera.target) )
    , ( "navigation", Encode.string (Hash.navigationToString camera.navigation) )
    ]


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


{-| Advance the viewport. Besides the new model and commands it returns the
`Event`s the host should act on, in order.
-}
update : Msg -> Model -> ( Model, Cmd Msg, List Event )
update msg model =
    case msg of
        CanvasPressed ( left, top ) innerMsg ->
            let
                newModel =
                    { model | canvasOrigin = ( left, top ) }
            in
            update innerMsg newModel

        _ ->
            let
                ( newModel, cmd, events ) =
                    updateInner (toCanvasPixels model.canvasOrigin msg) model
            in
            -- The instanced renderer's settle countdown is maintained at this single
            -- choke point, so no camera-mutating branch has to thread it through.
            ( { newModel | refineFrames = nextRefineFrames msg model newModel }
            , cmd
            , events
            )


{-| Pointer input arrives in window (client) coordinates; everything downstream
— picking, gizmo handles, the `Clicked`/`Press…`/`Drag…` events — works in
canvas pixels. The two only coincide when the canvas sits at the window origin,
so translate once here, at the single entry point.
-}
toCanvasPixels : ( Float, Float ) -> Msg -> Msg
toCanvasPixels ( left, top ) msg =
    let
        point p =
            { p | x = p.x - left, y = p.y - top }
    in
    case msg of
        CanvasPressed origin innerMsg ->
            CanvasPressed origin (toCanvasPixels ( left, top ) innerMsg)

        MouseButtonDown x y button shift accel ->
            MouseButtonDown (x - left) (y - top) button shift accel

        MouseMove x y ->
            MouseMove (x - left) (y - top)

        MouseUp x y ->
            MouseUp (x - left) (y - top)

        DoubleClick x y ->
            DoubleClick (x - left) (y - top)

        TouchStart points ->
            TouchStart (List.map point points)

        TouchMove points ->
            TouchMove (List.map point points)

        TouchEnd points ->
            TouchEnd (List.map point points)

        PointerTouchStart p ->
            PointerTouchStart (point p)

        PointerTouchMove p ->
            PointerTouchMove (point p)

        _ ->
            msg


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


updateInner : Msg -> Model -> ( Model, Cmd Msg, List Event )
updateInner msg model =
    case msg of
        CanvasPressed ( left, top ) innerMsg ->
            updateInner innerMsg { model | canvasOrigin = ( left, top ) }

        WindowResize w h ->
            ( { model
                | width = w
                , height = h

                -- Orbit and pan sensitivity are relative to canvas height.
                , camera = Camera.setViewportHeight (toFloat h) model.camera
              }
            , Cmd.none
            , []
            )

        ContextMenuOpened ->
            ( model, Cmd.none, [] )

        MouseButtonDown x y button shiftHeld accelHeld ->
            if touchInputActive model then
                ( model, Cmd.none, [] )

            else
                let
                    -- Right-press during a captured left drag cancels it. Orbit
                    -- follows the right button from there on.
                    cancelling =
                        button == 2 && model.captured

                    spacePanning =
                        model.spacePan && isSpaceHeld model

                    -- A Shift + left press on a host that asked for it
                    -- (`setShiftPressGrabs`) is offered to the host as a press,
                    -- which decides between grabbing a part and panning.
                    shiftGrab =
                        model.shiftPressGrabs && shiftHeld && button == 0 && not (accelHeld || spacePanning)

                    -- Shift, the middle button and Space pan; Ctrl/Cmd and the
                    -- right button orbit.
                    panning =
                        (shiftHeld && not shiftGrab) || button == 1 || spacePanning

                    -- Only a plain left press (or a Shift + left press offered
                    -- for grabbing) may grab a part.
                    plainLeft =
                        (button == 0 && not (shiftHeld || accelHeld || spacePanning)) || shiftGrab
                in
                ( { model
                    | camera = Camera.onMouseDown x y model.camera
                    , dragTravel = 0.0
                    , cameraMode = CameraManual
                    , mousePanDragging = panning
                    , pressButton = button
                    , pressToggle = shiftHeld || accelHeld
                    , pressShift = shiftHeld
                    , mousePressed = True
                    , captured = model.captured && not cancelling
                    , lastPointer = ( x, y )
                  }
                , Cmd.none
                , if cancelling then
                    [ DragCancelled ]

                  else if plainLeft then
                    [ PressStarted x y ]

                  else
                    []
                )

        MouseMove x y ->
            if touchInputActive model then
                ( model, Cmd.none, [] )

            else if model.captured then
                ( { model | lastPointer = ( x, y ) }, Cmd.none, [ DragMoved x y ] )

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
                in
                ( { model
                    | camera = nextCamera
                    , dragTravel = model.dragTravel + stepDist

                    -- The move subscription is document-global: a hover (no
                    -- button down, the camera unchanged) is not a camera move
                    -- and must not end auto-fit.
                    , cameraMode =
                        if model.mousePressed then
                            CameraManual

                        else
                            model.cameraMode
                    , lastPointer = ( x, y )
                  }
                , Cmd.none
                , []
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
                , []
                )

            else if not model.mousePressed then
                -- The release belongs to a press that did not start on the canvas
                -- (a toolbar button, say). The subscription is document-wide, so
                -- without this every such click looked like a click on the model.
                ( model, Cmd.none, [] )

            else
                ( { model
                    | camera = Camera.onMouseUp model.camera
                    , dragTravel = 0.0
                    , captured = False
                    , mousePressed = False
                    , touchGesture = NoTouchGesture
                    , activeTouches = Dict.empty
                    , mousePanDragging = False
                  }
                , Cmd.none
                , if model.captured then
                    [ DragEnded x y ]

                  else if model.dragTravel >= 3.0 then
                    [ CameraSettled ]

                  else if model.pressButton == 0 then
                    [ Clicked x y model.pressToggle ]

                  else
                    -- Right and middle clicks select nothing.
                    []
                )

        DoubleClick x y ->
            ( model, Cmd.none, [ DoubleClicked x y ] )

        Wheel delta ->
            if model.captured then
                ( model, Cmd.none, [ WheelScrolled delta ] )

            else
                ( { model
                    | camera = Navigation.applyWheel delta model.camera
                    , cameraMode = CameraManual
                  }
                , Cmd.none
                , [ CameraSettled ]
                )

        TouchStart touches ->
            touchesDown (touchDictFromList touches) model

        TouchMove touches ->
            touchesMoved (touchDictFromList touches) model

        TouchEnd touches ->
            touchesUp (touchDictFromList touches) model

        PointerTouchStart touchPoint ->
            touchesDown (Dict.insert touchPoint.id touchPoint model.activeTouches) model

        PointerTouchMove touchPoint ->
            if Dict.member touchPoint.id model.activeTouches then
                touchesMoved (Dict.insert touchPoint.id touchPoint model.activeTouches) model

            else
                ( model, Cmd.none, [] )

        PointerTouchEnd touchId ->
            -- Guarded like PointerTouchMove: iOS Safari fires both Touch and
            -- Pointer events, and the dict is keyed by whichever family opened
            -- the gesture. Removing an unknown id used to be a no-op that still
            -- ran endTouchGesture over the remaining fingers, restarting a
            -- single-touch drag from stale coordinates.
            if Dict.member touchId model.activeTouches then
                touchesUp (Dict.remove touchId model.activeTouches) model

            else
                ( model, Cmd.none, [] )

        FileContentReceived text ->
            handleTopLevelText text model

        FileLoadError err ->
            ( { model | errorMsg = Just err, loadingProgressPct = 0, loadingProgressTick = Nothing }
            , Cmd.none
            , [ LoadFailed err ]
            )

        GeometryFlattened payload ->
            case Decode.decodeString Wire.flattenResultDecoder payload of
                Ok result ->
                    let
                        scene =
                            Scene.buildScene
                                { triangles = result.triangles
                                , lines = result.lines
                                , conditionalLines = result.conditionalLines
                                }
                                result.bfcCertified
                    in
                    ( { model
                        | scene = Just scene
                        , loadPhase = Ready
                        , loadingProgressPct = 100
                        , loadingProgressTick = Nothing
                      }
                    , Cmd.none
                    , [ ModelReady ]
                    )

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
                    , Cmd.none
                    , [ ModelReady ]
                    )

        GeometryFlattenFailed err ->
            let
                fallback =
                    buildSceneFallback model
            in
            ( { fallback | errorMsg = Just ("Geometry worker failed; used local flatten. " ++ err) }
            , Cmd.none
            , [ ModelReady ]
            )

        DismissError ->
            ( { model | errorMsg = Nothing }, Cmd.none, [] )

        TopLevelLoaded _ (Err err) ->
            failLoad ("Failed to load model: " ++ httpErrString err) model

        TopLevelLoaded _ (Ok text) ->
            if looksLikeHtmlResponse text then
                failLoad
                    "Top-level URL returned HTML instead of LDraw text. Run `make sync-ldraw` or use a direct LDraw URL."
                    model

            else
                handleTopLevelText text model

        PartLoaded name result ->
            handlePartResult name result model

        AnimationFrame now ->
            let
                progressModel =
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
                        { model
                            | loadingProgressPct = nextPct
                            , loadingProgressTick = Just now
                        }

                    else
                        { model | loadingProgressTick = Nothing }

                ( buttonModel, buttonEvents ) =
                    applyHeldControl now progressModel

                ( heldModel, keyEvents ) =
                    applyHeldKeys now buttonModel

                ( pivotModel, pivotEvents ) =
                    stepPivotTween now heldModel
            in
            ( pivotModel, Cmd.none, buttonEvents ++ keyEvents ++ pivotEvents )

        RotateCameraBy azimuthDelta elevationDelta ->
            ( { model
                | camera = Navigation.applyLook azimuthDelta elevationDelta model.camera
                , cameraMode = CameraManual
              }
            , Cmd.none
            , [ CameraSettled ]
            )

        StartHoldRotate azimuthStep elevationStep ->
            ( { model
                | heldControl = HoldRotate azimuthStep elevationStep
                , heldControlTick = Nothing
              }
            , Cmd.none
            , []
            )

        EndHoldRotate ->
            ( { model
                | heldControl = NoHeldControl
                , heldControlTick = Nothing
              }
            , Cmd.none
            , []
            )

        FitView ->
            ( frameModelBounds model, Cmd.none, [ CameraSettled ] )

        ToggleWalkMode ->
            ( toggleWalkMode model, Cmd.none, [ CameraSettled ] )

        SetViewPreset preset ->
            ( applyViewPreset preset model, Cmd.none, [ CameraSettled ] )

        SetRenderQuality quality ->
            ( applyRenderQuality quality model, Cmd.none, [] )

        KeyPressed event ->
            if Shortcuts.isTextEntryTarget event || Shortcuts.isWidgetKey event then
                -- Someone is typing into a host page's form field, or the focused
                -- control handles the key itself (Space on a button, arrows in a
                -- swatch grid). The keydown subscription is document-global, so
                -- without this a space in a URL box toggles playback in the
                -- embedded viewer.
                ( model, Cmd.none, [] )

            else
                let
                    hasModifier =
                        event.ctrl || event.meta || event.alt

                    heldModel =
                        if hasModifier then
                            model

                        else
                            { model | heldKeys = Set.insert (Shortcuts.normalizeKey event.key) model.heldKeys }
                in
                -- A held key repeats at the OS rate; movement is integrated per
                -- frame instead, so only the first press does discrete work.
                if event.repeat then
                    ( heldModel, Cmd.none, [] )

                else
                    ( heldModel, Cmd.none, [ KeyAccepted event ] )

        KeyReleased key ->
            ( { model | heldKeys = Set.remove (Shortcuts.normalizeKey key) model.heldKeys }
            , Cmd.none
            , []
            )

        AllKeysReleased ->
            -- A key-up lost to a window blur or tab switch would otherwise leave
            -- the camera walking forever.
            ( { model | heldKeys = Set.empty }, Cmd.none, [] )


{-| Stop loading and show why.
-}
failLoad : String -> Model -> ( Model, Cmd Msg, List Event )
failLoad message model =
    ( { model
        | errorMsg = Just message
        , loadPhase = Idle
        , loadingProgressPct = 0
        , loadingProgressTick = Nothing
      }
    , Cmd.none
    , [ LoadFailed message ]
    )


{-| Whether Space is currently down, for Space-to-pan.
-}
isSpaceHeld : Model -> Bool
isSpaceHeld model =
    Set.member " " model.heldKeys || Set.member "Spacebar" model.heldKeys


{-| Move and look according to the keys currently held down.

Shares `heldControlTick` with the on-screen rotate buttons so there is a single
notion of frame time, and integrates over it rather than counting key repeats —
so movement speed is independent of the OS repeat rate.

-}
applyHeldKeys : Time.Posix -> Model -> ( Model, List Event )
applyHeldKeys now model =
    -- A captured drag belongs to the host, so keys must not move the camera
    -- underneath it.
    if model.captured || not (Shortcuts.hasContinuousInput model.heldKeys) then
        ( { model | heldKeysTick = Nothing }, [] )

    else
        let
            dtSeconds =
                case model.heldKeysTick of
                    Just prev ->
                        -- Capped: flattening geometry blocks the main thread, and
                        -- an uncapped delta would spend the whole stall as one
                        -- stride and teleport you across the model.
                        clamp 0 Navigation.maxHeldKeyStep (toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0)

                    Nothing ->
                        0.016
        in
        if dtSeconds <= 0 then
            ( { model | heldKeysTick = Just now }, [] )

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
                    Navigation.keyboardTurnRate * dtSeconds

                nextCamera =
                    model.camera
                        |> Camera.moveBy
                            { forward = movement.forward * stride
                            , right = movement.right * stride
                            , up = movement.up * stride
                            }
                        |> Navigation.applyLook (look.azimuth * turn) (look.elevation * turn)

                nextModel =
                    { model
                        | camera = nextCamera
                        , cameraMode = CameraManual
                        , heldKeysTick = Just now
                    }
            in
            ( nextModel, [ CameraSettled ] )


applyHeldControl : Time.Posix -> Model -> ( Model, List Event )
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
                ( { model | heldControlTick = Just now }, [] )

            else
                let
                    baseCamera =
                        model.camera

                    nextCamera =
                        Navigation.applyLook
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
                ( nextModel, [ CameraSettled ] )

        NoHeldControl ->
            ( { model | heldControlTick = Nothing }, [] )


handlePartResult : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg, List Event )
handlePartResult name result model =
    if model.loadPhase /= ResolvingParts then
        let
            newCache =
                Resolve.updateCache name result Parser.parseFile model.partCache
        in
        case model.sync of
            Just _ ->
                -- Editing: a part fetched for a new placement. Fetch whatever it
                -- references in turn, then build the placements that are ready.
                let
                    newLines =
                        case result of
                            Ok text ->
                                Parser.parseFile text

                            Err _ ->
                                []

                    pending =
                        Resolve.pendingParts newLines newCache

                    ( reconciled, reconcileCmd ) =
                        reconcile { model | partCache = Resolve.markLoading pending newCache }
                in
                ( reconciled
                , Cmd.batch [ fetchPending model.resolverConfig pending, reconcileCmd ]
                , []
                )

            Nothing ->
                ( { model | partCache = newCache }, Cmd.none, [] )

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
            ( updatedModel, Cmd.none, [] )

        else
            ( updatedModel, fetchPending model.resolverConfig allPending, [] )


{-| Fingers went down. One finger starts a press the host may capture; a second
finger ends any capture and becomes a camera gesture.
-}
touchesDown : Dict Int TouchPoint -> Model -> ( Model, Cmd Msg, List Event )
touchesDown touches model =
    let
        list =
            touchesFromDict touches

        begun =
            beginTouchGesture list { model | activeTouches = touches }
    in
    case list of
        [ p ] ->
            ( { begun | lastPointer = ( p.x, p.y ) }, Cmd.none, [ PressStarted p.x p.y ] )

        _ :: _ :: _ ->
            if model.captured then
                ( { begun | captured = False }, Cmd.none, [ DragEnded (Tuple.first model.lastPointer) (Tuple.second model.lastPointer) ] )

            else
                ( begun, Cmd.none, [] )

        [] ->
            ( begun, Cmd.none, [] )


touchesMoved : Dict Int TouchPoint -> Model -> ( Model, Cmd Msg, List Event )
touchesMoved touches model =
    let
        list =
            touchesFromDict touches
    in
    case ( model.captured, list ) of
        ( True, p :: _ ) ->
            ( { model | activeTouches = touches, lastPointer = ( p.x, p.y ) }
            , Cmd.none
            , [ DragMoved p.x p.y ]
            )

        ( False, [ p ] ) ->
            -- Travel is kept for one finger so a tap can be told from a drag.
            let
                ( lx, ly ) =
                    model.lastPointer

                travel =
                    case model.touchGesture of
                        SingleTouchGesture _ ->
                            model.dragTravel + sqrt (((p.x - lx) * (p.x - lx)) + ((p.y - ly) * (p.y - ly)))

                        _ ->
                            model.dragTravel

                moved =
                    advanceTouchGesture list { model | activeTouches = touches }
            in
            ( { moved | dragTravel = travel, lastPointer = ( p.x, p.y ) }, Cmd.none, [] )

        _ ->
            ( advanceTouchGesture list { model | activeTouches = touches }, Cmd.none, [] )


{-| Fingers came up. A captured gesture ends with them; a one-finger gesture that
barely moved is a tap.
-}
touchesUp : Dict Int TouchPoint -> Model -> ( Model, Cmd Msg, List Event )
touchesUp touches model =
    let
        list =
            touchesFromDict touches

        ended =
            endTouchGesture list { model | activeTouches = touches }

        ( x, y ) =
            model.lastPointer

        wasTap =
            List.isEmpty list
                && model.dragTravel
                < tapSlop
                && (case model.touchGesture of
                        SingleTouchGesture _ ->
                            True

                        _ ->
                            False
                   )
    in
    if model.captured then
        ( { ended | captured = False }, Cmd.none, [ DragEnded x y ] )

    else if wasTap then
        ( ended, Cmd.none, [ Tapped x y, CameraSettled ] )

    else
        ( ended, Cmd.none, [ CameraSettled ] )


{-| How far, in pixels, a finger may wander and still count as a tap.
-}
tapSlop : Float
tapSlop =
    8.0


{-| Let a Shift + left press on empty canvas or on a part reach the host as
`PressStarted`, instead of always panning. The host then decides: `captureDrag`
to grab the part, or `panCurrentGesture` to pan. Off by default, so Shift +
drag pans (studio, the viewer). Brick Play turns it on
(through `Studio.Core.setDragMode`). A Shift press that is not captured and does
not travel is still a `Clicked` with the toggle flag set.
-}
setShiftPressGrabs : Bool -> Model -> Model
setShiftPressGrabs enabled model =
    { model | shiftPressGrabs = enabled }


{-| Whether the press that started the current mouse gesture had Shift held.
Lets a host that receives `PressStarted` for a Shift press tell it apart from a
plain press, even when its own key tracking missed the Shift key-down.
-}
pressedWithShift : Model -> Bool
pressedWithShift model =
    model.pressShift


{-| Let Space + drag pan the camera, for hosts that use Space for something else
otherwise (studio workspace). Off by default.
-}
setSpacePan : Bool -> Model -> Model
setSpacePan enabled model =
    { model | spacePan = enabled }


{-| Take the current mouse or touch gesture for the host: the camera stops
following it, and the host receives `DragMoved` and `DragEnded` instead. Call
from the handler of `PressStarted`.
-}
captureDrag : Model -> Model
captureDrag model =
    { model | captured = True, camera = Camera.onMouseUp model.camera }


{-| Turn the gesture in progress into a pan, as if it had started with Shift
held: the rest of a plain left drag (or a one-finger drag) moves the camera
target with the pointer instead of orbiting. Call from the handler of
`PressStarted` — for a press that hit nothing the host wants to grab, say, so
left-drag on empty canvas pans.

The gesture stays the viewport's: a press-release without travel is still a
`Clicked` (or `Tapped`), and a drag ends in `CameraSettled`. Does nothing for a
captured gesture, a two-finger gesture, or when no button or finger is down.

-}
panCurrentGesture : Model -> Model
panCurrentGesture model =
    let
        oneFinger =
            case model.touchGesture of
                SingleTouchGesture _ ->
                    True

                _ ->
                    False
    in
    if model.captured then
        model

    else if oneFinger || (model.mousePressed && not (touchInputActive model)) then
        { model | mousePanDragging = True }

    else
        model


{-| Whether the gesture in progress is a touch (one or more fingers) rather than
the mouse. Lets a `PressStarted` handler treat fingers and the mouse apart.
-}
isTouchGesture : Model -> Bool
isTouchGesture =
    touchInputActive


beginTouchGesture : List TouchPoint -> Model -> Model
beginTouchGesture touches model =
    case touches of
        p1 :: p2 :: _ ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture (beginPinch p1 p2)
                , cameraMode = CameraManual
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        p1 :: [] ->
            { model
                | camera = Camera.onMouseDown p1.x p1.y model.camera
                , touchGesture = SingleTouchGesture p1.id
                , cameraMode = CameraManual
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        [] ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = NoTouchGesture
                , dragTravel = 0.0
                , mousePanDragging = False
            }


advanceTouchGesture : List TouchPoint -> Model -> Model
advanceTouchGesture touches model =
    case model.touchGesture of
        SingleTouchGesture touchId ->
            case findTouch touchId touches of
                Just point ->
                    let
                        -- `panCurrentGesture` makes one finger pan, like Shift + drag.
                        nextCamera =
                            case ( model.mousePanDragging, model.camera.lastMousePos ) of
                                ( True, Just ( lx, ly ) ) ->
                                    Camera.onPan (point.x - lx) (point.y - ly) model.camera
                                        |> (\cam -> { cam | lastMousePos = Just ( point.x, point.y ) })

                                _ ->
                                    Camera.onMouseMove point.x point.y model.camera
                    in
                    { model
                        | camera = nextCamera
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
                                Navigation.applyPinchZoom (pinch.distance / newDistance) cameraAfterPan

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
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        [ p1 ] ->
            { model
                | camera = Camera.onMouseDown p1.x p1.y (Camera.onMouseUp model.camera)
                , touchGesture = SingleTouchGesture p1.id
                , cameraMode = CameraManual
                , dragTravel = 0.0
                , mousePanDragging = False
            }

        p1 :: p2 :: _ ->
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture (beginPinch p1 p2)
                , cameraMode = CameraManual
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
                    Framing.cameraForBounds model.width model.height bounds.center bounds.radius model.camera
                        -- Framing the whole model is an outside-in view; walking
                        -- with a 7500 LDU pivot in front of you is not.
                        |> Camera.setNavigation Camera.Orbit
                , cameraMode = CameraAutoFit
            }

        Nothing ->
            model


{-| Orbit around `target` (render space, Y up) from now on, by sliding the view
onto it: see `Render.Camera.repivot`. Azimuth, elevation and distance are kept —
no rotation, no zoom — so the new pivot ends up in the centre of the screen and
screen-relative directions (camera-left, away) do not change. The camera becomes
manual, so auto-fit no longer overrides it; `FitView` recentres on the model
again.

The view slides over a few animation frames rather than snapping. Any other
camera change, or a drag capturing the pointer, ends the glide where it is.

A no-op before any model content has been measured (the first content still
auto-fits) and in walk navigation.

-}
pivotOn : Vec3.Vec3 -> Model -> Model
pivotOn target model =
    case model.modelBounds of
        Just _ ->
            if Camera.repivot target model.camera == model.camera then
                model

            else
                { model
                    | cameraMode = CameraManual
                    , pivotTween =
                        Just
                            { from = model.camera.target
                            , to = target
                            , base = model.camera
                            , last = model.camera
                            , startedAt = Nothing
                            }
                }

        Nothing ->
            model


{-| Advance a running `pivotOn` glide by one animation frame.
-}
stepPivotTween : Time.Posix -> Model -> ( Model, List Event )
stepPivotTween now model =
    case model.pivotTween of
        Just tween ->
            if model.captured || model.camera /= tween.last then
                ( { model | pivotTween = Nothing }, [] )

            else
                let
                    started =
                        Maybe.withDefault now tween.startedAt

                    elapsed =
                        toFloat (Time.posixToMillis now - Time.posixToMillis started)

                    s =
                        clamp 0 1 (elapsed / pivotTweenMillis)

                    eased =
                        s * s * (3 - 2 * s)

                    target =
                        Vec3.add tween.from (Vec3.scale eased (Vec3.sub tween.to tween.from))

                    camera =
                        Camera.repivot target tween.base
                in
                if s >= 1 then
                    ( { model | camera = camera, pivotTween = Nothing }, [ CameraSettled ] )

                else
                    ( { model | camera = camera, pivotTween = Just { tween | last = camera, startedAt = Just started } }, [] )

        Nothing ->
            ( model, [] )


{-| Drop to street level, or step back out to orbit.

Needs the model bounds to know where the ground is, so it is a no-op before a
model has loaded.

-}
toggleWalkMode : Model -> Model
toggleWalkMode model =
    case ( model.camera.navigation, model.modelBounds ) of
        ( Camera.Orbit, Just bounds ) ->
            { model
                | camera = Camera.enterWalk (Framing.groundLevel bounds) model.camera
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
                    Framing.viewPresetAngles preset

                framed =
                    Framing.cameraForBounds model.width model.height bounds.center bounds.radius model.camera
            in
            { model
                | camera =
                    { framed | azimuth = azimuth, elevation = elevation }
                        |> Camera.setNavigation Camera.Orbit
                , cameraMode = CameraManual
            }

        Nothing ->
            model


{-| Shared handler for HTTP-fetched and file-uploaded LDraw text.
-}
handleTopLevelText : String -> Model -> ( Model, Cmd Msg, List Event )
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

        cleanedCache =
            Set.foldl Dict.remove model.partCache model.submodelNames

        ( seededCache, lines ) =
            if isMpd then
                let
                    ( newCache, mainFile ) =
                        Resolve.seedFromMpd text cleanedCache

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
                ( cleanedCache, Parser.parseFile text )
    in
    beginLoad { lines = lines, cache = seededCache, submodelNames = submodelNames, document = Nothing } model


{-| Start loading a model whose top-level lines and embedded submodels are
already known: resolve every part they need, then hand over to `finishLoading`.
-}
beginLoad :
    { lines : List LDrawLine
    , cache : PartCache
    , submodelNames : Set String
    , document : Maybe Document
    }
    -> Model
    -> ( Model, Cmd Msg, List Event )
beginLoad { lines, cache, submodelNames, document } model =
    let
        -- Also scan embedded submodels (seeded from MPD) for their own dependencies,
        -- since those lines are never passed through handlePartResult's additionalPending.
        seededSubmodelLines =
            cache
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
            Resolve.pendingParts (lines ++ seededSubmodelLines) cache

        cacheWithLoading =
            Resolve.markLoading pending cache

        loadingModel =
            { model
                | topLevelLines = lines
                , partCache = cacheWithLoading
                , submodelNames = submodelNames
                , document = document
                , sync = Nothing
                , activePath = []
                , activeSources = Nothing
                , loadPhase = ResolvingParts
                , partsTotal = List.length pending
                , partsLoaded = 0
                , scene = Nothing
                , instancedScene = Nothing
                , modelBounds = Nothing
                , errorMsg = Nothing
                , dragTravel = 0.0
                , loadingProgressPct = partsProgressPct 0 (List.length pending)
                , loadingProgressTick = Nothing
                , heldControl = NoHeldControl
                , heldControlTick = Nothing
            }
    in
    if List.isEmpty pending then
        finishLoading loadingModel
            |> prependEvent LoadStarted

    else
        ( loadingModel, fetchPending model.resolverConfig pending, [ LoadStarted ] )


{-| Load an editable document. Unlike a plain file, a document is always shown
with the instanced renderer — every placement stays individually addressable —
and later changes go through `applyDocument`.

The document's submodels other than the main one are seeded into the part cache,
so references to them expand in place.

-}
loadDocument : Document -> Model -> ( Model, Cmd Msg, List Event )
loadDocument document model =
    let
        subfiles =
            Document.subfiles document

        cleanedCache =
            Set.foldl Dict.remove model.partCache model.submodelNames

        seeded =
            List.foldl
                (\( name, lines ) acc -> Dict.insert name (Loaded lines) acc)
                cleanedCache
                subfiles
    in
    beginLoad
        { lines = Document.toLines document
        , cache = seeded
        , submodelNames = Set.fromList (List.map Tuple.first subfiles)
        , document = Just document
        }
        model


{-| Bring the scene in line with an edited document.

Edits to the main submodel touch only the placements that changed: the instanced
scene swaps those sources in place and rebuilds just the proxy cells they were
in. Parts that have not been seen before are fetched first; the placement
appears once they arrive. An edit inside another submodel reloads the document,
keeping the camera where it is.

A document that arrives while the previous one is still loading is picked up
when that load finishes.

-}
applyDocument : Document -> Model -> ( Model, Cmd Msg, List Event )
applyDocument document model =
    case model.sync of
        Nothing ->
            if model.document == Nothing then
                loadDocument document model

            else
                -- Still loading: finishLoadingDocument builds from the latest.
                ( { model | document = Just document }, Cmd.none, [] )

        Just _ ->
            let
                updatedCache =
                    Sync.updateSubmodelCache document model.partCache

                updatedSubmodelNames =
                    document.submodels
                        |> List.drop 1
                        |> List.map (.name >> Document.normalise)
                        |> Set.fromList

                ( reconciled, cmd ) =
                    reconcile
                        { model
                            | document = Just document
                            , partCache = updatedCache
                            , submodelNames = updatedSubmodelNames
                        }
            in
            ( reconciled, cmd, [] )


{-| Apply whatever `Viewport.Sync.plan` says is out of date.
-}
reconcile : Model -> ( Model, Cmd Msg )
reconcile model =
    case ( model.sync, model.instancedScene, model.document ) of
        ( Just state, Just scene, Just document ) ->
            let
                plan =
                    Sync.plan model.partCache model.submodelNames document state

                afterRemoval =
                    List.foldl Instanced.removeSource scene plan.removed

                nextScene =
                    List.foldl
                        (\sourced acc -> Instanced.replaceSource model.partCache sourced.source sourced.raws acc)
                        afterRemoval
                        plan.replaced

                built =
                    List.foldl
                        (\sourced acc -> Dict.insert sourced.source sourced.fingerprint acc)
                        (List.foldl Dict.remove state.built plan.removed)
                        plan.replaced

                nextState =
                    { state
                        | document = document
                        , built = built
                        , pending = plan.pending
                        , readyParts = plan.readyParts
                        , subfilesSignature = Document.subfilesSignature document
                    }

                nextActiveSources =
                    if List.isEmpty nextState.activePath then
                        model.activeSources

                    else
                        Sync.activeSourcesFor nextState.activePath document

                unchanged =
                    List.isEmpty plan.removed && List.isEmpty plan.replaced && List.isEmpty plan.missing
            in
            if unchanged then
                ( { model | sync = Just nextState, activeSources = nextActiveSources }, Cmd.none )

            else
                ( frameEditedScene
                    { model
                        | instancedScene = Just nextScene
                        , sync = Just nextState
                        , activeSources = nextActiveSources
                        , partCache = Resolve.markLoading plan.missing model.partCache
                    }
                , fetchPending model.resolverConfig plan.missing
                )

        _ ->
            ( model, Cmd.none )


{-| After an edit, keep the zoom range and far plane matched to the model, and
frame the camera on the first content a document gets. Later edits leave the
camera alone: it is the user's.
-}
frameEditedScene : Model -> Model
frameEditedScene model =
    case model.instancedScene of
        Just scene ->
            if List.isEmpty scene.placements then
                { model | modelBounds = Nothing }

            else
                let
                    bounds =
                        Framing.modelBoundsFromInstanced scene

                    firstContent =
                        model.modelBounds == Nothing

                    camera =
                        if firstContent && model.cameraMode == CameraAutoFit then
                            Framing.cameraForBounds model.width model.height bounds.center bounds.radius model.camera

                        else
                            Framing.applyZoomRange model.width model.height bounds.radius model.camera
                in
                { model | camera = camera, modelBounds = Just bounds }

        Nothing ->
            model


{-| Document path of `finishLoading`: build the instanced scene with every main
placement tagged by its document id.
-}
finishLoadingDocument : Document -> Model -> ( Model, Cmd Msg, List Event )
finishLoadingDocument document model =
    let
        sourced =
            Sync.buildAll model.partCache model.submodelNames document

        instanced =
            Instanced.buildFrom model.partCache
                (List.concatMap (\s -> List.map (Tuple.pair s.source) s.raws) sourced)

        ready =
            Set.fromList (Dict.keys instanced.parts)

        initialSync =
            Sync.initialState document ready sourced

        loaded =
            { model
                | instancedScene = Just instanced
                , scene = Nothing
                , sync = Just { initialSync | activePath = model.activePath }
                , loadPhase = Ready
                , loadingProgressPct = 100
                , loadingProgressTick = Nothing
            }

        -- A document that was edited while loading may differ from `document`.
        ( reconciled, cmd ) =
            reconcile { loaded | document = model.document }
    in
    ( frameEditedScene reconciled, cmd, [ ModelReady ] )


type CandidateKind
    = MeshCandidate
        { mesh : Instanced.PartMesh
        , localOrigin : Vec3.Vec3
        , localDir : Vec3.Vec3
        , isFar : Bool
        }
    | FallbackCandidate


{-| The document placement under a screen point, nearest first.

Placements are culled by bounding sphere, then by ray-AABB in each part's local
coordinates, and finally hit-tested against the part's triangles: the visually
nearest surface wins, so a brick seen through the empty corners of a gear's box
is picked rather than the gear. Candidates are visited in box-entry order, so a
part whose box starts behind the best hit so far is never triangle-tested.
Surfaces at the same depth (within `pickDepthTolerance`) go to the smaller part
(e.g. brick on baseplate). A ray that touches no triangle picks nothing.
When ghost mode is active (`activeSources /= Nothing`), only placements
belonging to the active submodel are pickable.

The local ray direction is the inverse-transformed world direction, unnormalised,
so a local `t` is also the world distance along the (unit) world ray.

-}
pickSource : Float -> Float -> Model -> Maybe Int
pickSource x y model =
    pickPoint x y model |> Maybe.map .source


{-| `pickSource` with where the ray met the part: the world-space (render, Y up)
point under the pointer. A host dragging a part by its surface keeps that point
under the cursor.
-}
pickPoint : Float -> Float -> Model -> Maybe { source : Int, point : Vec3.Vec3 }
pickPoint =
    pickWith False


{-| The body-drag pick: `pickPoint`, then, when the ray touches no triangle,
the axle-hole fallback described at `axleHoleFraction`. Used to grab a part by
its body; click selection keeps using `pickPoint`.
-}
pickBodyPoint : Float -> Float -> Model -> Maybe { source : Int, point : Vec3.Vec3 }
pickBodyPoint =
    pickWith True


{-| Fraction of a placement's bounding radius within which a ray that touches no
triangle still hits the part, measured from the part's centre to the ray's
closest approach. A gear's axle hole is empty space in the mesh, so a press on
it would otherwise pan the camera instead of grabbing the gear.
-}
axleHoleFraction : Float
axleHoleFraction =
    0.35


pickWith : Bool -> Float -> Float -> Model -> Maybe { source : Int, point : Vec3.Vec3 }
pickWith fallback x y model =
    case ( model.instancedScene, Pick.screenRay model.camera model.width model.height x y ) of
        ( Just scene, Just ( origin, direction ) ) ->
            let
                nearPlane =
                    Camera.nearPlane model.camera

                eye =
                    Camera.position model.camera

                isPickable p =
                    p.source
                        >= 0
                        && (case model.activeSources of
                                Just sources ->
                                    Set.member p.source sources

                                Nothing ->
                                    True
                           )

                -- Sphere first: inverting every placement's matrix on each
                -- click is what makes picking slow on a city-scale model.
                boxCandidate p =
                    Pick.raySphereHit origin direction p.center p.radius
                        |> Maybe.andThen
                            (\_ ->
                                case ( Dict.get p.part scene.parts, Mat4.inverse p.matrix ) of
                                    ( Just mesh, Just inv ) ->
                                        let
                                            localOrigin =
                                                Mat4.transform inv origin

                                            localDir =
                                                Vec3.sub (Mat4.transform inv (Vec3.add origin direction)) localOrigin
                                        in
                                        Pick.rayAabbHitFrom nearPlane localOrigin localDir mesh.boundsMin mesh.boundsMax
                                            |> Maybe.map
                                                (\boxT ->
                                                    let
                                                        diameter =
                                                            2
                                                                * Instanced.projectedRadiusPx (toFloat model.height)
                                                                    (Vec3.distance eye p.center)
                                                                    p.radius

                                                        isFar =
                                                            Instanced.detailFor diameter == Instanced.Far
                                                    in
                                                    { boxT = boxT
                                                    , placement = p
                                                    , kind =
                                                        MeshCandidate
                                                            { mesh = mesh
                                                            , localOrigin = localOrigin
                                                            , localDir = localDir
                                                            , isFar = isFar
                                                            }
                                                    }
                                                )

                                    _ ->
                                        let
                                            rVec =
                                                Vec3.vec3 p.radius p.radius p.radius

                                            boxMin =
                                                Vec3.sub p.center rVec

                                            boxMax =
                                                Vec3.add p.center rVec
                                        in
                                        Pick.rayAabbHitFrom nearPlane origin direction boxMin boxMax
                                            |> Maybe.map
                                                (\boxT ->
                                                    { boxT = boxT
                                                    , placement = p
                                                    , kind = FallbackCandidate
                                                    }
                                                )
                            )

                surfaceHit c =
                    case c.kind of
                        FallbackCandidate ->
                            Just c.boxT

                        MeshCandidate m ->
                            if m.isFar then
                                Just c.boxT

                            else if List.isEmpty m.mesh.surface then
                                Just c.boxT

                            else
                                Pick.nearestTriangleHitByFrom nearPlane .position m.localOrigin m.localDir m.mesh.surface

                closer c best =
                    case best of
                        Just ( bestT, bestPlacement ) ->
                            if c.boxT > bestT + pickDepthTolerance then
                                best

                            else
                                case surfaceHit c of
                                    Just t ->
                                        if t >= nearPlane && t < bestT - pickDepthTolerance then
                                            Just ( t, c.placement )

                                        else if t >= nearPlane && t <= bestT + pickDepthTolerance && c.placement.radius < bestPlacement.radius then
                                            Just ( t, c.placement )

                                        else
                                            best

                                    Nothing ->
                                        best

                        Nothing ->
                            case surfaceHit c of
                                Just t ->
                                    if t >= nearPlane then
                                        Just ( t, c.placement )

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
            in
            let
                candidates =
                    scene.placements
                        |> List.filter isPickable
                        |> List.filterMap boxCandidate
                        |> List.sortBy .boxT

                -- Where the ray passes closest to the centre, if that is near enough.
                axleHole =
                    candidates
                        |> List.filter
                            (\c ->
                                let
                                    towardCentre =
                                        Vec3.sub c.placement.center origin

                                    along =
                                        Vec3.dot towardCentre direction

                                    offAxis =
                                        Vec3.length (Vec3.sub towardCentre (Vec3.scale along direction))
                                in
                                along >= nearPlane && offAxis <= axleHoleFraction * c.placement.radius
                            )
                        |> List.head
                        |> Maybe.map
                            (\c ->
                                { source = c.placement.source
                                , point =
                                    Vec3.add origin
                                        (Vec3.scale (Vec3.dot (Vec3.sub c.placement.center origin) direction) direction)
                                }
                            )
            in
            case List.foldl closer Nothing candidates of
                Just ( t, p ) ->
                    Just { source = p.source, point = Vec3.add origin (Vec3.scale t direction) }

                Nothing ->
                    if fallback then
                        axleHole

                    else
                        Nothing

        _ ->
            Nothing


{-| World-space intersection of the screen ray with the horizontal ground plane Y = 0.
-}
groundIntersection : Float -> Float -> Model -> Maybe Vec3.Vec3
groundIntersection x y model =
    Pick.screenRay model.camera model.width model.height x y
        |> Maybe.andThen (\( origin, direction ) -> Pick.rayGroundHit origin direction)
        |> Maybe.map Tuple.second


{-| Intersect the screen ray with scene geometry, excluding placements whose
`source` is in `excludedIds`. When geometry is hit, returns the hit source ID,
the world point under the pointer (render space, Y up), and the top bounding
elevation of the hit placement in LDraw coordinates (`topY`).
When no placements are hit, falls back to the horizontal ground plane (Y = 0)
with source -1 and topY 0.0.
-}
pickSurfaceExcluding : Set Int -> Float -> Float -> Model -> Maybe { source : Int, point : Vec3.Vec3, topY : Float }
pickSurfaceExcluding excludedIds x y model =
    case Pick.screenRay model.camera model.width model.height x y of
        Just ( origin, direction ) ->
            let
                nearPlane =
                    Camera.nearPlane model.camera

                eye =
                    Camera.position model.camera

                isPickable p =
                    p.source
                        >= 0
                        && not (Set.member p.source excludedIds)
                        && (case model.activeSources of
                                Just sources ->
                                    Set.member p.source sources

                                Nothing ->
                                    True
                           )

                boxCandidate scene p =
                    Pick.raySphereHit origin direction p.center p.radius
                        |> Maybe.andThen
                            (\_ ->
                                case ( Dict.get p.part scene.parts, Mat4.inverse p.matrix ) of
                                    ( Just mesh, Just inv ) ->
                                        let
                                            localOrigin =
                                                Mat4.transform inv origin

                                            localDir =
                                                Vec3.sub (Mat4.transform inv (Vec3.add origin direction)) localOrigin
                                        in
                                        Pick.rayAabbHitFrom nearPlane localOrigin localDir mesh.boundsMin mesh.boundsMax
                                            |> Maybe.map
                                                (\boxT ->
                                                    let
                                                        diameter =
                                                            2
                                                                * Instanced.projectedRadiusPx (toFloat model.height)
                                                                    (Vec3.distance eye p.center)
                                                                    p.radius

                                                        isFar =
                                                            Instanced.detailFor diameter == Instanced.Far
                                                    in
                                                    { boxT = boxT
                                                    , placement = p
                                                    , kind =
                                                        MeshCandidate
                                                            { mesh = mesh
                                                            , localOrigin = localOrigin
                                                            , localDir = localDir
                                                            , isFar = isFar
                                                            }
                                                    }
                                                )

                                    _ ->
                                        let
                                            rVec =
                                                Vec3.vec3 p.radius p.radius p.radius

                                            boxMin =
                                                Vec3.sub p.center rVec

                                            boxMax =
                                                Vec3.add p.center rVec
                                        in
                                        Pick.rayAabbHitFrom nearPlane origin direction boxMin boxMax
                                            |> Maybe.map
                                                (\boxT ->
                                                    { boxT = boxT
                                                    , placement = p
                                                    , kind = FallbackCandidate
                                                    }
                                                )
                            )

                surfaceHit c =
                    case c.kind of
                        FallbackCandidate ->
                            Just c.boxT

                        MeshCandidate m ->
                            if m.isFar then
                                Just c.boxT

                            else if List.isEmpty m.mesh.surface then
                                Just c.boxT

                            else
                                Pick.nearestTriangleHitByFrom nearPlane .position m.localOrigin m.localDir m.mesh.surface

                closer c best =
                    case best of
                        Just ( bestT, bestPlacement ) ->
                            if c.boxT > bestT + pickDepthTolerance then
                                best

                            else
                                case surfaceHit c of
                                    Just t ->
                                        if t >= nearPlane && t < bestT - pickDepthTolerance then
                                            Just ( t, c.placement )

                                        else if t >= nearPlane && t <= bestT + pickDepthTolerance && c.placement.radius < bestPlacement.radius then
                                            Just ( t, c.placement )

                                        else
                                            best

                                    Nothing ->
                                        best

                        Nothing ->
                            case surfaceHit c of
                                Just t ->
                                    if t >= nearPlane then
                                        Just ( t, c.placement )

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing

                placementTopY scene p =
                    case Dict.get p.part scene.parts of
                        Just mesh ->
                            let
                                minY =
                                    if Vec3.getY mesh.boundsMin < -0.1 then
                                        0

                                    else
                                        Vec3.getY mesh.boundsMin

                                minX =
                                    Vec3.getX mesh.boundsMin

                                maxX =
                                    Vec3.getX mesh.boundsMax

                                maxY =
                                    Vec3.getY mesh.boundsMax

                                minZ =
                                    Vec3.getZ mesh.boundsMin

                                maxZ =
                                    Vec3.getZ mesh.boundsMax

                                corners =
                                    [ Vec3.vec3 minX minY minZ
                                    , Vec3.vec3 minX minY maxZ
                                    , Vec3.vec3 minX maxY minZ
                                    , Vec3.vec3 minX maxY maxZ
                                    , Vec3.vec3 maxX minY minZ
                                    , Vec3.vec3 maxX minY maxZ
                                    , Vec3.vec3 maxX maxY minZ
                                    , Vec3.vec3 maxX maxY maxZ
                                    ]

                                worldCorners =
                                    List.map (Mat4.transform p.matrix) corners

                                maxRenderY =
                                    List.foldl (\pt acc -> max acc (Vec3.getY pt)) -1.0e30 worldCorners
                            in
                            -maxRenderY

                        Nothing ->
                            -(Vec3.getY p.center)

                maybeSceneHit =
                    model.instancedScene
                        |> Maybe.andThen
                            (\scene ->
                                scene.placements
                                    |> List.filter isPickable
                                    |> List.filterMap (boxCandidate scene)
                                    |> List.sortBy .boxT
                                    |> List.foldl closer Nothing
                                    |> Maybe.map
                                        (\( t, p ) ->
                                            { source = p.source
                                            , point = Vec3.add origin (Vec3.scale t direction)
                                            , topY = placementTopY scene p
                                            }
                                        )
                            )
            in
            case maybeSceneHit of
                Just hit ->
                    Just hit

                Nothing ->
                    Pick.rayGroundHit origin direction
                        |> Maybe.map
                            (\( _, groundPt ) ->
                                { source = -1
                                , point = groundPt
                                , topY = 0.0
                                }
                            )

        Nothing ->
            Nothing


{-| How close, in LDU, two surface hits must be to count as the same depth.
-}
pickDepthTolerance : Float
pickDepthTolerance =
    0.01


{-| Called when all parts have been resolved. Build the scene and gear graph.

Model size is measured first, because every step below — gear detection, the
auto-fit flatten, and the geometry worker — walks the fully expanded part tree
and none of them are viable at city scale. Above `Instanced` threshold the model
takes the instanced path instead; see `finishLoadingInstanced`.

-}
finishLoading : Model -> ( Model, Cmd Msg, List Event )
finishLoading model =
    case model.document of
        Just document ->
            finishLoadingDocument document model

        Nothing ->
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

Nothing is left for the host to analyse — a model this big is a display piece,
not a mechanism — so this finishes the load by itself and reports `LargeModel`.

-}
finishLoadingInstanced : List Instanced.RawPlacement -> Model -> ( Model, Cmd Msg, List Event )
finishLoadingInstanced placements model =
    let
        instanced =
            Instanced.build model.partCache placements

        bounds =
            Framing.modelBoundsFromInstanced instanced

        nextCamera =
            case model.cameraMode of
                CameraAutoFit ->
                    Framing.cameraForBounds model.width model.height bounds.center bounds.radius model.camera

                CameraManual ->
                    Framing.applyZoomRange model.width model.height bounds.radius model.camera
    in
    ( { model
        | camera = nextCamera
        , scene = Nothing
        , instancedScene = Just instanced
        , modelBounds = Just bounds
        , loadPhase = Ready
        , loadingProgressPct = 100
        , loadingProgressTick = Nothing
      }
    , Cmd.none
    , [ LargeModel (List.length placements), ModelReady ]
    )


{-| Original path, first half: measure the model and frame the camera, then ask
the host to analyse the lines. The load resumes at `beginFlatten`.
-}
finishLoadingBaked : Model -> ( Model, Cmd Msg, List Event )
finishLoadingBaked model =
    let
        bounds =
            Framing.modelBoundsFromLines model.topLevelLines model.partCache

        nextCamera =
            case ( model.cameraMode, bounds ) of
                ( CameraAutoFit, Just b ) ->
                    Framing.cameraForBounds model.width model.height b.center b.radius model.camera

                ( CameraManual, Just b ) ->
                    Framing.applyZoomRange model.width model.height b.radius model.camera

                _ ->
                    model.camera
    in
    ( { model
        | camera = nextCamera
        , modelBounds = bounds
        , scene = Nothing
      }
    , Cmd.none
    , [ PartsReady ]
    )


{-| Original path, second half: bake the host's static lines in the geometry
worker.

`lines` are what the worker flattens; `fallbackLines` are what the local flatten
uses if the worker fails. They differ because the host leaves moving parts out
of both, and only the worker path also leaves out parts that ride on them.

-}
beginFlatten : { lines : List LDrawLine, fallbackLines : List LDrawLine } -> Model -> ( Model, List Event )
beginFlatten { lines, fallbackLines } model =
    ( { model
        | loadPhase = FlatteningGeometry
        , fallbackLines = fallbackLines
        , loadingProgressPct = 90
        , loadingProgressTick = Nothing
      }
    , [ FlattenRequested (Wire.flattenRequest lines model.partCache) ]
    )


{-| Put an event in front of those a step already produced.
-}
prependEvent : Event -> ( Model, Cmd Msg, List Event ) -> ( Model, Cmd Msg, List Event )
prependEvent event ( model, cmd, events ) =
    ( model, cmd, event :: events )


fetchPending : ResolverConfig -> List String -> Cmd Msg
fetchPending resolver names =
    names
        |> List.map (\name -> Resolve.fetchPart resolver name PartLoaded)
        |> Cmd.batch


buildSceneFallback : Model -> Model
buildSceneFallback model =
    let
        geom =
            Geometry.flatten model.fallbackLines model.partCache 15 Mat4.identity

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


{-| Whether the viewport is doing per-frame work and therefore needs animation
frames. When this is `False` the `onAnimationFrame` subscription is dropped
entirely so a static model stops forcing a full update/view/diff at 60 fps.

The cases mirror the only branches of the `AnimationFrame` handler that do real
work: driving the flattening progress bar, applying a held rotate button or held
keys, and spending down the instanced renderer's settle countdown so the refined
frame gets drawn after the camera stops.

-}
needsAnimationFrame : Model -> Bool
needsAnimationFrame model =
    (model.loadPhase == FlatteningGeometry)
        || (model.pivotTween /= Nothing)
        || model.refineFrames
        > 0
        || Shortcuts.hasContinuousInput model.heldKeys
        || (case model.heldControl of
                NoHeldControl ->
                    False

                HoldRotate _ _ ->
                    True
           )


{-| Browser-event subscriptions: mouse drags, keys, window resize and animation
frames. Port-backed inputs (file contents, worker replies, host resize) belong to
the host, which forwards them as `Msg`s.
-}
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
        , Browser.Events.onKeyDown (Decode.map KeyPressed ViewportDecode.keyEventDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyReleased (Decode.field "key" Decode.string))

        -- Losing focus swallows the matching keyup, which would leave a held
        -- movement key stuck down for the rest of the session.
        , Browser.Events.onVisibilityChange (\_ -> AllKeysReleased)
        , if model.useWindowResize then
            Browser.Events.onResize WindowResize

          else
            Sub.none
        , if needsAnimationFrame model then
            Browser.Events.onAnimationFrame AnimationFrame

          else
            Sub.none
        ]



-- ── View ──────────────────────────────────────────────────────────────────────


{-| Set active placement sources for ghosted context rendering.
When set to `Just sources`, only placements matching those sources are rendered normally,
while all other geometry is rendered in ghost mode. `Nothing` disables ghost mode.
-}
setActiveSources : Maybe (Set Int) -> Model -> Model
setActiveSources sources model =
    { model | activeSources = sources }


{-| Placement sources the instanced render leaves out, for a host that draws
those placements itself — Brick Play's Technic mode hides the gears it animates so the
static copies do not show through the turning ones. Rendering only: hidden
placements stay pickable, collide and snap as before. `Set.empty` (the default)
draws everything. Applies to the instanced scene (documents); placements far
enough away to fall into a merged proxy chunk (only when every part of its
cell is that small) are still drawn by that chunk.
-}
setHiddenSources : Set Int -> Model -> Model
setHiddenSources sources model =
    { model | hiddenSources = sources }


{-| The `captureRender` request for the offline path tracer: the viewport's
visible triangles as drawn, followed by `extraTriangles` (flat, 14 floats per
triangle, from e.g. `Simulation.snapshotTriangles`) for host-owned moving parts.
-}
snapshotRequest : { filename : String, extraTriangles : List Float } -> Model -> Encode.Value
snapshotRequest config model =
    Snapshot.encodeRequest
        { filename = config.filename
        , camera = model.camera
        , triangles =
            Snapshot.sceneTriangles
                { scene = model.scene
                , instanced = model.instancedScene
                , hidden = model.hiddenSources
                , activeSources = model.activeSources
                }
                ++ config.extraTriangles
        }


{-| Where the canvas's top-left corner sits in the window, in CSS pixels.

Canvas `mousedown` and `pointerdown` events dynamically detect the canvas
origin via `clientX - offsetX` and `clientY - offsetY` on every press.
Explicitly setting `canvasOrigin` via `setCanvasOrigin` remains available as
an optimisation or override (e.g. before the first press).

-}
setCanvasOrigin : Float -> Float -> Model -> Model
setCanvasOrigin left top model =
    { model | canvasOrigin = ( left, top ) }


{-| Set the active submodel instance path (chain of placement ids from the main
submodel down to the active submodel). Opens up the active submodel's placements
as individually addressable sources and ghosts everything outside the active
instance. Passing `[]` returns to the top level.
-}
setActivePath : List Int -> Model -> Model
setActivePath activePath model =
    case ( model.sync, model.document ) of
        ( Just state, Just document ) ->
            reconcile
                { model
                    | activePath = activePath
                    , sync = Just { state | activePath = activePath }
                    , activeSources = Sync.activeSourcesFor activePath document
                }
                |> Tuple.first

        ( Nothing, Just document ) ->
            { model
                | activePath = activePath
                , activeSources = Sync.activeSourcesFor activePath document
            }

        _ ->
            { model | activePath = activePath, activeSources = Nothing }


{-| How the host decorates the canvas.

`extraEntities camera style aspect` adds host-owned entities — moving gears, a
selection outline — on top of the scene. `coarsenWhileMoving`
trades detail for latency while the camera moves — turn it off when the host
redraws continuously anyway, as a running simulation does.

-}
type alias ViewConfig =
    { extraEntities : Camera -> Style.Style -> Float -> List WebGL.Entity
    , coarsenWhileMoving : Bool
    }


{-| Switch the viewport's rendering quality preset (e.g. Draft or Realistic).
-}
setRenderQuality : Style.Quality -> Model -> Model
setRenderQuality quality model =
    applyRenderQuality quality model


{-| The current rendering quality preset of the viewport, as last set by
`setRenderQuality`. A host that customises `renderStyle` keeps its quality
label here, independent of the style's values.
-}
renderQuality : Model -> Style.Quality
renderQuality model =
    model.renderQuality


{-| Set both the quality preset and the style it maps to.
-}
applyRenderQuality : Style.Quality -> Model -> Model
applyRenderQuality quality model =
    { model
        | renderQuality = quality
        , renderStyle = Style.fromQuality quality
    }


{-| The instanced scene with the placements of `hidden` drawn as nothing (see
`setHiddenSources`).

They are collapsed to a point (a zero-scale model matrix, so every triangle and
edge is degenerate) rather than dropped: the renderer decides whether a spatial
cell draws its merged far-away proxy box by whether any placement in it is
resolved, and dropping a near placement would bring its cell's grey box back in
its place.

-}
withoutHidden : Set Int -> InstancedScene -> InstancedScene
withoutHidden hidden scene =
    if Set.isEmpty hidden then
        scene

    else
        { scene
            | placements =
                List.map
                    (\p ->
                        if Set.member p.source hidden then
                            { p | matrix = Mat4.makeScale3 0 0 0 }

                        else
                            p
                    )
                    scene.placements
        }


{-| The WebGL canvas with its mouse, wheel and touch/pointer listeners.
-}
view : ViewConfig -> Model -> Html Msg
view config model =
    let
        aspect =
            toFloat model.width / toFloat model.height

        entities =
            case ( model.scene, model.instancedScene ) of
                ( Just scene, _ ) ->
                    Scene.renderSceneWithStyle scene model.camera model.renderStyle model.width model.height
                        ++ config.extraEntities model.camera model.renderStyle aspect

                ( Nothing, Just instanced ) ->
                    Instanced.renderWithGhost
                        { camera = model.camera
                        , style = model.renderStyle
                        , width = model.width
                        , height = model.height
                        , coarsen = config.coarsenWhileMoving && model.refineFrames > 0
                        , activeSources = model.activeSources
                        }
                        (withoutHidden model.hiddenSources instanced)
                        ++ config.extraEntities model.camera model.renderStyle aspect

                ( Nothing, Nothing ) ->
                    []
    in
    WebGL.toHtmlWith
        [ WebGL.antialias
        , WebGL.alpha True
        , WebGL.depth 24
        , WebGL.preserveDrawingBuffer
        ]
        ([ Attr.width model.width
         , Attr.height model.height
         , Attr.style "display" "block"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "touch-action" "none"
         , Html.Events.preventDefaultOn "mousedown"
            (Decode.map6
                (\origin clientX clientY button shift accel ->
                    let
                        down =
                            MouseButtonDown clientX clientY button shift accel
                    in
                    case origin of
                        Just o ->
                            ( CanvasPressed o down, button == 1 )

                        Nothing ->
                            ( down, button == 1 )
                )
                ViewportDecode.canvasOriginDecoder
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                ViewportDecode.mouseButtonDecoder
                (Decode.field "shiftKey" Decode.bool)
                ViewportDecode.accelKeyDecoder
            )

         -- The browser menu would open over the right-drag orbit.
         , Html.Events.preventDefaultOn "contextmenu"
            (Decode.succeed ( ContextMenuOpened, True ))
         , Html.Events.on "dblclick"
            (Decode.map2 DoubleClick
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
         , Html.Events.preventDefaultOn "wheel"
            (Decode.map (\delta -> ( Wheel delta, True )) ViewportDecode.wheelDeltaDecoder)
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
            (Decode.map2
                (\origin touch ->
                    let
                        down =
                            PointerTouchStart touch

                        msg =
                            case origin of
                                Just o ->
                                    CanvasPressed o down

                                Nothing ->
                                    down
                    in
                    ( msg, True )
                )
                ViewportDecode.canvasOriginDecoder
                pointerTouchPointDecoder
            )
        , Html.Events.preventDefaultOn "pointermove"
            (Decode.map (\touch -> ( PointerTouchMove touch, True )) pointerTouchPointDecoder)
        , Html.Events.preventDefaultOn "pointerup"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        , Html.Events.preventDefaultOn "pointercancel"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        ]

    else
        [ Html.Events.preventDefaultOn "touchstart"
            (Decode.map2
                (\origin touches ->
                    let
                        down =
                            TouchStart touches

                        msg =
                            case origin of
                                Just o ->
                                    CanvasPressed o down

                                Nothing ->
                                    down
                    in
                    ( msg, True )
                )
                ViewportDecode.canvasOriginDecoder
                touchesDecoder
            )
        , Html.Events.preventDefaultOn "touchmove"
            (Decode.map (\touches -> ( TouchMove touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchend"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchcancel"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        ]
