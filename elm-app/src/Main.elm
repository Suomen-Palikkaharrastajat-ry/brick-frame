module Main exposing (main)

{-| Main application entrypoint for the LEGO Technic simulator.
-}

import Array
import Browser
import Browser.Dom
import Browser.Events
import Data
import Dict exposing (Dict)
import Gear.Animate as Animate exposing (MotorState)
import Gear.Components as Components
import Gear.Detect as Detect
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
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene exposing (Scene)
import Render.Shader as Shader
import Render.Style as Style
import Task
import Time
import UI.FileUpload as FileUpload
import UI.Theme as Theme
import Url
import WebGL
import WebGL.Settings.DepthTest as DepthTest


type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , defaultModelUrl : String
    , initialHash : String
    , maxRpm : Float
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


type alias TouchPoint =
    { id : Int
    , x : Float
    , y : Float
    }


type TouchGesture
    = NoTouchGesture
    | SingleTouchGesture Int
    | PinchGesture Int Int Float Float Float


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
    , errorMsg : Maybe String
    , urlInput : String
    , gearGraph : Maybe GearGraph
    , gearMeshes : Dict GearId GearRender
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
    }


type alias GearRender =
    { mesh : WebGL.Mesh Vertex
    , center : Vec3.Vec3
    , axis : Vec3.Vec3
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
        fallbackBase =
            if String.trim flags.ldrawFallbackBase == "" then
                Nothing

            else
                Just flags.ldrawFallbackBase

        resolver =
            Resolve.resolverConfig flags.ldrawBase fallbackBase

        initialHash =
            decodeHash flags.initialHash

        initialUrl =
            Maybe.withDefault flags.defaultModelUrl initialHash.url

        baseCamera =
            Camera.init

        initialCamera =
            { baseCamera
                | azimuth = Maybe.withDefault baseCamera.azimuth initialHash.azimuth
                , elevation = Maybe.withDefault baseCamera.elevation initialHash.elevation
                , distance = Maybe.withDefault baseCamera.distance initialHash.distance
            }

        initialTime =
            Maybe.withDefault 0.0 initialHash.time

        maxRpmValue =
            max 1 flags.maxRpm

        initialCameraMode =
            if hasExplicitHashCamera initialHash then
                CameraManual

            else
                CameraAutoFit
    in
    ( { camera = initialCamera
      , width = 800
      , height = 600
      , loadPhase = FetchingTopLevel initialUrl
      , topLevelLines = []
      , partCache = embeddedPartCache
      , resolverConfig = resolver
      , partsLoaded = 0
      , partsTotal = 0
      , scene = Nothing
      , errorMsg = Nothing
      , urlInput = initialUrl
      , gearGraph = Nothing
      , gearMeshes = Dict.empty
      , components = []
      , componentMeshes = []
      , componentRenders = []
      , motor = Animate.defaultMotor
      , playback =
            { running = False
            , currentTime = initialTime
            , motorGearId = Nothing
            , motorSpeedRadPerSec = 1.0
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
      }
    , Cmd.batch
        [ Browser.Dom.getViewport
            |> Task.perform
                (\vp -> WindowResize (round vp.viewport.width) (round vp.viewport.height))
        , fetchTopLevel initialUrl
        , Ports.setUrlHash (encodeHashString initialUrl initialCamera initialTime)
        ]
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


appRenderStyle : Style.Style
appRenderStyle =
    let
        baseStyle =
            Style.defaultStyle
    in
    Style.clampStyle
        { baseStyle
            | ambientStrength = 0.58
            , specularStrength = 0.16
            , specularPower = 20
            , rimStrength = 0.1
            , rimPower = 2.4
            , vibrance = 0.16
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
    | UrlInputChanged String
    | LoadUrl
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
    | ToggleControlsPanel
    | KeyPressed String



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize w h ->
            ( { model | width = w, height = h }, Cmd.none )

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
                in
                ( { model
                    | camera = nextCamera
                    , dragTravel = model.dragTravel + stepDist
                    , cameraMode = CameraManual
                  }
                , Cmd.none
                )

        MouseUp x y ->
            if touchInputActive model then
                ( model, Cmd.none )

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
                        | camera = Camera.onWheel delta model.camera
                        , cameraMode = CameraManual
                    }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        TouchStart touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    beginTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        TouchMove touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    advanceTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        TouchEnd touches ->
            let
                nextTouches =
                    touchDictFromList touches

                nextModel =
                    endTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        PointerTouchStart touchPoint ->
            let
                nextTouches =
                    Dict.insert touchPoint.id touchPoint model.activeTouches

                nextModel =
                    beginTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        PointerTouchMove touchPoint ->
            if Dict.member touchPoint.id model.activeTouches then
                let
                    nextTouches =
                        Dict.insert touchPoint.id touchPoint model.activeTouches

                    nextModel =
                        advanceTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
                in
                ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

            else
                ( model, Cmd.none )

        PointerTouchEnd touchId ->
            let
                nextTouches =
                    Dict.remove touchId model.activeTouches

                nextModel =
                    endTouchGesture (touchesFromDict nextTouches) { model | activeTouches = nextTouches }
            in
            ( nextModel, Ports.setUrlHash (encodeHash nextModel) )

        UrlInputChanged s ->
            ( { model | urlInput = s }, Cmd.none )

        LoadUrl ->
            let
                url =
                    String.trim model.urlInput
            in
            if String.isEmpty url then
                ( model, Cmd.none )

            else
                let
                    nextModel =
                        resetForLoad url model
                in
                ( nextModel
                , Cmd.batch
                    [ fetchTopLevel url
                    , Ports.setUrlHash (encodeHash nextModel)
                    ]
                )

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        FileContentReceived text ->
            handleTopLevelText text model

        FileLoadError err ->
            ( { model | errorMsg = Just err }, Cmd.none )

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
                    in
                    ( { model | scene = Just scene, loadPhase = Ready }, Cmd.none )

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
                    )

        GeometryFlattenFailed err ->
            let
                fallback =
                    buildSceneFallback model
            in
            ( { fallback | errorMsg = Just ("Geometry worker failed; used local flatten. " ++ err) }
            , Cmd.none
            )

        DismissError ->
            ( { model | errorMsg = Nothing }, Cmd.none )

        TopLevelLoaded _ (Err err) ->
            ( { model
                | errorMsg = Just ("Failed to load model: " ++ httpErrString err)
                , loadPhase = Idle
              }
            , Cmd.none
            )

        TopLevelLoaded _ (Ok text) ->
            if looksLikeHtmlResponse text then
                ( { model
                    | errorMsg =
                        Just "Top-level URL returned HTML instead of LDraw text. Run `make sync-ldraw` or use a direct LDraw URL."
                    , loadPhase = Idle
                  }
                , Cmd.none
                )

            else
                handleTopLevelText text model

        PartLoaded name result ->
            handlePartResult name result model

        AnimationFrame now ->
            if model.playback.running then
                let
                    dtSeconds =
                        case model.lastFrameTime of
                            Just prev ->
                                toFloat (Time.posixToMillis now - Time.posixToMillis prev) / 1000.0

                            Nothing ->
                                0.0

                    newTime =
                        model.playback.currentTime + dtSeconds

                    newAngles =
                        case model.gearGraph of
                            Just graph ->
                                Animate.gearAngles model.motor graph newTime

                            Nothing ->
                                Dict.empty

                    playbackState =
                        model.playback
                in
                ( { model
                    | playback =
                        { playbackState
                            | currentTime = newTime
                        }
                    , gearAngles = newAngles
                    , lastFrameTime = Just now
                  }
                , Cmd.none
                )

            else
                ( { model | lastFrameTime = Nothing }, Cmd.none )

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
            , Cmd.none
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
            , Cmd.none
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
            , Ports.setUrlHash (encodeHashString model.urlInput model.camera 0.0)
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

        ToggleControlsPanel ->
            ( { model | controlsCollapsed = not model.controlsCollapsed }, Cmd.none )

        KeyPressed key ->
            case key of
                " " ->
                    update ToggleMotor model

                "Spacebar" ->
                    update ToggleMotor model

                _ ->
                    ( model, Cmd.none )


handlePartResult : String -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
handlePartResult name result model =
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
            }
    in
    if List.isEmpty allPending then
        finishLoading updatedModel

    else
        ( updatedModel, fetchPending model.resolverConfig allPending )


{-| Reset model fields that depend on the loaded file, keeping the part cache.
-}
resetForLoad : String -> Model -> Model
resetForLoad url m =
    { m
        | loadPhase = FetchingTopLevel url
        , scene = Nothing
        , errorMsg = Nothing
        , topLevelLines = []
        , partsLoaded = 0
        , partsTotal = 0
        , gearGraph = Nothing
        , gearMeshes = Dict.empty
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
    }


beginTouchGesture : List TouchPoint -> Model -> Model
beginTouchGesture touches model =
    case touches of
        p1 :: p2 :: _ ->
            let
                dist =
                    touchDistance p1 p2

                ( midX, midY ) =
                    touchMidpoint p1 p2
            in
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture p1.id p2.id dist midX midY
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

        PinchGesture idA idB lastDist lastMidX lastMidY ->
            case ( findTouch idA touches, findTouch idB touches ) of
                ( Just a, Just b ) ->
                    let
                        newDist =
                            touchDistance a b

                        ( midX, midY ) =
                            touchMidpoint a b

                        zoomDelta =
                            -(newDist - lastDist) * 0.8

                        panDx =
                            midX - lastMidX

                        panDy =
                            midY - lastMidY

                        cameraAfterZoom =
                            Camera.onWheel zoomDelta model.camera

                        cameraAfterPan =
                            Camera.onPan panDx panDy cameraAfterZoom
                    in
                    { model
                        | camera = cameraAfterPan
                        , touchGesture = PinchGesture idA idB newDist midX midY
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
            let
                dist =
                    touchDistance p1 p2

                ( midX, midY ) =
                    touchMidpoint p1 p2
            in
            { model
                | camera = Camera.onMouseUp model.camera
                , touchGesture = PinchGesture p1.id p2.id dist midX midY
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


autoFitCamera : Int -> Int -> List LDrawLine -> PartCache -> Camera -> Camera
autoFitCamera width height lines cache currentCamera =
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
            currentCamera

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

                aspect =
                    toFloat width / max 1 (toFloat height)

                fovY =
                    45 * pi / 180

                fovX =
                    2 * atan (tan (fovY / 2) * aspect)

                limitingHalfFov =
                    max 0.15 (min (fovX / 2) (fovY / 2))

                distanceForSphere =
                    radius / sin limitingHalfFov

                distance =
                    clamp 8 5000 (distanceForSphere * 1.15)
            in
            { currentCamera
                | target = center
                , distance = distance
                , azimuth = 0.75
                , elevation = 0.45
            }


{-| Shared handler for HTTP-fetched and file-uploaded LDraw text.
-}
handleTopLevelText : String -> Model -> ( Model, Cmd Msg )
handleTopLevelText text model =
    let
        isMpd =
            String.contains "0 FILE " text

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

        pending =
            Resolve.pendingParts lines seededCache

        cacheWithLoading =
            Resolve.markLoading pending seededCache

        loadingModel =
            { model
                | topLevelLines = lines
                , partCache = cacheWithLoading
                , loadPhase = ResolvingParts
                , partsTotal = List.length pending
                , partsLoaded = 0
                , scene = Nothing
                , errorMsg = Nothing
                , gearGraph = Nothing
                , gearMeshes = Dict.empty
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
            }
    in
    if List.isEmpty pending then
        finishLoading loadingModel

    else
        ( loadingModel, fetchPending model.resolverConfig pending )


{-| Called when all parts have been resolved. Build the scene and gear graph.
-}
finishLoading : Model -> ( Model, Cmd Msg )
finishLoading model =
    let
        gearInstances =
            Detect.extractGears Data.gearParts model.topLevelLines model.partCache

        components =
            topLevelComponents model.topLevelLines

        drivenAxles =
            buildDrivenAxles model.partCache components gearInstances

        -- For each top-level line, compute the gear it co-rotates with (if any).
        -- Used to both exclude those lines from the static scene and build
        -- rotating meshes for them.
        coaxialGearByLine =
            List.map (\line -> topLevelCoaxialGear model.partCache drivenAxles line gearInstances) model.topLevelLines

        -- Lines that belong to the static (non-rotating) scene: drop gears,
        -- known components, and co-axial parts that will get their own
        -- rotating meshes.
        nonGearTopLevelLines =
            List.map2 Tuple.pair model.topLevelLines coaxialGearByLine
                |> List.filter
                    (\( line, coaxial ) ->
                        not (isTopLevelGearRef line)
                            && not (isTopLevelComponentRef line)
                            && coaxial
                            == Nothing
                    )
                |> List.map Tuple.first

        -- Non-gear, non-component top-level parts that share an axle with a
        -- gear and should rotate with it.
        coaxialPartData =
            List.map2 Tuple.pair model.topLevelLines coaxialGearByLine
                |> List.filterMap
                    (\( line, coaxial ) ->
                        case ( line, coaxial ) of
                            ( SubFileRef ref, Just gearId ) ->
                                if not (isTopLevelGearRef line) && not (isTopLevelComponentRef line) then
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

        -- Auto-select the first detected gear as the motor
        firstGearId =
            List.head gearInstances |> Maybe.map .id

        defaultMotor =
            Animate.defaultMotor

        motor =
            { defaultMotor | drivingGearId = firstGearId }

        startTime =
            max 0 model.playback.currentTime

        initAngles =
            Animate.gearAngles motor graph startTime

        componentRenders =
            buildComponentRenders model.partCache components gearInstances

        componentMeshes =
            buildComponentMeshRenders model.partCache components gearInstances
                ++ buildCoaxialMeshRenders model.partCache gearInstances coaxialPartData

        playbackState =
            model.playback

        nextCamera =
            case model.cameraMode of
                CameraAutoFit ->
                    autoFitCamera model.width model.height model.topLevelLines model.partCache model.camera

                CameraManual ->
                    model.camera
    in
    ( { model
        | camera = nextCamera
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
                , motorGearId = firstGearId
                , motorSpeedRadPerSec = motor.speedRadPerSec
            }
        , gearAngles = initAngles
      }
    , requestGeometryFlatten nonGearTopLevelLines model.partCache
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
        nonGearTopLevelLines =
            List.filter (not << isTopLevelGearRef) model.topLevelLines

        geom =
            Geometry.flatten nonGearTopLevelLines model.partCache 15 Mat4.identity

        scene =
            Scene.buildScene
                { triangles = geom.triangles
                , lines = geom.lines
                , conditionalLines = geom.conditionalLines
                }
                geom.bfcCertified
    in
    { model | scene = Just scene, loadPhase = Ready }


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


isTopLevelGearRef : LDrawLine -> Bool
isTopLevelGearRef line =
    case line of
        SubFileRef ref ->
            List.any (\spec -> spec.partFile == ref.file) Data.gearParts

        _ ->
            False


isTopLevelComponentRef : LDrawLine -> Bool
isTopLevelComponentRef line =
    case line of
        SubFileRef ref ->
            List.any (\spec -> spec.partFile == ref.file) Components.defaultSpecs

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
    Decode.map3
        (\p n c -> { position = p, normal = n, color = c })
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
                            , center = center
                            , axis = axis
                            }
                            acc

                    _ ->
                        acc
            )
            Dict.empty


toYUpPoint : Vec3.Vec3 -> Vec3.Vec3
toYUpPoint p =
    Vec3.vec3 (Vec3.getX p) -(Vec3.getY p) (Vec3.getZ p)


toYUpAxis : Vec3.Vec3 -> Vec3.Vec3
toYUpAxis axis =
    let
        raw =
            Vec3.vec3 (Vec3.getX axis) -(Vec3.getY axis) (Vec3.getZ axis)

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
                    Camera.projectionMatrix aspect 0.1 2000.0

                viewPos =
                    cameraPosition camera
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

                                        uniforms =
                                            { modelMatrix = modelMat
                                            , viewMatrix = viewMat
                                            , projectionMatrix = projMat
                                            , viewPosition = viewPos
                                            , lightDirection = style.lightDirection
                                            , ambientStrength = style.ambientStrength
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
                                            rendered.mesh
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
    ( { position = p1 }, { position = p2 } )


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
            Camera.projectionMatrix aspect 0.1 2000.0

        viewPos =
            cameraPosition camera
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
                        , ambientStrength = style.ambientStrength
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
            Camera.projectionMatrix aspect 0.1 2000.0
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
            cameraPosition camera

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
                    0.1

                farPlane =
                    2000.0

                tanHalfFov =
                    tan (45 * pi / 180 / 2)

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
                cameraPosition model.camera

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
                        tan (45 * pi / 180 / 2)

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


cameraPosition : Camera -> Vec3.Vec3
cameraPosition cam =
    let
        x =
            cam.distance * sin cam.azimuth * cos cam.elevation

        y =
            cam.distance * sin cam.elevation

        z =
            cam.distance * cos cam.azimuth * cos cam.elevation
    in
    Vec3.add cam.target (Vec3.vec3 x y z)


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
        , Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))
        , Browser.Events.onResize WindowResize
        , Ports.fileContentReceived FileContentReceived
        , Ports.fileLoadError FileLoadError
        , Ports.geometryFlattened GeometryFlattened
        , Ports.geometryFlattenFailed GeometryFlattenFailed
        , Browser.Events.onAnimationFrame AnimationFrame
        ]


touchesDecoder : Decode.Decoder (List TouchPoint)
touchesDecoder =
    Decode.field "touches" (Decode.list touchPointDecoder)


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
        [ Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
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
            case model.scene of
                Just scene ->
                    Scene.renderSceneWithStyle scene model.camera appRenderStyle aspect
                        ++ renderGearEntities model.camera appRenderStyle aspect model
                        ++ renderComponentEntities model.camera appRenderStyle aspect model
                        ++ renderComponentArrows model.camera aspect model

                Nothing ->
                    []
    in
    WebGL.toHtml
        [ Attr.width model.width
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
            (Decode.map (\delta -> ( Wheel delta, True ))
                (Decode.field "deltaY" Decode.float)
            )
        , Html.Events.preventDefaultOn "touchstart"
            (Decode.map (\touches -> ( TouchStart touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchmove"
            (Decode.map (\touches -> ( TouchMove touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchend"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "touchcancel"
            (Decode.map (\touches -> ( TouchEnd touches, True )) touchesDecoder)
        , Html.Events.preventDefaultOn "pointerdown"
            (Decode.map (\touch -> ( PointerTouchStart touch, True )) pointerTouchPointDecoder)
        , Html.Events.preventDefaultOn "pointermove"
            (Decode.map (\touch -> ( PointerTouchMove touch, True )) pointerTouchPointDecoder)
        , Html.Events.preventDefaultOn "pointerup"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        , Html.Events.preventDefaultOn "pointercancel"
            (Decode.map (\touchId -> ( PointerTouchEnd touchId, True )) pointerTouchIdDecoder)
        ]
        entities


viewOverlay : Model -> Html Msg
viewOverlay model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "pointer-events" "none"
        ]
        [ viewDebug model
        , viewStatus model
        , viewGearPanel model
        , viewToolbar model
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


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "padding" "10px 16px"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "box-shadow" "0 -8px 24px color-mix(in srgb, var(--color-brand) 8%, transparent)"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        ]
        (FileUpload.view
            { urlInput = model.urlInput
            , onUrlInput = UrlInputChanged
            , onLoadUrl = LoadUrl
            , onRequestFileUpload = RequestFileUpload
            }
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
                    let
                        pct =
                            if model.partsTotal == 0 then
                                0

                            else
                                round (toFloat model.partsLoaded / toFloat model.partsTotal * 100)
                    in
                    viewLoadingBox
                        ("Loading parts… "
                            ++ String.fromInt model.partsLoaded
                            ++ " / "
                            ++ String.fromInt model.partsTotal
                        )
                        (Just pct)

                FlatteningGeometry ->
                    viewLoadingBox "Flattening geometry…" Nothing


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


type alias HashState =
    { url : Maybe String
    , azimuth : Maybe Float
    , elevation : Maybe Float
    , distance : Maybe Float
    , time : Maybe Float
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

        getDecoded key =
            Dict.get key entries
                |> Maybe.andThen Url.percentDecode

        getFloat key =
            getDecoded key |> Maybe.andThen String.toFloat
    in
    { url = getDecoded "u"
    , azimuth = getFloat "az"
    , elevation = getFloat "el"
    , distance = getFloat "d"
    , time = getFloat "t"
    }


hasExplicitHashCamera : HashState -> Bool
hasExplicitHashCamera state =
    state.azimuth
        /= Nothing
        || state.elevation
        /= Nothing
        || state.distance
        /= Nothing


encodeHash : Model -> String
encodeHash model =
    encodeHashString model.urlInput model.camera model.playback.currentTime


encodeHashString : String -> Camera -> Float -> String
encodeHashString url camera timeValue =
    "u="
        ++ Url.percentEncode url
        ++ "&az="
        ++ String.fromFloat camera.azimuth
        ++ "&el="
        ++ String.fromFloat camera.elevation
        ++ "&d="
        ++ String.fromFloat camera.distance
        ++ "&t="
        ++ String.fromFloat timeValue


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
