module Simulation exposing
    ( Analysis
    , Config
    , DrivenPart
    , Event(..)
    , Model
    , Msg(..)
    , PlaybackState
    , analyse
    , drivenEntitiesFor
    , drivenPartsFor
    , entities
    , gearEntitiesFor
    , init
    , markUnavailable
    , needsAnimationFrame
    , pickGear
    , refreshLods
    , reset
    , rotatingGearIds
    , setTime
    , subscriptions
    , update
    )

{-| The gear simulation as a stateful component: detection, playback and motor.

It owns the gear graph and the meshes of everything that rotates, advances the
playback clock, and answers "which gear is under this pixel". Gear angles are a
pure function of the clock (`angle = ratio × speed × t`), so scrubbing needs no
integration state.

The simulation never touches the viewport. A host feeds it the model once the
viewport has loaded the parts:

    ( simulation, analysis ) =
        Simulation.analyse { lines = viewport.topLevelLines, cache = viewport.partCache } model

`analysis.staticLines` are the lines the viewport should bake into the static
scene (`Viewport.beginFlatten`) — everything that is not a gear or riding on one.
Each frame the host draws `Simulation.entities` next to the viewport's scene.

-}

import Array
import Browser.Events
import Dict exposing (Dict)
import Gear.Animate as Animate exposing (MotorState)
import Gear.Components as Components
import Gear.Detect as Detect
import Gear.Lod as Lod exposing (Lod)
import Gear.Types exposing (GearGraph, GearId, GearInstance, GearSpec)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Render.Camera as Camera exposing (Camera)
import Render.EdgeShader as EdgeShader exposing (EdgeVertex)
import Render.GuideShader as GuideShader
import Render.Pick as Pick
import Render.Shader as Shader
import Render.Style as Style
import Set exposing (Set)
import Simulation.Meshes as Meshes
    exposing
        ( ComponentMeshRender
        , ComponentRender
        , GearRender
        )
import Time
import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest



-- ── Model ─────────────────────────────────────────────────────────────────────


{-| Playback clock and the motor settings shown in the UI.
-}
type alias PlaybackState =
    { running : Bool
    , currentTime : Float
    , motorGearId : Maybe GearId
    , motorSpeedRadPerSec : Float
    }


{-| What `init` needs from the host.

`gearSpecs` lists the part files that count as gears. `lodCache` holds
simplified versions of those parts for the low-detail meshes used at a
distance. `requestedMotorIndex` picks which detected gear starts as the motor
(`Nothing` = the first).

-}
type alias Config =
    { gearSpecs : List GearSpec
    , lodCache : PartCache
    , maxRpm : Float
    , initialRpm : Float
    , requestedMotorIndex : Maybe Int
    }


{-| Simulation state. Hosts read fields to draw their panels but change them
only through `update`, `analyse` and `reset`.
-}
type alias Model =
    { gearSpecs : List GearSpec
    , lodCache : PartCache
    , gearGraph : Maybe GearGraph
    , gearMeshes : Dict GearId GearRender
    , gearLods : Dict GearId Lod
    , components : List Components.ComponentInstance
    , componentMeshes : List ComponentMeshRender
    , componentRenders : List ComponentRender
    , drivenParts : List DrivenPart
    , motor : MotorState
    , playback : PlaybackState
    , maxRpm : Float
    , gearAngles : Dict GearId Float
    , lastFrameTime : Maybe Time.Posix
    , simulationChecked : Bool
    , simulationAvailable : Bool
    , requestedMotorIndex : Maybe Int
    }


{-| Nothing loaded yet; the motor speed is the initial RPM clamped to `maxRpm`.
-}
init : Config -> Model
init config =
    let
        maxRpmValue =
            max 1 config.maxRpm

        initialMotorRpm =
            clamp -maxRpmValue maxRpmValue config.initialRpm

        defaultMotor =
            Animate.defaultMotor
    in
    { gearSpecs = config.gearSpecs
    , lodCache = config.lodCache
    , gearGraph = Nothing
    , gearMeshes = Dict.empty
    , gearLods = Dict.empty
    , components = []
    , componentMeshes = []
    , componentRenders = []
    , drivenParts = []
    , motor = { defaultMotor | speedRadPerSec = initialMotorRpm * 2 * pi / 60 }
    , playback =
        { running = False
        , currentTime = 0.0
        , motorGearId = Nothing
        , motorSpeedRadPerSec = initialMotorRpm * 2 * pi / 60
        }
    , maxRpm = maxRpmValue
    , gearAngles = Dict.empty
    , lastFrameTime = Nothing
    , simulationChecked = False
    , simulationAvailable = False
    , requestedMotorIndex =
        config.requestedMotorIndex
    }


{-| Forget everything derived from the previous model. Call on the viewport's
`LoadStarted`.
-}
reset : Model -> Model
reset model =
    { model
        | gearGraph = Nothing
        , gearMeshes = Dict.empty
        , gearLods = Dict.empty
        , components = []
        , componentMeshes = []
        , componentRenders = []
        , drivenParts = []
        , motor = Animate.defaultMotor
        , playback =
            { running = False
            , currentTime = 0.0
            , motorGearId = Nothing
            , motorSpeedRadPerSec = 1.0
            }
        , gearAngles = Dict.empty
        , lastFrameTime = Nothing
        , simulationChecked = False
        , simulationAvailable = False
    }


{-| The model took the viewport's large-model path, where gear detection is
skipped because it walks the fully expanded part tree.
-}
markUnavailable : Model -> Model
markUnavailable model =
    { model
        | gearGraph = Nothing
        , gearMeshes = Dict.empty
        , gearLods = Dict.empty
        , components = []
        , componentMeshes = []
        , componentRenders = []
        , drivenParts = []
        , gearAngles = Dict.empty
        , simulationChecked = True
        , simulationAvailable = False
    }



-- ── Analysis ──────────────────────────────────────────────────────────────────


{-| What analysing a model found.

`staticLines` go to the geometry worker; `fallbackLines` are what the viewport
flattens locally if the worker fails (only gears are left out there).

-}
type alias Analysis =
    { staticLines : List LDrawLine
    , fallbackLines : List LDrawLine
    , gearCount : Int
    }


{-| Detect gears, build the gear graph and the meshes of everything that turns,
and pick the starting motor gear.
-}
analyse : { lines : List LDrawLine, cache : PartCache } -> Model -> ( Model, Analysis )
analyse { lines, cache } model =
    let
        gearInstances =
            Detect.extractGears model.gearSpecs lines cache

        components =
            topLevelComponents lines

        drivenAxles =
            Meshes.buildDrivenAxles cache components gearInstances

        coaxialGearByLine =
            List.map (\line -> Meshes.topLevelCoaxialGear cache drivenAxles line gearInstances) lines

        nonGearTopLevelLines =
            List.map2 Tuple.pair lines coaxialGearByLine
                |> List.filter
                    (\( line, coaxial ) ->
                        not (isTopLevelGearRef model.gearSpecs line)
                            && not (isTopLevelAxleRef line)
                            && coaxial
                            == Nothing
                    )
                |> List.map Tuple.first

        coaxialPartData =
            List.map2 Tuple.pair lines coaxialGearByLine
                |> List.filterMap
                    (\( line, coaxial ) ->
                        case ( line, coaxial ) of
                            ( SubFileRef ref, Just gearId ) ->
                                if not (isTopLevelGearRef model.gearSpecs line) && not (isTopLevelAxleRef line) then
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

        -- Axles driven by a gear and parts riding on them, with the file and
        -- placement a host needs to draw them turning and hide their static copy.
        -- Only parts whose mesh loaded are listed, as those are the ones drawn.
        drivenParts =
            (List.map
                (\axle -> { drivingGearId = axle.drivingGearId, file = axle.file, transform = axle.transform })
                drivenAxles
                ++ List.map
                    (\part -> { drivingGearId = part.drivingGearId, file = part.file, transform = part.transform })
                    coaxialPartData
            )
                |> List.filter (\part -> isLoaded cache part.file)

        gearMeshes =
            Meshes.buildGearMeshes model.lodCache cache gearInstances

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
    in
    ( { model
        | gearGraph = Just graph
        , gearMeshes = gearMeshes
        , components = components
        , componentMeshes =
            Meshes.buildComponentMeshRenders cache components gearInstances
                ++ Meshes.buildCoaxialMeshRenders cache gearInstances coaxialPartData
        , componentRenders = Meshes.buildComponentRenders cache components gearInstances
        , drivenParts = drivenParts
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
        , simulationAvailable = not (List.isEmpty gearInstances)
      }
    , { staticLines = nonGearTopLevelLines
      , fallbackLines = List.filter (not << isTopLevelGearRef model.gearSpecs) lines
      , gearCount = List.length gearInstances
      }
    )


isLoaded : PartCache -> String -> Bool
isLoaded cache file =
    case Dict.get file cache of
        Just (Loaded _) ->
            True

        _ ->
            False


isTopLevelGearRef : List GearSpec -> LDrawLine -> Bool
isTopLevelGearRef specs line =
    case line of
        SubFileRef ref ->
            List.any (\spec -> spec.partFile == ref.file) specs

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



-- ── Update ────────────────────────────────────────────────────────────────────


{-| Playback and motor controls. `Tick` is the animation frame.
-}
type Msg
    = Tick Time.Posix
    | Play
    | Pause
    | Stop
    | ToggleMotor
    | SetMotorGear GearId
    | SetMotorSpeed Float


{-| What the host may want to report. The payload is the playback time in seconds.
-}
type Event
    = PlayStateChanged Bool Float


{-| Advance the simulation, returning events for the host to report.
-}
update : Msg -> Model -> ( Model, List Event )
update msg model =
    case msg of
        Tick now ->
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
                    | playback = { playbackState | currentTime = newTime }
                    , gearAngles = newAngles
                    , lastFrameTime = Just now
                  }
                , []
                )

            else
                ( { model | lastFrameTime = Nothing }, [] )

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
            , [ PlayStateChanged True model.playback.currentTime ]
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
            , [ PlayStateChanged False model.playback.currentTime ]
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
            , [ PlayStateChanged False 0.0 ]
            )

        ToggleMotor ->
            if model.playback.running then
                update Pause model

            else
                update Play model

        SetMotorGear id ->
            ( setMotorGear id model, [] )

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
            , []
            )


{-| Whether the playback clock needs animation frames.
-}
needsAnimationFrame : Model -> Bool
needsAnimationFrame model =
    model.playback.running


{-| Animation frames while playing, nothing otherwise.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if needsAnimationFrame model then
        Browser.Events.onAnimationFrame Tick

    else
        Sub.none


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


{-| Put the playback clock at `seconds` and recompute every gear angle from it,
for a host that runs its own clock instead of `Tick` (Brick Contraptions).
Pure in the time: `setTime t` gives the same angles however it is reached.
-}
setTime : Float -> Model -> Model
setTime seconds model =
    let
        playbackState =
            model.playback
    in
    { model
        | playback = { playbackState | currentTime = seconds }
        , gearAngles =
            case model.gearGraph of
                Just graph ->
                    Animate.gearAngles model.motor graph seconds

                Nothing ->
                    Dict.empty
    }


{-| The gears that turn with the motor and have a mesh to draw them with — the
gears `gearEntitiesFor` can animate. Gears outside the motor's gear train, or
whose part has not loaded, are left out.
-}
rotatingGearIds : Model -> Set GearId
rotatingGearIds model =
    Dict.keys model.gearAngles
        |> List.filter (\gid -> Dict.member gid model.gearMeshes)
        |> Set.fromList


{-| A top-level part that turns with a gear without being a gear: an axle the
gear sits on, or a part threaded onto a gear's axle. `file` and `transform` are
the part's line in the analysed model, so a host can find its placement.
-}
type alias DrivenPart =
    { drivingGearId : GearId
    , file : String
    , transform : Mat4.Mat4
    }


{-| The driven parts (see `DrivenPart`) turning with one of the given gears.
Parts outside the gear train, or on a gear not in the set, are left out.
-}
drivenPartsFor : Set GearId -> Model -> List DrivenPart
drivenPartsFor ids model =
    List.filter (\part -> Set.member part.drivingGearId ids) model.drivenParts


{-| Only the driven parts turning with one of the given gears, each rotated
about its own axis to its driving gear's current angle. Pair it with
`gearEntitiesFor` and hide the parts' static placements from the host's scene.
-}
drivenEntitiesFor : Set GearId -> Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
drivenEntitiesFor ids =
    componentEntitiesWhere
        (\component ->
            case component.drivingGearId of
                Just gid ->
                    Set.member gid ids

                Nothing ->
                    False
        )


{-| Recompute each gear's level of detail from its distance to the camera,
starting from its previous LOD so the switch has hysteresis (see `Gear.Lod`).
Call after every update that may have moved the camera.
-}
refreshLods : Camera -> Model -> Model
refreshLods camera model =
    let
        viewPos =
            Camera.position camera
    in
    { model
        | gearLods =
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
    }


setMotorGear : GearId -> Model -> Model
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
    { model
        | motor = newMotor
        , playback = { playbackState | motorGearId = Just id }
        , gearAngles = newAngles
    }


{-| The gear under a screen point (canvas pixels), nearest to the eye first.
Gears are tested as spheres of their pitch radius.
-}
pickGear : Camera -> Int -> Int -> Float -> Float -> Model -> Maybe GearId
pickGear camera width height mouseX mouseY model =
    case model.gearGraph of
        Nothing ->
            Nothing

        Just graph ->
            let
                maybeRay =
                    Pick.screenRay camera width height mouseX mouseY
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
                                        Meshes.toYUpPoint (Mat4.transform inst.worldMatrix (Vec3.vec3 0 0 0))
                                in
                                Pick.raySphereHit origin dir center inst.spec.pitchRadius
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



-- ── Rendering ─────────────────────────────────────────────────────────────────


{-| Everything the simulation draws: gears, rotating parts and axle arrows, in
that order. Meant for `Viewport.ViewConfig.extraEntities`.
-}
entities : Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
entities camera style aspect model =
    gearEntities camera style aspect model
        ++ componentEntities camera style aspect model
        ++ componentArrows camera aspect model


gearEntities : Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
gearEntities =
    gearEntitiesWhere (\_ -> True)


{-| Only the gears in `ids`, each turned to its current angle. A host that keeps
the rest of the model (non-turning gears included) in its own static scene
draws just the turning ones with this, e.g. `gearEntitiesFor (rotatingGearIds model)`.
-}
gearEntitiesFor : Set GearId -> Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
gearEntitiesFor ids =
    gearEntitiesWhere (\gid -> Set.member gid ids)


gearEntitiesWhere : (GearId -> Bool) -> Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
gearEntitiesWhere include camera styleInput aspect model =
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
                |> List.filter (\inst -> include inst.id)
                |> List.filterMap
                    (\inst ->
                        case Dict.get inst.id model.gearMeshes of
                            Nothing ->
                                Nothing

                            Just rendered ->
                                if Pick.isSphereVisible camera aspect rendered.center inst.spec.pitchRadius then
                                    let
                                        angle =
                                            Dict.get inst.id model.gearAngles
                                                |> Maybe.withDefault 0.0

                                        modelMat =
                                            Meshes.rotationAround angle rendered.axis rendered.center

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
                                            , bevelStrength = style.bevelStrength
                                            , studEmbossStrength = style.studEmbossStrength
                                            , u_ghostMode = 0.0
                                            , u_ghostAlpha = 1.0
                                            }

                                        triangleEntity =
                                            WebGL.entityWith
                                                [ DepthTest.default ]
                                                Shader.vertexShader
                                                Shader.fragmentShader
                                                meshForLod
                                                uniforms

                                        -- Edges are skipped on the simplified LOD to keep cost down.
                                        edgeEntities =
                                            case Dict.get inst.id model.gearLods of
                                                Just Lod.Simplified ->
                                                    []

                                                _ ->
                                                    [ edgeEntity modelMat viewMat projMat style aspect rendered.edges ]
                                    in
                                    Just (triangleEntity :: edgeEntities)

                                else
                                    Nothing
                    )
                |> List.concat


componentEntities : Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
componentEntities =
    componentEntitiesWhere (\_ -> True)


componentEntitiesWhere : (ComponentMeshRender -> Bool) -> Camera -> Style.Style -> Float -> Model -> List WebGL.Entity
componentEntitiesWhere include camera styleInput aspect model =
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
        |> List.filter include
        |> List.filter
            (\c ->
                let
                    orbitRadius =
                        Vec3.distance c.center c.rotationCenter
                in
                Pick.isSphereVisible camera aspect c.rotationCenter (orbitRadius + 10.0)
            )
        |> List.concatMap
            (\component ->
                let
                    angle =
                        case component.drivingGearId of
                            Just gid ->
                                Dict.get gid model.gearAngles |> Maybe.withDefault 0.0

                            Nothing ->
                                0.0

                    modelMat =
                        Meshes.rotationAround angle component.axis component.rotationCenter

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
                        , bevelStrength = style.bevelStrength
                        , studEmbossStrength = style.studEmbossStrength
                        , u_ghostMode = 0.0
                        , u_ghostAlpha = 1.0
                        }
                in
                [ WebGL.entityWith
                    [ DepthTest.default ]
                    Shader.vertexShader
                    Shader.fragmentShader
                    component.mesh
                    uniforms
                , edgeEntity modelMat viewMat projMat style aspect component.edges
                ]
            )


{-| Canvas height, in pixels, that edge widths are computed against.

`Viewport.ViewConfig.extraEntities` passes only the aspect ratio, not the canvas
size, so the edge shader's pixel-to-NDC scale uses this reference height. Moving
part outlines therefore come out at `edgeWidth * canvasHeight / edgeReferenceHeight`
pixels rather than exactly `edgeWidth`.

-}
edgeReferenceHeight : Float
edgeReferenceHeight =
    800.0


{-| Edge-line pass for one moving part, drawn with the same model matrix as its
triangle mesh. Mirrors the static scene's edge settings in `Render.Scene`.
-}
edgeEntity : Mat4.Mat4 -> Mat4.Mat4 -> Mat4.Mat4 -> Style.Style -> Float -> WebGL.Mesh EdgeVertex -> WebGL.Entity
edgeEntity modelMat viewMat projMat style aspect mesh =
    WebGL.entityWith
        [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
        , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
        ]
        EdgeShader.vertexShader
        EdgeShader.fragmentShader
        mesh
        { modelMatrix = modelMat
        , viewMatrix = viewMat
        , projectionMatrix = projMat
        , edgeColor = style.edgeColor
        , viewportWidth = aspect * edgeReferenceHeight
        , viewportHeight = edgeReferenceHeight
        , lineWidth = style.edgeWidth
        }


componentArrows : Camera -> Float -> Model -> List WebGL.Entity
componentArrows camera aspect model =
    let
        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)
    in
    model.componentRenders
        |> List.filter (\c -> Pick.isSphereVisible camera aspect c.center 14.0)
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
                        Meshes.rotationAround angle component.axis component.center

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
