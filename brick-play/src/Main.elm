module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , accomplishmentBanner
    , helpRows
    , init
    , kitItemName
    , main
    , remainingCount
    , remainingForItem
    , shareHash
    , subscriptions
    , technicMode
    , thumbnailRequests
    , update
    , view
    , viewToast
    )

{-| Main application for Brick Play.

A free-building studio with kit inventory tracking and stud snapping. There is
no axis gizmo: a drag on a part moves it relative to the view, a drag on empty
space pans and a right-drag orbits, and while something is selected a floating
action bar above the dock turns, tips, raises, lowers or deletes it (the same as
the keyboard). The camera has its own control bottom-right, the shared camera
globe (`UI.CameraGlobe`): drag it to orbit, tap it to fly there, and use its
zoom slider and reset button. On a narrow portrait window (a phone) the bottom
area shows either the dock or the camera globe, switched by a toggle button, and
the dock is shown first.

The dock lists either the curated Technic catalog with its category tabs, in
Technic mode, or else the counted kit (a `parts=` kit from the URL, or the
default 10 yellow bricks). In Technic mode everything is unlimited: a URL kit
adds a Project tab in front of the catalog tabs, listing the kit's parts, and
that tab is selected first. Each card is a rendered picture of the part
(`UI.PartCard.compactView`, fed by the shared thumbnail pipeline through `Ports`).

Technic mode is derived, not set: it is on when the document holds a gear that
`Contraption.Machine` recognises, or when the URL kit lists a gear. In Technic
mode the play button shows and Space plays and pauses the machine; the gear mesh
status pill, automatic tooth alignment and the accomplishment banner come on, and
drags keep their height. Outside it, Space is unbound and the play button is
hidden. Technic parts (gears, axles, beams, pins) go on the shared axle line in
either mode (`Contraption.Placement`), and other parts rest on the ground. Any
edit stops a running machine and rewinds it to 0.

The top-right menu (`UI.HamburgerMenu`) holds undo, redo, delete and clear,
beside a visible ? button that opens the shortcut list and the play button;
Escape closes the menu first.

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
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.GearFeedback as GearFeedback
import Studio.Shortcuts as StudioShortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
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


{-| Default inventory when none is provided: 10 yellow 2x4 bricks.
-}
defaultInventory : List PartKitItem
defaultInventory =
    [ { part = "3001.dat", color = 14, count = 10 } ]


{-| The height of the motor, in revolutions per minute, for the Technic machine.
-}
motorRpm : Float
motorRpm =
    30.0


{-| Main application model.
-}
type alias Model =
    { studio : Core.Model
    , inventory : List PartKitItem
    , kitFromUrl : Bool
    , machine : MachineState
    , playing : Bool
    , currentTime : Float
    , simulation : Simulation.Model
    , animated : Animated
    , activeCategory : Category
    , helpOpen : Bool
    , menuOpen : Bool
    , windowWidth : Int
    , windowHeight : Int
    , thumbnails : Dict String Thumbnail
    , globeDrag : Maybe CameraGlobe.Drag
    , cameraShown : Bool
    , lastTapped : Maybe String
    , toast : Maybe { message : String, id : Int }
    , toastCounter : Int
    , snapshotModal : Maybe SnapshotModal.Model
    }


{-| A rendered dock image: its data URL and the part's .dat title (`""` when
unknown). Kept under `PartCard.thumbnailKey file color`.
-}
type alias Thumbnail =
    { url : String
    , title : String
    }


{-| Messages handled by the Brick Play application.
-}
type Msg
    = StudioMsg Core.Msg
    | AddPart PartKitItem
    | AddCatalogPart PartItem
    | SetCategory Category
    | Undo
    | Redo
    | Delete
    | TurnSelection TurnDirection
    | RaiseSelection
    | LowerSelection
    | ToggleFine
    | Clear
    | FitView
    | TogglePlay
    | Tick Float
    | ToggleRealistic
    | CaptureSnapshot
    | SnapshotReady { dataUrl : String, filename : String }
    | SnapshotProgress { fraction : Float, samples : Int }
    | SnapshotFailed String
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
    | WindowResized Int Int
    | ThumbnailLoaded { file : String, color : Int, url : String, title : String }


{-| Initialize the Brick Play application.
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

        kitFromUrl =
            not (List.isEmpty payload.parts)

        inventory =
            if kitFromUrl then
                payload.parts

            else
                defaultInventory

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
                        -- Mouse: left-drag empty space pans, right-drag orbits.
                        -- Touch: one finger on empty space pans; the globe orbits.
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                    )

        initialModel =
            { studio = initialStudio
            , inventory = inventory
            , kitFromUrl = kitFromUrl
            , machine = Machine.analyseMachine initialDoc initialStudio.viewport.partCache
            , playing = False
            , currentTime = 0.0
            , simulation =
                Simulation.init
                    { gearSpecs = Data.gearParts
                    , lodCache = EmbeddedParts.lodCache
                    , maxRpm = motorRpm
                    , initialRpm = motorRpm
                    , requestedMotorIndex = Nothing
                    }
            , animated = Animation.none
            , activeCategory =
                if kitFromUrl then
                    ProjectCategory

                else
                    GearsCategory
            , helpOpen = False
            , menuOpen = False
            , windowWidth = 1280
            , windowHeight = 800
            , thumbnails = Dict.empty
            , globeDrag = Nothing
            , cameraShown = False
            , lastTapped = Nothing
            , toast = Nothing
            , toastCounter = 0
            , snapshotModal = Nothing
            }

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))
    in
    ( withTechnicSettings initialModel
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , fetchViewportSize
        , requestDockThumbnails initialModel
        ]
    )


{-| Technic mode, derived from the model: on when the document holds a gear that
`Contraption.Machine` recognises, or when the URL kit lists a gear part. Technic
mode is also what offers the catalog dock (see `catalogDock`).
-}
technicMode : Model -> Bool
technicMode model =
    not (List.isEmpty model.machine.gears)
        || (model.kitFromUrl && List.any (\item -> isGearFile item.part) model.inventory)


isGearFile : String -> Bool
isGearFile file =
    List.any (\spec -> spec.partFile == file) Data.gearParts


{-| The dock lists the Technic catalog, with its category tabs, exactly when
Technic mode is on (unlimited). Otherwise it lists the counted kit.
-}
catalogDock : Model -> Bool
catalogDock model =
    technicMode model


{-| The URL kit as catalog parts, for the Project tab: empty without a URL kit.
-}
projectParts : Model -> List PartItem
projectParts model =
    if model.kitFromUrl then
        List.map Catalog.fromKitItem model.inventory

    else
        []


{-| The catalog parts of the visible category, in tab order: the project parts
for the Project tab, else the curated parts of the category.
-}
catalogItems : Model -> List PartItem
catalogItems model =
    Catalog.partsForCategoryWithProject (projectParts model) model.activeCategory


{-| The (file, colour) pairs the dock shows: the catalog parts of the visible
category in catalog mode, else the kit items.
-}
dockPairs : Model -> List ( String, Int )
dockPairs model =
    if catalogDock model then
        List.map (\item -> ( item.file, item.color )) (catalogItems model)

    else
        List.map (\item -> ( item.part, item.color )) model.inventory


{-| Ask for the images of the parts in the dock that have none yet.
-}
requestDockThumbnails : Model -> Cmd Msg
requestDockThumbnails model =
    case thumbnailRequests (dockPairs model) model.thumbnails of
        [] ->
            Cmd.none

        requests ->
            Ports.requestThumbnails requests


{-| The thumbnails to ask the pipeline for: one per distinct (file, colour) in
the list that has none yet. `rgb` comes from the colour table the 3D view uses,
so the pictures match the parts on the canvas.
-}
thumbnailRequests : List ( String, Int ) -> Dict String Thumbnail -> List { file : String, color : Int, rgb : List Float }
thumbnailRequests items thumbnails =
    let
        addRequest ( file, color ) ( seen, acc ) =
            let
                key =
                    PartCard.thumbnailKey file color
            in
            if List.member key seen || Dict.member key thumbnails then
                ( seen, acc )

            else
                ( key :: seen
                , { file = file
                  , color = color
                  , rgb =
                        Colors.resolveColor color 16
                            |> (\c -> [ c.r, c.g, c.b ])
                  }
                    :: acc
                )
    in
    List.foldl addRequest ( [], [] ) items
        |> Tuple.second
        |> List.reverse


{-| The readable name of a dock catalog part: a project part is named as a placed
part is (its title from the pipeline, else its part number), a curated part by
its catalogue name.
-}
dockName : Model -> PartItem -> String
dockName model item =
    case item.category of
        ProjectCategory ->
            placedName model item.file item.color

        _ ->
            item.name


{-| The readable name of a kit item: the part's .dat title once the pipeline has
sent it ("Brick 2 x 4"), else the part number ("3001").
-}
kitItemName : Model -> PartKitItem -> String
kitItemName model item =
    case Dict.get (PartCard.thumbnailKey item.part item.color) model.thumbnails of
        Just thumb ->
            if String.isEmpty (String.trim thumb.title) then
                partNumber item.part

            else
                thumb.title

        Nothing ->
            partNumber item.part


partNumber : String -> String
partNumber file =
    String.replace ".dat" "" file


{-| The name of the selection for the action bar: the part's name when one part
is selected, else how many parts are.
-}
selectionName : Model -> String
selectionName model =
    case Set.toList model.studio.selection of
        [ id ] ->
            case List.filter (\p -> p.id == id) (Core.getPlacements model.studio) of
                placed :: _ ->
                    placedName model placed.part placed.color

                [] ->
                    ""

        ids ->
            String.fromInt (List.length ids) ++ " parts"


{-| A placed part's name: its Technic catalogue name when it has one, else the
title the pipeline sent, else its part number.
-}
placedName : Model -> String -> Int -> String
placedName model file color =
    case List.filter (\item -> item.file == file) Catalog.curatedParts of
        item :: _ ->
            item.name

        [] ->
            case Dict.get (PartCard.thumbnailKey file color) model.thumbnails of
                Just thumb ->
                    if String.isEmpty (String.trim thumb.title) then
                        partNumber file

                    else
                        thumb.title

                Nothing ->
                    partNumber file


{-| Remaining count for a specific kit item.
-}
remainingForItem : Model -> PartKitItem -> Int
remainingForItem model item =
    let
        placed =
            Core.getPlacements model.studio
                |> List.filter (\p -> p.part == item.part && p.color == item.color)
                |> List.length
    in
    max 0 (item.count - placed)


{-| Total remaining parts across the entire kit inventory.
-}
remainingCount : Model -> Int
remainingCount model =
    List.sum (List.map (remainingForItem model) model.inventory)


{-| Encode current document and kit inventory into a shareable URL hash. The
Technic catalog is not part of a link: its gears come with the model.
-}
shareHash : Model -> String
shareHash model =
    let
        doc =
            model.studio.document

        modelStr =
            Writer.writeDocument doc

        -- Technic mode without a URL kit shares the model alone: its catalog
        -- is not part of a link. A URL kit is kept, unlimited, as its Project tab.
        partsStr =
            if technicMode model && not model.kitFromUrl then
                ""

            else
                UrlPayload.encodeParts model.inventory

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


{-| Update the application model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if closesSnapshot msg model then
        ( { model | snapshotModal = Nothing }, cancelSnapshotCmd model.snapshotModal )

    else
        updateApp msg model


{-| Stop the path tracer when the snapshot modal closes while it is still rendering.
-}
cancelSnapshotCmd : Maybe SnapshotModal.Model -> Cmd msg
cancelSnapshotCmd modal =
    case modal of
        Just rendering ->
            if SnapshotModal.isRendering rendering then
                Ports.cancelSnapshot ()

            else
                Cmd.none

        Nothing ->
            Cmd.none


{-| Escape closes an open snapshot modal before anything else sees the key, so
the editing keys and the layers underneath do not react as well.
-}
closesSnapshot : Msg -> Model -> Bool
closesSnapshot msg model =
    case ( model.snapshotModal, msg ) of
        ( Just _, StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            ViewportShortcuts.closesDialog keyEvent

        _ ->
            False


{-| Run a message, then bring the derived state up to date (`settle`). The dock
asks for the pictures of its new parts when the parts it lists change.
-}
updateApp : Msg -> Model -> ( Model, Cmd Msg )
updateApp msg model =
    let
        ( next, cmd ) =
            step msg model

        settled =
            settle model next

        dockCmd =
            if dockPairs model /= dockPairs settled then
                requestDockThumbnails settled

            else
                Cmd.none
    in
    ( settled, Cmd.batch [ cmd, dockCmd ] )


{-| Brings the derived state in line with what a message just did. A changed
document is analysed again for its gears, and the machine comes to rest. Then
Technic mode sets the studio's gear alignment and drag settling, and a machine
that is no longer Technic stops.
-}
settle : Model -> Model -> Model
settle before after =
    let
        edited =
            after.studio.document.submodels /= before.studio.document.submodels

        analysed =
            if edited then
                { after | machine = Machine.analyseMachine after.studio.document after.studio.viewport.partCache }

            else
                after

        rested =
            if edited || (not (technicMode analysed) && (analysed.playing || analysed.currentTime /= 0)) then
                stopForEdit analysed

            else
                analysed
    in
    withTechnicSettings rested


{-| Technic mode turns on gear alignment (a meshing gear turns its axle line so
the teeth interlock) and stops drags from settling to the ground, so gear and
axle axes stay on the axle line. Plain Brick Play has neither.
-}
withTechnicSettings : Model -> Model
withTechnicSettings model =
    let
        technic =
            technicMode model
    in
    { model
        | studio =
            model.studio
                |> Core.setGearAlignment technic
                |> Core.setDragSettles (not technic)
    }


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


{-| Handle one message, without the derived-state bookkeeping of `updateApp`.
-}
step : Msg -> Model -> ( Model, Cmd Msg )
step msg model =
    case msg of
        StudioMsg studioMsg ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.update studioMsg model.studio

                portCmds =
                    handleCoreEvents events

                nextModel =
                    { model | studio = nextStudio }

                syncCmd =
                    syncDocHash nextModel events
            in
            List.foldl applyShortcut
                ( nextModel
                , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncCmd ]
                )
                (List.filterMap extractKey events)

        TogglePlay ->
            -- Outside Technic mode there is no machine, so Space is unbound.
            if not (technicMode model) then
                ( model, Cmd.none )

            else if model.playing then
                ( { model | playing = False }, Cmd.none )

            else
                ( startPlaying model, Cmd.none )

        AddPart item ->
            if remainingForItem model item <= 0 then
                ( model, Cmd.none )

            else
                placePart item.part item.color (kitItemName model item) model

        AddCatalogPart item ->
            placePart item.file item.color (dockName model item) model

        SetCategory category ->
            ( { model | activeCategory = category }, Cmd.none )

        Undo ->
            runCore Core.undo model

        Redo ->
            runCore Core.redo model

        Delete ->
            runCore Core.deleteSelection model

        TurnSelection direction ->
            runCore (Core.turnSelected direction) model

        RaiseSelection ->
            runCore Core.raiseSelected model

        LowerSelection ->
            runCore Core.lowerSelected model

        Clear ->
            runCore (Core.resetDocument Document.empty) model

        ToggleFine ->
            ( { model | studio = Core.setFineMove (not (Core.fineMove model.studio)) model.studio }
            , Cmd.none
            )

        ToggleRealistic ->
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
            ( { model | snapshotModal = Just (SnapshotModal.init { filename = "brick-play-render.png" }) }
            , Ports.captureRender
                (Viewport.snapshotRequest
                    { filename = "brick-play-render.png"
                    , extraTriangles = snapshotTriangles model
                    }
                    model.studio.viewport
                )
            )

        SnapshotReady payload ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setReady payload) model.snapshotModal }
            , Cmd.none
            )

        SnapshotProgress progress ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setProgress progress) model.snapshotModal }
            , Cmd.none
            )

        SnapshotFailed message ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setFailed message) model.snapshotModal }
            , Cmd.none
            )

        SnapshotSave payload ->
            ( { model | snapshotModal = Maybe.map SnapshotModal.markSaved model.snapshotModal }
            , Ports.saveSnapshot payload
            )

        SnapshotClose ->
            ( { model | snapshotModal = Nothing }
            , cancelSnapshotCmd model.snapshotModal
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
                    newTime =
                        model.currentTime + delta / 1000.0
                in
                ( { model
                    | currentTime = newTime
                    , simulation = Simulation.setTime newTime model.simulation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        GlobePressed point ->
            ( { model | globeDrag = Just (CameraGlobe.startDrag model.studio.viewport.camera point) }, Cmd.none )

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

        WindowResized w h ->
            -- The viewport projects picks and gizmo handles through its own
            -- size, so it must track the canvas, not keep its 800×600 default.
            update (viewportMsg (Viewport.WindowResize w h)) { model | windowWidth = w, windowHeight = h }

        ThumbnailLoaded { file, color, url, title } ->
            ( { model
                | thumbnails =
                    Dict.insert (PartCard.thumbnailKey file color) { url = url, title = title } model.thumbnails
              }
            , Cmd.none
            )


{-| Put a dock part into the studio: a Technic part on the shared axle line, next
to the selection (`Contraption.Placement`), anything else on a free ground spot
clear of every piece already placed.
-}
placePart : String -> Int -> String -> Model -> ( Model, Cmd Msg )
placePart file color name model =
    let
        ( nextStudio, studioCmd, events ) =
            case Placement.targetFor file model.studio of
                Placement.AtPlacement matrix ->
                    Core.addPartAt file color matrix model.studio

                Placement.OnGround spot ->
                    Core.addPart file color spot model.studio

        nextModel =
            { model | studio = nextStudio, lastTapped = Just name }
    in
    ( nextModel
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events, syncDocHash nextModel events ]
    )


{-| The gears and driven parts drawn turning in the snapshot: only in Technic
mode, where the machine has a simulation to draw.
-}
snapshotTriangles : Model -> List Float
snapshotTriangles model =
    if technicMode model then
        Simulation.snapshotTriangles model.simulation

    else
        []


syncKeyPause : Model -> Model
syncKeyPause model =
    { model | studio = Core.setHelpVisible (model.helpOpen || model.menuOpen) model.studio }


{-| Run a studio edit and forward its effects.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore edit model =
    let
        ( nextStudio, studioCmd, events ) =
            edit model.studio

        nextModel =
            { model | studio = nextStudio }
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


{-| Map a viewport shortcut to the message it stands for. Space is
`TogglePlay`, which does nothing outside Technic mode.
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


{-| Global subscriptions for Brick Play. The machine's clock runs only while it
plays.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , Ports.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , Ports.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Browser.Events.onResize WindowResized
        , Ports.thumbnailLoaded ThumbnailLoaded
        , Ports.snapshotReady SnapshotReady
        , Ports.snapshotProgress SnapshotProgress
        , Ports.snapshotFailed SnapshotFailed
        , if model.playing then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]


{-| Render the Brick Play interface.
-}
view : Model -> Html Msg
view model =
    let
        accomplished =
            technicMode model && Machine.isAccomplished model.machine

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
        [ -- One row across the top: the accomplishment banner (Technic mode) sits in
          -- the middle, the help, play and menu buttons on the right. The banner
          -- shares the buttons' height and top edge, and shrinks before it reaches them.
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
            [ h1 [ Attr.class "sr-only" ] [ text "Palikkakehys Play" ]
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
                , if technicMode model then
                    playButton model.playing

                  else
                    text ""
                , HamburgerMenu.view (menuConfig model)
                ]
            ]

        -- 3D WebGL Canvas
        , Html.map StudioMsg (viewCanvas model)

        -- Camera control: on a wide window the camera globe sits bottom-right,
        -- beside the dock. On a narrow window it shares the bottom row instead.
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
            -- Above the header and bottom area (z-index 10 and 20), which would
            -- otherwise cover the top and bottom of the dialog.
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" "0"
                , Attr.style "z-index" "30"
                , Attr.style "pointer-events" "none"
                ]
                [ ShortcutHelp.view { rows = helpRows model, onClose = ToggleHelp } ]

          else
            text ""

        -- Bottom: the gear pill (Technic mode) and the selection action bar float
        -- above the dock
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


{-| The bottom area: the gear pill (Technic mode only), the action bar for the
selection, then the dock. On a narrow window the dock row also carries the camera
toggle, and shows the camera globe instead of the dock while the toggle is on.
-}
bottomArea : Model -> Html Msg
bottomArea model =
    div
        ([ Attr.style "position" "absolute"
         , Attr.style "bottom" "0.75rem"
         , Attr.style "display" "flex"
         , Attr.style "flex-direction" "column"
         , Attr.style "align-items" "center"
         , Attr.style "gap" "0.5rem"
         , Attr.style "z-index" "10"
         , Attr.style "pointer-events" "none"
         ]
            ++ dockPlacement model.windowWidth
        )
        [ if technicMode model then
            gearRow model

          else
            text ""
        , selectionBar model
        , touchLabel model
        , if isNarrow model then
            narrowRow model

          else
            viewDock model
        ]


{-| The narrow bottom row: the dock (or the camera globe) and the toggle between
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


{-| The shared camera globe, with the zoom slider, the reset button and the
realistic-rendering and snapshot buttons.
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
                    ToggleRealistic
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


{-| Where the bottom area sits. Centred when there is room, leaving the globe its
corner on the right; on a narrow window it spans the width.
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


{-| The row holding the gear mesh status pill, truncated rather than pushing the
action bar off a phone.
-}
gearRow : Model -> Html Msg
gearRow model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "min-width" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "white-space" "nowrap"
        ]
        [ gearPill model ]


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
    "play-menu"


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
            if technicMode model && Animation.isAnimating { playing = model.playing, currentTime = model.currentTime } then
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


{-| Buttons for the selected parts, the touch- and mouse-friendly stand-in for an
axis gizmo. Each does what its keyboard shortcut does. The name of the selection
is shown on touch screens, where tooltips never show. The bar wraps onto two rows
rather than overflowing a phone. The Fine toggle is always last, so it can be set
before anything is selected; without a selection the bar shows only that toggle.
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


{-| The dock: the Technic catalog with its tabs in Technic mode, else the
counted kit.
-}
viewDock : Model -> Html Msg
viewDock model =
    if catalogDock model then
        catalogDockView model

    else
        kitDockView model


{-| The counted kit: one picture card per kit item, its remaining count on the
image corner.
-}
kitDockView : Model -> Html Msg
kitDockView model =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Kit"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "height" PartCard.compactPanelHeight
        , Attr.style "max-width" "100%"
        , Attr.style "overflow-x" "auto"
        , Attr.style "padding" "0.5rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.9)"
        , Attr.style "backdrop-filter" "blur(12px)"
        , Attr.style "border-radius" "1rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
        , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
        , Attr.style "pointer-events" "auto"
        , keepFocusOnCanvas
        ]
        (List.map (viewKitCard model) model.inventory)


{-| A kit item as a compact picture card. Its name, part number, colour and
count are in the tooltip and accessible name; the image corner shows "left/kit".
-}
viewKitCard : Model -> PartKitItem -> Html Msg
viewKitCard model item =
    let
        remaining =
            remainingForItem model item

        name =
            kitItemName model item

        colourName =
            Colours.nameOf item.color
    in
    PartCard.compactView
        { imageUrl =
            Dict.get (PartCard.thumbnailKey item.part item.color) model.thumbnails
                |> Maybe.map .url
        , tooltip = name ++ " (" ++ partNumber item.part ++ ", " ++ colourName ++ ") — " ++ String.fromInt remaining ++ " of " ++ String.fromInt item.count ++ " available"
        , ariaLabel = "Add " ++ name ++ " (" ++ colourName ++ "), " ++ String.fromInt remaining ++ " of " ++ String.fromInt item.count ++ " left"
        , badge =
            Just
                { text = String.fromInt remaining ++ "/" ++ String.fromInt item.count
                , muted = remaining == 0
                }
        , onClick =
            if remaining > 0 then
                Just (AddPart item)

            else
                Nothing
        , attributes = [ keepFocusOnCanvas ]
        }


{-| The Technic catalog dock: the category tabs in a narrow column on the left,
then the picture cards of the visible category. The cards are unlimited.
-}
catalogDockView : Model -> Html Msg
catalogDockView model =
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
            (List.map (catalogCard model) (catalogItems model))
        ]


{-| The category tabs: a narrow column on the left of the catalog dock. The
Project tab comes first, and only when the URL gave a kit.
-}
categoryTabs : Model -> Html Msg
categoryTabs model =
    let
        categories =
            if model.kitFromUrl then
                [ ProjectCategory, GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]

            else
                [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
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


{-| One catalog part as a compact picture card. Its name, part number and
description are in the tooltip and the accessible name.
-}
catalogCard : Model -> PartItem -> Html Msg
catalogCard model item =
    let
        name =
            dockName model item
    in
    PartCard.compactView
        { imageUrl = Maybe.map .url (Dict.get (PartCard.thumbnailKey item.file item.color) model.thumbnails)
        , tooltip = name ++ " (" ++ item.file ++ ")" ++ descriptionSuffix item.description
        , ariaLabel = "Add " ++ name ++ " (" ++ partNumber item.file ++ ")"
        , badge = Nothing
        , onClick = Just (AddCatalogPart item)
        , attributes = [ keepFocusOnCanvas ]
        }


descriptionSuffix : String -> String
descriptionSuffix description =
    if String.isEmpty description then
        ""

    else
        " — " ++ description


{-| Clicking a toolbar, dock or pad button must not leave it focused: a focused
button takes Space for itself, so Space would undo or add a part instead of
playing and pausing.
-}
keepFocusOnCanvas : Html.Attribute Msg
keepFocusOnCanvas =
    Focus.keepFocusOnCanvas NoOp


{-| The shortcut list. The Machine section (Space and the gear rules) is listed
only in Technic mode, where it applies.
-}
helpRows : Model -> List ( String, List ( String, String ) )
helpRows model =
    [ ( "Build"
      , [ ( "Menu, top right", "Undo, redo, delete, clear, and this list" )
        , ( "Drag a part", "Move it as seen from the camera (snaps to studs)" )
        , ( "Fine (action bar) or Shift", "1 LDU moves for drags and W A S D Q E nudges" )
        , ( "Wheel while dragging", "Raise / lower (Shift: coarser, Alt: finer)" )
        , ( "Right click mid-drag", "Cancel the drag" )
        , ( "Action bar (above the dock)", "Turn left / right, tip, raise, lower or remove the selected part; Fine toggles 1 LDU moves" )
        , ( "W A S D  Q E", "Move the selected part (Shift: fine steps)" )
        , ( "← → ↑ ↓", "Turn the selected part (Shift: 45°)" )
        , ( "Delete", "Remove the selected part" )
        , ( "Ctrl + Z", "Undo" )
        , ( "Ctrl + Shift + Z  Ctrl + Y", "Redo" )
        , ( "Ctrl + A", "Select all" )
        , ( "Ctrl + click  Shift + click", "Add to or remove from the selection" )
        , ( "Esc", "Cancel a drag, or deselect" )
        ]
      )
    , ( "Camera"
      , [ ( "Drag the globe, bottom right", "Orbit the camera (also by touch)" )
        , ( "Tap the globe", "Fly to that side of the model" )
        , ( "Zoom slider", "Zoom in and out" )
        , ( "Reset view, or F", "Fit the whole model in view" )
        , ( "Drag empty space", "Pan" )
        , ( "Right-drag", "Orbit" )
        , ( "Middle drag", "Pan" )
        , ( "One finger on empty space", "Pan (two fingers: pan and pinch-zoom)" )
        , ( "Wheel  + -", "Zoom" )
        , ( "1 2 3 4 5 6  `", "Views: front, back, left, right, top, bottom, 3/4" )
        , ( "G", "Street-level walk, on / off" )
        ]
      )
    ]
        ++ (if technicMode model then
                [ ( "Machine"
                  , [ ( "Space", "Play / pause the machine (an edit stops it and rewinds)" )
                    , ( "Gears turn to mesh", "A gear that meets another turns its axle line so the teeth interlock" )
                    , ( "Gear pill", "With one gear selected, says how it meets its nearest gear" )
                    ]
                  )
                ]

            else
                []
           )
        ++ [ ( "Help"
             , [ ( "?  /", "This list" ) ]
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
