module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , accomplishmentBanner
    , hashFor
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

There are two modes, chosen by the URL hash (see `Link` for the formats):

  - **Free building** (no hash, or `free=1`): the dock is the curated catalog of
    `Free.Catalog`, unlimited, with a colour row that sets the colour of the parts
    added next and a search box over the whole part library (`parts-index.json`,
    loaded on first use). Parts can be glued to the board (the Glue button, or `L`):
    a glued part is locked, and a glued part is written as `0 !PALIKKA GLUED`.
  - **Puzzle** (any other hash): the dock is the counted kit (`parts=`, or the
    default 10 yellow bricks for a bare `model=`). Glued parts are locked and
    cannot be unglued, and the Glue button is hidden. Clear keeps the glued parts.

Each dock card is a rendered picture of the part (`UI.PartCard.compactView`, fed
by the shared thumbnail pipeline through `Ports`).

Technic mode is derived, not set: it is on when the document holds a gear that
`Technic.Machine` recognises, or when a puzzle's kit lists a gear. In Technic
mode the play button shows and Space plays and pauses the machine; the gear mesh
status pill, automatic tooth alignment and the accomplishment banner come on, and
drags keep their height. Outside it, Space is unbound and the play button is
hidden. Technic parts (gears, axles, beams, pins) go on the shared axle line in
either mode (`Technic.Placement`), and other parts rest on the ground. Any
edit stops a running machine and rewinds it to 0.

The top-right menu (`UI.HamburgerMenu`) holds share link, undo, redo, delete and
clear, beside a visible ? button that opens the shortcut list and the play button;
Escape closes the menu first. Share link copies a puzzle: for a free build, its
glued parts stay on the board and everything else becomes the kit.

Tidy up (`Studio.Health`) checks the build after each change: a studded part that
sits off the stud grid, or parts that overlap, show a pill above the action bar
("2 loose bricks · Tidy up"). A healthy build shows nothing. The pill opens a
preview: the parts that move are outlined, a translucent ghost shows each new
place, and Apply makes all the moves one undo step. While the preview is open
editing is paused; Escape or Cancel leaves the build as it was.

-}

import Array
import Browser
import Browser.Dom
import Browser.Events
import Data
import Dict exposing (Dict)
import EmbeddedParts
import FeatherIcons
import Free.Catalog as FreeCatalog
import Gear.Placements as Placements
import Html exposing (Html, button, div, h1, h2, header, input, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onFocus, onInput)
import Html.Keyed as Keyed
import Http
import LDraw.Colors as Colors
import LDraw.Document as Document
import Link
import Ports
import Process
import Render.Camera as Camera
import Render.Style as Style
import Set
import Shortcuts as PlayShortcuts
import Simulation
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.GearFeedback as GearFeedback
import Studio.Health as Health
import Studio.PartIndex as PartIndex
import Studio.PartSearch as PartSearch
import Studio.Shortcuts as StudioShortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
import Task
import Technic.Animation as Animation exposing (Animated)
import Technic.Catalog as Catalog
import Technic.Machine as Machine exposing (MachineState)
import Technic.Placement as Placement
import UI.Button as Button
import UI.CameraGlobe as CameraGlobe
import UI.Focus as Focus
import UI.HamburgerMenu as HamburgerMenu
import UI.Icon as Icon
import UI.PartCard as PartCard
import UI.ShortcutHelp as ShortcutHelp
import UI.SnapshotModal as SnapshotModal
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
    , free : Bool
    , inventory : List PartKitItem
    , ldrawBase : String
    , machine : MachineState
    , playing : Bool
    , currentTime : Float
    , simulation : Simulation.Model
    , animated : Animated
    , activeTab : FreeCatalog.Tab
    , paintColour : Int
    , search : Search
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
    , tidy : List Health.Fix
    , tidyOpen : Bool
    }


{-| A rendered dock image: its data URL and the part's .dat title (`""` when
unknown). Kept under `PartCard.thumbnailKey file color`.
-}
type alias Thumbnail =
    { url : String
    , title : String
    }


{-| The dock's part search: what was typed, the part library once it has been
fetched (`parts-index.json` is only read when the search is first used), and the
matches for the query.
-}
type alias Search =
    { query : String
    , index : SearchIndex
    , results : List PartIndex.Part
    }


{-| Whether the part library has arrived.
-}
type SearchIndex
    = NotLoaded
    | Loading
    | Loaded PartIndex.PartIndex
    | Failed String


{-| The most matches the dock lists at once.
-}
resultLimit : Int
resultLimit =
    40


{-| The colours the free dock's colour row offers, in row order (LDraw codes:
black, blue, green, red, white, light bluish gray, tan, yellow).
-}
paintColours : List Int
paintColours =
    [ 14, 15, 4, 1, 2, 0, 71, 19 ]


{-| Messages handled by the Brick Play application.
-}
type Msg
    = StudioMsg Core.Msg
    | AddPart PartKitItem
    | AddFreePart { file : String, color : Int, name : String }
    | SetFreeTab FreeCatalog.Tab
    | SetPaintColour Int
    | SearchInput String
    | SearchOpened
    | SearchLoaded (Result Http.Error PartIndex.PartIndex)
    | ToggleGlue
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
    | OpenTidy
    | CancelTidy
    | ApplyTidy
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

        -- No hash, or `free=1`, is free building; any other hash is a puzzle.
        free =
            payload.free || String.isEmpty (String.trim (String.replace "#" "" flags.initialHash))

        inventory =
            if free then
                []

            else if payload.partsListed then
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
                        -- Glue locks parts in a puzzle; free building may glue and unglue.
                        >> Core.setGlueEditable free
                    )

        initialModel =
            { studio = initialStudio
            , free = free
            , inventory = inventory
            , ldrawBase = flags.ldrawBase
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
            , activeTab = FreeCatalog.BricksTab
            , paintColour = 14
            , search = emptySearch
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
            , tidy = []
            , tidyOpen = False
            }

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))
    in
    ( withTechnicSettings (withTidy initialModel)
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , fetchViewportSize
        , requestDockThumbnails initialModel
        ]
    )


{-| Technic mode, derived from the model: on when the document holds a gear that
`Technic.Machine` recognises, or when a puzzle's kit lists a gear part. Free
building has no kit, so only the board's gears count there.
-}
technicMode : Model -> Bool
technicMode model =
    not (List.isEmpty model.machine.gears)
        || List.any (\item -> isGearFile item.part) model.inventory


isGearFile : String -> Bool
isGearFile file =
    List.any (\spec -> spec.partFile == file) Data.gearParts


{-| The search box is empty (or only spaces), so the dock shows its tab.
-}
notSearching : Model -> Bool
notSearching model =
    PartSearch.normaliseQuery model.search.query == ""


{-| The parts the free dock lists: the tab's entries, or the search results while
a search is typed. Results are offered in the chosen colour like the tabs.
-}
freeEntries : Model -> List FreeCatalog.Entry
freeEntries model =
    if notSearching model then
        FreeCatalog.entriesFor model.activeTab

    else
        List.map (\part -> { file = part.file, name = part.description, description = part.description }) model.search.results


{-| The (file, colour) pairs the dock shows: the free entries in the paint colour
in free building, else the kit items.
-}
dockPairs : Model -> List ( String, Int )
dockPairs model =
    if model.free then
        List.map (\entry -> ( entry.file, model.paintColour )) (freeEntries model)

    else
        List.map (\item -> ( item.part, item.color )) model.inventory


{-| The search state for a query, with its matches when the library is loaded.
-}
searchState : String -> SearchIndex -> Search
searchState query index =
    { query = query
    , index = index
    , results =
        case index of
            Loaded library ->
                PartSearch.matching resultLimit query library.parts

            _ ->
                []
    }


emptySearch : Search
emptySearch =
    searchState "" NotLoaded


{-| The part library, fetched on first use: returns the index state and the
command that fetches it, if it has to.
-}
ensureLibrary : String -> SearchIndex -> ( SearchIndex, Cmd Msg )
ensureLibrary ldrawBase index =
    case index of
        NotLoaded ->
            ( Loading, PartIndex.fetch ldrawBase SearchLoaded )

        _ ->
            ( index, Cmd.none )


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


{-| Remaining count for a specific kit item: its count less the unglued parts of
that file and colour on the board. A glued part is part of the puzzle, not drawn
from the kit, so it does not count.
-}
remainingForItem : Model -> PartKitItem -> Int
remainingForItem model item =
    let
        placed =
            Core.getPlacements model.studio
                |> List.filter (\p -> p.part == item.part && p.color == item.color && not (Core.isGlued p.id model.studio))
                |> List.length
    in
    max 0 (item.count - placed)


{-| Total remaining parts across the entire kit inventory.
-}
remainingCount : Model -> Int
remainingCount model =
    List.sum (List.map (remainingForItem model) model.inventory)


{-| The hash the address bar keeps for the build (see `Link`): the whole board
with its glue while free building, else the puzzle with its kit.
-}
liveHash : Model -> String
liveHash model =
    if model.free then
        Link.freeHash model.studio.document

    else
        Link.puzzleHash model.studio.document model.inventory


{-| The link Share link copies. A puzzle keeps the current state. A free build
shares a puzzle: its glued parts stay on the board, and the rest become the kit.
-}
shareHash : Model -> String
shareHash model =
    if model.free then
        Link.freeShareHash model.studio.document

    else
        liveHash model


{-| The hash to write after an update from `before` to `after`: the live hash
when the build changed (its document, its mode or its kit), else nothing. It
compares the models rather than reading the studio's events, so an edit that
emits no `DocumentChanged` (or a new one that forgets to) cannot leave the
address bar behind.
-}
hashFor : Model -> Model -> Maybe String
hashFor before after =
    if before.studio.document /= after.studio.document || before.free /= after.free || before.inventory /= after.inventory then
        Just (liveHash after)

    else
        Nothing


{-| Update the application model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if model.tidyOpen then
        updateTidying msg model

    else if closesSnapshot msg model then
        ( { model | snapshotModal = Nothing }, cancelSnapshotCmd model.snapshotModal )

    else
        updateApp msg model


{-| While the Tidy up preview is open editing is paused. Escape cancels, Apply
and Cancel act, and only camera, loading and layout messages pass through, so a
pointer press cannot start a drag and no key can edit the build.
-}
updateTidying : Msg -> Model -> ( Model, Cmd Msg )
updateTidying msg model =
    case msg of
        StudioMsg (Core.ViewportMsg (Viewport.KeyPressed keyEvent)) ->
            if ViewportShortcuts.closesDialog keyEvent then
                updateApp CancelTidy model

            else
                ( model, Cmd.none )

        CancelTidy ->
            updateApp msg model

        ApplyTidy ->
            updateApp msg model

        _ ->
            if passesWhileTidying msg then
                updateApp msg model

            else
                ( model, Cmd.none )


{-| The messages a Tidy up preview lets through: loading, layout, and the
camera's own controls (none of which edits the document).
-}
passesWhileTidying : Msg -> Bool
passesWhileTidying msg =
    case msg of
        StudioMsg (Core.ViewportMsg viewportEvent) ->
            case viewportEvent of
                Viewport.WindowResize _ _ ->
                    True

                Viewport.TopLevelLoaded _ _ ->
                    True

                Viewport.PartLoaded _ _ ->
                    True

                Viewport.FileContentReceived _ ->
                    True

                Viewport.FileLoadError _ ->
                    True

                Viewport.GeometryFlattened _ ->
                    True

                Viewport.GeometryFlattenFailed _ ->
                    True

                Viewport.AnimationFrame _ ->
                    True

                Viewport.Wheel _ ->
                    True

                Viewport.MouseMove _ _ ->
                    True

                Viewport.MouseUp _ _ ->
                    True

                Viewport.RotateCameraBy _ _ ->
                    True

                Viewport.StartHoldRotate _ _ ->
                    True

                Viewport.EndHoldRotate ->
                    True

                Viewport.FitView ->
                    True

                Viewport.ToggleWalkMode ->
                    True

                Viewport.SetViewPreset _ ->
                    True

                Viewport.SetRenderQuality _ ->
                    True

                Viewport.KeyReleased _ ->
                    True

                Viewport.AllKeysReleased ->
                    True

                Viewport.DismissError ->
                    True

                Viewport.ContextMenuOpened ->
                    True

                _ ->
                    False

        StudioMsg (Core.GeometryFlattened _) ->
            True

        StudioMsg (Core.GeometryFlattenFailed _) ->
            True

        WindowResized _ _ ->
            True

        ThumbnailLoaded _ ->
            True

        Tick _ ->
            True

        DismissToast _ ->
            True

        GlobePressed _ ->
            True

        GlobeMoved _ ->
            True

        GlobeReleased ->
            True

        ZoomTo _ ->
            True

        ZoomDone ->
            True

        ResetView ->
            True

        NoOp ->
            True

        _ ->
            False


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

        hashCmd =
            case hashFor model settled of
                Just hash ->
                    Ports.setUrlHash hash

                Nothing ->
                    Cmd.none
    in
    ( settled, Cmd.batch [ cmd, dockCmd, hashCmd ] )


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

        -- A part's geometry arriving can change what the build check can see.
        partsArrived =
            Dict.size before.studio.viewport.partCache /= Dict.size after.studio.viewport.partCache

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

        -- Mid-drag the document changes on every move, so the check waits for the drop.
        checked =
            if (edited || partsArrived) && after.studio.drag == Nothing then
                withTidy rested

            else
                rested
    in
    withTechnicSettings checked


{-| The Tidy up plan for the studio's document: what `Studio.Health` would move.
Empty when the build is healthy.
-}
tidyPlan : Core.Model -> List Health.Fix
tidyPlan studio =
    let
        document =
            studio.document

        cache =
            studio.viewport.partCache

        glued =
            Document.gluedIds document
    in
    Health.plan cache document glued (Health.check cache document glued)


withTidy : Model -> Model
withTidy model =
    { model | tidy = tidyPlan model.studio }


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
            in
            List.foldl applyShortcut
                ( nextModel
                , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
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

        AddFreePart part ->
            placePart part.file part.color part.name model

        SetFreeTab tab ->
            ( { model | activeTab = tab }, Cmd.none )

        SetPaintColour colour ->
            ( { model | paintColour = colour }, Cmd.none )

        SearchInput query ->
            let
                ( index, loadCmd ) =
                    ensureLibrary model.ldrawBase model.search.index
            in
            ( { model | search = searchState query index }, loadCmd )

        SearchOpened ->
            let
                ( index, loadCmd ) =
                    ensureLibrary model.ldrawBase model.search.index
            in
            ( { model | search = searchState model.search.query index }, loadCmd )

        SearchLoaded result ->
            let
                index =
                    case result of
                        Ok library ->
                            Loaded library

                        Err _ ->
                            Failed "the part library could not be fetched"
            in
            ( { model | search = searchState model.search.query index }, Cmd.none )

        ToggleGlue ->
            runCore (\studio -> Core.setGlued (not (Core.selectionGlued studio)) studio) model

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
            if model.free then
                runCore (Core.resetDocument Document.empty) model

            else
                -- A puzzle keeps its glued parts.
                runCore Core.clearUnglued model

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

        OpenTidy ->
            if List.isEmpty model.tidy then
                ( model, Cmd.none )

            else
                ( syncKeyPause
                    { model
                        | tidyOpen = True
                        , menuOpen = False
                        , studio = Core.setPreview model.tidy model.studio
                    }
                , requestTidyTitles model
                )

        CancelTidy ->
            ( syncKeyPause { model | tidyOpen = False, studio = Core.setPreview [] model.studio }, Cmd.none )

        ApplyTidy ->
            let
                ( studio, studioCmd, events ) =
                    Core.applyFixes model.tidy model.studio
            in
            ( syncKeyPause { model | tidyOpen = False, studio = studio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
            )

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
            -- The address bar keeps the build itself (`hashFor`); a free build's
            -- shared link is a puzzle, so only the copy goes out.
            ( { model
                | menuOpen = False
                , toast = Just { message = "Link copied to clipboard!", id = nextId }
                , toastCounter = nextId
              }
            , Cmd.batch
                [ Ports.copyToClipboard ("#" ++ hash)
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
to the selection (`Technic.Placement`), anything else on a free ground spot
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
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
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
    { model | studio = Core.setHelpVisible (model.helpOpen || model.menuOpen || model.tidyOpen) model.studio }


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
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, handleCoreEvents events ]
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
        case PlayShortcuts.actionFor { glueEditable = model.free, helpVisible = model.helpOpen, menuOpen = model.menuOpen } keyEvent of
            Just PlayShortcuts.ToggleGlue ->
                let
                    ( next, actionCmd ) =
                        update ToggleGlue model
                in
                ( next, Cmd.batch [ cmd, actionCmd ] )

            Nothing ->
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
        , if model.tidyOpen then
            tidyPanel model

          else
            text ""
        ]


{-| The Tidy up pill: shown only when the build has problems and no preview is
open. It sits in the bottom column, beside the gear pill, above the action bar.
-}
tidyRow : Model -> Html Msg
tidyRow model =
    if model.tidyOpen || List.isEmpty model.tidy then
        text ""

    else
        div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "center"
            , Attr.style "max-width" "100%"
            , Attr.style "min-width" "0"
            , Attr.style "overflow" "hidden"
            , Attr.style "white-space" "nowrap"
            ]
            [ tidyPill (List.length model.tidy) ]


tidyPill : Int -> Html Msg
tidyPill count =
    button
        [ onClick OpenTidy
        , Attr.attribute "data-tidy-pill" ""
        , Attr.title "Preview the tidy-up moves"
        , Attr.attribute "aria-haspopup" "dialog"
        , Attr.style "pointer-events" "auto"
        , Attr.style "align-self" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.3125rem 0.875rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border" "1px solid var(--color-brand-yellow)"
        , Attr.style "color" "var(--color-brand-yellow)"
        , Attr.style "font-family" "var(--font-sans)"
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "600"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "cursor" "pointer"
        ]
        [ text (tidyLabel count) ]


{-| The pill's words: "1 loose brick · Tidy up", "3 loose bricks · Tidy up".
-}
tidyLabel : Int -> String
tidyLabel count =
    (if count == 1 then
        "1 loose brick"

     else
        String.fromInt count ++ " loose bricks"
    )
        ++ " · Tidy up"


{-| The Tidy up preview: an accessible dialog listing the moves, with Apply and
Cancel. The moved parts are outlined and their ghosts drawn in the studio.
-}
tidyPanel : Model -> Html Msg
tidyPanel model =
    let
        count =
            List.length model.tidy

        summary =
            String.fromInt count
                ++ (if count == 1 then
                        " part will move."

                    else
                        " parts will move."
                   )
                ++ " The translucent copies show where."
    in
    div
        [ Attr.attribute "role" "dialog"
        , Attr.attribute "aria-modal" "true"
        , Attr.attribute "aria-labelledby" "tidy-title"
        , Attr.attribute "aria-describedby" "tidy-summary"
        , Attr.attribute "data-tidy-preview" ""
        , Attr.style "position" "absolute"
        , Attr.style "top" "4.5rem"
        , Attr.style "left" "1rem"
        , Attr.style "width" "min(22rem, calc(100% - 2rem))"
        , Attr.style "max-height" "calc(100% - 9rem)"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "1rem 1.125rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.95)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "border-radius" "0.75rem"
        , Attr.style "box-shadow" "0 4px 16px rgba(0, 0, 0, 0.5)"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "font-family" "var(--font-sans)"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "z-index" "25"
        , Attr.style "pointer-events" "auto"
        ]
        [ h2
            [ Attr.id "tidy-title"
            , Attr.style "margin" "0 0 0.375rem"
            , Attr.style "font-size" "1rem"
            , Attr.style "font-weight" "700"
            ]
            [ text "Tidy up" ]
        , p
            [ Attr.id "tidy-summary"
            , Attr.style "margin" "0 0 0.625rem"
            ]
            [ text summary ]
        , ul
            [ Attr.attribute "aria-label" "Planned moves"
            , Attr.style "margin" "0 0 0.875rem"
            , Attr.style "padding-left" "1.125rem"
            ]
            (List.map
                (\fix -> li [ Attr.style "margin" "0.125rem 0" ] [ text (Health.describe (tidyPartTitle model) model.studio.document fix) ])
                model.tidy
            )
        , div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "flex-end"
            , Attr.style "gap" "0.5rem"
            ]
            [ tidyButton False CancelTidy "Cancel"
            , tidyButton True ApplyTidy "Apply"
            ]
        ]


tidyButton : Bool -> Msg -> String -> Html Msg
tidyButton primary msg label =
    button
        [ onClick msg
        , Attr.style "padding" "0.375rem 0.875rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if primary then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if primary then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.class "touch-target"
        ]
        [ text label ]


{-| A part's readable name for Tidy up's sentences: its curated Free catalog
description ("2 x 4 brick") when it has one, else the .dat title the thumbnail
pipeline sent, lower-cased so it reads inside a sentence, else its part number.
-}
tidyPartTitle : Model -> String -> Int -> String
tidyPartTitle model file color =
    case
        List.concatMap FreeCatalog.entriesFor FreeCatalog.tabs
            |> List.filter (\entry -> String.toLower entry.file == String.toLower file)
            |> List.head
    of
        Just entry ->
            entry.description

        Nothing ->
            case Dict.get (PartCard.thumbnailKey file color) model.thumbnails of
                Just thumb ->
                    if String.isEmpty (String.trim thumb.title) then
                        "part " ++ partNumber file

                    else
                        String.toLower thumb.title

                Nothing ->
                    "part " ++ partNumber file


{-| Asks the thumbnail pipeline for the parts a Tidy up move names, so that the
move list can use their .dat titles. A part that already has a thumbnail is
skipped, and the pipeline's title arrives as `ThumbnailLoaded`.
-}
requestTidyTitles : Model -> Cmd Msg
requestTidyTitles model =
    let
        moved =
            List.filterMap
                (\fix ->
                    Document.getPlacement fix.placement model.studio.document
                        |> Maybe.map (\placed -> ( placed.part, placed.color ))
                )
                model.tidy
    in
    case thumbnailRequests moved model.thumbnails of
        [] ->
            Cmd.none

        requests ->
            Ports.requestThumbnails requests


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
        , tidyRow model
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


{-| The width of both docks (the free dock and the kit). Fixed, so the cards in
the dock, or the search results, never widen it: a dock sized to its content
would grow under the camera globe. A narrower window clips it to its own width.
-}
dockWidth : String
dockWidth =
    "40rem"


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
        , { label = "Clear", shortcut = "", enabled = clearable model, onSelect = MenuPicked Clear }
        ]
    }


{-| Whether Clear has anything to remove: any part in free building, else any
unglued part (a puzzle's glued parts stay).
-}
clearable : Model -> Bool
clearable model =
    if model.free then
        not (List.isEmpty (Core.getPlacements model.studio))

    else
        List.any (\p -> not (Core.isGlued p.id model.studio)) (Core.getPlacements model.studio)


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

        -- A selection of glued parts only is locked: its move buttons are off.
        movable =
            Core.selectionMovable model.studio

        glued =
            Core.selectionGlued model.studio

        -- The label says "Glued" when every selected part is, as the Glue button does.
        label =
            selectionName model
                ++ (if glued && not movable then
                        " (glued)"

                    else
                        ""
                   )

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
                , actionButton movable (TurnSelection TurnLeft) "rotate-ccw" "Turn left" "←"
                , actionButton movable (TurnSelection TurnRight) "rotate-cw" "Turn right" "→"
                , actionButton movable (TurnSelection TipForward) "corner-right-down" "Tip" "↑"
                , actionButton movable RaiseSelection "arrow-up" "Raise" "E"
                , actionButton movable LowerSelection "arrow-down" "Lower" "Q"
                , actionButton movable Delete "trash-2" "Delete" "Delete"
                ]
                    ++ (if model.free then
                            [ glueButton glued ]

                        else
                            []
                       )

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


{-| A selection button. When `enabled` is off it is dimmed and does nothing.
-}
actionButton : Bool -> Msg -> String -> String -> String -> Html Msg
actionButton enabled msg icon label shortcut =
    button
        ((if enabled then
            [ onClick msg ]

          else
            [ Attr.disabled True ]
         )
            ++ [ Attr.attribute "aria-label" label
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
               , Attr.style "opacity" (boolOpacity enabled)
               , Attr.style "cursor" (boolCursor enabled)
               , Attr.style "transition" "all 150ms ease"
               , Attr.class "touch-target"
               ]
        )
        [ Icon.featherSized 18 icon
        , span [ Attr.attribute "aria-hidden" "true" ] [ text label ]
        ]


boolOpacity : Bool -> String
boolOpacity enabled =
    if enabled then
        "1"

    else
        "0.4"


boolCursor : Bool -> String
boolCursor enabled =
    if enabled then
        "pointer"

    else
        "not-allowed"


{-| The Glue toggle of the action bar (free building): `aria-pressed` is on while
the selection holds glued parts, and a press glues the unglued ones or unglues the
glued ones. Yellow while on, like the Fine toggle.
-}
glueButton : Bool -> Html Msg
glueButton glued =
    button
        [ onClick ToggleGlue
        , Attr.attribute "aria-pressed" (boolText glued)
        , Attr.attribute "aria-label" (glueLabel glued)
        , Attr.title "Glue the selection to the board, locked (L)"
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
        , Attr.style "background" (fineBackground glued)
        , Attr.style "color" (fineColour glued)
        , Attr.style "font-size" "0.6875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        , Attr.class "touch-target"
        ]
        [ Icon.featherSized 18
            (if glued then
                "lock"

             else
                "unlock"
            )
        , span [ Attr.attribute "aria-hidden" "true" ] [ text (glueLabel glued) ]
        ]


glueLabel : Bool -> String
glueLabel glued =
    if glued then
        "Glued"

    else
        "Glue"


{-| The dock: the free catalog in free building, else the counted kit.
-}
viewDock : Model -> Html Msg
viewDock model =
    if model.free then
        freeDockView model

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
        , Attr.style "width" dockWidth
        , Attr.style "max-width" "100%"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.class "scroll-thin"
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


{-| The free-building dock: the colour row on the left, then the tabs and the part
search above the cards of the visible tab (or the search results). The cards are
unlimited.
-}
freeDockView : Model -> Html Msg
freeDockView model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "stretch"
        , Attr.style "gap" "0.5rem"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "height" PartCard.compactPanelHeight
        , Attr.style "width" dockWidth
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
        [ colourRow model
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "0.375rem"
            , Attr.style "flex" "1 1 auto"
            , Attr.style "min-width" "0"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.375rem"
                , Attr.style "min-width" "0"
                ]
                [ tabRow model
                , searchField model
                ]
            , cardsRow model
            ]
        ]


{-| The colour row: the swatches of `paintColours`, two to a column, the chosen
one outlined. Parts added from the dock take this colour.
-}
colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Colour of parts added"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(2, 1.25rem)"
        , Attr.style "grid-template-rows" "repeat(4, 1.25rem)"
        , Attr.style "gap" "0.25rem"
        , Attr.style "align-content" "center"
        , Attr.style "flex-shrink" "0"
        ]
        (List.map (swatch model.paintColour) paintColours)


swatch : Int -> Int -> Html Msg
swatch chosen code =
    let
        name =
            Colours.nameOf code

        isChosen =
            chosen == code
    in
    button
        [ onClick (SetPaintColour code)
        , Attr.attribute "aria-label" name
        , Attr.attribute "aria-pressed" (boolText isChosen)
        , Attr.title name
        , Attr.style "width" "1.25rem"
        , Attr.style "height" "1.25rem"
        , Attr.style "padding" "0"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" (swatchBackground code)
        , Attr.style "border"
            (if isChosen then
                "2px solid var(--color-brand-yellow)"

             else
                "1px solid rgba(255, 255, 255, 0.35)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.class "touch-target"
        ]
        []


{-| A swatch's CSS colour, from the colour table the 3D view uses.
-}
swatchBackground : Int -> String
swatchBackground code =
    let
        rgb =
            Colors.resolveColor code 16

        channel value =
            String.fromInt (round (value * 255))
    in
    "rgb(" ++ channel rgb.r ++ ", " ++ channel rgb.g ++ ", " ++ channel rgb.b ++ ")"


{-| The free dock's tabs, in a row. The search results replace the cards while a
search is typed, and the tabs stay in place.
-}
tabRow : Model -> Html Msg
tabRow model =
    div
        [ Attr.attribute "role" "tablist"
        , Attr.attribute "aria-label" "Part categories"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "flex" "1 1 auto"
        , Attr.style "min-width" "0"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.class "scroll-thin"
        ]
        (List.map (tabButton model.activeTab) FreeCatalog.tabs)


tabButton : FreeCatalog.Tab -> FreeCatalog.Tab -> Html Msg
tabButton active tab =
    let
        isSelected =
            active == tab
    in
    button
        [ onClick (SetFreeTab tab)
        , Attr.attribute "role" "tab"
        , Attr.attribute "aria-selected" (boolText isSelected)
        , Attr.style "flex-shrink" "0"
        , Attr.style "padding" "0.25rem 0.5rem"
        , Attr.style "font-size" "0.75rem"
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
        [ text (FreeCatalog.tabLabel tab) ]


{-| The part search box. Typing searches the whole part library; a form field, so
no keyboard shortcut fires while it has the focus.
-}
searchField : Model -> Html Msg
searchField model =
    input
        [ Attr.type_ "search"
        , Attr.id "play-part-search"
        , Attr.attribute "aria-label" "Search all parts"
        , Attr.placeholder "Search parts"
        , Attr.value model.search.query
        , onInput SearchInput
        , onFocus SearchOpened
        , Attr.style "width" "8.5rem"
        , Attr.style "min-width" "0"
        , Attr.style "flex-shrink" "1"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "padding" "0.25rem 0.5rem"
        , Attr.style "font-size" "0.75rem"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
        , Attr.style "color" "var(--color-text-on-dark)"
        ]
        []


{-| The cards row: the tab's parts, or the search results, with a note while the
library loads, when it cannot be read, or when nothing matches.
-}
cardsRow : Model -> Html Msg
cardsRow model =
    let
        note message =
            [ div
                [ Attr.style "color" "var(--color-text-subtle)"
                , Attr.style "font-size" "0.8125rem"
                , Attr.style "white-space" "nowrap"
                , Attr.style "padding" "0 0.25rem"
                ]
                [ text message ]
            ]

        children =
            if notSearching model then
                List.map (freeCard model) (freeEntries model)

            else
                case model.search.index of
                    NotLoaded ->
                        note "Loading the part library…"

                    Loading ->
                        note "Loading the part library…"

                    Failed reason ->
                        note ("No part library: " ++ reason ++ ".")

                    Loaded _ ->
                        case model.search.results of
                            [] ->
                                note ("No parts match \"" ++ String.trim model.search.query ++ "\".")

                            _ ->
                                List.map (freeCard model) (freeEntries model)
    in
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Parts"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.5rem"
        , Attr.style "flex" "1 1 auto"
        , Attr.style "min-height" "0"
        , Attr.style "min-width" "0"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.class "scroll-thin"
        , Attr.style "padding" "0.125rem"
        ]
        children


{-| One free-dock part as a compact picture card, in the paint colour. Its name
and part number are in the tooltip and the accessible name.
-}
freeCard : Model -> FreeCatalog.Entry -> Html Msg
freeCard model entry =
    let
        colourName =
            Colours.nameOf model.paintColour
    in
    PartCard.compactView
        { imageUrl = Maybe.map .url (Dict.get (PartCard.thumbnailKey entry.file model.paintColour) model.thumbnails)
        , tooltip = entry.name ++ " (" ++ partNumber entry.file ++ ", " ++ colourName ++ ")" ++ descriptionSuffix entry.description
        , ariaLabel = "Add " ++ entry.name ++ " (" ++ colourName ++ ")"
        , badge = Nothing
        , onClick = Just (AddFreePart { file = entry.file, color = model.paintColour, name = entry.name })
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
      , [ ( "Menu, top right", "Share link, undo, redo, delete, clear, and this list" )
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
        , ( "Tidy up (pill above the action bar)", "Shown when parts sit off the studs or overlap. Tidy up previews the moves as ghosts; Apply makes them one undo step, Cancel or Esc changes nothing" )
        , ( "Esc", "Cancel a drag, or deselect" )
        ]
            ++ (if model.free then
                    [ ( "L  (action bar: Glue)", "Glue the selected parts to the board, locked, or unglue them" ) ]

                else
                    []
               )
      )
    , ( "Dock"
      , if model.free then
            [ ( "Tabs and colour row", "Bricks, plates, slopes and tiles, Technic; the colour row sets the colour of the next part" )
            , ( "Search box", "Find any part in the whole library by name or number, then tap it to add it" )
            ]

        else
            [ ( "Cards", "Each card is one part of the kit; its count shows on the picture" )
            , ( "Glued parts", "A glued part is locked: it cannot be moved, turned or recoloured" )
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
