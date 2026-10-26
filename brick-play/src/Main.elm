module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , helpRows
    , init
    , kitItemName
    , main
    , remainingCount
    , remainingForItem
    , shareHash
    , subscriptions
    , thumbnailRequests
    , update
    , view
    , viewToast
    )

{-| Main application for Brick Play.

Provides a minimal, elegant free-building studio with kit inventory
tracking and stud snapping. There is no axis gizmo: a drag on a brick moves it
relative to the view, a drag on empty space pans and a right-drag orbits, and
while something is selected a floating action bar above the dock turns, tips,
raises, lowers or deletes it (the same as the keyboard). The camera has its own
control bottom-right, the shared camera globe (`UI.CameraGlobe`): drag it to
orbit, tap it to fly there, and use its zoom slider and reset button. On a narrow
portrait window (a phone) the bottom area shows either the kit or the camera
globe, switched by a toggle button, and the kit is shown first.

The dock shows each kit item as a rendered picture of the part in its colour
(`UI.PartCard.compactView`, fed by the shared thumbnail pipeline through
`Ports`); clicking a card adds one of that part, and its name, part number and
count are in the tooltip and on the image corner. The top-right menu
(`UI.HamburgerMenu`) holds undo, redo, delete and clear, beside a visible ?
button that opens the shortcut list; Escape closes the menu first.

-}

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import FeatherIcons
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
import Studio.Colours as Colours
import Studio.Core as Core
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


{-| Main application model.
-}
type alias Model =
    { studio : Core.Model
    , inventory : List PartKitItem
    , helpOpen : Bool
    , menuOpen : Bool
    , renderQuality : Style.Quality
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


{-| A rendered kit part: its image as a data URL and the part's .dat title
(`""` when unknown). Kept under `PartCard.thumbnailKey file color`.
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
    | Undo
    | Redo
    | Delete
    | TurnSelection TurnDirection
    | RaiseSelection
    | LowerSelection
    | ToggleFine
    | Clear
    | FitView
    | ToggleRaytrace
    | CaptureSnapshot
    | SnapshotReady { dataUrl : String, filename : String }
    | SnapshotSave { dataUrl : String, filename : String }
    | SnapshotToggleAgreed Bool
    | SnapshotContinue
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

        inventory =
            if List.isEmpty payload.parts then
                defaultInventory

            else
                payload.parts

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
            , helpOpen = False
            , menuOpen = False
            , renderQuality = Style.Draft
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

        requests =
            thumbnailRequests inventory initialModel.thumbnails

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))
    in
    ( initialModel
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , fetchViewportSize
        , if List.isEmpty requests then
            Cmd.none

          else
            Ports.requestThumbnails requests
        ]
    )


{-| The thumbnails to ask the pipeline for: one per distinct part and colour in
the kit that has none yet. `rgb` comes from the colour table the 3D view uses,
so the pictures match the bricks on the canvas.
-}
thumbnailRequests : List PartKitItem -> Dict String Thumbnail -> List { file : String, color : Int, rgb : List Float }
thumbnailRequests inventory thumbnails =
    let
        step item ( seen, acc ) =
            let
                key =
                    PartCard.thumbnailKey item.part item.color
            in
            if List.member key seen || Dict.member key thumbnails then
                ( seen, acc )

            else
                ( key :: seen
                , { file = item.part
                  , color = item.color
                  , rgb =
                        Colors.resolveColor item.color 16
                            |> (\c -> [ c.r, c.g, c.b ])
                  }
                    :: acc
                )
    in
    List.foldl step ( [], [] ) inventory
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
                partNumber item

            else
                thumb.title

        Nothing ->
            partNumber item


partNumber : PartKitItem -> String
partNumber item =
    String.replace ".dat" "" item.part


{-| The name of the selection for the action bar: the brick's name when one
brick is selected, else how many bricks are.
-}
selectionName : Model -> String
selectionName model =
    case Set.toList model.studio.selection of
        [ id ] ->
            case List.filter (\p -> p.id == id) (Core.getPlacements model.studio) of
                placed :: _ ->
                    case Dict.get (PartCard.thumbnailKey placed.part placed.color) model.thumbnails of
                        Just thumb ->
                            if String.isEmpty (String.trim thumb.title) then
                                String.replace ".dat" "" placed.part

                            else
                                thumb.title

                        Nothing ->
                            String.replace ".dat" "" placed.part

                [] ->
                    ""

        ids ->
            String.fromInt (List.length ids) ++ " bricks"


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


{-| Total remaining bricks across the entire kit inventory.
-}
remainingCount : Model -> Int
remainingCount model =
    List.sum (List.map (remainingForItem model) model.inventory)


{-| Encode current document and kit inventory into a shareable URL hash.
-}
shareHash : Model -> String
shareHash model =
    let
        doc =
            model.studio.document

        modelStr =
            Writer.writeDocument doc

        partsStr =
            UrlPayload.encodeParts model.inventory
    in
    if List.isEmpty (Document.mainPlacements doc) then
        "parts=" ++ Url.percentEncode partsStr

    else
        "model=" ++ Url.percentEncode modelStr ++ "&parts=" ++ Url.percentEncode partsStr


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

        AddPart item ->
            addPartItem item { model | lastTapped = Just (kitItemName model item) }

        Undo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.undo model.studio

                nextModel =
                    { model | studio = nextStudio }

                portCmds =
                    handleCoreEvents events
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        Redo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.redo model.studio

                nextModel =
                    { model | studio = nextStudio }

                portCmds =
                    handleCoreEvents events
            in
            ( nextModel
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
            )

        Delete ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.deleteSelection model.studio

                nextModel =
                    { model | studio = nextStudio }

                portCmds =
                    handleCoreEvents events
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
                    case model.renderQuality of
                        Style.Draft ->
                            Style.Realistic

                        Style.Realistic ->
                            Style.Draft
            in
            ( { model
                | renderQuality = nextQuality
                , studio = Core.setRenderQuality nextQuality model.studio
              }
            , Cmd.none
            )

        CaptureSnapshot ->
            ( { model | snapshotModal = Just (SnapshotModal.init { filename = "brick-play-render.png" }) }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = False
                , filename = "brick-play-render.png"
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

        SnapshotToggleAgreed agreed ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setAgreed agreed) model.snapshotModal }
            , Cmd.none
            )

        SnapshotContinue ->
            ( { model | snapshotModal = Nothing }
            , Cmd.none
            )

        Clear ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.resetDocument Document.empty model.studio

                nextModel =
                    { model | studio = nextStudio }

                portCmds =
                    handleCoreEvents events
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


{-| The studio pauses its editing keys while a layer is open (the help dialog or
the menu), so Escape reaches the app to close that layer first.
-}
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


extractKey : Core.Event -> Maybe ViewportShortcuts.KeyEvent
extractKey event =
    case event of
        Core.KeyAccepted keyEvent ->
            Just keyEvent

        _ ->
            Nothing


{-| What `Escape` and `?` need to know to pick between their meanings.
-}
shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.studio.viewport.camera.navigation == Camera.Walk
    }


{-| Map a viewport shortcut to the message it stands for. Brick Play has no
simulation, so Space is deliberately left unbound.
-}
msgForAction : ViewportShortcuts.Action -> Msg
msgForAction action =
    case action of
        ViewportShortcuts.TogglePlayback ->
            NoOp

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


{-| The shortcut list for Brick Play: editing keys on the selected brick, then
the camera.
-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Build"
      , [ ( "Menu, top right", "Undo, redo, delete, clear, and this list" )
        , ( "Drag a brick", "Move it as seen from the camera (snaps to studs)" )
        , ( "Fine (action bar) or Shift", "1 LDU moves for drags and W A S D Q E nudges" )
        , ( "Wheel while dragging", "Raise / lower (Shift: coarser, Alt: finer)" )
        , ( "Right click mid-drag", "Cancel the drag" )
        , ( "Action bar (above the dock)", "Turn left / right, tip, raise, lower or remove the selected brick; Fine toggles 1 LDU moves" )
        , ( "W A S D  Q E", "Move the selected brick (Shift: fine steps)" )
        , ( "← → ↑ ↓", "Turn the selected brick (Shift: 45°)" )
        , ( "Delete", "Remove the selected brick" )
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
    , ( "Help"
      , [ ( "?  /", "This list" ) ]
      )
    ]


{-| Keep focus off toolbar and dock buttons after a mouse click, so the
keyboard shortcuts keep working. A focused button would otherwise take the keys
for itself.
-}
keepFocusOnCanvas : Html.Attribute Msg
keepFocusOnCanvas =
    Focus.keepFocusOnCanvas NoOp


addPartItem : PartKitItem -> Model -> ( Model, Cmd Msg )
addPartItem item model =
    let
        remaining =
            remainingForItem model item
    in
    if remaining <= 0 then
        ( model, Cmd.none )

    else
        let
            -- Next to the selection (or in the middle of the view for the first
            -- piece), on the ground, clear of every piece already placed.
            ( nextStudio, studioCmd, events ) =
                Core.addPart item.part item.color (Core.freeSpotFor item.part model.studio) model.studio

            nextModel =
                { model | studio = nextStudio }

            portCmds =
                handleCoreEvents events
        in
        ( nextModel
        , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds, syncDocHash nextModel events ]
        )


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


{-| Global subscriptions for Brick Play.
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
        ]


{-| Render the Brick Play interface.
-}
view : Model -> Html Msg
view model =
    div
        [ Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- Top right: the menu. The page heading is for screen readers only.
          header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "1rem"
            , Attr.style "right" "1rem"
            , Attr.style "z-index" "20"
            , Attr.style "pointer-events" "none"
            , keepFocusOnCanvas
            ]
            [ h1 [ Attr.class "sr-only" ] [ text "Palikkakehys Play" ]
            , div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "pointer-events" "auto"
                ]
                [ helpButton
                , HamburgerMenu.view (menuConfig model)
                ]
            ]

        -- 3D WebGL Canvas
        , Html.map StudioMsg (Core.view { width = model.windowWidth, height = model.windowHeight } model.studio)

        -- Shortcut reference, open from the menu
        , if model.helpOpen then
            ShortcutHelp.view { rows = helpRows, onClose = ToggleHelp }

          else
            text ""

        -- Bottom: the selection action bar floats above the inventory dock
        , bottomArea model

        -- Wide windows: the camera globe sits beside the dock, bottom-right
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
        , case model.toast of
            Just t ->
                viewToast t.message

            Nothing ->
                text ""
        , case model.snapshotModal of
            Just modal ->
                SnapshotModal.view
                    { onSave = SnapshotSave
                    , onToggleAgreed = SnapshotToggleAgreed
                    , onContinue = SnapshotContinue
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
either the kit or the camera globe, never both.
-}
narrowWidth : Int
narrowWidth =
    600


isNarrow : Model -> Bool
isNarrow model =
    model.windowWidth < narrowWidth


{-| The bottom area: the action bar for the selection, then the kit. On a narrow
window the kit row also carries the camera toggle, and shows the camera globe
instead of the kit while the toggle is on.
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
        [ selectionBar model
        , touchLabel model
        , if isNarrow model then
            narrowRow model

          else
            viewDock model
        ]


{-| The narrow bottom row: the kit (or the camera globe) and the toggle that
switches between them, at a finger-sized width.
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


{-| The toggle between the kit and the camera globe on a narrow window. Its icon
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
                    , label = "Raytrace mode"
                    , shortcut = Nothing
                    , pressed = Just (model.renderQuality == Style.Realistic)
                    , enabled = True
                    , showLabel = False
                    }
                    ToggleRaytrace
                , Button.icon
                    { icon = Icon.feather "camera"
                    , label = "Render snapshot"
                    , shortcut = Nothing
                    , pressed = Nothing
                    , enabled = True
                    , showLabel = False
                    }
                    CaptureSnapshot
                ]
            ]
        }


{-| On touch screens, where tooltips never show: the name of the kit card last
tapped, above the kit. Hidden while the camera globe takes the bottom row.
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


{-| The top-right menu: undo, redo, delete, clear, fit view and the shortcut
list. Each item does what its keyboard shortcut does.
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


{-| Buttons for the selected bricks, the touch- and mouse-friendly stand-in for
an axis gizmo. Each does what its keyboard shortcut does. The name of the
selection is shown on touch screens, where tooltips never show. The bar wraps
onto two rows rather than overflowing a phone. The Fine toggle is always last,
so it can be set before anything is selected; without a selection the bar
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
        , Attr.attribute "aria-label" "Selected brick"
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
        , tooltip = name ++ " (" ++ partNumber item ++ ", " ++ colourName ++ ") — " ++ String.fromInt remaining ++ " of " ++ String.fromInt item.count ++ " available"
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
