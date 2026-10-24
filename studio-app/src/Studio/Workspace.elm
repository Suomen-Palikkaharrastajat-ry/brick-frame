module Studio.Workspace exposing
    ( CoordinateFrame(..)
    , Flags
    , Model
    , Msg(..)
    , currentArcHandles
    , currentHandles
    , init
    , placeArmedPart
    , restore
    , subscriptions
    , update
    , view
    )

{-| The studio's editing page: a document shown in a viewport.

The `Document` is the source of truth; the `Viewport` only displays it. Every
change is an `Edit.Command`, recorded in the undo history and then pushed to the
viewport with `Viewport.applyDocument`, which updates the scene in place.

Input is split by who owns it. The camera keeps orbit, pan, pinch and walk; the
tools are explicit modes. A click selects. In the move tool, pressing on a
gizmo handle takes that gesture away from the camera (`Viewport.captureDrag`)
and turns it into a move along one axis, snapped to the grid; pressing on a part
itself grabs it and slides it over the horizontal plane at the grabbed height.
Empty space and Shift-drag stay with the camera.

-}

import Browser.Dom
import Browser.Events
import Data
import Dict exposing (Dict)
import EmbeddedParts
import FeatherIcons
import Gear.Joint as Joint
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode as Decode
import LDraw.BOM as BOM exposing (ModelBOM)
import LDraw.Catalog as Catalog exposing (ElementCatalog, PartIndex)
import LDraw.ColorData as ColorData
import LDraw.Colors as Colors
import LDraw.Connectivity as Connectivity
    exposing
        ( BoundingBox3D
        , CollisionMode(..)
        , ConnectionPoint
        , SnapCandidate
        , SnapGuide(..)
        , WorldConnectionPoint
        )
import LDraw.Document as Document exposing (Document)
import LDraw.Export as Export
import LDraw.Resolve as Resolve
import LDraw.Types exposing (LDrawLine(..))
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3)
import Ports
import Process
import Render.Camera as Camera
import Render.Pick as Pick
import Render.Style as Style
import Set exposing (Set)
import Studio.BOM as StudioBOM
import Studio.BuildGroups as BuildGroups
import Studio.Colours as Colours
import Studio.Edit as Edit exposing (Axis(..), Command(..), History)
import Studio.Gizmo as Gizmo exposing (ArcHandle, DragHandle(..), Handle)
import Studio.Hints as Hints
import Studio.Inspector as Inspector
import Studio.Layout as Layout exposing (Layout, Panel(..))
import Studio.Overlay as Overlay
import Studio.Palette as Palette
import Studio.PartIndex as PartIndex exposing (Part)
import Studio.Shortcuts as StudioShortcuts exposing (Tool(..))
import Studio.Transform as Transform
import Studio.Tree as Tree
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


{-| Start-up values from the JavaScript runtime. The shape is shared with the
viewer (`docs/library-usage.md`); the studio ignores the playback fields.
-}
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
    , draft : Maybe String
    , favorites : Maybe (List String)
    , recentParts : Maybe (List String)
    }


{-| Which coordinate frame gizmo translations use inside submodels.
-}
type CoordinateFrame
    = LocalFrame
    | WorldFrame


{-| A level in the submodel navigation hierarchy.
-}
type alias SubmodelStep =
    { submodelName : String
    , instancePlacementId : Int
    , localTransform : Mat4
    , accumulatedTransform : Mat4
    }


{-| Current editing context within submodels and coordinate frames.
-}
type alias WorkspaceContext =
    { activeSubmodel : String
    , path : List SubmodelStep
    , coordinateFrame : CoordinateFrame
    }


{-| The document, how it is being edited, and the viewport showing it.
-}
type alias Model =
    { viewport : Viewport.Model
    , document : Document
    , history : History
    , selection : Set Int
    , tool : Tool
    , gridStep : Int
    , rotateStep : Int
    , drag : Maybe Drag
    , helpOpen : Bool
    , palette : Palette.State
    , thumbnails : Dict String String
    , preview : Viewport.Model
    , activeColour : Int
    , layout : Layout
    , window : ( Int, Int )
    , panel : Maybe Panel
    , hints : Hints.Hints
    , hintGeneration : Int
    , bomOpen : Bool
    , bomState : StudioBOM.State
    , elementCatalog : ElementCatalog
    , baseBOM : Maybe ModelBOM
    , renderQuality : Style.Quality
    , exportMenuOpen : Bool
    , context : WorkspaceContext
    , snapDistance : Float
    , collisionMode : CollisionMode
    , snapRotationStep : Int
    , activeSnap : Maybe SnapCandidate
    , collidingPlacements : Set Int
    , altHeld : Bool
    , shiftHeld : Bool
    , inFlightRotationStep : Int
    , announcement : String
    , descriptions : Dict String String
    , treeFocus : Maybe String
    , inspectorFocus : Maybe String
    }


{-| A handle being dragged: which one, what it moves, where the press began, the
document as it was then (every step is computed from it, so the drag can wander
and come back without drift) and how far it has gone.
-}
type alias Drag =
    { kind : DragHandle
    , ids : List Int
    , start : ( Float, Float )
    , lastPoint : ( Float, Float )
    , original : Document
    , amount : Float
    }


type Msg
    = ViewportMsg Viewport.Msg
    | FileTextReceived String
    | FileFailed String
    | RequestFileUpload
    | NewDocument
    | ToggleHelp
    | UseTool Tool
    | Undo
    | Redo
    | DeleteSelection
    | DuplicateSelection
    | SelectAll
    | ClearSelection
    | RotateSelection Axis Int
    | SetColorSelection Int
    | GroupSelection
    | ExplodeSelection
    | EnterSubmodel
    | ExitSubmodel
    | NavigateToSubmodel Int
    | SelectSubmodel String
    | ToggleCoordinateFrame
    | NextGridStep
    | NextRotateStep
    | NextSnapDistance
    | NextCollisionMode
    | SetGridStep Int
    | SetRotateStep Int
    | KeyUp String
    | Save
    | ToggleRenderQuality
    | ToggleExportMenu
    | ExportFormat Export.ExportFormat
    | CaptureSnapshot
    | WindowSized Int Int
    | PartIndexLoaded (Result Http.Error PartIndex.PartIndex)
    | PaletteQuery String
    | PaletteGroup (Maybe String)
    | PaletteGroupAndFocus (Maybe String) String
    | PaletteCategory (Maybe String)
    | PaletteToggleFavorite String
    | PaletteSelectShelf Palette.Shelf
    | PaletteToggleColorFilter
    | PaletteInspect (Maybe Part)
    | PaletteRotateTurntable Float
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | ArmPart Part
    | StartDragPart Part
    | AddArmedPart
    | ManualPart String
    | AddManualPart
    | PickColour Int
    | PreviewMsg Viewport.Msg
    | SetPosition Axis Float
    | SetRotation Axis Float
    | SelectPlacement Int Bool
    | OpenPanel (Maybe Panel)
    | SimulateRequested
    | InstructionsRequested
    | HintsDue Int
    | ToggleBOM
    | CloseBOM
    | BOMQuery String
    | BOMCategory (Maybe String)
    | BOMSubmodel (Maybe String)
    | BOMSort StudioBOM.SortColumn
    | BOMSelectLot String Int
    | BOMExportBrickLink
    | BOMExportRebrickable
    | BOMCopyMarkdown
    | BOMCopyPlainText
    | BOMToggleDiff
    | ElementCatalogLoaded (Result Http.Error ElementCatalog)
    | FocusInspectorField String
    | BlurInspectorField
    | CommitInspectorField String Msg
    | CancelInspectorField String
    | FocusElement String
    | FocusSearch
    | CoarserGridStep
    | NudgeSelection StudioShortcuts.Nudge
    | Request Msg
    | MenuPick Msg
    | NoOp


{-| An empty document in a fresh viewport.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        viewportConfig =
            { ldrawBase = flags.ldrawBase
            , ldrawFallbackBase = flags.ldrawFallbackBase
            , defaultModelUrl = ""
            , initialHash = flags.initialHash

            -- The canvas is one pane of the layout, not the window, so its size
            -- comes from `WindowSized` rather than from the viewport's own listener.
            , useWindowResize = False
            , supportsPointerEvents = flags.supportsPointerEvents
            , renderStyle = renderStyle flags
            , partCache = EmbeddedParts.cache
            }

        ( viewport, viewportCmd ) =
            Viewport.init viewportConfig

        ( preview, previewCmd ) =
            Viewport.init { viewportConfig | initialHash = "" }

        layout =
            Layout.compute 1280 800

        model =
            { viewport = viewport
            , document = Document.empty
            , history = Edit.emptyHistory
            , selection = Set.empty
            , tool = SelectTool
            , gridStep = 20
            , rotateStep = 90
            , drag = Nothing
            , helpOpen = False
            , palette =
                Palette.initWith
                    { favorites = Maybe.withDefault [] flags.favorites
                    , recentParts = Maybe.withDefault [] flags.recentParts
                    }
            , thumbnails = Dict.empty
            , preview = sized previewSize preview
            , activeColour = 4
            , layout = layout
            , window = ( 1280, 800 )
            , panel = Nothing
            , hints = Hints.empty
            , hintGeneration = 0
            , bomOpen = False
            , bomState = StudioBOM.init
            , elementCatalog = Catalog.emptyCatalog
            , baseBOM = Nothing
            , renderQuality = Style.Draft
            , exportMenuOpen = False
            , context =
                { activeSubmodel = ""
                , path = []
                , coordinateFrame = LocalFrame
                }
            , snapDistance = Connectivity.defaultSnapDistance
            , collisionMode = Connectivity.CollisionStrict
            , snapRotationStep = 0
            , activeSnap = Nothing
            , collidingPlacements = Set.empty
            , altHeld = False
            , shiftHeld = False
            , inFlightRotationStep = 0
            , announcement = ""
            , descriptions = Dict.empty
            , treeFocus = Nothing
            , inspectorFocus = Nothing
            }

        ( opened, openCmd ) =
            openDocument (Maybe.map Document.parse flags.draft |> Maybe.withDefault Document.empty) (resized 1280 800 model)
    in
    ( opened
    , Cmd.batch
        [ Cmd.map ViewportMsg viewportCmd
        , Cmd.map PreviewMsg previewCmd
        , openCmd

        -- No hash write here: the camera has not been framed yet, and a hash
        -- holding this default camera made every reload skip auto-fit.
        , PartIndex.fetch flags.ldrawBase PartIndexLoaded
        , Http.get
            { url = withoutTrailingSlash flags.ldrawBase ++ "/elements-index.json"
            , expect = Http.expectJson ElementCatalogLoaded Catalog.elementCatalogDecoder
            }
        , Browser.Dom.getViewport
            |> Task.perform (\vp -> WindowSized (round vp.viewport.width) (round vp.viewport.height))
        ]
    )


{-| The part preview's canvas, in pixels.
-}
previewSize : ( Int, Int )
previewSize =
    ( 264, 150 )


withoutTrailingSlash : String -> String
withoutTrailingSlash base =
    if String.endsWith "/" base then
        String.dropRight 1 base

    else
        base


sized : ( Int, Int ) -> Viewport.Model -> Viewport.Model
sized ( w, h ) viewport =
    Viewport.update (Viewport.WindowResize w h) viewport
        |> (\( v, _, _ ) -> v)


{-| Lay the panes out for a window and give the canvas its share.
-}
resized : Int -> Int -> Model -> Model
resized width height model =
    let
        layout =
            Layout.compute width height
    in
    { model
        | layout = layout
        , window = ( width, height )
        , viewport =
            sized ( layout.canvasWidth, layout.canvasHeight ) model.viewport
                |> Viewport.setCanvasOrigin (toFloat layout.canvasLeft) (toFloat layout.canvasTop)
        , panel =
            -- Panels are sheets of the narrow layout; the wide one shows them all.
            if layout.mode == Layout.Wide then
                Nothing

            else
                model.panel
    }


{-| Come back from the preview: the camera the preview ended on becomes the
workspace's, and the canvas is sized for the editing layout again. Selection,
document, history and tools are untouched, since the preview never had them.
-}
restore : Camera.Camera -> Model -> Model
restore camera model =
    let
        viewport =
            model.viewport
    in
    { model | viewport = { viewport | camera = camera } }
        |> (\m -> resized (Tuple.first m.window) (Tuple.second m.window) m)


renderStyle : Flags -> Style.Style
renderStyle flags =
    let
        base =
            Style.defaultStyle
    in
    Style.clampStyle
        { base
            | ambientStrength = Maybe.withDefault base.ambientStrength flags.ambientStrength
            , lightStrength = Maybe.withDefault base.lightStrength flags.lightStrength
            , vibrance = Maybe.withDefault base.vibrance flags.vibrance
            , edgeWidth = Maybe.withDefault base.edgeWidth flags.edgeWidth
        }



-- ── Update ────────────────────────────────────────────────────────────────────


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            route msg model
    in
    case announce msg model next of
        "" ->
            ( next, cmd )

        said ->
            ( { next | announcement = said }, cmd )


{-| What a screen reader should hear after `msg` took `before` to `after`, or
"" for nothing new. Shown in the polite live region (`viewAnnouncement`).
-}
announce : Msg -> Model -> Model -> String
announce msg before after =
    let
        count n =
            String.fromInt n
                ++ (if n == 1 then
                        " part"

                    else
                        " parts"
                   )

        -- Did the command go through? Every edit and every undo/redo moves a
        -- command between the history's stacks. (Comparing documents would
        -- compare matrices, which Elm's equality cannot do reliably.)
        changed =
            ( List.length after.history.undo, List.length after.history.redo )
                /= ( List.length before.history.undo, List.length before.history.redo )
    in
    case msg of
        Undo ->
            ifTrue changed "Undone"

        Redo ->
            ifTrue changed "Redone"

        DeleteSelection ->
            ifTrue changed ("Deleted " ++ count (Set.size before.selection))

        DuplicateSelection ->
            ifTrue changed ("Duplicated " ++ count (Set.size before.selection))

        NudgeSelection _ ->
            ifTrue changed ("Moved " ++ count (Set.size after.selection))

        RotateSelection _ _ ->
            ifTrue changed ("Turned " ++ count (Set.size after.selection) ++ " by " ++ String.fromInt after.rotateStep ++ "°")

        UseTool tool ->
            case tool of
                SelectTool ->
                    "Select tool"

                MoveTool ->
                    "Move tool: drag a part to slide it, or an arrow for one axis"

                RotateTool ->
                    "Rotate tool"

        NextGridStep ->
            "Move step " ++ String.fromInt after.gridStep ++ " LDU"

        CoarserGridStep ->
            "Move step " ++ String.fromInt after.gridStep ++ " LDU"

        NextRotateStep ->
            "Turn step " ++ String.fromInt after.rotateStep ++ "°"

        NextSnapDistance ->
            "Snap " ++ snapValue after.snapDistance

        NextCollisionMode ->
            "Collision " ++ collisionValue after.collisionMode

        PickColour code ->
            Colours.nameOf code
                ++ (if changed then
                        " applied to " ++ count (Set.size after.selection)

                    else
                        ""
                   )

        _ ->
            if after.selection /= before.selection then
                selectionSummary after

            else
                ""


ifTrue : Bool -> String -> String
ifTrue condition said =
    if condition then
        said

    else
        ""


{-| The selection in words: one part by name and colour, several by count.
-}
selectionSummary : Model -> String
selectionSummary model =
    case Set.toList model.selection |> List.filterMap (\id -> Document.getPlacement id model.document) of
        [] ->
            "Selection cleared"

        [ placement ] ->
            let
                description =
                    case partDescription model placement.fileText of
                        "" ->
                            partKey placement.fileText

                        found ->
                            found
            in
            description ++ ", " ++ Colours.nameOf placement.color ++ " selected"

        placements ->
            String.fromInt (List.length placements) ++ " parts selected"


{-| Route a message: viewport key presses may be nudges, other viewport
messages go to the viewport, the rest to `updatePage`.
-}
route : Msg -> Model -> ( Model, Cmd Msg )
route msg model =
    case msg of
        ViewportMsg (Viewport.KeyPressed keyEvent) ->
            -- With a selection, the arrows and Page keys nudge it. They are kept
            -- from the viewport, which would otherwise orbit the camera at the
            -- same time; key repeat keeps nudging while the key is held.
            -- During a drag the arrows turn the dragged part instead.
            case ( model.drag, StudioShortcuts.nudgeFor (shortcutContext model) keyEvent ) of
                ( Nothing, Just nudge ) ->
                    update (NudgeSelection nudge) model

                _ ->
                    updateViewport (Viewport.KeyPressed keyEvent) model

        ViewportMsg viewportMsg ->
            updateViewport viewportMsg model

        _ ->
            updatePage msg model


{-| Pass a message to the viewport and act on the events it reports.
-}
updateViewport : Viewport.Msg -> Model -> ( Model, Cmd Msg )
updateViewport viewportMsg model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.update viewportMsg model.viewport

        ( next, eventCmds ) =
            handleEvents events ( { model | viewport = viewport }, [] )
    in
    ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )


{-| Focus an element once the next view has been drawn — a dialog or sheet that
is opening does not exist in the DOM until then.
-}
focusSoon : String -> Cmd Msg
focusSoon id =
    Process.sleep 30
        |> Task.andThen (\_ -> Browser.Dom.focus id)
        |> Task.attempt (\_ -> NoOp)


{-| How far a keyboard nudge moves the selection, in LDraw coordinates.

The arrows follow the screen: the camera's right and forward directions are
snapped to whichever ground axis they are closest to, so "right" stays right
however the model has been orbited. LDraw is Y-down and its Z runs opposite to
the camera's, hence the sign flips.

-}
nudgeDelta : Model -> StudioShortcuts.Nudge -> Vec3.Vec3
nudgeDelta model (StudioShortcuts.Nudge nudge) =
    let
        azimuth =
            model.viewport.camera.azimuth

        horizontal =
            if nudge.fine then
                1

            else
                toFloat model.gridStep

        vertical =
            if nudge.fine then
                1

            else
                min 8 (toFloat model.gridStep)

        -- Screen right and screen-forward on the ground, in camera (Y-up) space.
        ( rightX, rightZ ) =
            ( cos azimuth, -(sin azimuth) )

        ( awayX, awayZ ) =
            ( -(sin azimuth), -(cos azimuth) )

        onGround ( x, z ) amount =
            if abs x >= abs z then
                Vec3.vec3 (amount * sign x) 0 0

            else
                -- Camera Z is LDraw −Z.
                Vec3.vec3 0 0 (-amount * sign z)

        sign v =
            if v < 0 then
                -1

            else
                1
    in
    case nudge.direction of
        StudioShortcuts.Right ->
            onGround ( rightX, rightZ ) horizontal

        StudioShortcuts.Left ->
            onGround ( rightX, rightZ ) -horizontal

        StudioShortcuts.Away ->
            onGround ( awayX, awayZ ) horizontal

        StudioShortcuts.Toward ->
            onGround ( awayX, awayZ ) -horizontal

        StudioShortcuts.Up ->
            -- LDraw's Y points down.
            Vec3.vec3 0 -vertical 0

        StudioShortcuts.Down ->
            Vec3.vec3 0 vertical 0


{-| Everything that is not a key press for the viewport.
-}
updatePage : Msg -> Model -> ( Model, Cmd Msg )
updatePage msg model =
    case msg of
        ViewportMsg viewportMsg ->
            updateViewport viewportMsg model

        FileTextReceived text ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                openDocument (Document.parse text) model

        FileFailed message ->
            update (ViewportMsg (Viewport.FileLoadError message)) model

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        NewDocument ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                let
                    ( opened, openCmd ) =
                        openDocument Document.empty model
                in
                ( opened, Cmd.batch [ openCmd, Ports.saveDraft "" ] )

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }
            , if model.helpOpen then
                -- Back to the canvas, where the keys the dialog listed work.
                focusSoon "canvas"

              else
                focusSoon "shortcut-help-close"
            )

        UseTool tool ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                ( { model | tool = tool }, Cmd.none )

        Undo ->
            stepHistory Edit.undo model

        Redo ->
            stepHistory Edit.redo model

        DeleteSelection ->
            whenSelected (\ids -> commit (Remove ids)) model

        DuplicateSelection ->
            whenSelected duplicate model

        SelectAll ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                ( { model | selection = Set.fromList (List.map .id (Document.placementsOf model.context.activeSubmodel model.document)) }
                , Cmd.none
                )

        ClearSelection ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                ( { model | selection = Set.empty }, Cmd.none )

        GroupSelection ->
            whenSelected
                (\ids m ->
                    let
                        existingNames =
                            "main.ldr" :: List.map (.name >> Document.normalise) m.document.submodels

                        candidateName n =
                            "submodel_" ++ String.fromInt n ++ ".ldr"

                        findName n =
                            let
                                name =
                                    candidateName n
                            in
                            if List.member (Document.normalise name) existingNames then
                                findName (n + 1)

                            else
                                name

                        submodelName =
                            findName 1

                        newId =
                            m.document.nextId
                    in
                    commit (GroupIntoSubmodel submodelName ids) { m | selection = Set.singleton newId }
                )
                model

        ExplodeSelection ->
            whenSelected
                (\ids m ->
                    case ids of
                        [ id ] ->
                            case Document.getPlacement id m.document of
                                Just p ->
                                    if Document.isSubmodelReference p m.document then
                                        commit (ExplodeSubmodel id) { m | selection = Set.empty }

                                    else
                                        ( m, Cmd.none )

                                Nothing ->
                                    ( m, Cmd.none )

                        _ ->
                            ( m, Cmd.none )
                )
                model

        EnterSubmodel ->
            whenSelected
                (\ids m ->
                    case ids of
                        [ id ] ->
                            case Document.getPlacement id m.document of
                                Just p ->
                                    if Document.isSubmodelReference p m.document then
                                        ( enterSubmodelPlacement p m, Cmd.none )

                                    else
                                        ( m, Cmd.none )

                                Nothing ->
                                    ( m, Cmd.none )

                        _ ->
                            ( m, Cmd.none )
                )
                model

        ExitSubmodel ->
            if List.isEmpty model.context.path then
                ( model, Cmd.none )

            else
                update (NavigateToSubmodel (List.length model.context.path - 1)) model

        NavigateToSubmodel targetIdx ->
            if targetIdx <= 0 then
                let
                    newContext =
                        { activeSubmodel = (Document.mainSubmodel model.document).name
                        , path = []
                        , coordinateFrame = model.context.coordinateFrame
                        }

                    updatedViewport =
                        Viewport.setActivePath [] model.viewport
                in
                ( { model | context = newContext, viewport = updatedViewport, selection = Set.empty }
                , Cmd.none
                )

            else
                let
                    newPath =
                        List.take targetIdx model.context.path

                    newActive =
                        case List.reverse newPath of
                            head :: _ ->
                                head.submodelName

                            [] ->
                                (Document.mainSubmodel model.document).name

                    newContext =
                        { activeSubmodel = newActive
                        , path = newPath
                        , coordinateFrame = model.context.coordinateFrame
                        }

                    updatedViewport =
                        Viewport.setActivePath (List.map .instancePlacementId newPath) model.viewport
                in
                ( { model | context = newContext, viewport = updatedViewport, selection = Set.empty }
                , Cmd.none
                )

        SelectSubmodel name ->
            let
                normName =
                    Document.normalise name

                mainName =
                    (Document.mainSubmodel model.document).name
            in
            if name == "" || name == "Model" || normName == Document.normalise mainName then
                update (NavigateToSubmodel 0) model

            else
                case
                    model.context.path
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, s ) -> Document.normalise s.submodelName == normName)
                        |> List.head
                of
                    Just ( ancestorIdx, _ ) ->
                        update (NavigateToSubmodel (ancestorIdx + 1)) model

                    Nothing ->
                        case
                            Document.placementsOf model.context.activeSubmodel model.document
                                |> List.filter (\p -> Document.normalise p.part == normName)
                                |> List.head
                        of
                            Just childPlacement ->
                                ( enterSubmodelPlacement childPlacement model, Cmd.none )

                            Nothing ->
                                let
                                    newPath =
                                        findSubmodelPath name model.document
                                            |> Maybe.withDefault []

                                    newContext =
                                        { activeSubmodel = name
                                        , path = newPath
                                        , coordinateFrame = model.context.coordinateFrame
                                        }

                                    updatedViewport =
                                        Viewport.setActivePath (List.map .instancePlacementId newPath) model.viewport
                                in
                                ( { model | context = newContext, viewport = updatedViewport, selection = Set.empty }
                                , Cmd.none
                                )

        ToggleCoordinateFrame ->
            let
                context =
                    model.context

                nextFrame =
                    case context.coordinateFrame of
                        LocalFrame ->
                            WorldFrame

                        WorldFrame ->
                            LocalFrame
            in
            ( { model | context = { context | coordinateFrame = nextFrame } }, Cmd.none )

        RotateSelection axis sign ->
            whenSelected
                (\ids m ->
                    case Edit.selectionCenter ids m.document of
                        Just pivot ->
                            commit
                                (Rotate ids
                                    { pivot = pivot
                                    , axis = axis
                                    , radians = degrees (toFloat (sign * m.rotateStep))
                                    }
                                )
                                m

                        Nothing ->
                            ( m, Cmd.none )
                )
                model

        SetColorSelection colour ->
            whenSelected (\ids -> commit (Recolor (List.map (\id -> ( id, colour )) ids))) model

        NextGridStep ->
            ( { model | gridStep = nextStep gridSteps model.gridStep }, Cmd.none )

        NextRotateStep ->
            ( { model | rotateStep = nextStep rotateSteps model.rotateStep }, Cmd.none )

        CoarserGridStep ->
            ( { model | gridStep = nextStep (List.reverse gridSteps) model.gridStep }, Cmd.none )

        NudgeSelection nudge ->
            whenSelected (\ids m -> commit (Translate ids (nudgeDelta m nudge)) m) model

        NextSnapDistance ->
            ( { model | snapDistance = nextSnapStep snapSteps model.snapDistance }, Cmd.none )

        NextCollisionMode ->
            ( { model | collisionMode = nextCollisionMode model.collisionMode }, Cmd.none )

        SetGridStep step ->
            ( { model | gridStep = step }, Cmd.none )

        SetRotateStep step ->
            ( { model | rotateStep = step }, Cmd.none )

        KeyUp key ->
            if key == "Alt" then
                let
                    nextModel =
                        { model | altHeld = False }
                in
                case nextModel.drag of
                    Just d ->
                        moveDrag d d.lastPoint nextModel

                    Nothing ->
                        ( nextModel, Cmd.none )

            else if key == "Shift" then
                let
                    nextModel =
                        { model | shiftHeld = False }
                in
                case nextModel.drag of
                    Just d ->
                        moveDrag d d.lastPoint nextModel

                    Nothing ->
                        ( nextModel, Cmd.none )

            else
                ( model, Cmd.none )

        Save ->
            ( model
            , Ports.downloadFile
                { filename =
                    if Document.isMultiFile model.document then
                        "model.mpd"

                    else
                        "model.ldr"
                , mime = "text/plain"
                , text = Writer.writeDocument model.document
                }
            )

        ToggleRenderQuality ->
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
                , viewport = Viewport.setRenderQuality nextQuality model.viewport
              }
            , Cmd.none
            )

        ToggleExportMenu ->
            ( { model | exportMenuOpen = not model.exportMenuOpen }, Cmd.none )

        ExportFormat format ->
            let
                downloadCmd =
                    case format of
                        Export.GlTFBinary ->
                            let
                                payload =
                                    Export.exportGlTF model.document model.viewport.partCache Export.defaultConfig
                            in
                            Ports.downloadFile
                                { filename = payload.filename
                                , mime = "model/gltf-binary;application/octet-stream"
                                , text = payload.glbBase64
                                }

                        Export.BlenderScript ->
                            Ports.downloadFile
                                { filename = "model_cycles_setup.py"
                                , mime = "text/x-python"
                                , text = Export.exportBlenderScript model.document Export.defaultConfig
                                }

                        Export.UsdzArchive ->
                            let
                                payload =
                                    Export.exportUSDZ model.document model.viewport.partCache Export.defaultConfig
                            in
                            Ports.downloadFile
                                { filename = payload.filename
                                , mime = "model/vnd.usdz+zip;application/octet-stream"
                                , text = payload.usdzBase64
                                }
            in
            ( { model | exportMenuOpen = False }, downloadCmd )

        CaptureSnapshot ->
            ( { model | exportMenuOpen = False }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = True
                , filename = "palikkakehys-render.png"
                }
            )

        WindowSized width height ->
            ( resized width height model, Cmd.none )

        PartIndexLoaded (Ok index) ->
            let
                palette =
                    model.palette

                loaded =
                    { model
                        | palette =
                            Palette.refreshFiltered model.activeColour
                                { palette | index = Palette.Loaded index }
                        , descriptions =
                            index.parts
                                |> List.map (\part -> ( partKey part.file, part.description ))
                                |> Dict.fromList
                    }
            in
            ( loaded, requestVisibleThumbnails loaded )

        PartIndexLoaded (Err error) ->
            let
                palette =
                    model.palette
            in
            ( { model | palette = { palette | index = Palette.Failed (httpError error) } }, Cmd.none )

        PaletteQuery query ->
            let
                palette =
                    model.palette

                newGroup =
                    if palette.query == "" && query /= "" then
                        Nothing

                    else if query == "" && palette.group == Nothing then
                        Just BuildGroups.defaultGroup

                    else
                        palette.group

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | query = query, group = newGroup }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated, requestVisibleThumbnails updated )

        PaletteGroup group ->
            let
                palette =
                    model.palette

                -- A group and an LDraw category are two ways to narrow the
                -- shelf; the category picker only shows under "Everything".
                -- Picking a group narrows the current search to that group
                -- (or browses it if no search), and leaves personal shelves.
                shelf =
                    if palette.shelf == Palette.FavoritesShelf || palette.shelf == Palette.RecentShelf then
                        Palette.AllShelf

                    else
                        palette.shelf

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | group = group, category = Nothing, shelf = shelf }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated, requestVisibleThumbnails updated )

        PaletteGroupAndFocus group domId ->
            let
                palette =
                    model.palette

                shelf =
                    if palette.shelf == Palette.FavoritesShelf || palette.shelf == Palette.RecentShelf then
                        Palette.AllShelf

                    else
                        palette.shelf

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | group = group, category = Nothing, shelf = shelf }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated
            , Cmd.batch
                [ requestVisibleThumbnails updated
                , Task.attempt (\_ -> NoOp) (Browser.Dom.focus domId)
                ]
            )

        PaletteCategory category ->
            let
                palette =
                    model.palette

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | category = category }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated, requestVisibleThumbnails updated )

        PaletteToggleFavorite file ->
            let
                palette =
                    model.palette

                newFavorites =
                    if Set.member file palette.favorites then
                        Set.remove file palette.favorites

                    else
                        Set.insert file palette.favorites

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | favorites = newFavorites }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated
            , Cmd.batch
                [ Ports.saveFavorites (Set.toList newFavorites)
                , requestVisibleThumbnails updated
                ]
            )

        PaletteSelectShelf shelf ->
            let
                palette =
                    model.palette

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | shelf = shelf }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated, requestVisibleThumbnails updated )

        PaletteToggleColorFilter ->
            let
                palette =
                    model.palette

                newMode =
                    case palette.colorFilterMode of
                        Palette.ShowAllColors ->
                            Palette.OfficialOnly

                        Palette.OfficialOnly ->
                            Palette.ShowAllColors

                updatedPalette =
                    Palette.refreshFiltered model.activeColour
                        { palette | colorFilterMode = newMode }

                updated =
                    { model | palette = updatedPalette }
            in
            ( updated, requestVisibleThumbnails updated )

        PaletteInspect maybePart ->
            case maybePart of
                Just part ->
                    let
                        ( armedModel, armCmd ) =
                            armPart part.file model
                                |> (\( m, cmd ) -> ( withArmed (Just part) m, cmd ))

                        palette =
                            armedModel.palette
                    in
                    ( { armedModel | palette = { palette | inspectingPart = Just part } }, armCmd )

                Nothing ->
                    let
                        palette =
                            model.palette
                    in
                    ( { model | palette = { palette | inspectingPart = Nothing } }, Cmd.none )

        PaletteRotateTurntable angle ->
            let
                palette =
                    model.palette

                preview =
                    model.preview

                cam =
                    preview.camera

                rotatedCam =
                    { cam | azimuth = degrees angle }
            in
            ( { model
                | palette = { palette | turntableAngle = angle }
                , preview = { preview | camera = rotatedCam }
              }
            , Cmd.none
            )

        ThumbnailLoaded { file, color, url } ->
            if color == model.activeColour then
                ( { model | thumbnails = Dict.insert file url model.thumbnails }, Cmd.none )

            else
                ( model, Cmd.none )

        ArmPart part ->
            armPart part.file model
                |> (\( m, cmd ) -> ( withArmed (Just part) m, cmd ))

        StartDragPart part ->
            let
                ( armedModel, armCmd ) =
                    armPart part.file model
                        |> (\( m, cmd ) -> ( withArmed (Just part) m, cmd ))

                newId =
                    armedModel.document.nextId

                initialPos =
                    vec3 0 0 0

                tempPlacement =
                    { part = part.file
                    , color = armedModel.activeColour
                    , matrix = Mat4.makeTranslate initialPos
                    }

                ( docWithPart, _ ) =
                    Edit.apply (AddIn armedModel.context.activeSubmodel [ tempPlacement ]) armedModel.document

                drag =
                    { kind = PlaneDrag initialPos
                    , ids = [ newId ]
                    , start = ( 0, 0 )
                    , lastPoint = ( 0, 0 )
                    , original = armedModel.document
                    , amount = 0
                    }

                ( synced, syncCmd ) =
                    syncDocumentLive
                        { armedModel
                            | document = docWithPart
                            , viewport = Viewport.captureDrag armedModel.viewport
                            , drag = Just drag
                            , selection = Set.singleton newId
                            , inFlightRotationStep = 0
                            , snapRotationStep = 0
                        }
            in
            ( synced, Cmd.batch [ armCmd, syncCmd ] )

        ManualPart text ->
            let
                palette =
                    model.palette
            in
            ( { model | palette = { palette | manual = text } }, Cmd.none )

        AddManualPart ->
            case manualPartFile model.palette.manual of
                Just file ->
                    addPart file model

                Nothing ->
                    ( model, Cmd.none )

        AddArmedPart ->
            case model.palette.armed of
                Just part ->
                    addPart part.file model

                Nothing ->
                    ( model, Cmd.none )

        PickColour code ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                let
                    picked =
                        { model
                            | activeColour = code
                            , palette = Palette.refreshFiltered code model.palette
                            , thumbnails = Dict.empty
                        }

                    ( recoloured, recolourCmd ) =
                        whenSelected (\ids -> commit (Recolor (List.map (\id -> ( id, code )) ids))) picked

                    ( previewed, previewCmd ) =
                        case model.palette.armed of
                            Just part ->
                                armPart part.file recoloured

                            Nothing ->
                                ( recoloured, Cmd.none )
                in
                ( previewed
                , Cmd.batch
                    [ recolourCmd
                    , previewCmd
                    , requestVisibleThumbnails previewed
                    ]
                )

        PreviewMsg previewMsg ->
            let
                ( preview, previewCmd, _ ) =
                    Viewport.update previewMsg model.preview
            in
            ( { model | preview = preview }, Cmd.map PreviewMsg previewCmd )

        SetPosition axis value ->
            whenSelected
                (\ids m ->
                    case Edit.selectionCenter ids m.document of
                        Just center ->
                            let
                                current =
                                    Vec3.toRecord center

                                currentAxis =
                                    case axis of
                                        AxisX ->
                                            current.x

                                        AxisY ->
                                            current.y

                                        AxisZ ->
                                            current.z

                                delta =
                                    case axis of
                                        AxisX ->
                                            Vec3.vec3 (value - current.x) 0 0

                                        AxisY ->
                                            Vec3.vec3 0 (value - current.y) 0

                                        AxisZ ->
                                            Vec3.vec3 0 0 (value - current.z)
                            in
                            if abs (value - currentAxis) < 0.001 then
                                ( m, Cmd.none )

                            else
                                commit (Translate ids delta) m

                        Nothing ->
                            ( m, Cmd.none )
                )
                model

        SetRotation axis degrees ->
            case Set.toList model.selection |> List.filterMap (\id -> Document.getPlacement id model.document) of
                [ placement ] ->
                    let
                        current =
                            Transform.eulerDegrees placement.matrix

                        currentDeg =
                            case axis of
                                AxisX ->
                                    current.x

                                AxisY ->
                                    current.y

                                AxisZ ->
                                    current.z

                        next =
                            case axis of
                                AxisX ->
                                    { current | x = degrees }

                                AxisY ->
                                    { current | y = degrees }

                                AxisZ ->
                                    { current | z = degrees }
                    in
                    if abs (degrees - currentDeg) < 0.001 then
                        ( model, Cmd.none )

                    else
                        commit (SetMatrix [ ( placement.id, Transform.withEuler next placement.matrix ) ]) model

                _ ->
                    ( model, Cmd.none )

        SelectPlacement id shift ->
            if model.drag /= Nothing then
                ( model, Cmd.none )

            else
                ( { model
                    | selection =
                        if shift then
                            if Set.member id model.selection then
                                Set.remove id model.selection

                            else
                                Set.insert id model.selection

                        else
                            Set.singleton id
                  }
                , Cmd.none
                )

        OpenPanel panel ->
            ( { model | panel = panel }, Cmd.none )

        HintsDue generation ->
            -- Only the last edit's timer counts; earlier ones were overtaken.
            if generation == model.hintGeneration then
                ( { model | hints = Hints.analyse Data.gearParts model.document }, Cmd.none )

            else
                ( model, Cmd.none )

        SimulateRequested ->
            -- The page above this one changes pages; the workspace stays as it is.
            ( model, Cmd.none )

        ToggleBOM ->
            let
                willOpen =
                    not model.bomOpen

                baseBOM =
                    if willOpen && model.baseBOM == Nothing then
                        Just (computeBOM model)

                    else
                        model.baseBOM
            in
            ( { model | bomOpen = willOpen, baseBOM = baseBOM }
            , if willOpen then
                -- Into the dialog, so Escape closes it and Tab stays inside.
                focusSoon "bom-close"

              else
                focusSoon "canvas"
            )

        CloseBOM ->
            ( { model | bomOpen = False }, focusSoon "canvas" )

        BOMQuery query ->
            let
                bomState =
                    model.bomState
            in
            ( { model | bomState = { bomState | query = query } }, Cmd.none )

        BOMCategory category ->
            let
                bomState =
                    model.bomState
            in
            ( { model | bomState = { bomState | category = category } }, Cmd.none )

        BOMSubmodel submodel ->
            let
                bomState =
                    model.bomState
            in
            ( { model | bomState = { bomState | submodel = submodel } }, Cmd.none )

        BOMSort column ->
            let
                bomState =
                    model.bomState

                ( newCol, newAsc ) =
                    if bomState.sortColumn == column then
                        ( column, not bomState.sortAscending )

                    else
                        ( column, False )
            in
            ( { model | bomState = { bomState | sortColumn = newCol, sortAscending = newAsc } }, Cmd.none )

        BOMSelectLot partId colorId ->
            let
                bom =
                    computeBOM model

                matchingLot =
                    List.filter (\l -> l.partId == partId && l.colorId == colorId) bom.lots
                        |> List.head

                newSelection =
                    case matchingLot of
                        Just lot ->
                            Set.fromList lot.placementIds

                        Nothing ->
                            model.selection

                bomState =
                    model.bomState
            in
            ( { model
                | selection = newSelection
                , bomState = { bomState | selectedLot = Just ( partId, colorId ) }
              }
            , Cmd.none
            )

        BOMExportBrickLink ->
            let
                bom =
                    computeBOM model
            in
            ( model
            , Ports.downloadFile
                { filename = "wanted-list.xml"
                , mime = "application/xml"
                , text = BOM.toBrickLinkXml bom
                }
            )

        BOMExportRebrickable ->
            let
                bom =
                    computeBOM model
            in
            ( model
            , Ports.downloadFile
                { filename = "rebrickable-parts.csv"
                , mime = "text/csv"
                , text = BOM.toRebrickableCsv bom
                }
            )

        BOMCopyMarkdown ->
            let
                bom =
                    computeBOM model
            in
            ( model
            , Ports.copyToClipboard (BOM.toMarkdown bom)
            )

        BOMCopyPlainText ->
            let
                bom =
                    computeBOM model
            in
            ( model
            , Ports.copyToClipboard (BOM.toPlainText bom)
            )

        BOMToggleDiff ->
            let
                bomState =
                    model.bomState
            in
            ( { model | bomState = { bomState | showDiff = not bomState.showDiff } }, Cmd.none )

        ElementCatalogLoaded (Ok catalog) ->
            ( { model | elementCatalog = catalog }, Cmd.none )

        ElementCatalogLoaded (Err _) ->
            ( model, Cmd.none )

        InstructionsRequested ->
            -- The page above this one changes pages; the workspace stays as it is.
            ( model, Cmd.none )

        FocusSearch ->
            -- On a phone the search lives in the Parts sheet, which has to be
            -- drawn before it can take focus.
            ( if model.layout.mode == Layout.Narrow then
                { model | panel = Just PartsPanel }

              else
                model
            , focusSoon "part-search"
            )

        MenuPick inner ->
            update inner { model | exportMenuOpen = False }

        Request inner ->
            ( model, Task.perform identity (Task.succeed inner) )

        FocusElement id ->
            -- Roving focus in the tree and the swatch grid: the arrow keys move
            -- DOM focus, which only a command can do. The tree's last row is
            -- remembered so Tab returns to it.
            ( if String.startsWith "tree-" id then
                { model | treeFocus = Just id }

              else
                model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)
            )

        FocusInspectorField fieldId ->
            ( { model | inspectorFocus = Just fieldId }, Cmd.none )

        BlurInspectorField ->
            ( { model | inspectorFocus = Nothing }, Cmd.none )

        CommitInspectorField fieldId actionMsg ->
            let
                ( nextModel, cmd ) =
                    update actionMsg { model | inspectorFocus = Nothing }
            in
            ( nextModel
            , Cmd.batch
                [ cmd
                , Task.attempt (\_ -> NoOp) (Browser.Dom.blur fieldId)
                ]
            )

        CancelInspectorField fieldId ->
            ( { model | inspectorFocus = Nothing }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.blur fieldId)
            )

        NoOp ->
            ( model, Cmd.none )


{-| Remember which part is picked and show it in the preview.
-}
withArmed : Maybe Part -> Model -> Model
withArmed part model =
    let
        palette =
            model.palette
    in
    { model | palette = { palette | armed = part } }


{-| Load one part, in the active colour, into the preview.
-}
armPart : String -> Model -> ( Model, Cmd Msg )
armPart file model =
    let
        ( document, _ ) =
            Document.addPlacement ""
                { part = file, color = model.activeColour, matrix = Mat4.identity }
                Document.empty

        ( preview, previewCmd, _ ) =
            Viewport.loadDocument document model.preview
    in
    ( { model | preview = preview }, Cmd.map PreviewMsg previewCmd )


{-| A typed part number as a file name: `3001`, `3001.dat` and `3001.DAT` all
mean `3001.dat`. Anything with characters a part file cannot have is refused.
-}
manualPartFile : String -> Maybe String
manualPartFile text =
    let
        cleaned =
            String.toLower (String.trim text)

        file =
            if String.endsWith ".dat" cleaned then
                cleaned

            else
                cleaned ++ ".dat"
    in
    if cleaned /= "" && String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_' || c == '.') cleaned then
        Just file

    else
        Nothing


{-| Add a part at the middle of the view, snapped to the grid, in the active
colour, and select it.
-}
addPart : String -> Model -> ( Model, Cmd Msg )
addPart file model =
    if model.drag /= Nothing then
        ( model, Cmd.none )

    else
        let
            -- The camera looks at a render-space (Y up) point; the document is Y down.
            target =
                Gizmo.toRender model.viewport.camera.target

            snap v =
                Edit.snap (toFloat model.gridStep) v

            targetPos =
                Vec3.vec3 (snap (Vec3.getX target)) (snap (Vec3.getY target)) (snap (Vec3.getZ target))

            localPos =
                case List.reverse model.context.path of
                    head :: _ ->
                        case Mat4.inverse head.accumulatedTransform of
                            Just inv ->
                                Mat4.transform inv targetPos

                            Nothing ->
                                targetPos

                    [] ->
                        targetPos

            placement =
                Mat4.makeTranslate localPos

            newId =
                model.document.nextId

            ( document, history ) =
                Edit.record (AddIn model.context.activeSubmodel [ { part = file, color = model.activeColour, matrix = placement } ]) model.document model.history

            newRecents =
                file
                    :: List.filter (\f -> f /= file) model.palette.recentParts
                    |> List.take 20

            palette =
                model.palette

            ( synced, syncCmd ) =
                syncDocument
                    { model
                        | document = document
                        , history = history
                        , selection = Set.singleton newId
                        , palette =
                            Palette.refreshFiltered model.activeColour
                                { palette | recentParts = newRecents }
                    }
        in
        ( synced, Cmd.batch [ syncCmd, Ports.saveRecents newRecents ] )


{-| Place an armed part on surface contact at the clicked canvas point (x, y).
-}
placeArmedPart : PartIndex.Part -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
placeArmedPart part ( x, y ) model =
    if model.drag /= Nothing then
        ( model, Cmd.none )

    else
        let
            resolveLines name =
                case Dict.get name model.viewport.partCache of
                    Just (Resolve.Loaded lines) ->
                        Just lines

                    _ ->
                        Nothing

            conn =
                Connectivity.extractPartConnectivity part.file resolveLines

            rawBox =
                List.head conn.boundingBoxes
                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }

            box =
                Connectivity.effectiveEnvelope rawBox conn.connections

            bottomExtent =
                case model.viewport.instancedScene |> Maybe.andThen (\s -> Dict.get part.file s.parts) of
                    Just m ->
                        Vec3.getY m.boundsMax

                    Nothing ->
                        Vec3.getY box.max

            surfaceHit =
                Viewport.pickSurfaceExcluding Set.empty x y model.viewport
        in
        case surfaceHit of
            Just hit ->
                let
                    targetY =
                        hit.topY - bottomExtent

                    docHit =
                        Gizmo.toRender hit.point

                    docHitX =
                        Vec3.getX docHit

                    docHitZ =
                        Vec3.getZ docHit

                    step =
                        toFloat (max 1 model.gridStep)

                    targetX =
                        toFloat (round (docHitX / step)) * step

                    targetZ =
                        toFloat (round (docHitZ / step)) * step

                    targetPos =
                        vec3 targetX targetY targetZ

                    effectivePos =
                        if not (List.isEmpty model.context.path) then
                            case List.reverse model.context.path of
                                head :: _ ->
                                    case Mat4.inverse head.accumulatedTransform of
                                        Just inv ->
                                            Mat4.transform inv targetPos

                                        Nothing ->
                                            targetPos

                                [] ->
                                    targetPos

                        else
                            targetPos

                    surfaceMatrix =
                        Mat4.makeTranslate effectivePos

                    otherPlacements =
                        Document.placementsOf model.context.activeSubmodel model.document

                    targetWorldPoints =
                        otherPlacements
                            |> List.concatMap
                                (\targetP ->
                                    let
                                        pConn =
                                            Connectivity.extractPartConnectivity targetP.part resolveLines
                                    in
                                    List.map (Connectivity.transformPoint targetP.matrix targetP.id) pConn.connections
                                )

                    maybeSnap =
                        if model.altHeld || model.snapDistance <= 0 then
                            Nothing

                        else
                            Connectivity.solveSnap
                                Connectivity.snapEnterDistance
                                model.snapRotationStep
                                surfaceMatrix
                                conn.connections
                                targetWorldPoints

                    finalMatrix =
                        case maybeSnap of
                            Just snap ->
                                snap.snapMatrix

                            Nothing ->
                                surfaceMatrix

                    newId =
                        model.document.nextId

                    ( document, history ) =
                        Edit.record
                            (AddIn model.context.activeSubmodel [ { part = part.file, color = model.activeColour, matrix = finalMatrix } ])
                            model.document
                            model.history

                    newRecents =
                        part.file
                            :: List.filter (\f -> f /= part.file) model.palette.recentParts
                            |> List.take 20

                    palette =
                        model.palette

                    ( synced, syncCmd ) =
                        syncDocument
                            { model
                                | document = document
                                , history = history
                                , selection = Set.singleton newId
                                , palette =
                                    Palette.refreshFiltered model.activeColour
                                        { palette | recentParts = newRecents }
                            }
                in
                ( synced, Cmd.batch [ syncCmd, Ports.saveRecents newRecents ] )

            Nothing ->
                ( model, Cmd.none )


{-| Request on-demand thumbnails for currently visible parts in the palette.
-}
requestVisibleThumbnails : Model -> Cmd Msg
requestVisibleThumbnails model =
    case model.palette.index of
        Palette.Loaded _ ->
            let
                missing =
                    model.palette.filteredParts
                        |> List.filter (\p -> not (Dict.member p.file model.thumbnails))

                -- The renderer has no colour table of its own; send the one the
                -- 3D view uses so cards match the preview.
                thumbnailRgb =
                    Colors.resolveColor model.activeColour 16
                        |> (\c -> [ c.r, c.g, c.b ])
            in
            if List.isEmpty missing then
                Cmd.none

            else
                Ports.requestThumbnails
                    (List.map (\p -> { file = p.file, color = model.activeColour, rgb = thumbnailRgb }) missing)

        _ ->
            Cmd.none


httpError : Http.Error -> String
httpError error =
    case error of
        Http.BadUrl url ->
            "bad URL " ++ url

        Http.Timeout ->
            "timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus 404 ->
            "not found"

        Http.BadStatus code ->
            "HTTP " ++ String.fromInt code

        Http.BadBody message ->
            message


{-| Move steps in LDU, coarsest first: a stud pitch, a plate-ish half step, a
quarter step and a single unit.
-}
gridSteps : List Int
gridSteps =
    [ 20, 8, 4, 1 ]


rotateSteps : List Int
rotateSteps =
    [ 90, 15 ]


{-| The step after `current` in `steps`, wrapping round.
-}
nextStep : List Int -> Int -> Int
nextStep steps current =
    let
        index =
            steps
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, step ) -> step == current)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1
    in
    steps
        |> List.drop (modBy (List.length steps) (index + 1))
        |> List.head
        |> Maybe.withDefault current


snapSteps : List Float
snapSteps =
    [ 12.0, 20.0, 8.0, 0.0 ]


nextSnapStep : List Float -> Float -> Float
nextSnapStep steps current =
    let
        index =
            steps
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, step ) -> abs (step - current) < 0.1)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault -1
    in
    steps
        |> List.drop (modBy (List.length steps) (index + 1))
        |> List.head
        |> Maybe.withDefault 12.0


nextCollisionMode : CollisionMode -> CollisionMode
nextCollisionMode mode =
    case mode of
        CollisionStrict ->
            CollisionSoft

        CollisionSoft ->
            CollisionOff

        CollisionOff ->
            CollisionStrict


snapLabel : Float -> String
snapLabel dist =
    if dist <= 0 then
        "Snap: Off"

    else
        "Snap: " ++ String.fromInt (round dist) ++ " LDU"


collisionLabel : CollisionMode -> String
collisionLabel mode =
    case mode of
        CollisionStrict ->
            "Collision: Strict"

        CollisionSoft ->
            "Collision: Soft"

        CollisionOff ->
            "Collision: Off"


{-| Run an edit on the selected placements, or do nothing when none are.
-}
whenSelected : (List Int -> Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
whenSelected edit model =
    if model.drag /= Nothing then
        ( model, Cmd.none )

    else
        case Set.toList model.selection of
            [] ->
                ( model, Cmd.none )

            ids ->
                edit ids model


{-| Do an edit: record it for undo, then bring the viewport and draft up to date.
-}
commit : Command -> Model -> ( Model, Cmd Msg )
commit command model =
    if model.drag /= Nothing then
        ( model, Cmd.none )

    else
        let
            ( document, history ) =
                Edit.record command model.document model.history
        in
        syncDocument { model | document = document, history = history }


{-| Copy the selection, offset by one grid step along X, and select the copies.
-}
duplicate : List Int -> Model -> ( Model, Cmd Msg )
duplicate ids model =
    let
        offset =
            Mat4.makeTranslate3 (toFloat model.gridStep) 0 0

        parts =
            ids
                |> List.filterMap (\id -> Document.getPlacement id model.document)
                |> List.map
                    (\p ->
                        { part = p.fileText
                        , color = p.color
                        , matrix = Mat4.mul offset p.matrix
                        }
                    )

        ( document, history ) =
            Edit.record (AddIn model.context.activeSubmodel parts) model.document model.history

        -- Added placements take consecutive ids from the document's counter.
        newIds =
            List.range model.document.nextId (model.document.nextId + List.length parts - 1)
    in
    syncDocument
        { model | document = document, history = history, selection = Set.fromList newIds }


stepHistory : (Document -> History -> ( Document, History )) -> Model -> ( Model, Cmd Msg )
stepHistory step model =
    if model.drag /= Nothing then
        ( model, Cmd.none )

    else
        let
            ( document, history ) =
                step model.document model.history
        in
        syncDocument { model | document = document, history = history }


{-| Enter a submodel reference placement from the currently active submodel.
-}
enterSubmodelPlacement : Document.Placement -> Model -> Model
enterSubmodelPlacement placement model =
    let
        parentAccum =
            case List.reverse model.context.path of
                head :: _ ->
                    head.accumulatedTransform

                [] ->
                    Mat4.identity

        accum =
            Mat4.mul parentAccum placement.matrix

        step =
            { submodelName = placement.part
            , instancePlacementId = placement.id
            , localTransform = placement.matrix
            , accumulatedTransform = accum
            }

        newPath =
            model.context.path ++ [ step ]

        newContext =
            { activeSubmodel = placement.part
            , path = newPath
            , coordinateFrame = model.context.coordinateFrame
            }
    in
    { model
        | context = newContext
        , viewport = Viewport.setActivePath (List.map .instancePlacementId newPath) model.viewport
        , selection = Set.empty
    }


{-| Find the first reference path from the main submodel to `targetName`.
-}
findSubmodelPath : String -> Document -> Maybe (List SubmodelStep)
findSubmodelPath targetName doc =
    let
        normTarget =
            Document.normalise targetName

        mainName =
            (Document.mainSubmodel doc).name

        search : Set String -> String -> Mat4 -> List Document.Placement -> Maybe (List SubmodelStep)
        search visited currentSubmodel parentAccum placements =
            case placements of
                [] ->
                    Nothing

                p :: rest ->
                    let
                        normPart =
                            Document.normalise p.part

                        accum =
                            Mat4.mul parentAccum p.matrix

                        step =
                            { submodelName = p.part
                            , instancePlacementId = p.id
                            , localTransform = p.matrix
                            , accumulatedTransform = accum
                            }
                    in
                    if normPart == normTarget && Document.isSubmodelReference p doc then
                        Just [ step ]

                    else if Document.isSubmodelReference p doc && not (Set.member normPart visited) then
                        case
                            search
                                (Set.insert normPart visited)
                                p.part
                                accum
                                (Document.placementsOf p.part doc)
                        of
                            Just subPath ->
                                Just (step :: subPath)

                            Nothing ->
                                search visited currentSubmodel parentAccum rest

                    else
                        search visited currentSubmodel parentAccum rest
    in
    search
        (Set.singleton (Document.normalise mainName))
        mainName
        Mat4.identity
        (Document.placementsOf mainName doc)


{-| Keep the editing context valid after an edit or undo/redo. If a parent
submodel instance along `context.path` was removed, truncate the path back to
the deepest surviving ancestor; if the main submodel was renamed (e.g. `""` to
`"main.ldr"` when grouping), keep `activeSubmodel` in sync.
-}
reconcileContext : Document -> WorkspaceContext -> WorkspaceContext
reconcileContext doc context =
    let
        mainName =
            (Document.mainSubmodel doc).name

        walkPath : String -> Mat4 -> List SubmodelStep -> List SubmodelStep
        walkPath parentSubmodel parentAccum steps =
            case steps of
                [] ->
                    []

                step :: rest ->
                    case
                        Document.placementsOf parentSubmodel doc
                            |> List.filter (\p -> p.id == step.instancePlacementId && Document.isSubmodelReference p doc)
                            |> List.head
                    of
                        Just p ->
                            let
                                accum =
                                    Mat4.mul parentAccum p.matrix

                                updatedStep =
                                    { submodelName = p.part
                                    , instancePlacementId = p.id
                                    , localTransform = p.matrix
                                    , accumulatedTransform = accum
                                    }
                            in
                            updatedStep :: walkPath p.part accum rest

                        Nothing ->
                            []

        validPath =
            walkPath mainName Mat4.identity context.path

        newActive =
            case List.reverse validPath of
                head :: _ ->
                    head.submodelName

                [] ->
                    if not (List.isEmpty context.path) then
                        mainName

                    else if context.activeSubmodel == "" then
                        mainName

                    else if
                        List.any
                            (\s -> Document.normalise s.name == Document.normalise context.activeSubmodel)
                            doc.submodels
                    then
                        context.activeSubmodel

                    else
                        mainName
    in
    { context
        | activeSubmodel = newActive
        , path = validPath
    }


{-| Push the document to the viewport, forget which placements changed, drop
selected ids that no longer exist and queue the draft.
-}
syncDocument : Model -> ( Model, Cmd Msg )
syncDocument model =
    let
        reconciledContext =
            reconcileContext model.document model.context

        activePathIds =
            List.map .instancePlacementId reconciledContext.path

        viewportWithPath =
            if activePathIds /= model.viewport.activePath then
                Viewport.setActivePath activePathIds model.viewport

            else
                model.viewport

        ( viewport, viewportCmd, events ) =
            Viewport.applyDocument model.document viewportWithPath

        present =
            Document.placementsOf reconciledContext.activeSubmodel model.document
                |> List.map .id
                |> Set.fromList

        synced =
            { model
                | viewport = viewport
                , document = Document.clearTouched model.document
                , context = reconciledContext
                , selection = Set.filter (\id -> Set.member id present) model.selection
                , collidingPlacements = Set.filter (\id -> Set.member id present) model.collidingPlacements
            }

        ( final, eventCmds ) =
            handleEvents events ( synced, [] )

        ( withTimer, hintCmd ) =
            scheduleHints final
    in
    ( withTimer
    , Cmd.batch (hintCmd :: Cmd.map ViewportMsg viewportCmd :: Ports.saveDraft (Writer.writeDocument model.document) :: eventCmds)
    )


{-| Push the live drag document to the viewport and drop selected ids that no longer exist,
without scheduling gear hints or saving the draft until the drag completes.
-}
syncDocumentLive : Model -> ( Model, Cmd Msg )
syncDocumentLive model =
    let
        reconciledContext =
            reconcileContext model.document model.context

        ( viewport, viewportCmd, events ) =
            Viewport.applyDocument model.document model.viewport

        present =
            Document.placementsOf reconciledContext.activeSubmodel model.document
                |> List.map .id
                |> Set.fromList

        synced =
            { model
                | viewport = viewport
                , document = Document.clearTouched model.document
                , context = reconciledContext
                , selection = Set.filter (\id -> Set.member id present) model.selection
            }

        ( final, eventCmds ) =
            handleEvents events ( synced, [] )
    in
    ( final
    , Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds)
    )


{-| Re-check the gears once editing has paused for a moment. Each edit starts a
new timer and makes the earlier ones stale, so a burst of edits (a drag) costs
one analysis.
-}
scheduleHints : Model -> ( Model, Cmd Msg )
scheduleHints model =
    let
        generation =
            model.hintGeneration + 1
    in
    ( { model | hintGeneration = generation }
    , Process.sleep 500 |> Task.perform (\_ -> HintsDue generation)
    )


{-| Replace the document wholesale and load it into the viewport. History and
selection start afresh.
-}
openDocument : Document -> Model -> ( Model, Cmd Msg )
openDocument document model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.loadDocument document model.viewport

        ( next, eventCmds ) =
            handleEvents events
                ( { model
                    | viewport = Viewport.setActivePath [] viewport
                    , document = Document.clearTouched document
                    , history = Edit.emptyHistory
                    , selection = Set.empty
                    , drag = Nothing
                    , bomOpen = False
                    , bomState = StudioBOM.init
                    , baseBOM = Nothing
                    , context =
                        { activeSubmodel = (Document.mainSubmodel document).name
                        , path = []
                        , coordinateFrame = LocalFrame
                        }
                  }
                , []
                )

        ( withTimer, hintCmd ) =
            scheduleHints next
    in
    ( withTimer
    , Cmd.batch (hintCmd :: Cmd.map ViewportMsg viewportCmd :: eventCmds)
    )


handleEvents : List Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvents events start =
    List.foldl handleEvent start events


handleEvent : Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvent event ( model, cmds ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ] )

        Viewport.FlattenRequested payload ->
            ( model, cmds ++ [ Ports.requestGeometryFlatten payload ] )

        Viewport.Clicked x y shift ->
            ( select x y shift model, cmds )

        Viewport.DoubleClicked x y ->
            case Viewport.pickSource x y model.viewport of
                Just sourceId ->
                    case Document.getPlacement sourceId model.document of
                        Just placement ->
                            if Document.isSubmodelReference placement model.document then
                                ( enterSubmodelPlacement placement model, cmds )

                            else
                                ( model, cmds )

                        Nothing ->
                            ( model, cmds )

                Nothing ->
                    ( model, cmds )

        Viewport.Tapped x y ->
            case model.palette.armed of
                Just armedPart ->
                    let
                        ( placedModel, placeCmd ) =
                            placeArmedPart armedPart ( x, y ) model
                    in
                    ( placedModel, cmds ++ [ placeCmd ] )

                Nothing ->
                    ( select x y False model, cmds )

        Viewport.PressStarted x y ->
            case startDrag ( x, y ) model of
                Just drag ->
                    -- Grabbing a part by its body makes it the selection.
                    ( { model
                        | viewport = Viewport.captureDrag model.viewport
                        , drag = Just drag
                        , selection = Set.fromList drag.ids
                        , inFlightRotationStep = 0
                        , snapRotationStep = 0
                      }
                    , cmds
                    )

                Nothing ->
                    ( model, cmds )

        Viewport.DragMoved x y ->
            case model.drag of
                Just drag ->
                    let
                        ( next, cmd ) =
                            moveDrag drag ( x, y ) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    ( model, cmds )

        Viewport.DragEnded _ _ ->
            let
                ( next, endCmd ) =
                    endDrag model
            in
            ( next, cmds ++ [ endCmd ] )

        Viewport.KeyAccepted keyEvent ->
            let
                withMods =
                    { model
                        | altHeld = keyEvent.alt || keyEvent.key == "Alt"
                        , shiftHeld = keyEvent.shift || keyEvent.key == "Shift"
                    }

                handleShortcut m =
                    case StudioShortcuts.actionFor (shortcutContext m) keyEvent of
                        Just action ->
                            let
                                ( next, cmd ) =
                                    update (msgForStudioAction action) m
                            in
                            ( next, cmds ++ [ cmd ] )

                        Nothing ->
                            case Shortcuts.actionFor (viewportShortcutContext m) keyEvent of
                                Just action ->
                                    let
                                        ( next, cmd ) =
                                            update (msgForAction action) m
                                    in
                                    ( next, cmds ++ [ cmd ] )

                                Nothing ->
                                    ( m, cmds )
            in
            case withMods.drag of
                Just d ->
                    let
                        isRotLeft =
                            keyEvent.key == "ArrowLeft" || keyEvent.key == "ArrowDown" || keyEvent.key == "["

                        isRotRight =
                            keyEvent.key == "ArrowRight" || keyEvent.key == "ArrowUp" || keyEvent.key == "r" || keyEvent.key == "R" || keyEvent.key == "]"
                    in
                    if isRotLeft then
                        let
                            updated =
                                if withMods.activeSnap /= Nothing then
                                    { withMods | snapRotationStep = model.snapRotationStep - 1 }

                                else
                                    { withMods | inFlightRotationStep = model.inFlightRotationStep - 1 }

                            ( next, cmd ) =
                                moveDrag d d.lastPoint updated
                        in
                        ( next, cmds ++ [ cmd ] )

                    else if isRotRight then
                        let
                            updated =
                                if withMods.activeSnap /= Nothing then
                                    { withMods | snapRotationStep = model.snapRotationStep + 1 }

                                else
                                    { withMods | inFlightRotationStep = model.inFlightRotationStep + 1 }

                            ( next, cmd ) =
                                moveDrag d d.lastPoint updated
                        in
                        ( next, cmds ++ [ cmd ] )

                    else if keyEvent.key == "Alt" || keyEvent.key == "Shift" then
                        let
                            ( next, cmd ) =
                                moveDrag d d.lastPoint withMods
                        in
                        ( next, cmds ++ [ cmd ] )

                    else
                        handleShortcut withMods

                Nothing ->
                    handleShortcut withMods

        Viewport.LoadStarted ->
            ( model, cmds )

        Viewport.PartsReady ->
            ( model, cmds )

        Viewport.LargeModel _ ->
            ( model, cmds )

        Viewport.ModelReady ->
            ( model, cmds )

        Viewport.LoadFailed _ ->
            ( model, cmds )


{-| Select what is under a point. Shift toggles it in or out of the selection;
a plain click on nothing clears it.
-}
select : Float -> Float -> Bool -> Model -> Model
select x y shift model =
    case ( Viewport.pickSource x y model.viewport, shift ) of
        ( Just id, True ) ->
            { model
                | selection =
                    if Set.member id model.selection then
                        Set.remove id model.selection

                    else
                        Set.insert id model.selection
            }

        ( Just id, False ) ->
            { model | selection = Set.singleton id }

        ( Nothing, True ) ->
            model

        ( Nothing, False ) ->
            { model | selection = Set.empty }


{-| The handles for the current selection, when the move tool is active.
-}
currentHandles : Model -> List Handle
currentHandles model =
    if model.tool == MoveTool then
        case Edit.selectionCenter (Set.toList model.selection) model.document of
            Just localCenter ->
                let
                    accum =
                        case List.reverse model.context.path of
                            head :: _ ->
                                head.accumulatedTransform

                            [] ->
                                Mat4.identity

                    worldCenter =
                        Mat4.transform accum localCenter

                    frameMatrix =
                        case model.context.coordinateFrame of
                            LocalFrame ->
                                accum

                            WorldFrame ->
                                Mat4.identity
                in
                Gizmo.handlesWithFrame frameMatrix model.viewport.camera model.viewport.width model.viewport.height worldCenter

            Nothing ->
                []

    else
        []


{-| The rotational arc handles for the current selection when the rotate tool is active.
-}
currentArcHandles : Model -> List ArcHandle
currentArcHandles model =
    if model.tool == RotateTool then
        case model.drag of
            Just { kind } ->
                case kind of
                    ArcDrag arcHandle ->
                        [ arcHandle ]

                    LinearDrag _ ->
                        []

                    PlaneDrag _ ->
                        []

            Nothing ->
                let
                    activePlacements =
                        Document.placementsOf model.context.activeSubmodel model.document

                    joints =
                        Joint.detectJoints activePlacements

                    selectedJoints =
                        List.filter
                            (\j ->
                                Set.member j.parentPlacementId model.selection
                                    || Set.member j.childPlacementId model.selection
                            )
                            joints

                    accum =
                        case List.reverse model.context.path of
                            head :: _ ->
                                head.accumulatedTransform

                            [] ->
                                Mat4.identity
                in
                case selectedJoints of
                    firstJoint :: _ ->
                        case firstJoint.jointType of
                            Joint.RevoluteJoint rev ->
                                case
                                    Gizmo.jointArcHandleWithFrame
                                        accum
                                        model.viewport.camera
                                        model.viewport.width
                                        model.viewport.height
                                        firstJoint.pivotWorld
                                        rev.axis
                                of
                                    Just handle ->
                                        [ handle ]

                                    Nothing ->
                                        defaultArcHandles model

                            _ ->
                                defaultArcHandles model

                    [] ->
                        defaultArcHandles model

    else
        []


defaultArcHandles : Model -> List ArcHandle
defaultArcHandles model =
    case Edit.selectionCenter (Set.toList model.selection) model.document of
        Just localCenter ->
            let
                accum =
                    case List.reverse model.context.path of
                        head :: _ ->
                            head.accumulatedTransform

                        [] ->
                            Mat4.identity

                worldCenter =
                    Mat4.transform accum localCenter

                frameMatrix =
                    case model.context.coordinateFrame of
                        LocalFrame ->
                            accum

                        WorldFrame ->
                            Mat4.identity
            in
            Gizmo.arcHandlesWithFrame frameMatrix model.viewport.camera model.viewport.width model.viewport.height worldCenter

        Nothing ->
            []


{-| How close, in pixels, a press has to be to a handle to grab it.
-}
grabRadius : Float
grabRadius =
    16


startDrag : ( Float, Float ) -> Model -> Maybe Drag
startDrag point model =
    if model.tool == MoveTool then
        case Gizmo.hit grabRadius point (currentHandles model) of
            Just handle ->
                Just
                    { kind = LinearDrag handle
                    , ids = Set.toList model.selection
                    , start = point
                    , lastPoint = point
                    , original = model.document
                    , amount = 0
                    }

            Nothing ->
                grabPart point model

    else if model.tool == RotateTool then
        Gizmo.hitArc grabRadius point (currentArcHandles model)
            |> Maybe.map
                (\arcHandle ->
                    { kind = ArcDrag arcHandle
                    , ids = Set.toList model.selection
                    , start = point
                    , lastPoint = point
                    , original = model.document
                    , amount = 0
                    }
                )

    else
        Nothing


{-| Pressing a part's body with the move tool grabs it: the whole selection if
the part is in it, else the part alone (which becomes the selection). A press on
empty space, or with Shift (which pans), leaves the camera to it.
-}
grabPart : ( Float, Float ) -> Model -> Maybe Drag
grabPart ( x, y ) model =
    if model.viewport.mousePanDragging then
        Nothing

    else
        Viewport.pickPoint x y model.viewport
            |> Maybe.map
                (\hit ->
                    { kind = PlaneDrag hit.point
                    , ids =
                        if Set.member hit.source model.selection then
                            Set.toList model.selection

                        else
                            [ hit.source ]
                    , start = ( x, y )
                    , lastPoint = ( x, y )
                    , original = model.document
                    , amount = 0
                    }
                )


{-| Follow the pointer: the document is the drag's original with the snapped
move or rotation applied, shown live but not yet recorded for undo.
-}
moveDrag : Drag -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
moveDrag drag point model =
    case drag.kind of
        LinearDrag handle ->
            moveLinearDrag drag handle point model

        PlaneDrag grab ->
            movePlaneDrag drag grab point model

        ArcDrag arcHandle ->
            let
                step =
                    if model.altHeld then
                        0

                    else
                        degrees (toFloat model.rotateStep)

                amount =
                    Gizmo.dragArcAngle step arcHandle drag.start point
            in
            if amount == drag.amount then
                ( model, Cmd.none )

            else
                let
                    rotationCommand =
                        if not (List.isEmpty model.context.path) then
                            case List.reverse model.context.path of
                                head :: _ ->
                                    case Mat4.inverse head.accumulatedTransform of
                                        Just inv ->
                                            let
                                                localPivot =
                                                    Mat4.transform inv arcHandle.center

                                                localAxis =
                                                    Gizmo.transformDirection inv arcHandle.axis

                                                effectiveRadians =
                                                    if Transform.determinant3 head.accumulatedTransform < 0 then
                                                        -amount

                                                    else
                                                        amount
                                            in
                                            RotateAroundAxis drag.ids
                                                { pivot = localPivot
                                                , axis = localAxis
                                                , radians = effectiveRadians
                                                }

                                        Nothing ->
                                            RotateAroundAxis drag.ids
                                                { pivot = arcHandle.center
                                                , axis = arcHandle.axis
                                                , radians = amount
                                                }

                                [] ->
                                    RotateAroundAxis drag.ids
                                        { pivot = arcHandle.center
                                        , axis = arcHandle.axis
                                        , radians = amount
                                        }

                        else
                            RotateAroundAxis drag.ids
                                { pivot = arcHandle.center
                                , axis = arcHandle.axis
                                , radians = amount
                                }

                    ( moved, _ ) =
                        Edit.apply rotationCommand drag.original
                in
                syncDocumentLive { model | document = moved, drag = Just { drag | amount = amount, lastPoint = point } }


moveLinearDrag : Drag -> Gizmo.Handle -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
moveLinearDrag drag handle point model =
    let
        amount =
            Gizmo.dragAmount (toFloat model.gridStep) handle drag.start point
    in
    applyDragDelta drag amount point (Vec3.scale amount (Edit.axisVector handle.axis)) model


{-| Slide a grabbed part over surfaces under the cursor: dynamically raycasts
the scene excluding dragged parts, auto-elevating the part bottom onto the hit
surface or ground plane Y = 0 (unless Shift is held to lock elevation).
From this surface-aligned pose, magnetic snapping with hysteresis is evaluated.
-}
movePlaneDrag : Drag -> Vec3.Vec3 -> ( Float, Float ) -> Model -> ( Model, Cmd Msg )
movePlaneDrag drag grab ( x, y ) model =
    let
        draggedIds =
            Set.fromList drag.ids

        surfaceHit =
            Viewport.pickSurfaceExcluding draggedIds x y model.viewport

        originalPlacements =
            drag.ids
                |> List.filterMap
                    (\id ->
                        case Document.getPlacement id drag.original of
                            Just p ->
                                Just p

                            Nothing ->
                                Document.getPlacement id model.document
                    )
    in
    case ( surfaceHit, originalPlacements ) of
        ( Just hit, leadP :: _ ) ->
            let
                resolveLines name =
                    case Dict.get name model.viewport.partCache of
                        Just (Resolve.Loaded lines) ->
                            Just lines

                        _ ->
                            Nothing

                leadConn =
                    Connectivity.extractPartConnectivity leadP.part resolveLines

                leadRawBox =
                    List.head leadConn.boundingBoxes
                        |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }

                leadBox =
                    Connectivity.effectiveEnvelope leadRawBox leadConn.connections

                bottomExtent =
                    case model.viewport.instancedScene |> Maybe.andThen (\s -> Dict.get leadP.part s.parts) of
                        Just m ->
                            Vec3.getY m.boundsMax

                        Nothing ->
                            Vec3.getY leadBox.max

                origPos =
                    Transform.position leadP.matrix

                targetY =
                    if model.shiftHeld then
                        Vec3.getY origPos

                    else
                        hit.topY - bottomExtent

                docHit =
                    Gizmo.toRender hit.point

                docHitX =
                    Vec3.getX docHit

                docHitZ =
                    Vec3.getZ docHit

                step =
                    toFloat (max 1 model.gridStep)

                targetX =
                    toFloat (round (docHitX / step)) * step

                targetZ =
                    toFloat (round (docHitZ / step)) * step

                targetPos =
                    vec3 targetX targetY targetZ

                effectiveTargetPos =
                    if not (List.isEmpty model.context.path) then
                        case List.reverse model.context.path of
                            head :: _ ->
                                case Mat4.inverse head.accumulatedTransform of
                                    Just inv ->
                                        Mat4.transform inv targetPos

                                    Nothing ->
                                        targetPos

                            [] ->
                                targetPos

                    else
                        targetPos

                inFlightAngle =
                    toFloat model.inFlightRotationStep * (pi / 2)

                leadSurfaceMatrix =
                    Mat4.mul
                        (Mat4.makeTranslate effectiveTargetPos)
                        (Mat4.mul
                            (Mat4.makeRotate inFlightAngle (vec3 0 1 0))
                            (Mat4.mul
                                (Mat4.makeTranslate (Vec3.negate origPos))
                                leadP.matrix
                            )
                        )

                deltaMat =
                    Mat4.mul leadSurfaceMatrix (Connectivity.invertRigid leadP.matrix)

                draggedSurfaceUpdates =
                    originalPlacements
                        |> List.map (\p -> ( p.id, Mat4.mul deltaMat p.matrix ))

                ( gizmoMovedDoc, _ ) =
                    Edit.apply (SetMatrix draggedSurfaceUpdates) drag.original

                effectiveBoxOf placement =
                    let
                        pConn =
                            Connectivity.extractPartConnectivity placement.part resolveLines

                        rawBox =
                            List.head pConn.boundingBoxes
                                |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                    in
                    Connectivity.effectiveEnvelope rawBox pConn.connections

                otherPlacements =
                    Document.placementsOf model.context.activeSubmodel gizmoMovedDoc
                        |> List.filter (\p -> not (List.member p.id drag.ids))

                sceneBoxes =
                    List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) otherPlacements

                currentDraggedPlacements =
                    drag.ids
                        |> List.filterMap (\id -> Document.getPlacement id gizmoMovedDoc)
            in
            if model.altHeld || model.snapDistance <= 0 then
                let
                    draggedBoxes =
                        List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) currentDraggedPlacements

                    collidingPairs =
                        Connectivity.checkCollisions 0.5 draggedBoxes sceneBoxes

                    collidingIds =
                        Set.fromList (List.map Tuple.first collidingPairs)

                    ( next, cmd ) =
                        syncDocumentLive
                            { model
                                | document = gizmoMovedDoc
                                , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                                , activeSnap = Nothing
                                , collidingPlacements =
                                    if model.collisionMode == Connectivity.CollisionOff then
                                        Set.empty

                                    else
                                        collidingIds
                            }
                in
                ( next, cmd )

            else
                let
                    targetWorldPoints =
                        otherPlacements
                            |> List.concatMap
                                (\targetP ->
                                    let
                                        pConn =
                                            Connectivity.extractPartConnectivity targetP.part resolveLines
                                    in
                                    List.map (Connectivity.transformPoint targetP.matrix targetP.id) pConn.connections
                                )

                    maybeSnap =
                        Connectivity.solveSnapWithHysteresis
                            model.snapDistance
                            (model.snapDistance * 1.5)
                            model.activeSnap
                            model.snapRotationStep
                            leadSurfaceMatrix
                            leadConn.connections
                            targetWorldPoints
                in
                case maybeSnap of
                    Just snap ->
                        let
                            snapDeltaMat =
                                Mat4.mul snap.snapMatrix (Connectivity.invertRigid leadP.matrix)

                            draggedSnapUpdates =
                                originalPlacements
                                    |> List.map (\p -> ( p.id, Mat4.mul snapDeltaMat p.matrix ))

                            ( snappedDoc, _ ) =
                                Edit.apply (SetMatrix draggedSnapUpdates) drag.original

                            snappedDraggedPlacements =
                                drag.ids
                                    |> List.filterMap (\id -> Document.getPlacement id snappedDoc)

                            draggedBoxes =
                                List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) snappedDraggedPlacements

                            activeSceneBoxes =
                                sceneBoxes
                                    |> List.filter (\( id, _, _ ) -> id /= snap.targetPlacementId)

                            collidingPairs =
                                Connectivity.checkCollisions 0.5 draggedBoxes activeSceneBoxes

                            isColliding =
                                not (List.isEmpty collidingPairs)

                            collidingIds =
                                Set.fromList (List.map Tuple.first collidingPairs)

                            guideWithCol =
                                Connectivity.setGuideColliding isColliding snap.guide

                            finalCandidate =
                                { snap | guide = guideWithCol }
                        in
                        syncDocumentLive
                            { model
                                | document = snappedDoc
                                , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                                , activeSnap = Just finalCandidate
                                , collidingPlacements =
                                    if model.collisionMode == Connectivity.CollisionOff then
                                        Set.empty

                                    else
                                        collidingIds
                            }

                    Nothing ->
                        let
                            draggedBoxes =
                                List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) currentDraggedPlacements

                            collidingPairs =
                                Connectivity.checkCollisions 0.5 draggedBoxes sceneBoxes

                            collidingIds =
                                Set.fromList (List.map Tuple.first collidingPairs)

                            ( next, cmd ) =
                                syncDocumentLive
                                    { model
                                        | document = gizmoMovedDoc
                                        , drag = Just { drag | amount = 0, lastPoint = ( x, y ) }
                                        , activeSnap = Nothing
                                        , collidingPlacements =
                                            if model.collisionMode == Connectivity.CollisionOff then
                                                Set.empty

                                            else
                                                collidingIds
                                    }
                        in
                        ( next, cmd )

        _ ->
            ( model, Cmd.none )


{-| Move the dragged parts by `delta` (document space) from where the drag
began, then snap to studs and check collisions as every move drag does.
-}
applyDragDelta : Drag -> Float -> ( Float, Float ) -> Vec3.Vec3 -> Model -> ( Model, Cmd Msg )
applyDragDelta drag amount point delta model =
    let
        effectiveDelta =
            if model.context.coordinateFrame == WorldFrame && not (List.isEmpty model.context.path) then
                case List.reverse model.context.path of
                    head :: _ ->
                        case Mat4.inverse head.accumulatedTransform of
                            Just inv ->
                                Vec3.sub (Mat4.transform inv delta) (Mat4.transform inv (Vec3.vec3 0 0 0))

                            Nothing ->
                                delta

                    [] ->
                        delta

            else
                delta

        ( gizmoMovedDoc, _ ) =
            Edit.apply (Translate drag.ids effectiveDelta) drag.original

        resolveLines name =
            case Dict.get name model.viewport.partCache of
                Just (Resolve.Loaded lines) ->
                    Just lines

                _ ->
                    Nothing

        effectiveBoxOf placement =
            let
                conn =
                    Connectivity.extractPartConnectivity placement.part resolveLines

                rawBox =
                    List.head conn.boundingBoxes
                        |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
            in
            Connectivity.effectiveEnvelope rawBox conn.connections

        otherPlacements =
            Document.placementsOf model.context.activeSubmodel gizmoMovedDoc
                |> List.filter (\p -> not (List.member p.id drag.ids))

        sceneBoxes =
            List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) otherPlacements

        draggedPlacements =
            drag.ids
                |> List.filterMap (\id -> Document.getPlacement id gizmoMovedDoc)
    in
    if model.altHeld || model.snapDistance <= 0 then
        let
            draggedBoxes =
                List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) draggedPlacements

            collidingPairs =
                Connectivity.checkCollisions 0.5 draggedBoxes sceneBoxes

            collidingIds =
                Set.fromList (List.map Tuple.first collidingPairs)

            ( next, cmd ) =
                syncDocumentLive
                    { model
                        | document = gizmoMovedDoc
                        , drag = Just { drag | amount = amount, lastPoint = point }
                        , activeSnap = Nothing
                        , collidingPlacements =
                            if model.collisionMode == Connectivity.CollisionOff then
                                Set.empty

                            else
                                collidingIds
                    }
        in
        ( next, cmd )

    else
        case draggedPlacements of
            leadPlacement :: _ ->
                let
                    targetWorldPoints =
                        otherPlacements
                            |> List.concatMap
                                (\targetP ->
                                    let
                                        pConn =
                                            Connectivity.extractPartConnectivity targetP.part resolveLines
                                    in
                                    List.map (Connectivity.transformPoint targetP.matrix targetP.id) pConn.connections
                                )

                    leadConn =
                        Connectivity.extractPartConnectivity leadPlacement.part resolveLines

                    maybeSnap =
                        Connectivity.solveSnapWithHysteresis
                            model.snapDistance
                            (model.snapDistance * 1.5)
                            model.activeSnap
                            model.snapRotationStep
                            leadPlacement.matrix
                            leadConn.connections
                            targetWorldPoints
                in
                case maybeSnap of
                    Just snap ->
                        let
                            leadOrig =
                                Document.getPlacement leadPlacement.id drag.original
                                    |> Maybe.withDefault leadPlacement

                            deltaMat =
                                Mat4.mul snap.snapMatrix (Connectivity.invertRigid leadOrig.matrix)

                            draggedUpdates =
                                draggedPlacements
                                    |> List.filterMap
                                        (\p ->
                                            Document.getPlacement p.id drag.original
                                                |> Maybe.map (\origP -> ( p.id, Mat4.mul deltaMat origP.matrix ))
                                        )

                            ( snappedDoc, _ ) =
                                Edit.apply (SetMatrix draggedUpdates) drag.original

                            snappedDraggedPlacements =
                                drag.ids
                                    |> List.filterMap (\id -> Document.getPlacement id snappedDoc)

                            draggedBoxes =
                                List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) snappedDraggedPlacements

                            activeSceneBoxes =
                                sceneBoxes
                                    |> List.filter (\( id, _, _ ) -> id /= snap.targetPlacementId)

                            collidingPairs =
                                Connectivity.checkCollisions 0.5 draggedBoxes activeSceneBoxes

                            isColliding =
                                not (List.isEmpty collidingPairs)

                            collidingIds =
                                Set.fromList (List.map Tuple.first collidingPairs)

                            guideWithCol =
                                Connectivity.setGuideColliding isColliding snap.guide

                            finalCandidate =
                                { snap | guide = guideWithCol }
                        in
                        syncDocumentLive
                            { model
                                | document = snappedDoc
                                , drag = Just { drag | amount = amount, lastPoint = point }
                                , activeSnap = Just finalCandidate
                                , collidingPlacements =
                                    if model.collisionMode == Connectivity.CollisionOff then
                                        Set.empty

                                    else
                                        collidingIds
                            }

                    Nothing ->
                        let
                            draggedBoxes =
                                List.map (\p -> ( p.id, p.matrix, effectiveBoxOf p )) draggedPlacements

                            collidingPairs =
                                Connectivity.checkCollisions 0.5 draggedBoxes sceneBoxes

                            collidingIds =
                                Set.fromList (List.map Tuple.first collidingPairs)

                            ( next, cmd ) =
                                syncDocumentLive
                                    { model
                                        | document = gizmoMovedDoc
                                        , drag = Just { drag | amount = amount, lastPoint = point }
                                        , activeSnap = Nothing
                                        , collidingPlacements =
                                            if model.collisionMode == Connectivity.CollisionOff then
                                                Set.empty

                                            else
                                                collidingIds
                                    }
                        in
                        ( next, cmd )

            [] ->
                ( model, Cmd.none )


{-| Finish a drag: the document already shows the move or rotation, so record its undo,
schedule hints and save the draft once.
-}
endDrag : Model -> ( Model, Cmd Msg )
endDrag model =
    case model.drag of
        Just drag ->
            if model.collisionMode == Connectivity.CollisionStrict && not (Set.isEmpty model.collidingPlacements) then
                let
                    ( reverted, cmd ) =
                        syncDocumentLive
                            { model
                                | document = drag.original
                                , drag = Nothing
                                , activeSnap = Nothing
                                , inFlightRotationStep = 0
                                , snapRotationStep = 0
                                , collidingPlacements = Set.empty
                            }
                in
                ( reverted, cmd )

            else
                let
                    currentPlacements =
                        drag.ids
                            |> List.filterMap (\id -> Document.getPlacement id model.document)

                    originalPlacements =
                        drag.ids
                            |> List.filterMap (\id -> Document.getPlacement id drag.original)

                    anyNew =
                        List.any (\id -> Document.getPlacement id drag.original == Nothing) drag.ids

                    hasMoved =
                        List.any
                            (\p ->
                                case Document.getPlacement p.id drag.original of
                                    Just origP ->
                                        p.matrix /= origP.matrix

                                    Nothing ->
                                        True
                            )
                            currentPlacements
                in
                if anyNew && not hasMoved then
                    syncDocumentLive
                        { model
                            | document = drag.original
                            , drag = Nothing
                            , activeSnap = Nothing
                            , inFlightRotationStep = 0
                            , snapRotationStep = 0
                            , collidingPlacements = Set.empty
                        }

                else if not hasMoved then
                    ( { model
                        | drag = Nothing
                        , activeSnap = Nothing
                        , inFlightRotationStep = 0
                        , snapRotationStep = 0
                        , collidingPlacements = Set.empty
                      }
                    , Cmd.none
                    )

                else if anyNew then
                    let
                        newParts =
                            currentPlacements
                                |> List.map (\p -> { part = p.part, color = p.color, matrix = p.matrix })

                        ( newDoc, newHistory ) =
                            Edit.record (AddIn model.context.activeSubmodel newParts) drag.original model.history

                        firstFile =
                            List.head currentPlacements |> Maybe.map .part |> Maybe.withDefault ""

                        newRecents =
                            if firstFile /= "" then
                                firstFile
                                    :: List.filter (\f -> f /= firstFile) model.palette.recentParts
                                    |> List.take 20

                            else
                                model.palette.recentParts

                        palette =
                            model.palette

                        updated =
                            { model
                                | document = newDoc
                                , history = newHistory
                                , drag = Nothing
                                , activeSnap = Nothing
                                , inFlightRotationStep = 0
                                , snapRotationStep = 0
                                , collidingPlacements =
                                    if model.collisionMode == Connectivity.CollisionOff then
                                        Set.empty

                                    else
                                        model.collidingPlacements
                                , palette =
                                    Palette.refreshFiltered model.activeColour
                                        { palette | recentParts = newRecents }
                            }

                        ( withTimer, hintCmd ) =
                            scheduleHints updated
                    in
                    ( withTimer
                    , Cmd.batch
                        [ hintCmd
                        , Ports.saveDraft (Writer.writeDocument updated.document)
                        , Ports.saveRecents newRecents
                        ]
                    )

                else
                    let
                        inversePairs =
                            List.map (\p -> ( p.id, p.matrix )) originalPlacements

                        updated =
                            { model
                                | drag = Nothing
                                , activeSnap = Nothing
                                , inFlightRotationStep = 0
                                , snapRotationStep = 0
                                , collidingPlacements =
                                    if model.collisionMode == Connectivity.CollisionOff then
                                        Set.empty

                                    else
                                        model.collidingPlacements
                                , history =
                                    Edit.pushApplied
                                        (SetMatrix inversePairs)
                                        model.history
                            }

                        ( withTimer, hintCmd ) =
                            scheduleHints updated
                    in
                    ( withTimer
                    , Cmd.batch
                        [ hintCmd
                        , Ports.saveDraft (Writer.writeDocument updated.document)
                        ]
                    )

        Nothing ->
            ( model, Cmd.none )


shortcutContext : Model -> StudioShortcuts.Context
shortcutContext model =
    { hasSelection = not (Set.isEmpty model.selection)
    , helpVisible = model.helpOpen
    , walking = model.viewport.camera.navigation == Camera.Walk
    , inSubmodel = not (List.isEmpty model.context.path)
    , menuOpen = model.exportMenuOpen
    }


viewportShortcutContext : Model -> { helpVisible : Bool, walking : Bool }
viewportShortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.viewport.camera.navigation == Camera.Walk
    }


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


msgForStudioAction : StudioShortcuts.Action -> Msg
msgForStudioAction action =
    case action of
        StudioShortcuts.Undo ->
            Undo

        StudioShortcuts.Redo ->
            Redo

        StudioShortcuts.DeleteSelection ->
            DeleteSelection

        StudioShortcuts.DuplicateSelection ->
            DuplicateSelection

        StudioShortcuts.SelectAll ->
            SelectAll

        StudioShortcuts.ClearSelection ->
            ClearSelection

        StudioShortcuts.GroupSubmodel ->
            GroupSelection

        StudioShortcuts.ExplodeSubmodel ->
            ExplodeSelection

        StudioShortcuts.EnterSubmodel ->
            EnterSubmodel

        StudioShortcuts.ExitSubmodel ->
            ExitSubmodel

        StudioShortcuts.UseTool tool ->
            UseTool tool

        StudioShortcuts.RotateSelection axis sign ->
            RotateSelection axis sign

        StudioShortcuts.NextGridStep ->
            NextGridStep

        StudioShortcuts.NextRotateStep ->
            NextRotateStep

        StudioShortcuts.CoarserGridStep ->
            CoarserGridStep

        StudioShortcuts.OpenFile ->
            RequestFileUpload

        StudioShortcuts.FocusSearch ->
            FocusSearch

        StudioShortcuts.ToggleBOM ->
            ToggleBOM

        StudioShortcuts.OpenInstructions ->
            -- The page change happens in `Main`, which only sees messages that
            -- reach it from outside, so the key re-sends the button's message.
            Request InstructionsRequested

        StudioShortcuts.Simulate ->
            Request SimulateRequested

        StudioShortcuts.CloseMenu ->
            ToggleExportMenu


{-| Map a viewport shortcut to the message it stands for. Playback shortcuts have
no meaning while editing.
-}
msgForAction : Shortcuts.Action -> Msg
msgForAction action =
    case action of
        Shortcuts.TogglePlayback ->
            NoOp

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
        , -- Window size, not the runtime's port: Elm replaces the mount node, so the
          -- runtime's ResizeObserver watches a detached element and never fires.
          Browser.Events.onResize WindowSized
        , Ports.fileContentReceived FileTextReceived
        , Ports.fileLoadError FileFailed
        , Ports.geometryFlattened (ViewportMsg << Viewport.GeometryFlattened)
        , Ports.geometryFlattenFailed (ViewportMsg << Viewport.GeometryFlattenFailed)
        , Ports.saveRequested (\_ -> Save)
        , Ports.thumbnailLoaded ThumbnailLoaded
        , Browser.Events.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
        ]



-- ── View ──────────────────────────────────────────────────────────────────────


view : Model -> Html Msg
view model =
    let
        narrow =
            model.layout.mode == Layout.Narrow

        body =
            case model.layout.mode of
                Layout.Wide ->
                    div
                        [ Attr.style "display" "flex"
                        , Attr.style "flex" "1"
                        , Attr.style "min-height" "0"
                        ]
                        [ pane "Parts and colours"
                            [ heading "parts-heading" "Parts"
                            , div [ Attr.style "flex" "1", Attr.style "min-height" "0" ] [ viewPalette model ]
                            , heading "colours-heading" "Colour"
                            , viewColours model
                            ]
                        , canvasArea model
                        , pane "Selection and structure"
                            [ heading "selection-heading" "Selection"
                            , viewInspector model
                            , heading "structure-heading" "Structure"
                            , div [ Attr.class "scroll-thin", Attr.style "flex" "1", Attr.style "min-height" "0", Attr.style "overflow-y" "auto" ] [ viewTree model ]
                            ]
                        ]

                Layout.Narrow ->
                    div
                        [ Attr.style "position" "relative"
                        , Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "flex" "1"
                        , Attr.style "min-height" "0"
                        ]
                        [ canvasArea model
                        , viewTabs model
                        , viewSheet model
                        ]
    in
    div
        [ Attr.style "position" "relative"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" Theme.appBackground
        , Attr.style "color" Theme.textPrimary
        ]
        ([ viewHeader model
         , body
         , viewAnnouncement model
         ]
            ++ (if model.exportMenuOpen then
                    [ div
                        -- A click anywhere else closes the menu.
                        [ Attr.style "position" "absolute"
                        , Attr.style "inset" "0"
                        , Attr.style "z-index" "29"
                        , Html.Events.onClick ToggleExportMenu
                        ]
                        []
                    , viewExportMenu model
                    ]

                else
                    []
               )
            ++ (if model.bomOpen then
                    [ viewBOMModal model ]

                else
                    []
               )
        )


viewBOMModal : Model -> Html Msg
viewBOMModal model =
    let
        bom =
            computeBOM model

        submodels =
            model.document.submodels
                |> List.map .name
                |> List.filter (\n -> String.trim n /= "")

        config =
            { onQuery = BOMQuery
            , onCategory = BOMCategory
            , onSubmodel = BOMSubmodel
            , onSort = BOMSort
            , onSelectLot = BOMSelectLot
            , onExportBrickLink = BOMExportBrickLink
            , onExportRebrickable = BOMExportRebrickable
            , onCopyMarkdown = BOMCopyMarkdown
            , onCopyPlainText = BOMCopyPlainText
            , onToggleDiff = BOMToggleDiff
            , onClose = CloseBOM
            }
    in
    StudioBOM.view config model.bomState submodels bom model.baseBOM


partIndexFromPalette : Palette.State -> PartIndex
partIndexFromPalette palette =
    case palette.index of
        Palette.Loaded pIndex ->
            Catalog.fromPartList (List.map (\p -> { file = p.file, description = p.description, category = p.category }) pIndex.parts)

        _ ->
            Catalog.emptyPartIndex


{-| Give "~Moved to" stubs the name and category of the part they point at.
The part index leaves stubs out, so a model that still uses an old number
(4185 for 4185a) listed it as just "Part 4185".
-}
withMovedParts : Model -> PartIndex -> PartIndex
withMovedParts model index =
    Document.placements model.document
        |> List.map .fileText
        |> Set.fromList
        |> Set.foldl
            (\file acc ->
                case Catalog.getPartInfo file acc of
                    Just _ ->
                        acc

                    Nothing ->
                        movedTo model (partKey file)
                            |> Maybe.andThen (\target -> Catalog.getPartInfo target acc)
                            |> Maybe.map (\info -> Catalog.insertPart file info acc)
                            |> Maybe.withDefault acc
            )
            index


computeBOM : Model -> ModelBOM
computeBOM model =
    let
        pIndex =
            withMovedParts model (partIndexFromPalette model.palette)
    in
    case model.bomState.submodel of
        Just subName ->
            BOM.generateSubmodelBOM subName model.document pIndex model.elementCatalog

        Nothing ->
            BOM.generateBOMInPath model.viewport.activePath model.document pIndex model.elementCatalog


{-| The 3D canvas with the tools, status and help drawn over it.
-}
canvasArea : Model -> Html Msg
canvasArea model =
    Html.main_
        [ Attr.style "position" "relative"
        , Attr.style "width" (String.fromInt model.layout.canvasWidth ++ "px")
        , Attr.style "height" (String.fromInt model.layout.canvasHeight ++ "px")
        , Attr.style "flex" "none"
        , Attr.style "overflow" "hidden"
        ]
        [ div
            -- The keyboard's home: focusable so the camera and nudge keys have
            -- somewhere to go, and `application` so screen readers pass them on.
            -- Clicking the canvas focuses it, which also takes focus off
            -- whichever toolbar button was used last.
            [ Attr.id "canvas"
            , Attr.class "canvas-focus"
            , Attr.tabindex 0
            , Attr.attribute "role" "application"
            , Attr.attribute "aria-roledescription" "3D model view"
            , Attr.attribute "aria-label" "Model"
            , Attr.attribute "aria-describedby" "canvas-help"
            , Attr.style "position" "absolute"
            , Attr.style "inset" "0"
            ]
            [ span [ Attr.id "canvas-help", Attr.class "sr-only" ]
                [ text "Drag to orbit, Shift and drag to pan, scroll to zoom. Arrow keys orbit the camera, or nudge the selected parts. Press question mark for all keyboard shortcuts." ]
            , canvasView model
            ]
        , overlay model
        ]


canvasView : Model -> Html Msg
canvasView model =
    Html.map ViewportMsg
        (Viewport.view
            { extraEntities =
                \camera _ aspect ->
                    Overlay.entities camera
                        aspect
                        model.viewport.instancedScene
                        { selection = model.selection

                        -- Soft collision keeps the set after a drag for the
                        -- inspector's warning; the 3D outline is drag feedback only.
                        , colliding =
                            if model.drag == Nothing then
                                Set.empty

                            else
                                model.collidingPlacements
                        , snapGuide = Maybe.map .guide model.activeSnap
                        }
                        (currentHandles model)
                        (currentArcHandles model)
            , coarsenWhileMoving = True
            }
            model.viewport
        )


{-| A side pane of the wide layout, as a labelled landmark.
-}
pane : String -> List (Html Msg) -> Html Msg
pane label children =
    Html.aside
        [ Attr.attribute "aria-label" label
        , Attr.style "width" (String.fromInt Layout.panelWidth ++ "px")
        , Attr.style "flex" "none"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "padding" "12px 14px"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "8px"
        , Attr.style "min-height" "0"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "border-left" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-right" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "overflow" "hidden"
        ]
        children


{-| A pane section title: a real heading, so screen readers can jump between
sections, styled as a small overline.
-}
heading : String -> String -> Html Msg
heading id title =
    Html.h2
        [ Attr.id id
        , Attr.style "margin" "6px 0 0"
        , Attr.style "color" Theme.textMuted
        , Attr.style "text-transform" "uppercase"
        , Attr.style "font-size" "11px"
        , Attr.style "font-weight" "600"
        , Attr.style "letter-spacing" "0.08em"
        ]
        [ text title ]


viewPalette : Model -> Html Msg
viewPalette model =
    Palette.view
        { onQuery = PaletteQuery
        , onGroup = PaletteGroup
        , onGroupAndFocus = PaletteGroupAndFocus
        , onCategory = PaletteCategory
        , onArm = ArmPart
        , onStartDrag = StartDragPart
        , onAdd = AddArmedPart
        , onManual = ManualPart
        , onAddManual = AddManualPart
        , onToggleFavorite = PaletteToggleFavorite
        , onSelectShelf = PaletteSelectShelf
        , onToggleColorFilter = PaletteToggleColorFilter
        , onInspect = PaletteInspect
        , onRotateTurntable = PaletteRotateTurntable
        , activeColour = model.activeColour
        , layoutMode = model.layout.mode
        , thumbnails = model.thumbnails
        , preview = viewPreview model
        }
        model.palette


{-| The picked part, drawn small by its own viewport.
-}
viewPreview : Model -> Html Msg
viewPreview model =
    let
        ( w, h ) =
            previewSize
    in
    div
        [ Attr.style "width" (String.fromInt w ++ "px")
        , Attr.style "height" (String.fromInt h ++ "px")
        , Attr.style "flex" "none"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "overflow" "hidden"
        , Attr.style "touch-action" "none"
        , Attr.style "background" Theme.panelSubtleBackground
        ]
        (case model.palette.armed of
            Just _ ->
                [ Html.map PreviewMsg
                    (Viewport.view { extraEntities = \_ _ _ -> [], coarsenWhileMoving = False } model.preview)
                ]

            Nothing ->
                [ div
                    [ Attr.style "height" "100%"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "padding" "12px"
                    , Attr.style "box-sizing" "border-box"
                    , Attr.style "text-align" "center"
                    , Attr.style "color" Theme.textMuted
                    , Attr.style "font-size" "13px"
                    ]
                    [ text "Pick a part to preview it" ]
                ]
        )


viewColours : Model -> Html Msg
viewColours model =
    let
        activePart =
            case model.palette.armed of
                Just part ->
                    Just part.file

                Nothing ->
                    case Set.toList model.selection |> List.filterMap (\id -> Document.getPlacement id model.document) of
                        placement :: _ ->
                            Just placement.fileText

                        [] ->
                            Nothing
    in
    Colours.view
        { current = model.activeColour
        , onPick = PickColour
        , focus = FocusElement
        , armedPart = activePart
        , filterOfficialOnly = model.palette.colorFilterMode == Palette.OfficialOnly
        , onToggleFilter = PaletteToggleColorFilter
        }


viewInspector : Model -> Html Msg
viewInspector model =
    Inspector.viewWithCollisions
        model.collidingPlacements
        { describe = partDescription model
        , onPosition = SetPosition
        , onRotation = SetRotation
        , onDuplicate = DuplicateSelection
        , onDelete = DeleteSelection
        , focusedField = model.inspectorFocus
        , onFocus = FocusInspectorField
        , onBlur = BlurInspectorField
        , onCommit = CommitInspectorField
        , onCancel = CancelInspectorField
        }
        model.document
        model.selection


viewTree : Model -> Html Msg
viewTree model =
    Tree.view
        { selection = model.selection
        , onSelect = SelectPlacement
        , activeSubmodel = model.context.activeSubmodel
        , onSelectSubmodel = SelectSubmodel
        , describe = \file -> partDescription model file
        , colourName = \code -> Dict.get code ColorData.colorNames |> Maybe.withDefault ("Colour " ++ String.fromInt code)
        , colourCss = \code -> Dict.get code ColorData.colorTable |> Maybe.map Colours.css |> Maybe.withDefault "transparent"
        , focus = FocusElement
        , lastFocused = model.treeFocus
        }
        model.document


{-| The palette's description for a part file, or "" when the catalogue is not
loaded or does not list the file. Matches case-insensitively, with or without ".dat".
-}
partDescription : Model -> String -> String
partDescription model file =
    let
        key =
            partKey file
    in
    case Dict.get key model.descriptions of
        Just description ->
            description

        Nothing ->
            -- "~Moved to 4185a" stubs are left out of the part index, but the
            -- part itself has been loaded for drawing: follow the stub once.
            movedTo model key
                |> Maybe.andThen (\target -> Dict.get (partKey target) model.descriptions)
                |> Maybe.withDefault ""


{-| The part a "~Moved to" stub points at, from the loaded part file.
-}
movedTo : Model -> String -> Maybe String
movedTo model key =
    [ key ++ ".dat", "parts/" ++ key ++ ".dat" ]
        |> List.filterMap (\candidate -> Dict.get candidate model.viewport.partCache)
        |> List.head
        |> Maybe.andThen
            (\status ->
                case status of
                    Resolve.Loaded ((Comment first) :: _) ->
                        let
                            words =
                                String.words first
                                    |> List.filter (\w -> w /= "0")
                        in
                        case words of
                            "~Moved" :: "to" :: target :: _ ->
                                Just target

                            _ ->
                                Nothing

                    _ ->
                        Nothing
            )


{-| A part file as the description lookup keys it: lower case, no `.dat`.
-}
partKey : String -> String
partKey name =
    let
        lower =
            String.toLower name
    in
    if String.endsWith ".dat" lower then
        String.dropRight 4 lower

    else
        lower


{-| The narrow layout's tab bar: one button per panel, tapping the open one closes it.
-}
viewTabs : Model -> Html Msg
viewTabs model =
    let
        tab panel label =
            button
                [ Attr.type_ "button"
                , Attr.class "ui-button"
                , Html.Events.onClick
                    (OpenPanel
                        (if model.panel == Just panel then
                            Nothing

                         else
                            Just panel
                        )
                    )
                , Attr.attribute "aria-expanded" (boolAttr (model.panel == Just panel))
                , Attr.attribute "aria-controls" "panel-sheet"
                , Attr.style "flex" "1"
                , Attr.style "padding" "0 8px"
                , Attr.style "border" "none"
                , Attr.style "border-top"
                    ("3px solid "
                        ++ (if model.panel == Just panel then
                                Theme.brandYellow

                            else
                                "transparent"
                           )
                    )
                , Attr.style "background" "transparent"
                , Attr.style "color"
                    (if model.panel == Just panel then
                        Theme.textPrimary

                     else
                        Theme.textMuted
                    )
                , Attr.style "font" "inherit"
                , Attr.style "font-size" "13px"
                , Attr.style "font-weight" "600"
                , Attr.style "cursor" "pointer"
                ]
                [ text label ]
    in
    Html.nav
        [ Attr.attribute "aria-label" "Panels"
        , Attr.style "height" (String.fromInt Layout.tabBarHeight ++ "px")
        , Attr.style "flex" "none"
        , Attr.style "display" "flex"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
        ]
        [ tab PartsPanel "Parts"
        , tab ColoursPanel "Colour"
        , tab InspectorPanel "Selection"
        , tab TreePanel "Structure"
        ]


{-| The open panel of the narrow layout, as a sheet over the lower half.
-}
viewSheet : Model -> Html Msg
viewSheet model =
    case model.panel of
        Nothing ->
            text ""

        Just panel ->
            Html.section
                [ Attr.id "panel-sheet"
                , Attr.attribute "aria-label" (panelName panel)
                , Attr.class "scroll-thin"
                , Attr.style "position" "absolute"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "bottom" (String.fromInt Layout.tabBarHeight ++ "px")
                , Attr.style "height" "45%"
                , Attr.style "box-sizing" "border-box"
                , Attr.style "padding" "12px"
                , Attr.style "overflow-y" "auto"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "border-top" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "12px 12px 0 0"
                , Attr.style "box-shadow" "0 -8px 24px color-mix(in srgb, var(--color-brand) 12%, transparent)"
                , Attr.style "z-index" "5"
                ]
                [ case panel of
                    PartsPanel ->
                        viewPalette model

                    ColoursPanel ->
                        viewColours model

                    InspectorPanel ->
                        viewInspector model

                    TreePanel ->
                        viewTree model
                ]


panelName : Panel -> String
panelName panel =
    case panel of
        PartsPanel ->
            "Parts"

        ColoursPanel ->
            "Colour"

        InspectorPanel ->
            "Selection"

        TreePanel ->
            "Structure"


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
         , viewBreadcrumbs model
         , viewToolRail model
         , viewSnapBar model
         ]
            ++ (if model.panel == Nothing then
                    [ viewPad model ]

                else
                    []
               )
            ++ (if model.helpOpen then
                    [ div
                        [ Attr.style "position" "absolute"
                        , Attr.style "inset" "0"
                        , Attr.style "background" "color-mix(in srgb, var(--color-brand) 18%, transparent)"
                        , Attr.style "pointer-events" "auto"
                        , Html.Events.onClick ToggleHelp
                        ]
                        []
                    , ShortcutHelp.view
                        { rows = Shortcuts.helpRows ++ StudioShortcuts.helpRows
                        , onClose = ToggleHelp
                        }
                    ]

                else
                    []
               )
        )


viewBreadcrumbs : Model -> Html Msg
viewBreadcrumbs model =
    if List.isEmpty model.context.path then
        text ""

    else
        let
            lastIndex =
                List.length model.context.path - 1

            crumb target label isCurrent =
                Html.li [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "6px" ]
                    [ if isCurrent then
                        span [ Attr.attribute "aria-current" "location", Attr.style "font-weight" "600" ] [ text label ]

                      else
                        button
                            [ Attr.type_ "button"
                            , Html.Events.onClick (NavigateToSubmodel target)
                            , Attr.style "background" "none"
                            , Attr.style "border" "none"
                            , Attr.style "padding" "2px 0"
                            , Attr.style "font" "inherit"
                            , Attr.style "color" Theme.textPrimary
                            , Attr.style "text-decoration" "underline"
                            , Attr.style "cursor" "pointer"
                            ]
                            [ text label ]
                    ]

            separator =
                Html.li [ Attr.attribute "aria-hidden" "true", Attr.style "color" Theme.textMuted ] [ text "›" ]

            segments =
                crumb 0 "Model" False
                    :: List.concat
                        (List.indexedMap
                            (\idx step -> [ separator, crumb (idx + 1) step.submodelName (idx == lastIndex) ])
                            model.context.path
                        )

            frameLabel =
                case model.context.coordinateFrame of
                    LocalFrame ->
                        "Frame: local"

                    WorldFrame ->
                        "Frame: world"
        in
        Html.nav
            [ Attr.attribute "aria-label" "Submodel path"
            , Attr.style "position" "absolute"
            , Attr.style "top" "12px"
            , Attr.style "left" "50%"
            , Attr.style "transform" "translateX(-50%)"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "10px"
            , Attr.style "padding" "4px 4px 4px 12px"
            , Attr.style "border-radius" "10px"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "box-shadow" floatingShadow
            , Attr.style "pointer-events" "auto"
            , Attr.style "font-size" "13px"
            , Attr.style "white-space" "nowrap"
            ]
            [ Html.ol [ Attr.style "display" "flex", Attr.style "gap" "6px", Attr.style "list-style" "none", Attr.style "margin" "0", Attr.style "padding" "0" ] segments
            , Button.secondary ToggleCoordinateFrame frameLabel
            , Button.icon
                { icon = featherIcon FeatherIcons.logOut
                , label = "Leave submodel"
                , shortcut = Just "Esc"
                , pressed = Nothing
                , enabled = True
                , showLabel = True
                }
                ExitSubmodel
            ]


{-| The app bar: what is open, then the document's actions.
-}
viewHeader : Model -> Html Msg
viewHeader model =
    let
        narrow =
            model.layout.mode == Layout.Narrow

        action iconName label shortcut msg =
            Button.icon
                { icon = featherIcon iconName
                , label = label
                , shortcut = shortcut
                , pressed = Nothing
                , enabled = True
                , showLabel = not narrow
                }
                msg

        group label children =
            div
                [ Attr.attribute "role" "group"
                , Attr.attribute "aria-label" label
                , Attr.style "display" "flex"
                , Attr.style "gap" "6px"
                , Attr.style "flex" "none"
                ]
                children

        divider =
            div
                [ Attr.attribute "aria-hidden" "true"
                , Attr.style "width" "1px"
                , Attr.style "height" "24px"
                , Attr.style "flex" "none"
                , Attr.style "background" Theme.borderDefault
                ]
                []
    in
    Html.header
        [ Attr.class "scroll-none"
        , Attr.style "height" (String.fromInt (Layout.headerHeight model.layout.mode) ++ "px")
        , Attr.style "flex" "none"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "10px"
        , Attr.style "padding" "0 12px"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        ]
        [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "10px", Attr.style "flex" "none" ]
            [ brandMark
            , Html.h1
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "15px"
                , Attr.style "font-weight" "700"
                , Attr.style "letter-spacing" "-0.01em"
                , Attr.style "white-space" "nowrap"
                ]
                (if narrow then
                    [ span [ Attr.class "sr-only" ] [ text "Palikkakehys " ], text "Studio" ]

                 else
                    [ text "Palikkakehys Studio" ]
                )
            ]
        , div
            [ Attr.style "flex" "1"
            , Attr.style "min-width" "40px"
            , Attr.style "overflow" "hidden"
            , Attr.style "text-overflow" "ellipsis"
            , Attr.style "white-space" "nowrap"
            , Attr.style "color" Theme.textMuted
            , Attr.style "font-size" "13px"
            ]
            [ text (documentSummary model) ]
        , if narrow then
            text ""

          else
            group "File"
                [ action FeatherIcons.filePlus "New" Nothing NewDocument
                , action FeatherIcons.folder "Open" (Just "Ctrl O") RequestFileUpload
                , action FeatherIcons.save "Save" (Just "Ctrl S") Save
                , button
                    [ Attr.type_ "button"
                    , Attr.id "export-button"
                    , Attr.class "ui-button"
                    , Html.Events.onClick ToggleExportMenu
                    , Attr.attribute "aria-expanded" (boolAttr model.exportMenuOpen)
                    , Attr.attribute "aria-haspopup" "true"
                    , Attr.attribute "aria-controls" "export-menu"
                    , Attr.title "Export"
                    , Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "gap" "6px"
                    , Attr.style "height" "36px"
                    , Attr.style "min-width" "36px"
                    , Attr.style "justify-content" "center"
                    , Attr.style "padding"
                        (if narrow then
                            "0"

                         else
                            "0 10px 0 12px"
                        )
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "6px"
                    , Attr.style "background"
                        (if model.exportMenuOpen then
                            Theme.brandYellow

                         else
                            Theme.panelSubtleBackground
                        )
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "font" "inherit"
                    , Attr.style "font-size" "13px"
                    , Attr.style "font-weight" "500"
                    , Attr.style "cursor" "pointer"
                    ]
                    (if narrow then
                        [ featherIcon FeatherIcons.share, span [ Attr.class "sr-only" ] [ text "Export" ] ]

                     else
                        [ featherIcon FeatherIcons.share, text "Export", featherIcon FeatherIcons.chevronDown ]
                    )
                ]
        , if narrow then
            text ""

          else
            divider
        , if narrow then
            text ""

          else
            group "Model"
                [ Button.icon
                    { icon = featherIcon FeatherIcons.sun
                    , label = "Realistic"
                    , shortcut = Nothing
                    , pressed = Just (model.renderQuality == Style.Realistic)
                    , enabled = True
                    , showLabel = not narrow
                    }
                    ToggleRenderQuality
                , action FeatherIcons.list
                    ("Parts list ("
                        ++ String.fromInt (List.length (Document.placements model.document))
                        ++ ")"
                    )
                    (Just "B")
                    ToggleBOM
                , action FeatherIcons.bookOpen "Instructions" (Just "I") InstructionsRequested
                , simulateButton narrow
                ]
        , if narrow then
            div [ Attr.style "display" "flex", Attr.style "gap" "6px", Attr.style "flex" "none" ]
                [ simulateButton narrow
                , button
                    [ Attr.type_ "button"
                    , Attr.id "export-button"
                    , Attr.class "ui-button"
                    , Html.Events.onClick ToggleExportMenu
                    , Attr.attribute "aria-expanded" (boolAttr model.exportMenuOpen)
                    , Attr.attribute "aria-haspopup" "true"
                    , Attr.attribute "aria-controls" "export-menu"
                    , Attr.attribute "aria-label" "More actions"
                    , Attr.title "More actions"
                    , Attr.style "display" "inline-flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "width" "36px"
                    , Attr.style "height" "36px"
                    , Attr.style "padding" "0"
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "border-radius" "6px"
                    , Attr.style "background"
                        (if model.exportMenuOpen then
                            Theme.brandYellow

                         else
                            Theme.panelSubtleBackground
                        )
                    , Attr.style "color" Theme.textPrimary
                    , Attr.style "cursor" "pointer"
                    ]
                    [ featherIcon FeatherIcons.moreVertical ]
                ]

          else
            text ""
        ]


{-| The header's leading action, in the dark brand colour.
-}
simulateButton : Bool -> Html Msg
simulateButton narrow =
    button
        [ Attr.type_ "button"
        , Attr.class "ui-button"
        , Html.Events.onClick SimulateRequested
        , Attr.title "Simulate gears (P)"
        , Attr.attribute "aria-keyshortcuts" "P"
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        , Attr.style "height" "36px"
        , Attr.style "min-width" "36px"
        , Attr.style "padding"
            (if narrow then
                "0"

             else
                "0 14px"
            )
        , Attr.style "border" ("1px solid " ++ Theme.brand)
        , Attr.style "border-radius" "6px"
        , Attr.style "background" Theme.brand
        , Attr.style "color" Theme.textOnDark
        , Attr.style "font" "inherit"
        , Attr.style "font-size" "13px"
        , Attr.style "font-weight" "600"
        , Attr.style "cursor" "pointer"
        ]
        (if narrow then
            [ featherIcon FeatherIcons.play, span [ Attr.class "sr-only" ] [ text "Simulate" ] ]

         else
            [ featherIcon FeatherIcons.play, text "Simulate" ]
        )


{-| A small 2×2 brick in the brand yellow, the studio's logo.
-}
brandMark : Html Msg
brandMark =
    let
        stud =
            span
                [ Attr.style "width" "7px"
                , Attr.style "height" "7px"
                , Attr.style "border-radius" "50%"
                , Attr.style "background" "color-mix(in srgb, var(--color-brand-yellow) 70%, white)"
                , Attr.style "box-shadow" "0 1px 0 color-mix(in srgb, var(--color-brand) 30%, transparent)"
                ]
                []
    in
    span
        [ Attr.attribute "aria-hidden" "true"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "7px 7px"
        , Attr.style "gap" "4px"
        , Attr.style "padding" "5px"
        , Attr.style "border-radius" "6px"
        , Attr.style "background" Theme.brandYellow
        , Attr.style "box-shadow" "inset 0 -2px 0 color-mix(in srgb, var(--color-brand) 18%, transparent)"
        ]
        [ stud, stud, stud, stud ]


viewExportMenu : Model -> Html Msg
viewExportMenu model =
    let
        narrow =
            model.layout.mode == Layout.Narrow

        sectionTitle title =
            div
                [ Attr.attribute "aria-hidden" "true"
                , Attr.style "padding" "8px 12px 2px"
                , Attr.style "font-size" "11px"
                , Attr.style "font-weight" "600"
                , Attr.style "text-transform" "uppercase"
                , Attr.style "letter-spacing" "0.06em"
                , Attr.style "color" Theme.textMuted
                ]
                [ text title ]

        -- On a phone the header has room for Simulate only; the rest of its
        -- actions live at the top of this menu.
        documentItems =
            if narrow then
                [ sectionTitle "Document"
                , item NewDocument "New" "Start an empty model"
                , item RequestFileUpload "Open…" "An .ldr, .mpd or .io file"
                , item Save "Save" "Download the model as .ldr"
                , item ToggleBOM ("Parts list (" ++ String.fromInt (List.length (Document.placements model.document)) ++ ")") "Every part, with BrickLink export"
                , item InstructionsRequested "Instructions" "Step-by-step building pages"
                , item ToggleRenderQuality
                    (if model.renderQuality == Style.Realistic then
                        "Draft rendering"

                     else
                        "Realistic rendering"
                    )
                    "Bevels and stud logos, slower"
                , sectionTitle "Export"
                ]

            else
                []

        item msg label description =
            button
                [ Attr.type_ "button"
                , Attr.class "ui-button"
                , Html.Events.onClick (MenuPick msg)
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "flex-start"
                , Attr.style "gap" "2px"
                , Attr.style "width" "100%"
                , Attr.style "padding" "8px 12px"
                , Attr.style "border" "1px solid transparent"
                , Attr.style "border-radius" "6px"
                , Attr.style "background" "transparent"
                , Attr.style "color" Theme.textPrimary
                , Attr.style "font" "inherit"
                , Attr.style "text-align" "left"
                , Attr.style "cursor" "pointer"
                ]
                [ span [ Attr.style "font-size" "13px", Attr.style "font-weight" "600" ] [ text label ]
                , span [ Attr.style "font-size" "12px", Attr.style "color" Theme.textMuted ] [ text description ]
                ]
    in
    div
        [ Attr.id "export-menu"
        , Attr.attribute "role" "group"
        , Attr.attribute "aria-label"
            (if narrow then
                "More actions"

             else
                "Export formats"
            )
        , Attr.class "scroll-thin"
        , Attr.style "max-height" "calc(100% - 80px)"
        , Attr.style "overflow-y" "auto"
        , Attr.style "position" "absolute"
        , Attr.style "top" (String.fromInt (Layout.headerHeight (modeFor narrow) + 4) ++ "px")
        , Attr.style "right"
            (if narrow then
                "8px"

             else
                String.fromInt (Layout.panelWidth + 12) ++ "px"
            )
        , Attr.style "width" "260px"
        , Attr.style "max-width" "calc(100% - 16px)"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "2px"
        , Attr.style "padding" "6px"
        , Attr.style "background" Theme.panelSurface
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "10px"
        , Attr.style "box-shadow" floatingShadow
        , Attr.style "z-index" "30"
        ]
        (documentItems
            ++ [ item (ExportFormat Export.GlTFBinary) "glTF (.glb)" "For three.js, Blender and most 3D tools"
               , item (ExportFormat Export.BlenderScript) "Blender script (.py)" "Rebuilds the model with bevelled parts"
               , item (ExportFormat Export.UsdzArchive) "AR model (.usdz)" "Quick Look on iPhone and iPad"
               , item CaptureSnapshot "Picture (.png)" "A 1080p render of the current view"
               ]
        )


modeFor : Bool -> Layout.Mode
modeFor narrow =
    if narrow then
        Layout.Narrow

    else
        Layout.Wide


{-| Tools and edit actions: an opaque card down the left edge of the canvas
(along the top on a phone), so nothing behind it bleeds through.
-}
viewToolRail : Model -> Html Msg
viewToolRail model =
    let
        hasSelection =
            not (Set.isEmpty model.selection)

        hasSelectedSubmodel =
            case Set.toList model.selection of
                [ id ] ->
                    Document.getPlacement id model.document
                        |> Maybe.map (\p -> Document.isSubmodelReference p model.document)
                        |> Maybe.withDefault False

                _ ->
                    False

        narrow =
            model.layout.mode == Layout.Narrow

        tool t iconName label key =
            Button.icon
                { icon = featherIcon iconName
                , label = label
                , shortcut = Just key
                , pressed = Just (model.tool == t)
                , enabled = True
                , showLabel = False
                }
                (UseTool t)

        act iconName label key enabled msg =
            Button.icon
                { icon = featherIcon iconName
                , label = label
                , shortcut = Just key
                , pressed = Nothing
                , enabled = enabled
                , showLabel = False
                }
                msg

        divider =
            div
                [ Attr.attribute "aria-hidden" "true"
                , Attr.style "flex" "none"
                , Attr.style "background" Theme.borderDefault
                , Attr.style "margin"
                    (if narrow then
                        "4px 2px"

                     else
                        "2px 4px"
                    )
                , Attr.style
                    (if narrow then
                        "width"

                     else
                        "height"
                    )
                    "1px"
                ]
                []
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "flex-start"
        , Attr.style "gap" "8px"
        , Attr.style "pointer-events" "none"
        , Attr.style "max-width" "calc(100% - 24px)"
        , Attr.style "flex-direction"
            (if narrow then
                "column"

             else
                "row"
            )
        ]
        [ div
            [ Attr.attribute "role" "group"
            , Attr.attribute "aria-label" "Edit tools"
            , Attr.class "scroll-none"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction"
                (if narrow then
                    "row"

                 else
                    "column"
                )
            , Attr.style "gap" "4px"
            , Attr.style "padding" "6px"
            , Attr.style "max-width" "100%"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "overflow-x" "auto"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "10px"
            , Attr.style "box-shadow" floatingShadow
            , Attr.style "pointer-events" "auto"
            , Attr.style "touch-action" "none"
            ]
            [ tool SelectTool FeatherIcons.mousePointer "Select" "V"
            , tool MoveTool FeatherIcons.move "Move" "M"
            , tool RotateTool FeatherIcons.rotateCw "Rotate" "R"
            , divider
            , act FeatherIcons.cornerUpLeft "Undo" "Ctrl Z" (Edit.canUndo model.history) Undo
            , act FeatherIcons.cornerUpRight "Redo" "Ctrl Shift Z" (Edit.canRedo model.history) Redo
            , divider
            , act FeatherIcons.copy "Duplicate" "Ctrl D" hasSelection DuplicateSelection
            , act FeatherIcons.trash2 "Delete" "Delete" hasSelection DeleteSelection
            , divider
            , act FeatherIcons.package "Group into submodel" "Ctrl G" hasSelection GroupSelection
            , act FeatherIcons.box "Explode submodel" "Ctrl Shift G" hasSelectedSubmodel ExplodeSelection
            , act FeatherIcons.logIn "Enter submodel" "Enter" hasSelectedSubmodel EnterSubmodel
            ]
        , viewTurnPanel model hasSelection
        ]


{-| With the rotate tool: a turn either way about each axis, by the turn step.
-}
viewTurnPanel : Model -> Bool -> Html Msg
viewTurnPanel model hasSelection =
    if model.tool /= RotateTool then
        text ""

    else
        let
            turn ( name, axis ) =
                div [ Attr.style "display" "flex", Attr.style "gap" "4px", Attr.style "align-items" "center" ]
                    [ span [ Attr.style "width" "14px", Attr.style "font-weight" "700", Attr.style "font-size" "12px", Attr.style "color" (axisColour axis), Attr.attribute "aria-hidden" "true" ] [ text name ]
                    , Button.icon
                        { icon = featherIcon FeatherIcons.rotateCcw
                        , label = "Turn " ++ String.fromInt model.rotateStep ++ "° back about " ++ name
                        , shortcut = Just ("Shift " ++ name)
                        , pressed = Nothing
                        , enabled = hasSelection
                        , showLabel = False
                        }
                        (RotateSelection axis -1)
                    , Button.icon
                        { icon = featherIcon FeatherIcons.rotateCw
                        , label = "Turn " ++ String.fromInt model.rotateStep ++ "° about " ++ name
                        , shortcut = Just name
                        , pressed = Nothing
                        , enabled = hasSelection
                        , showLabel = False
                        }
                        (RotateSelection axis 1)
                    ]
        in
        div
            [ Attr.attribute "role" "group"
            , Attr.attribute "aria-label" "Turn the selection"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "4px"
            , Attr.style "padding" "8px"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "10px"
            , Attr.style "box-shadow" floatingShadow
            , Attr.style "pointer-events" "auto"
            ]
            (div [ Attr.style "font-size" "11px", Attr.style "font-weight" "600", Attr.style "color" Theme.textMuted, Attr.style "text-transform" "uppercase", Attr.style "letter-spacing" "0.06em" ]
                [ text ("Turn " ++ String.fromInt model.rotateStep ++ "°") ]
                :: List.map turn [ ( "X", AxisX ), ( "Y", AxisY ), ( "Z", AxisZ ) ]
            )


{-| The axis colours the gizmo uses, for the turn panel's labels.
-}
axisColour : Axis -> String
axisColour axis =
    case axis of
        AxisX ->
            Theme.brandRed

        AxisY ->
            "color-mix(in srgb, var(--color-brand-yellow) 45%, var(--color-brand))"

        AxisZ ->
            "color-mix(in srgb, var(--color-brand) 40%, var(--color-bg-page))"


{-| Snapping and collision settings along the bottom of the canvas, each a
button that steps to the next value, and the live gear hint beside them.
-}
viewSnapBar : Model -> Html Msg
viewSnapBar model =
    let
        narrow =
            model.layout.mode == Layout.Narrow

        chip msg label value tip =
            button
                [ Attr.type_ "button"
                , Attr.class "ui-button"
                , Html.Events.onClick msg
                , Attr.title tip
                , Attr.attribute "aria-label" (label ++ ": " ++ value ++ ". " ++ tip)
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "baseline"
                , Attr.style "gap" "6px"
                , Attr.style "height" "30px"
                , Attr.style "padding" "0 10px"
                , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                , Attr.style "border-radius" "15px"
                , Attr.style "background" Theme.panelSurface
                , Attr.style "color" Theme.textPrimary
                , Attr.style "font" "inherit"
                , Attr.style "font-size" "12px"
                , Attr.style "line-height" "28px"
                , Attr.style "cursor" "pointer"
                , Attr.style "flex" "none"
                ]
                [ span [ Attr.style "color" Theme.textMuted ] [ text label ]
                , span [ Attr.style "font-weight" "600" ] [ text value ]
                ]

        hint =
            Hints.summary model.hints
    in
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Snapping"
        , Attr.class "scroll-none"
        , Attr.style "position" "absolute"
        , Attr.style "left" "12px"
        , Attr.style
            (if narrow then
                "top"

             else
                "bottom"
            )
            (if narrow then
                "76px"

             else
                "12px"
            )
        , Attr.style "right"
            (if narrow then
                "12px"

             else
                "190px"
            )
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "overflow-x" "auto"
        , Attr.style "pointer-events" "none"
        ]
        [ div [ Attr.style "display" "flex", Attr.style "gap" "6px", Attr.style "pointer-events" "auto", Attr.style "box-shadow" "none" ]
            [ chip NextGridStep "Move step" (String.fromInt model.gridStep ++ " LDU") "Change move step ( [ or ] )"
            , chip NextRotateStep "Turn step" (String.fromInt model.rotateStep ++ "°") "Change turn step ( , or . )"
            , chip NextSnapDistance "Snap" (snapValue model.snapDistance) "Change connection snapping distance"
            , chip NextCollisionMode "Collision" (collisionValue model.collisionMode) "Change how overlapping parts are handled"
            ]
        , if hint == "" || narrow then
            text ""

          else
            span
                [ Attr.style "flex" "none"
                , Attr.style "padding" "5px 10px"
                , Attr.style "border-radius" "15px"
                , Attr.style "background" "color-mix(in srgb, var(--color-bg-page) 85%, transparent)"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                ]
                [ text hint ]
        ]


snapValue : Float -> String
snapValue dist =
    if dist <= 0 then
        "off"

    else
        String.fromInt (round dist) ++ " LDU"


collisionValue : CollisionMode -> String
collisionValue mode =
    case mode of
        CollisionStrict ->
            "strict"

        CollisionSoft ->
            "soft"

        CollisionOff ->
            "off"


{-| The polite live region: the last thing an action did, for screen readers.
-}
viewAnnouncement : Model -> Html Msg
viewAnnouncement model =
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.class "sr-only"
        ]
        [ text model.announcement ]


{-| A Feather icon at the size the studio's buttons use.
-}
featherIcon : FeatherIcons.Icon -> Html msg
featherIcon icon =
    icon
        |> FeatherIcons.withSize 18
        |> FeatherIcons.withStrokeWidth 1.75
        |> FeatherIcons.toHtml [ Attr.attribute "aria-hidden" "true", Attr.attribute "focusable" "false" ]


floatingShadow : String
floatingShadow =
    "0 6px 20px color-mix(in srgb, var(--color-brand) 12%, transparent), 0 1px 2px color-mix(in srgb, var(--color-brand) 8%, transparent)"


boolAttr : Bool -> String
boolAttr value =
    if value then
        "true"

    else
        "false"


documentSummary : Model -> String
documentSummary model =
    let
        activePlacements =
            Document.placementsOf model.context.activeSubmodel model.document

        count =
            List.length activePlacements

        selected =
            Set.size model.selection

        prefix =
            if model.context.activeSubmodel /= "" && model.context.activeSubmodel /= (Document.mainSubmodel model.document).name then
                "[" ++ model.context.activeSubmodel ++ "] "

            else
                ""
    in
    prefix
        ++ String.fromInt count
        ++ (if count == 1 then
                " part"

            else
                " parts"
           )
        ++ (if selected > 0 then
                ", " ++ String.fromInt selected ++ " selected"

            else
                ""
           )
        ++ (case Hints.summary model.hints of
                "" ->
                    ""

                hint ->
                    "  ·  " ++ hint
           )


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
