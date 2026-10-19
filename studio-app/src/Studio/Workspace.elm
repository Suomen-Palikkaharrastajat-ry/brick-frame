module Studio.Workspace exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

{-| The studio's editing page: a document shown in a viewport.

The `Document` is the source of truth; the `Viewport` only displays it. Every
change goes through the document and is then pushed to the viewport with
`Viewport.applyDocument`, which updates the scene in place.

-}

import EmbeddedParts
import Html exposing (Html, div, span, text)
import Html.Attributes as Attr
import LDraw.Document as Document exposing (Document)
import Ports
import Render.Camera as Camera
import Render.Style as Style
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
    }


{-| The document, and the viewport showing it.
-}
type alias Model =
    { viewport : Viewport.Model
    , document : Document
    , helpOpen : Bool
    }


type Msg
    = ViewportMsg Viewport.Msg
    | FileTextReceived String
    | FileFailed String
    | RequestFileUpload
    | NewDocument
    | ToggleHelp
    | NoOp


{-| An empty document in a fresh viewport.
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( viewport, viewportCmd ) =
            Viewport.init
                { ldrawBase = flags.ldrawBase
                , ldrawFallbackBase = flags.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = flags.initialHash
                , supportsPointerEvents = flags.supportsPointerEvents
                , useWindowResize = flags.useWindowResize
                , renderStyle = renderStyle flags
                , partCache = EmbeddedParts.cache
                }

        ( loaded, loadCmd, events ) =
            Viewport.loadDocument Document.empty viewport

        model =
            { viewport = loaded, document = Document.empty, helpOpen = False }

        ( final, eventCmds ) =
            handleEvents events ( model, [] )
    in
    ( final
    , Cmd.batch
        (Cmd.map ViewportMsg viewportCmd
            :: Cmd.map ViewportMsg loadCmd
            :: Ports.setUrlHash (Hash.encodeCamera loaded.camera)
            :: eventCmds
        )
    )


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
    case msg of
        ViewportMsg viewportMsg ->
            let
                ( viewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( next, eventCmds ) =
                    handleEvents events ( { model | viewport = viewport }, [] )
            in
            ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )

        FileTextReceived text ->
            openDocument (Document.parse text) model

        FileFailed message ->
            update (ViewportMsg (Viewport.FileLoadError message)) model

        RequestFileUpload ->
            ( model, Ports.requestFileUpload () )

        NewDocument ->
            openDocument Document.empty model

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


{-| Replace the document wholesale and load it into the viewport.
-}
openDocument : Document -> Model -> ( Model, Cmd Msg )
openDocument document model =
    let
        ( viewport, viewportCmd, events ) =
            Viewport.loadDocument document model.viewport

        ( next, eventCmds ) =
            handleEvents events
                ( { model | viewport = viewport, document = Document.clearTouched document }, [] )
    in
    ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )


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

        Viewport.KeyAccepted keyEvent ->
            case Shortcuts.actionFor (shortcutContext model) keyEvent of
                Just action ->
                    let
                        ( next, cmd ) =
                            update (msgForAction action) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    ( model, cmds )

        Viewport.Clicked _ _ ->
            ( model, cmds )

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


shortcutContext : Model -> { helpVisible : Bool, walking : Bool }
shortcutContext model =
    { helpVisible = model.helpOpen
    , walking = model.viewport.camera.navigation == Camera.Walk
    }


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


{-| Map a shortcut to the message it stands for. Playback shortcuts have no
meaning while editing.
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
        , Ports.viewportResized (\size -> ViewportMsg (Viewport.WindowResize size.width size.height))
        , Ports.fileContentReceived FileTextReceived
        , Ports.fileLoadError FileFailed
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
                { extraEntities = \_ _ _ -> []
                , coarsenWhileMoving = True
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
         , viewPad model
         ]
            ++ (if model.helpOpen then
                    [ ShortcutHelp.view { rows = Shortcuts.helpRows, onClose = ToggleHelp } ]

                else
                    []
               )
        )


viewTopBar : Model -> Html Msg
viewTopBar model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "right" "12px"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        , Attr.style "color" Theme.textMuted
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        ]
        [ span [ Attr.style "color" Theme.textPrimary, Attr.style "font-weight" "600" ] [ text "Palikkakehys Studio" ]
        , span [] [ text (documentSummary model.document) ]
        , div [ Attr.style "flex" "1" ] []
        , Button.secondary NewDocument "New"
        , Button.secondary RequestFileUpload "Open file"
        ]


documentSummary : Document -> String
documentSummary document =
    let
        count =
            List.length (Document.mainPlacements document)
    in
    String.fromInt count
        ++ (if count == 1 then
                " part"

            else
                " parts"
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
