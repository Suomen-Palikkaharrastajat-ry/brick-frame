module Studio.Instructions exposing
    ( Config
    , InstructionsViewMode(..)
    , Model
    , Msg(..)
    , init
    , restoreCamera
    , subscriptions
    , update
    , view
    )

{-| Step-by-step building instructions viewer and interactive playback.

Allows stepping through an LDraw model step-by-step, with automated
camera framing per step, parts callout box overlays, step scrubbing/playback,
printable sheet view, and standalone HTML instructions export.

-}

import Browser.Dom
import Browser.Events
import Dict
import EmbeddedParts
import FeatherIcons
import Html exposing (Html, div, input, span, text)
import Html.Attributes as Attr
import Html.Events
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Steps as Steps exposing (CalloutItem, Step)
import Ports
import Render.Camera as Camera exposing (Camera)
import Render.Style as Style
import Studio.CameraControls as CameraControls
import Studio.Colours as Colours
import Task
import Time
import UI.Button as Button
import UI.CameraGlobe as CameraGlobe
import UI.ShortcutHelp as ShortcutHelp
import UI.Theme as Theme
import Viewport
import Viewport.Framing as Framing exposing (ViewPreset(..))
import Viewport.Hash as Hash
import Viewport.Shortcuts as Shortcuts


{-| Display mode for building instructions.
-}
type InstructionsViewMode
    = Mode3D
    | ModeSheet


{-| Configuration needed to initialize the instructions viewer.
-}
type alias Config =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , supportsPointerEvents : Bool
    , renderStyle : Style.Style
    , snapshot : Document
    , camera : Camera
    , loadedParts : PartCache
    , window : ( Int, Int )
    }


{-| State for the building instructions page.
-}
type alias Model =
    { viewport : Viewport.Model
    , document : Document
    , steps : List Step
    , currentStepIndex : Int
    , isPlaying : Bool
    , autoFrame : Bool
    , viewMode : InstructionsViewMode
    , helpOpen : Bool
    , renderQuality : Style.Quality
    , globeDrag : Maybe CameraGlobe.Drag
    , window : ( Int, Int )
    }


{-| Instructions page messages.
-}
type Msg
    = ViewportMsg Viewport.Msg
    | NextStep
    | PrevStep
    | GoToStep Int
    | SetStepSlider String
    | TogglePlay
    | PlaybackTick Time.Posix
    | ToggleAutoFrame
    | ToggleRaytrace
    | CaptureSnapshot
    | FocusActiveStep
    | ToggleViewMode
    | AutoStepBy Int
    | ExportHtml
    | ToggleHelp
    | GlobePressed ( Float, Float )
    | GlobeMoved ( Float, Float )
    | GlobeReleased
    | ZoomTo Float
    | ZoomDone
    | ResetView
    | WindowSized Int Int
    | Leave
    | NoOp


{-| Extract the current camera to restore it when leaving.
-}
restoreCamera : Model -> Camera
restoreCamera model =
    model.viewport.camera


{-| Initialize instructions from a document snapshot.
-}
init : Config -> ( Model, Cmd Msg )
init config =
    let
        ( width, height ) =
            config.window

        parsedSteps =
            Steps.fromDocument config.snapshot

        steps =
            if List.isEmpty parsedSteps then
                Steps.autoPartition 2 (Document.placements config.snapshot)

            else
                parsedSteps

        initialStepIndex =
            1

        initialDoc =
            if List.isEmpty steps then
                config.snapshot

            else
                Steps.documentAtStep config.snapshot initialStepIndex

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

        framedCamera =
            case Steps.stepAtIndex initialStepIndex steps of
                Just step ->
                    Framing.cameraForStep width height step config.camera

                Nothing ->
                    config.camera

        placedViewport =
            { viewport
                | camera = framedCamera
                , cameraMode = Viewport.CameraManual
            }

        model =
            { viewport = placedViewport
            , document = config.snapshot
            , steps = steps
            , currentStepIndex = initialStepIndex
            , isPlaying = False
            , autoFrame = True
            , viewMode = Mode3D
            , helpOpen = False
            , renderQuality = Style.Draft
            , globeDrag = Nothing
            , window = config.window
            }
    in
    update (WindowSized width height) model
        |> andThen (applyCurrentStepDoc initialDoc)


applyCurrentStepDoc : Document -> Model -> ( Model, Cmd Msg )
applyCurrentStepDoc doc model =
    let
        ( nextViewport, cmd, events ) =
            Viewport.applyDocument doc model.viewport

        ( nextModel, eventCmds ) =
            List.foldl handleEvent ( { model | viewport = nextViewport }, [] ) events
    in
    ( nextModel, Cmd.batch (Cmd.map ViewportMsg cmd :: eventCmds) )


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen next ( model, cmd ) =
    let
        ( final, nextCmd ) =
            next model
    in
    ( final, Cmd.batch [ cmd, nextCmd ] )


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


{-| Update loop for building instructions.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportMsg viewportMsg ->
            let
                ( viewport, viewportCmd, events ) =
                    Viewport.update viewportMsg model.viewport

                ( next, eventCmds ) =
                    List.foldl handleEvent ( { model | viewport = viewport }, [] ) events
            in
            ( next, Cmd.batch (Cmd.map ViewportMsg viewportCmd :: eventCmds) )

        NextStep ->
            let
                maxStep =
                    List.length model.steps
            in
            if model.currentStepIndex < maxStep then
                update (GoToStep (model.currentStepIndex + 1)) model

            else
                ( { model | isPlaying = False }, Cmd.none )

        PrevStep ->
            if model.currentStepIndex > 1 then
                update (GoToStep (model.currentStepIndex - 1)) model

            else
                ( model, Cmd.none )

        GoToStep rawTarget ->
            let
                total =
                    List.length model.steps

                targetIndex =
                    clamp 1 (max 1 total) rawTarget
            in
            if targetIndex == model.currentStepIndex && model.viewport.document /= Nothing then
                ( model, Cmd.none )

            else
                let
                    stepDoc =
                        Steps.documentAtStep model.document targetIndex

                    ( nextViewport, cmd, events ) =
                        Viewport.applyDocument stepDoc model.viewport

                    framedViewport =
                        if model.autoFrame then
                            case Steps.stepAtIndex targetIndex model.steps of
                                Just step ->
                                    let
                                        ( width, height ) =
                                            model.window
                                    in
                                    { nextViewport | camera = Framing.cameraForStep width height step nextViewport.camera }

                                Nothing ->
                                    nextViewport

                        else
                            nextViewport

                    updatedModel =
                        { model
                            | viewport = framedViewport
                            , currentStepIndex = targetIndex
                        }

                    ( finalModel, eventCmds ) =
                        List.foldl handleEvent ( updatedModel, [] ) events
                in
                ( finalModel, Cmd.batch (Cmd.map ViewportMsg cmd :: eventCmds) )

        SetStepSlider textValue ->
            case String.toInt textValue of
                Just stepNum ->
                    update (GoToStep stepNum) model

                Nothing ->
                    ( model, Cmd.none )

        TogglePlay ->
            let
                nextPlaying =
                    not model.isPlaying

                total =
                    List.length model.steps

                resetModel =
                    if nextPlaying && model.currentStepIndex >= total then
                        { model | currentStepIndex = 0 }

                    else
                        model
            in
            if nextPlaying && resetModel.currentStepIndex == 0 then
                update (GoToStep 1) { resetModel | isPlaying = True }

            else
                ( { resetModel | isPlaying = nextPlaying }, Cmd.none )

        PlaybackTick _ ->
            if model.isPlaying then
                update NextStep model

            else
                ( model, Cmd.none )

        ToggleAutoFrame ->
            ( { model | autoFrame = not model.autoFrame }, Cmd.none )

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
                , viewport = Viewport.setRenderQuality nextQuality model.viewport
              }
            , Cmd.none
            )

        CaptureSnapshot ->
            ( model, Cmd.none )

        FocusActiveStep ->
            case Steps.stepAtIndex model.currentStepIndex model.steps of
                Just step ->
                    let
                        ( width, height ) =
                            model.window

                        cam =
                            Framing.cameraForStep width height step model.viewport.camera

                        vp =
                            model.viewport
                    in
                    ( { model | viewport = { vp | camera = cam } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggleViewMode ->
            let
                nextMode =
                    case model.viewMode of
                        Mode3D ->
                            ModeSheet

                        ModeSheet ->
                            Mode3D
            in
            ( { model | viewMode = nextMode }, Cmd.none )

        AutoStepBy n ->
            let
                steppedDoc =
                    Steps.autoStepDocument n model.document

                newSteps =
                    Steps.fromDocument steppedDoc
            in
            update (GoToStep 1)
                { model
                    | document = steppedDoc
                    , steps = newSteps
                    , currentStepIndex = 0
                }

        ExportHtml ->
            let
                html =
                    Steps.exportHtml "Palikkakehys Building Instructions" model.steps
            in
            ( model
            , Ports.downloadFile
                { filename = "building-instructions.html"
                , mime = "text/html"
                , text = html
                }
            )

        ToggleHelp ->
            ( { model | helpOpen = not model.helpOpen }, Cmd.none )

        GlobePressed point ->
            ( { model | globeDrag = Just (CameraGlobe.startDrag model.viewport.camera point) }, Cmd.none )

        GlobeMoved point ->
            case model.globeDrag of
                Just drag ->
                    let
                        ( ( azimuthDelta, elevationDelta ), next ) =
                            CameraGlobe.moveDrag drag point
                    in
                    update (ViewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta))
                        { model | globeDrag = Just next }

                Nothing ->
                    ( model, Cmd.none )

        GlobeReleased ->
            let
                released =
                    { model | globeDrag = Nothing }
            in
            case model.globeDrag |> Maybe.andThen (CameraGlobe.releaseDrag model.viewport.camera) of
                Just ( azimuthDelta, elevationDelta ) ->
                    update (ViewportMsg (Viewport.RotateCameraBy azimuthDelta elevationDelta)) released

                Nothing ->
                    ( released, Cmd.none )

        ZoomTo level ->
            update (ViewportMsg (Viewport.Wheel (CameraGlobe.wheelDeltaForZoom model.viewport.camera level))) model

        ZoomDone ->
            ( model, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.zoomSliderId) )

        ResetView ->
            let
                ( fitted, fitCmd ) =
                    update (ViewportMsg Viewport.FitView) model
            in
            ( fitted, Cmd.batch [ fitCmd, Task.attempt (\_ -> NoOp) (Browser.Dom.blur CameraGlobe.resetButtonId) ] )

        WindowSized width height ->
            let
                ( vp, cmd, events ) =
                    Viewport.update (Viewport.WindowResize width height) model.viewport

                ( next, eventCmds ) =
                    List.foldl handleEvent ( { model | viewport = vp, window = ( width, height ) }, [] ) events
            in
            ( next, Cmd.batch (Cmd.map ViewportMsg cmd :: eventCmds) )

        Leave ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleEvent : Viewport.Event -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
handleEvent event ( model, cmds ) =
    case event of
        Viewport.CameraSettled ->
            ( model, cmds ++ [ Ports.setUrlHash (Hash.encodeCamera model.viewport.camera) ] )

        Viewport.FlattenRequested payload ->
            ( model, cmds ++ [ Ports.requestGeometryFlatten payload ] )

        Viewport.KeyAccepted keyEvent ->
            let
                walking =
                    model.viewport.camera.navigation == Camera.Walk
            in
            case Shortcuts.actionFor { helpVisible = model.helpOpen, walking = walking } keyEvent of
                Just action ->
                    let
                        ( next, cmd ) =
                            update (msgForAction action) model
                    in
                    ( next, cmds ++ [ cmd ] )

                Nothing ->
                    if Shortcuts.isTextEntryTarget keyEvent || keyEvent.ctrl || keyEvent.meta || keyEvent.alt then
                        ( model, cmds )

                    else
                        case keyEvent.key of
                            "ArrowRight" ->
                                if walking then
                                    ( model, cmds )

                                else
                                    let
                                        ( next, cmd ) =
                                            update NextStep model
                                    in
                                    ( next, cmds ++ [ cmd ] )

                            "ArrowLeft" ->
                                if walking then
                                    ( model, cmds )

                                else
                                    let
                                        ( next, cmd ) =
                                            update PrevStep model
                                    in
                                    ( next, cmds ++ [ cmd ] )

                            "Escape" ->
                                -- `Main` switches pages, and only sees messages
                                -- from outside: re-send `Leave` as a command.
                                ( model, cmds ++ [ Task.perform identity (Task.succeed Leave) ] )

                            _ ->
                                ( model, cmds )

        _ ->
            ( model, cmds )


{-| Wheel delta one `+`/`-` press is worth — two notches of a mouse wheel.
-}
keyboardZoomStep : Float
keyboardZoomStep =
    200.0


msgForAction : Shortcuts.Action -> Msg
msgForAction action =
    case action of
        Shortcuts.TogglePlayback ->
            TogglePlay

        Shortcuts.FitView ->
            FocusActiveStep

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

        Shortcuts.PresetBottom ->
            ViewportMsg (Viewport.SetViewPreset BottomView)

        Shortcuts.PresetIsometric ->
            ViewportMsg (Viewport.SetViewPreset IsometricView)



-- ── Subscriptions ─────────────────────────────────────────────────────────────


{-| Active subscriptions: viewport events, window resizing, and playback ticker.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        baseSubs =
            [ Sub.map ViewportMsg (Viewport.subscriptions model.viewport)
            , -- Window size, not the runtime's port: Elm replaces the mount node, so the
              -- runtime's ResizeObserver watches a detached element and never fires.
              Browser.Events.onResize WindowSized
            , Ports.geometryFlattened (ViewportMsg << Viewport.GeometryFlattened)
            , Ports.geometryFlattenFailed (ViewportMsg << Viewport.GeometryFlattenFailed)
            ]

        ticker =
            if model.isPlaying then
                [ Time.every 2200 PlaybackTick ]

            else
                []
    in
    Sub.batch (baseSubs ++ ticker)



-- ── View ──────────────────────────────────────────────────────────────────────


{-| Render the instructions viewer.
-}
view : Model -> Html Msg
view model =
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" Theme.appBackground
        ]
        [ case model.viewMode of
            Mode3D ->
                Html.map ViewportMsg
                    (Viewport.view
                        { extraEntities = \_ _ _ -> []
                        , coarsenWhileMoving = False
                        }
                        model.viewport
                    )

            ModeSheet ->
                viewSheet model
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
        ([ viewTopBar model ]
            ++ (if model.viewMode == Mode3D then
                    let
                        currentStep =
                            Steps.stepAtIndex model.currentStepIndex model.steps
                    in
                    [ case currentStep of
                        Just st ->
                            viewCalloutBox st

                        Nothing ->
                            text ""
                    , viewNavigation model
                    , viewCamera model
                    ]

                else
                    []
               )
            ++ (if model.helpOpen then
                    [ ShortcutHelp.view
                        { rows =
                            [ ( "Navigation"
                              , [ ( "← / →", "Previous / Next step" )
                                , ( "Space", "Play / Pause" )
                                , ( "F", "Focus camera on current step" )
                                , ( "Esc", "Return to editing" )
                                ]
                              )
                            ]
                                ++ Shortcuts.helpRows
                        , onClose = ToggleHelp
                        }
                    ]

                else
                    []
               )
        )


viewTopBar : Model -> Html Msg
viewTopBar model =
    let
        total =
            List.length model.steps

        partCount =
            List.length (Document.placements model.document)

        partitionButtons =
            if partCount > 0 then
                [ div
                    [ Attr.style "display" "flex"
                    , Attr.style "gap" "4px"
                    , Attr.attribute "role" "group"
                    , Attr.attribute "aria-label" "Split into steps automatically"
                    ]
                    [ Button.secondary (AutoStepBy 1) "1 part per step"
                    , Button.secondary (AutoStepBy 2) "2 parts per step"
                    ]
                ]

            else
                []
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "12px"
        , Attr.style "left" "12px"
        , Attr.style "right" "12px"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "justify-content" "space-between"
        , Attr.style "align-items" "flex-start"
        , Attr.style "gap" "8px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "none"
        ]
        [ Html.header
            (cardStyle
                ++ [ Attr.style "gap" "12px"
                   , Attr.style "padding" "6px 14px 6px 6px"
                   , Attr.style "max-width" "100%"
                   ]
            )
            [ Button.icon
                { icon = icon FeatherIcons.arrowLeft
                , label = "Back to editing"
                , shortcut = Just "Esc"
                , pressed = Nothing
                , enabled = True
                , showLabel = True
                }
                Leave
            , Html.h1
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "15px"
                , Attr.style "font-weight" "700"
                , Attr.style "color" Theme.textPrimary
                , Attr.style "white-space" "nowrap"
                ]
                [ text "Building instructions" ]
            , span
                [ Attr.attribute "role" "status"
                , Attr.style "overflow" "hidden"
                , Attr.style "text-overflow" "ellipsis"
                , Attr.style "white-space" "nowrap"
                ]
                [ text (summary total partCount) ]
            ]
        , div
            (cardStyle
                ++ [ Attr.style "flex-wrap" "wrap"
                   , Attr.style "gap" "6px"
                   , Attr.style "padding" "6px"
                   , Attr.attribute "role" "group"
                   , Attr.attribute "aria-label" "Instruction options"
                   ]
            )
            (partitionButtons
                ++ [ Button.icon
                        { icon = icon FeatherIcons.fileText
                        , label = "Sheet view"
                        , shortcut = Nothing
                        , pressed = Just (model.viewMode == ModeSheet)
                        , enabled = True
                        , showLabel = True
                        }
                        ToggleViewMode
                   , Button.icon
                        { icon = icon FeatherIcons.download
                        , label = "Export HTML"
                        , shortcut = Nothing
                        , pressed = Nothing
                        , enabled = True
                        , showLabel = True
                        }
                        ExportHtml
                   , Button.icon
                        { icon = icon FeatherIcons.helpCircle
                        , label = "Keyboard shortcuts"
                        , shortcut = Just "?"
                        , pressed = Nothing
                        , enabled = True
                        , showLabel = False
                        }
                        ToggleHelp
                   ]
            )
        ]


{-| The card look shared by the floating panels: opaque, bordered, brand-tinted shadow.
Callers add their own padding and gap.
-}
cardStyle : List (Html.Attribute msg)
cardStyle =
    [ Attr.style "display" "flex"
    , Attr.style "align-items" "center"
    , Attr.style "box-sizing" "border-box"
    , Attr.style "background" Theme.panelSurface
    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
    , Attr.style "border-radius" "10px"
    , Attr.style "box-shadow" floatingShadow
    , Attr.style "color" Theme.textMuted
    , Attr.style "font-size" "13px"
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


{-| A count with its noun, singular for one: "1 part", "34 parts".
-}
countOf : Int -> String -> String -> String
countOf n singular plural =
    String.fromInt n
        ++ " "
        ++ (if n == 1 then
                singular

            else
                plural
           )


summary : Int -> Int -> String
summary total partCount =
    countOf total "step" "steps" ++ " · " ++ countOf partCount "part" "parts"


{-| The file name as it reads on screen: "2431.dat" becomes "2431".
-}
partName : String -> String
partName file =
    if String.endsWith ".dat" file then
        String.dropRight 4 file

    else
        file


viewCalloutBox : Step -> Html Msg
viewCalloutBox step =
    div
        (cardStyle
            ++ [ Attr.style "position" "absolute"
               , Attr.style "top" "72px"
               , Attr.style "left" "12px"
               , Attr.style "width" "280px"
               , Attr.style "max-width" "calc(100% - 24px)"
               , Attr.style "padding" "6px"
               , Attr.style "flex-direction" "column"
               , Attr.style "align-items" "stretch"
               , Attr.style "gap" "6px"
               , Attr.style "pointer-events" "auto"
               ]
        )
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "8px"
            , Attr.style "padding" "4px 6px 2px"
            ]
            [ Html.h2
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text ("Step " ++ String.fromInt step.index) ]
            , span
                [ Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                , Attr.style "font-variant-numeric" "tabular-nums"
                ]
                [ text (countOf (List.length step.placements) "part" "parts") ]
            ]
        , if List.isEmpty step.callout then
            div
                [ Attr.style "padding" "4px 6px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                ]
                [ text "No new parts in this step" ]

          else
            Html.ul
                [ Attr.style "list-style" "none"
                , Attr.style "margin" "0"
                , Attr.style "padding" "0"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "6px"
                , Attr.style "max-height" "220px"
                , Attr.style "overflow-y" "auto"
                ]
                (List.map viewCalloutItem step.callout)
        ]


viewCalloutItem : CalloutItem -> Html Msg
viewCalloutItem item =
    Html.li
        [ Attr.attribute "aria-label"
            (String.fromInt item.count ++ " × " ++ partName item.part ++ ", " ++ Steps.colorName item.color)
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "margin" "0"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "padding" "4px 8px"
        , Attr.style "border-radius" "6px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        ]
        [ div
            [ Attr.attribute "aria-hidden" "true"
            , Attr.style "width" "14px"
            , Attr.style "height" "14px"
            , Attr.style "border-radius" "3px"
            , Attr.style "border" "1px solid rgba(0, 0, 0, 0.25)"
            , Attr.style "background" (Colours.css (Colors.resolveColor 16 item.color))
            , Attr.style "flex-shrink" "0"
            ]
            []
        , span
            [ Attr.attribute "aria-hidden" "true"
            , Attr.style "font-weight" "700"
            , Attr.style "color" Theme.brand
            , Attr.style "font-variant-numeric" "tabular-nums"
            ]
            [ text (String.fromInt item.count ++ "×") ]
        , span
            [ Attr.attribute "aria-hidden" "true"
            , Attr.style "font-weight" "600"
            , Attr.style "flex" "1"
            , Attr.style "overflow" "hidden"
            , Attr.style "text-overflow" "ellipsis"
            ]
            [ text (partName item.part) ]
        , span
            [ Attr.attribute "aria-hidden" "true"
            , Attr.style "color" Theme.textMuted
            , Attr.style "font-size" "11px"
            ]
            [ text (Steps.colorName item.color) ]
        ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    let
        total =
            List.length model.steps

        hasPrev =
            model.currentStepIndex > 1

        hasNext =
            model.currentStepIndex < total
    in
    div
        (cardStyle
            ++ [ Attr.style "position" "absolute"
               , Attr.style "bottom" "16px"
               , Attr.style "left" "50%"
               , Attr.style "transform" "translateX(-50%)"
               , Attr.style "max-width" "calc(100% - 24px)"
               , Attr.style "flex-wrap" "wrap"
               , Attr.style "justify-content" "center"
               , Attr.style "gap" "6px"
               , Attr.style "padding" "6px"
               , Attr.style "pointer-events" "auto"
               , Attr.attribute "role" "group"
               , Attr.attribute "aria-label" "Steps"
               ]
        )
        [ Button.icon
            { icon = icon FeatherIcons.chevronLeft
            , label = "Previous step"
            , shortcut = Just "ArrowLeft"
            , pressed = Nothing
            , enabled = hasPrev
            , showLabel = False
            }
            PrevStep
        , Button.icon
            { icon =
                icon
                    (if model.isPlaying then
                        FeatherIcons.pause

                     else
                        FeatherIcons.play
                    )
            , label =
                if model.isPlaying then
                    "Pause"

                else
                    "Play"
            , shortcut = Just "Space"
            , pressed = Nothing
            , enabled = total > 1
            , showLabel = False
            }
            TogglePlay
        , Button.icon
            { icon = icon FeatherIcons.chevronRight
            , label = "Next step"
            , shortcut = Just "ArrowRight"
            , pressed = Nothing
            , enabled = hasNext
            , showLabel = False
            }
            NextStep
        , span
            [ Attr.attribute "role" "status"
            , Attr.attribute "aria-live" "polite"
            , Attr.attribute "aria-label" ("Step " ++ String.fromInt model.currentStepIndex ++ " of " ++ String.fromInt total)
            , Attr.style "font-weight" "600"
            , Attr.style "color" Theme.textPrimary
            , Attr.style "min-width" "56px"
            , Attr.style "text-align" "center"
            , Attr.style "font-variant-numeric" "tabular-nums"
            ]
            [ text (String.fromInt model.currentStepIndex ++ " / " ++ String.fromInt total) ]
        , input
            [ Attr.type_ "range"
            , Attr.attribute "aria-label" "Step"
            , Attr.attribute "aria-valuetext" ("Step " ++ String.fromInt model.currentStepIndex ++ " of " ++ String.fromInt total)
            , Attr.min "1"
            , Attr.max (String.fromInt (max 1 total))
            , Attr.value (String.fromInt model.currentStepIndex)
            , Html.Events.onInput SetStepSlider
            , Attr.style "cursor" "pointer"
            , Attr.style "width" "120px"
            ]
            []
        , Button.icon
            { icon = icon FeatherIcons.crosshair
            , label = "Focus"
            , shortcut = Just "F"
            , pressed = Nothing
            , enabled = True
            , showLabel = True
            }
            FocusActiveStep
        , Button.toggle { active = model.autoFrame, enabled = True } ToggleAutoFrame "Auto-frame"
        ]


viewCamera : Model -> Html Msg
viewCamera model =
    CameraControls.floating cameraActions
        { camera = model.viewport.camera
        , helpOpen = model.helpOpen
        , renderQuality = model.renderQuality
        }


cameraActions : CameraControls.Actions Msg
cameraActions =
    { onPress = GlobePressed
    , onMove = GlobeMoved
    , onRelease = GlobeReleased
    , onZoom = ZoomTo
    , onZoomDone = ZoomDone
    , onReset = ResetView
    , onWalk = ViewportMsg Viewport.ToggleWalkMode
    , onHelp = ToggleHelp
    , onRaytrace = ToggleRaytrace
    , onSnapshot = CaptureSnapshot
    , noOp = NoOp
    }


viewSheet : Model -> Html Msg
viewSheet model =
    let
        total =
            List.length model.steps
    in
    div
        [ Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "64px 20px 40px"
        , Attr.style "box-sizing" "border-box"
        ]
        [ Html.main_
            [ Attr.style "max-width" "760px"
            , Attr.style "margin" "0 auto"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "20px"
            ]
            (List.map (viewSheetStep total) model.steps)
        ]


viewSheetStep : Int -> Step -> Html Msg
viewSheetStep total step =
    Html.section
        (cardStyle
            ++ [ Attr.style "flex-direction" "column"
               , Attr.style "align-items" "stretch"
               , Attr.style "gap" "12px"
               , Attr.style "padding" "16px"
               ]
        )
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "8px"
            , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "padding-bottom" "10px"
            ]
            [ Html.h2
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "14px"
                , Attr.style "font-weight" "700"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text ("Step " ++ String.fromInt step.index ++ " of " ++ String.fromInt total) ]
            , span
                [ Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                , Attr.style "font-variant-numeric" "tabular-nums"
                ]
                [ text
                    (countOf (List.length step.placements) "new part" "new parts"
                        ++ " · "
                        ++ String.fromInt (List.length step.accumulatedPlacements)
                        ++ " in total"
                    )
                ]
            ]
        , if List.isEmpty step.callout then
            div
                [ Attr.style "color" Theme.textMuted
                , Attr.style "font-size" "12px"
                , Attr.style "font-style" "italic"
                ]
                [ text "No new parts in this step" ]

          else
            Html.ul
                [ Attr.style "list-style" "none"
                , Attr.style "margin" "0"
                , Attr.style "padding" "0"
                , Attr.style "display" "flex"
                , Attr.style "flex-wrap" "wrap"
                , Attr.style "gap" "8px"
                ]
                (List.map viewCalloutItem step.callout)
        ]
