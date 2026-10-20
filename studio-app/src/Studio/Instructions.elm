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

import Dict
import EmbeddedParts
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
import Studio.Colours as Colours
import Time
import UI.Button as Button
import UI.ShortcutHelp as ShortcutHelp
import UI.Theme as Theme
import UI.ViewPad as ViewPad
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
    | FocusActiveStep
    | ToggleViewMode
    | AutoStepBy Int
    | ExportHtml
    | ToggleHelp
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
                newSteps =
                    Steps.autoPartition n (Document.placements model.document)
            in
            update (GoToStep 1) { model | steps = newSteps }

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
            case keyEvent.key of
                "ArrowRight" ->
                    let
                        ( next, cmd ) =
                            update NextStep model
                    in
                    ( next, cmds ++ [ cmd ] )

                "ArrowLeft" ->
                    let
                        ( next, cmd ) =
                            update PrevStep model
                    in
                    ( next, cmds ++ [ cmd ] )

                " " ->
                    let
                        ( next, cmd ) =
                            update TogglePlay model
                    in
                    ( next, cmds ++ [ cmd ] )

                "f" ->
                    let
                        ( next, cmd ) =
                            update FocusActiveStep model
                    in
                    ( next, cmds ++ [ cmd ] )

                "Escape" ->
                    let
                        ( next, cmd ) =
                            update Leave model
                    in
                    ( next, cmds ++ [ cmd ] )

                _ ->
                    ( model, cmds )

        _ ->
            ( model, cmds )



-- ── Subscriptions ─────────────────────────────────────────────────────────────


{-| Active subscriptions: viewport events, window resizing, and playback ticker.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        baseSubs =
            [ Sub.map ViewportMsg (Viewport.subscriptions model.viewport)
            , Ports.viewportResized (\size -> WindowSized size.width size.height)
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
                    , viewPad model
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
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "8px"
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
        , Attr.style "white-space" "nowrap"
        ]
        [ Button.secondary Leave "← Back to Studio"
        , span [ Attr.style "color" Theme.textPrimary, Attr.style "font-weight" "600" ]
            [ text "Building Instructions" ]
        , span [ Attr.style "flex" "1", Attr.style "overflow" "hidden", Attr.style "text-overflow" "ellipsis" ]
            [ text
                (String.fromInt total
                    ++ " steps • "
                    ++ String.fromInt (List.length (Document.placements model.document))
                    ++ " parts"
                )
            ]
        , if not (Steps.hasStepMarkers model.document) then
            div [ Attr.style "display" "flex", Attr.style "gap" "4px" ]
                [ Button.secondary (AutoStepBy 1) "1 pc/step"
                , Button.secondary (AutoStepBy 2) "2 pcs/step"
                ]

          else
            text ""
        , Button.toggle
            { active = model.viewMode == ModeSheet, enabled = True }
            ToggleViewMode
            (if model.viewMode == ModeSheet then
                "3D View"

             else
                "📄 Sheet"
            )
        , Button.secondary ExportHtml "💾 Export HTML"
        , Button.secondary ToggleHelp "?"
        ]


viewCalloutBox : Step -> Html Msg
viewCalloutBox step =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "48px"
        , Attr.style "left" "12px"
        , Attr.style "width" "280px"
        , Attr.style "max-width" "calc(100vw - 24px)"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "padding" "12px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "color" Theme.textPrimary
        , Attr.style "box-shadow" "0 4px 16px rgba(0, 0, 0, 0.12)"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "margin-bottom" "8px"
            , Attr.style "font-weight" "600"
            ]
            [ span [] [ text ("Step " ++ String.fromInt step.index ++ " Parts") ]
            , span [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px" ]
                [ text (String.fromInt (List.length step.placements) ++ " pcs") ]
            ]
        , if List.isEmpty step.callout then
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-style" "italic" ]
                [ text "No new parts in this step" ]

          else
            div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "6px"
                , Attr.style "max-height" "220px"
                , Attr.style "overflow-y" "auto"
                ]
                (List.map viewCalloutItem step.callout)
        ]


viewCalloutItem : CalloutItem -> Html Msg
viewCalloutItem item =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "padding" "4px 8px"
        , Attr.style "border-radius" "4px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        ]
        [ div
            [ Attr.style "width" "14px"
            , Attr.style "height" "14px"
            , Attr.style "border-radius" "2px"
            , Attr.style "border" "1px solid rgba(0, 0, 0, 0.25)"
            , Attr.style "background" (Colours.css (Colors.resolveColor 16 item.color))
            , Attr.style "flex-shrink" "0"
            ]
            []
        , span [ Attr.style "font-weight" "700", Attr.style "color" Theme.brand ]
            [ text (String.fromInt item.count ++ "×") ]
        , span [ Attr.style "font-weight" "600", Attr.style "flex" "1", Attr.style "overflow" "hidden", Attr.style "text-overflow" "ellipsis" ]
            [ text item.part ]
        , span [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "11px" ]
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
        [ Attr.style "position" "absolute"
        , Attr.style "bottom" "16px"
        , Attr.style "left" "50%"
        , Attr.style "transform" "translateX(-50%)"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "background" Theme.panelBackground
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "padding" "8px 14px"
        , Attr.style "pointer-events" "auto"
        , Attr.style "box-shadow" "0 8px 24px rgba(0, 0, 0, 0.15)"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "white-space" "nowrap"
        ]
        [ Button.toggle { active = False, enabled = hasPrev } PrevStep "◀ Prev"
        , Button.toggle { active = model.isPlaying, enabled = total > 1 }
            TogglePlay
            (if model.isPlaying then
                "⏸ Pause"

             else
                "▶ Play"
            )
        , Button.toggle { active = False, enabled = hasNext } NextStep "Next ▶"
        , span [ Attr.style "font-weight" "600", Attr.style "min-width" "80px", Attr.style "text-align" "center" ]
            [ text (String.fromInt model.currentStepIndex ++ " / " ++ String.fromInt total) ]
        , input
            [ Attr.type_ "range"
            , Attr.min "1"
            , Attr.max (String.fromInt (max 1 total))
            , Attr.value (String.fromInt model.currentStepIndex)
            , Html.Events.onInput SetStepSlider
            , Attr.style "cursor" "pointer"
            , Attr.style "width" "100px"
            ]
            []
        , Button.secondary FocusActiveStep "🎯 Focus"
        , Button.toggle { active = model.autoFrame, enabled = True } ToggleAutoFrame "Auto-frame"
        ]


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
        [ div
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
    div
        [ Attr.style "background" Theme.panelBackground
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "8px"
        , Attr.style "padding" "16px"
        , Attr.style "font-family" "monospace"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "border-bottom" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "padding-bottom" "10px"
            , Attr.style "margin-bottom" "12px"
            ]
            [ span [ Attr.style "font-size" "16px", Attr.style "font-weight" "700" ]
                [ text ("Step " ++ String.fromInt step.index ++ " of " ++ String.fromInt total) ]
            , span [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ]
                [ text
                    (String.fromInt (List.length step.placements)
                        ++ " new parts ("
                        ++ String.fromInt (List.length step.accumulatedPlacements)
                        ++ " total)"
                    )
                ]
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-wrap" "wrap"
            , Attr.style "gap" "8px"
            ]
            (if List.isEmpty step.callout then
                [ div [ Attr.style "color" Theme.textMuted, Attr.style "font-style" "italic" ]
                    [ text "No new parts in this step" ]
                ]

             else
                List.map viewCalloutItem step.callout
            )
        ]
