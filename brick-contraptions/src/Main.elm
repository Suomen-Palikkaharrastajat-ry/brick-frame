module Main exposing
    ( Flags
    , Mode(..)
    , Model
    , Msg(..)
    , init
    , main
    , subscriptions
    , update
    , view
    )

{-| Main application for Brick Contraptions.

Provides a Technic building playground where gears, beams, axles, and pins
can be assembled with stud/axle snapping and simulated with real-time gear physics.
Accomplishment is celebrated when all placed gears are engaged in the gear graph
and rotating as a single machine.

-}

import Browser
import Browser.Dom
import Browser.Events
import Contraption.Catalog as Catalog exposing (Category(..), PartItem)
import Contraption.Machine as Machine exposing (MachineState)
import Html exposing (Html, button, div, header, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import LDraw.Document as Document
import Math.Vector3 exposing (vec3)
import Ports
import Studio.Core as Core
import Studio.UrlPayload as UrlPayload
import Task


{-| Application flags received from JavaScript runtime.
-}
type alias Flags =
    { ldrawBase : String
    , ldrawFallbackBase : String
    , initialHash : String
    , supportsPointerEvents : Bool
    }


{-| Playground mode: interactive editing or live simulation.
-}
type Mode
    = EditMode
    | SimulateMode


{-| Main Contraptions model.
-}
type alias Model =
    { studio : Core.Model
    , machine : MachineState
    , mode : Mode
    , activeCategory : Category
    , projectParts : List PartItem
    , currentTime : Float
    , motorRpm : Float
    , windowWidth : Int
    , windowHeight : Int
    }


{-| Messages handled by the Contraptions application.
-}
type Msg
    = StudioMsg Core.Msg
    | ToggleMode
    | SetCategory Category
    | AddPart PartItem
    | Undo
    | Redo
    | Delete
    | Reset
    | FitView
    | Tick Float
    | WindowResized Int Int


{-| Initialize the Contraptions application.
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

        projectParts =
            List.map Catalog.fromKitItem payload.parts

        activeCategory =
            if List.isEmpty projectParts then
                GearsCategory

            else
                ProjectCategory

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

        initialMachine =
            Machine.analyseMachine initialDoc initialStudio.viewport.partCache

        initialModel =
            { studio = initialStudio
            , machine = initialMachine
            , mode = EditMode
            , activeCategory = activeCategory
            , projectParts = projectParts
            , currentTime = 0.0
            , motorRpm = 30.0
            , windowWidth = 1280
            , windowHeight = 800
            }

        fetchViewportSize =
            Browser.Dom.getViewport
                |> Task.perform (\info -> WindowResized (round info.viewport.width) (round info.viewport.height))
    in
    ( initialModel
    , Cmd.batch
        [ Cmd.map StudioMsg studioCmd
        , fetchViewportSize
        ]
    )


{-| Update the application model.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StudioMsg studioMsg ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.update studioMsg model.studio

                newMachine =
                    case List.filterMap extractDocChanged events of
                        updatedDoc :: _ ->
                            Machine.analyseMachine updatedDoc nextStudio.viewport.partCache

                        [] ->
                            model.machine

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        ToggleMode ->
            let
                nextMode =
                    case model.mode of
                        EditMode ->
                            SimulateMode

                        SimulateMode ->
                            EditMode
            in
            ( { model | mode = nextMode }, Cmd.none )

        SetCategory category ->
            ( { model | activeCategory = category }, Cmd.none )

        AddPart item ->
            let
                placedCount =
                    List.length (Core.getPlacements model.studio)

                offset =
                    toFloat (placedCount - 2) * 40.0

                pos =
                    vec3 offset 0 0

                ( nextStudio, studioCmd, events ) =
                    Core.addPart item.file item.color pos model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Undo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.undo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Redo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.redo model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Delete ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.deleteSelection model.studio

                newMachine =
                    Machine.analyseMachine nextStudio.document nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Reset ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.resetDocument Document.empty model.studio

                newMachine =
                    Machine.analyseMachine Document.empty nextStudio.viewport.partCache

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio, machine = newMachine, currentTime = 0.0 }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        FitView ->
            let
                ( nextStudio, studioCmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = nextStudio }, Cmd.map StudioMsg studioCmd )

        Tick delta ->
            if model.mode == SimulateMode then
                let
                    dt =
                        delta / 1000.0

                    newTime =
                        model.currentTime + dt
                in
                ( { model | currentTime = newTime }, Cmd.none )

            else
                ( model, Cmd.none )

        WindowResized w h ->
            ( { model | windowWidth = w, windowHeight = h }, Cmd.none )


extractDocChanged : Core.Event -> Maybe Document.Document
extractDocChanged event =
    case event of
        Core.DocumentChanged doc ->
            Just doc

        _ ->
            Nothing


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


{-| Global subscriptions for Contraptions.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , Ports.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , Ports.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Browser.Events.onResize WindowResized
        , if model.mode == SimulateMode then
            Browser.Events.onAnimationFrameDelta Tick

          else
            Sub.none
        ]


{-| Render the Contraptions application.
-}
view : Model -> Html Msg
view model =
    let
        accomplished =
            Machine.isAccomplished model.machine

        gearCount =
            List.length model.machine.gears

        placedCount =
            List.length (Core.getPlacements model.studio)

        hasUndo =
            Core.canUndo model.studio

        hasRedo =
            Core.canRedo model.studio

        hasSel =
            Core.hasSelection model.studio
    in
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- Top toolbar & header
          header
            [ Attr.style "position" "absolute"
            , Attr.style "top" "1rem"
            , Attr.style "left" "1rem"
            , Attr.style "right" "1rem"
            , Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "z-index" "10"
            , Attr.style "pointer-events" "none"
            ]
            [ -- App Title badge
              div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.75rem"
                , Attr.style "padding" "0.5rem 1rem"
                , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "border-radius" "9999px"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "color" "var(--color-text-on-dark)"
                , Attr.style "pointer-events" "auto"
                ]
                [ span
                    [ Attr.style "font-weight" "700"
                    , Attr.style "letter-spacing" "-0.01em"
                    ]
                    [ text "Palikkakehys Contraptions" ]
                , span
                    [ Attr.style "font-size" "0.875rem"
                    , Attr.style "color" "var(--color-brand-yellow)"
                    ]
                    [ text (String.fromInt gearCount ++ " gears") ]
                ]

            -- Action Controls Toolbar
            , div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "padding" "0.375rem"
                , Attr.style "background" "rgba(5, 19, 29, 0.85)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "border-radius" "0.75rem"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "pointer-events" "auto"
                ]
                [ modeToggleButton model.mode
                , toolbarButton Undo hasUndo "Undo"
                , toolbarButton Redo hasRedo "Redo"
                , toolbarButton Delete hasSel "Delete"
                , toolbarButton FitView True "Fit View"
                , toolbarButton Reset (placedCount > 0) "Reset"
                ]
            ]

        -- Celebration Accomplishment Banner
        , if accomplished then
            div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "4.5rem"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "1rem"
                , Attr.style "padding" "0.75rem 1.5rem"
                , Attr.style "background" "rgba(201, 26, 9, 0.95)"
                , Attr.style "backdrop-filter" "blur(12px)"
                , Attr.style "border-radius" "9999px"
                , Attr.style "border" "2px solid var(--color-brand-yellow)"
                , Attr.style "color" "var(--color-text-on-dark)"
                , Attr.style "box-shadow" "0 10px 25px rgba(0, 0, 0, 0.6)"
                , Attr.style "z-index" "20"
                , Attr.style "animation" "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite"
                ]
                [ span
                    [ Attr.style "font-size" "1.25rem" ]
                    [ text "⚙️" ]
                , div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "column"
                    ]
                    [ span
                        [ Attr.style "font-weight" "800"
                        , Attr.style "font-size" "1.125rem"
                        , Attr.style "letter-spacing" "-0.01em"
                        ]
                        [ text "Contraption Accomplished!" ]
                    , span
                        [ Attr.style "font-size" "0.875rem"
                        , Attr.style "color" "var(--color-brand-yellow)"
                        ]
                        [ text ("All " ++ String.fromInt gearCount ++ " gears are rotating together!") ]
                    ]
                ]

          else
            text ""

        -- 3D WebGL Canvas
        , Html.map StudioMsg (Core.view { width = model.windowWidth, height = model.windowHeight } model.studio)

        -- Bottom Technic Dock
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "bottom" "1.5rem"
            , Attr.style "left" "50%"
            , Attr.style "transform" "translateX(-50%)"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.5rem"
            , Attr.style "padding" "0.75rem 1rem"
            , Attr.style "background" "rgba(5, 19, 29, 0.92)"
            , Attr.style "backdrop-filter" "blur(12px)"
            , Attr.style "border-radius" "1.25rem"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
            , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
            , Attr.style "z-index" "10"
            , Attr.style "max-width" "90vw"
            ]
            [ -- Category Tabs
              div
                [ Attr.style "display" "flex"
                , Attr.style "gap" "0.375rem"
                , Attr.style "margin-bottom" "0.25rem"
                ]
                (let
                    categories =
                        if List.isEmpty model.projectParts then
                            [ GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]

                        else
                            [ ProjectCategory, GearsCategory, BeamsCategory, AxlesCategory, PinsCategory ]
                 in
                 List.map (categoryTab model.activeCategory) categories
                )

            -- Parts Row
            , div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "overflow-x" "auto"
                , Attr.style "max-width" "100%"
                , Attr.style "padding" "0.25rem"
                ]
                (List.map partDockButton (Catalog.partsForCategoryWithProject model.projectParts model.activeCategory))
            ]
        ]


categoryTab : Category -> Category -> Html Msg
categoryTab active cat =
    let
        isSelected =
            active == cat
    in
    button
        [ onClick (SetCategory cat)
        , Attr.style "padding" "0.375rem 0.75rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "600"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
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
        ]
        [ text (Catalog.categoryLabel cat) ]


partDockButton : PartItem -> Html Msg
partDockButton item =
    button
        [ onClick (AddPart item)
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.25rem"
        , Attr.style "padding" "0.5rem 0.75rem"
        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.1)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "color" "var(--color-text-on-dark)"
        , Attr.style "cursor" "pointer"
        , Attr.style "min-width" "5rem"
        , Attr.style "transition" "all 150ms ease"
        ]
        [ span
            [ Attr.style "font-size" "0.875rem"
            , Attr.style "font-weight" "600"
            ]
            [ text item.name ]
        , span
            [ Attr.style "font-size" "0.75rem"
            , Attr.style "color" "var(--color-text-subtle)"
            ]
            [ text item.file ]
        ]


modeToggleButton : Mode -> Html Msg
modeToggleButton mode =
    let
        isSim =
            mode == SimulateMode
    in
    button
        [ onClick ToggleMode
        , Attr.style "padding" "0.375rem 0.875rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "700"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if isSim then
                "var(--color-brand-yellow)"

             else
                "rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if isSim then
                "var(--color-brand)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "transition" "all 150ms ease"
        ]
        [ text
            (if isSim then
                "⚙️ Simulating (Run)"

             else
                "✏️ Edit Mode"
            )
        ]


toolbarButton : Msg -> Bool -> String -> Html Msg
toolbarButton msg enabled label =
    button
        [ onClick msg
        , Attr.disabled (not enabled)
        , Attr.style "padding" "0.375rem 0.75rem"
        , Attr.style "font-size" "0.875rem"
        , Attr.style "font-weight" "500"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "0.375rem"
        , Attr.style "background"
            (if enabled then
                "rgba(255, 255, 255, 0.1)"

             else
                "transparent"
            )
        , Attr.style "color"
            (if enabled then
                "var(--color-text-on-dark)"

             else
                "rgba(255, 255, 255, 0.3)"
            )
        , Attr.style "cursor"
            (if enabled then
                "pointer"

             else
                "not-allowed"
            )
        , Attr.style "transition" "all 150ms ease"
        ]
        [ text label ]


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
