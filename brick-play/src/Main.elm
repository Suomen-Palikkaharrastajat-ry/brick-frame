module Main exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , main
    , remainingCount
    , remainingForItem
    , subscriptions
    , update
    , view
    )

{-| Main application for Brick Play.

Provides a minimal, elegant free-building studio with kit inventory
tracking, stud snapping, and gizmo controls.

-}

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, button, div, header, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import LDraw.Colors as Colors
import LDraw.Document as Document
import Math.Vector3 exposing (vec3)
import Ports
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.UrlPayload as UrlPayload exposing (PartKitItem)
import Task


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
    , windowWidth : Int
    , windowHeight : Int
    }


{-| Messages handled by the Brick Play application.
-}
type Msg
    = StudioMsg Core.Msg
    | AddBrick
    | AddPart PartKitItem
    | Undo
    | Redo
    | Delete
    | Reset
    | FitView
    | WindowResized Int Int


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

        initialModel =
            { studio = initialStudio
            , inventory = inventory
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
            in
            ( { model | studio = nextStudio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        AddBrick ->
            case model.inventory of
                first :: _ ->
                    addPartItem first model

                [] ->
                    ( model, Cmd.none )

        AddPart item ->
            addPartItem item model

        Undo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.undo model.studio

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Redo ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.redo model.studio

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Delete ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.deleteSelection model.studio

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        Reset ->
            let
                ( nextStudio, studioCmd, events ) =
                    Core.resetDocument Document.empty model.studio

                portCmds =
                    handleCoreEvents events
            in
            ( { model | studio = nextStudio }
            , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
            )

        FitView ->
            let
                ( nextStudio, studioCmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = nextStudio }, Cmd.map StudioMsg studioCmd )

        WindowResized w h ->
            ( { model | windowWidth = w, windowHeight = h }, Cmd.none )


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
            placedCount =
                List.length (Core.getPlacements model.studio)

            offset =
                toFloat (placedCount - 2) * 30.0

            pos =
                vec3 offset 0 0

            ( nextStudio, studioCmd, events ) =
                Core.addPart item.part item.color pos model.studio

            portCmds =
                handleCoreEvents events
        in
        ( { model | studio = nextStudio }
        , Cmd.batch [ Cmd.map StudioMsg studioCmd, portCmds ]
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
        ]


{-| Render the Brick Play interface.
-}
view : Model -> Html Msg
view model =
    let
        placed =
            List.length (Core.getPlacements model.studio)

        hasUndo =
            Core.canUndo model.studio

        hasRedo =
            Core.canRedo model.studio

        hasSel =
            Core.hasSelection model.studio

        totalBricks =
            List.sum (List.map .count model.inventory)

        headerSubtitle =
            if List.length model.inventory == 1 && (List.head model.inventory |> Maybe.map .part) == Just "3001.dat" then
                "10 Yellow Bricks"

            else
                "Kit (" ++ String.fromInt totalBricks ++ " pieces)"
    in
    div
        [ Attr.style "position" "relative"
        , Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "var(--color-brand)"
        , Attr.style "font-family" "var(--font-sans)"
        ]
        [ -- Top toolbar
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
                    [ text "Palikkakehys Play" ]
                , span
                    [ Attr.style "font-size" "0.875rem"
                    , Attr.style "color" "var(--color-text-subtle)"
                    ]
                    [ text headerSubtitle ]
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
                [ toolbarButton Undo hasUndo "Undo"
                , toolbarButton Redo hasRedo "Redo"
                , toolbarButton Delete hasSel "Delete"
                , toolbarButton FitView True "Fit View"
                , toolbarButton Reset (placed > 0) "Reset"
                ]
            ]

        -- 3D WebGL Canvas
        , Html.map StudioMsg (Core.view { width = model.windowWidth, height = model.windowHeight } model.studio)

        -- Bottom Dock: Inventory Cards
        , div
            [ Attr.style "position" "absolute"
            , Attr.style "bottom" "1.5rem"
            , Attr.style "left" "50%"
            , Attr.style "transform" "translateX(-50%)"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "0.75rem"
            , Attr.style "max-width" "calc(100vw - 3rem)"
            , Attr.style "overflow-x" "auto"
            , Attr.style "padding" "0.75rem 1rem"
            , Attr.style "background" "rgba(5, 19, 29, 0.9)"
            , Attr.style "backdrop-filter" "blur(12px)"
            , Attr.style "border-radius" "1rem"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
            , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
            , Attr.style "z-index" "10"
            ]
            (List.map (viewKitCard model) model.inventory)
        ]


viewKitCard : Model -> PartKitItem -> Html Msg
viewKitCard model item =
    let
        remaining =
            remainingForItem model item

        canAdd =
            remaining > 0

        displayName =
            String.replace ".dat" "" item.part
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "0.75rem"
        , Attr.style "padding" "0.5rem 0.75rem"
        , Attr.style "background" "rgba(255, 255, 255, 0.05)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.1)"
        , Attr.style "border-radius" "0.5rem"
        , Attr.style "flex-shrink" "0"
        ]
        [ -- Color swatch
          span
            [ Attr.style "display" "inline-block"
            , Attr.style "width" "1.25rem"
            , Attr.style "height" "0.75rem"
            , Attr.style "background" (Colours.css (Colors.resolveColor 16 item.color))
            , Attr.style "border-radius" "2px"
            , Attr.style "border" "1px solid rgba(255, 255, 255, 0.2)"
            ]
            []

        -- Part name and count info
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "gap" "0.125rem"
            ]
            [ span
                [ Attr.style "font-size" "0.875rem"
                , Attr.style "font-weight" "600"
                , Attr.style "color" "var(--color-text-on-dark)"
                ]
                [ text displayName ]
            , span
                [ Attr.style "font-size" "0.75rem"
                , Attr.style "color"
                    (if canAdd then
                        "var(--color-text-subtle)"

                     else
                        "var(--color-brand-red)"
                    )
                ]
                [ text (String.fromInt remaining ++ " / " ++ String.fromInt item.count ++ " available") ]
            ]

        -- "+ Add" Button
        , button
            [ onClick (AddPart item)
            , Attr.disabled (not canAdd)
            , Attr.style "padding" "0.375rem 0.625rem"
            , Attr.style "font-size" "0.75rem"
            , Attr.style "font-weight" "600"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "0.375rem"
            , Attr.style "background"
                (if canAdd then
                    "var(--color-brand-yellow)"

                 else
                    "rgba(255, 255, 255, 0.1)"
                )
            , Attr.style "color"
                (if canAdd then
                    "var(--color-brand)"

                 else
                    "var(--color-text-subtle)"
                )
            , Attr.style "cursor"
                (if canAdd then
                    "pointer"

                 else
                    "not-allowed"
                )
            , Attr.style "transition" "all 150ms ease"
            ]
            [ text "+ Add" ]
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
