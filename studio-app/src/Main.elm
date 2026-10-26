module Main exposing (Flags, Model, Msg(..), init, main, update)

{-| Palikkakehys Studio: the LDraw editor.

A thin shell over the studio's pages. Each page keeps its own `Model` and `Msg`
and this module only routes between them: the workspace where the document is
edited (`WorkspaceMsg`) and the simulation preview that runs on a snapshot of it
(`PreviewMsg`). Switching pages is the one thing handled here — the workspace is
kept as it is while the preview is open, so leaving the preview puts the user
back exactly where they were.

-}

import Browser
import Html exposing (Html, div)
import LDraw.Resolve exposing (PartCache)
import Ports
import Studio.Instructions as Instructions
import Studio.Preview as Preview
import Studio.Workspace as Workspace
import UI.SnapshotModal as SnapshotModal
import Viewport
import Viewport.Shortcuts as Shortcuts


{-| Flags from `initBricksRuntime`; the same record the viewer takes.
-}
type alias Flags =
    Workspace.Flags


type alias Model =
    { flags : Flags
    , workspace : Workspace.Model
    , preview : Maybe Preview.Model
    , instructions : Maybe Instructions.Model
    , snapshotModal : Maybe SnapshotModal.Model
    }


type Msg
    = WorkspaceMsg Workspace.Msg
    | PreviewMsg Preview.Msg
    | InstructionsMsg Instructions.Msg
    | SnapshotReady { dataUrl : String, filename : String }
    | SnapshotSave { dataUrl : String, filename : String }
    | SnapshotClose


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( workspace, cmd ) =
            Workspace.init flags
    in
    ( { flags = flags, workspace = workspace, preview = Nothing, instructions = Nothing, snapshotModal = Nothing }
    , Cmd.map WorkspaceMsg cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if closesSnapshot msg model then
        ( { model | snapshotModal = Nothing }, Cmd.none )

    else
        updateApp msg model


{-| Escape closes an open snapshot modal before anything else sees the key, so
the editing keys and the layers underneath do not react as well.
-}
closesSnapshot : Msg -> Model -> Bool
closesSnapshot msg model =
    case ( model.snapshotModal, msg ) of
        ( Just _, WorkspaceMsg (Workspace.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            Shortcuts.closesDialog keyEvent

        ( Just _, PreviewMsg (Preview.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            Shortcuts.closesDialog keyEvent

        ( Just _, InstructionsMsg (Instructions.ViewportMsg (Viewport.KeyPressed keyEvent)) ) ->
            Shortcuts.closesDialog keyEvent

        _ ->
            False


updateApp : Msg -> Model -> ( Model, Cmd Msg )
updateApp msg model =
    case ( msg, model.preview, model.instructions ) of
        ( WorkspaceMsg Workspace.SimulateRequested, Nothing, Nothing ) ->
            openPreview model

        ( WorkspaceMsg Workspace.InstructionsRequested, Nothing, Nothing ) ->
            openInstructions model

        ( WorkspaceMsg Workspace.CaptureSnapshot, Nothing, Nothing ) ->
            let
                ( workspace, _ ) =
                    Workspace.update Workspace.CaptureSnapshot model.workspace
            in
            ( { model
                | workspace = workspace
                , snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" })
              }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = False
                , filename = "palikkakehys-render.png"
                }
            )

        ( WorkspaceMsg workspaceMsg, Nothing, Nothing ) ->
            let
                ( workspace, cmd ) =
                    Workspace.update workspaceMsg model.workspace
            in
            ( { model | workspace = workspace }, Cmd.map WorkspaceMsg cmd )

        ( WorkspaceMsg (Workspace.WindowSized width height), Just _, _ ) ->
            -- The preview has its own listener; the workspace only needs to know
            -- the size for when it comes back.
            let
                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | workspace = workspace }, Cmd.none )

        ( WorkspaceMsg (Workspace.WindowSized width height), _, Just _ ) ->
            let
                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | workspace = workspace }, Cmd.none )

        ( WorkspaceMsg _, _, _ ) ->
            ( model, Cmd.none )

        ( PreviewMsg Preview.Leave, Just preview, _ ) ->
            ( { model
                | workspace = Workspace.restore preview.viewport.camera model.workspace
                , preview = Nothing
              }
            , Cmd.none
            )

        ( PreviewMsg (Preview.WindowSized width height), Just preview, _ ) ->
            -- Keep the workspace's idea of the window current for when it returns.
            let
                ( next, cmd ) =
                    Preview.update (Preview.WindowSized width height) preview

                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | preview = Just next, workspace = workspace }, Cmd.map PreviewMsg cmd )

        ( PreviewMsg Preview.CaptureSnapshot, Just preview, _ ) ->
            let
                ( next, _ ) =
                    Preview.update Preview.CaptureSnapshot preview
            in
            ( { model
                | preview = Just next
                , snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" })
              }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = False
                , filename = "palikkakehys-render.png"
                }
            )

        ( PreviewMsg previewMsg, Just preview, _ ) ->
            let
                ( next, cmd ) =
                    Preview.update previewMsg preview
            in
            ( { model | preview = Just next }, Cmd.map PreviewMsg cmd )

        ( PreviewMsg _, Nothing, _ ) ->
            ( model, Cmd.none )

        ( InstructionsMsg Instructions.Leave, _, Just instructions ) ->
            ( { model
                | workspace = Workspace.restore (Instructions.restoreCamera instructions) model.workspace
                , instructions = Nothing
              }
            , Cmd.none
            )

        ( InstructionsMsg (Instructions.WindowSized width height), _, Just instructions ) ->
            let
                ( next, cmd ) =
                    Instructions.update (Instructions.WindowSized width height) instructions

                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | instructions = Just next, workspace = workspace }, Cmd.map InstructionsMsg cmd )

        ( InstructionsMsg Instructions.CaptureSnapshot, _, Just instructions ) ->
            let
                ( next, _ ) =
                    Instructions.update Instructions.CaptureSnapshot instructions
            in
            ( { model
                | instructions = Just next
                , snapshotModal = Just (SnapshotModal.init { filename = "palikkakehys-render.png" })
              }
            , Ports.captureRender
                { width = 1920
                , height = 1080
                , transparent = False
                , filename = "palikkakehys-render.png"
                }
            )

        ( InstructionsMsg instructionsMsg, _, Just instructions ) ->
            let
                ( next, cmd ) =
                    Instructions.update instructionsMsg instructions
            in
            ( { model | instructions = Just next }, Cmd.map InstructionsMsg cmd )

        ( InstructionsMsg _, _, Nothing ) ->
            ( model, Cmd.none )

        ( SnapshotReady payload, _, _ ) ->
            ( { model | snapshotModal = Maybe.map (SnapshotModal.setReady payload) model.snapshotModal }
            , Cmd.none
            )

        ( SnapshotSave payload, _, _ ) ->
            ( { model | snapshotModal = Maybe.map SnapshotModal.markSaved model.snapshotModal }
            , Ports.saveSnapshot payload
            )

        ( SnapshotClose, _, _ ) ->
            ( { model | snapshotModal = Nothing }
            , Cmd.none
            )


{-| Snapshot the document and start the simulation on it.
-}
openPreview : Model -> ( Model, Cmd Msg )
openPreview model =
    let
        workspace =
            model.workspace

        ( preview, cmd ) =
            Preview.init
                { ldrawBase = model.flags.ldrawBase
                , ldrawFallbackBase = model.flags.ldrawFallbackBase
                , supportsPointerEvents = model.flags.supportsPointerEvents
                , renderStyle = workspace.viewport.renderStyle
                , maxRpm = model.flags.maxRpm
                , initialRpm = model.flags.initialRpm
                , snapshot = workspace.document
                , camera = workspace.viewport.camera
                , loadedParts = loadedParts workspace
                , window = workspace.window
                }
    in
    ( { model | preview = Just preview }, Cmd.map PreviewMsg cmd )


{-| Snapshot the document and start the building instructions viewer on it.
-}
openInstructions : Model -> ( Model, Cmd Msg )
openInstructions model =
    let
        workspace =
            model.workspace

        ( instructions, cmd ) =
            Instructions.init
                { ldrawBase = model.flags.ldrawBase
                , ldrawFallbackBase = model.flags.ldrawFallbackBase
                , supportsPointerEvents = model.flags.supportsPointerEvents
                , renderStyle = workspace.viewport.renderStyle
                , snapshot = workspace.document
                , camera = workspace.viewport.camera
                , loadedParts = loadedParts workspace
                , window = workspace.window
                }
    in
    ( { model | instructions = Just instructions }, Cmd.map InstructionsMsg cmd )


loadedParts : Workspace.Model -> PartCache
loadedParts workspace =
    workspace.viewport.partCache


view : Model -> Html Msg
view model =
    let
        pageContent =
            case ( model.preview, model.instructions ) of
                ( Just preview, _ ) ->
                    Html.map PreviewMsg (Preview.view preview)

                ( Nothing, Just instructions ) ->
                    Html.map InstructionsMsg (Instructions.view instructions)

                ( Nothing, Nothing ) ->
                    Html.map WorkspaceMsg (Workspace.view model.workspace)
    in
    case model.snapshotModal of
        Just modal ->
            div []
                [ pageContent
                , SnapshotModal.view
                    { onSave = SnapshotSave
                    , onClose = SnapshotClose
                    }
                    modal
                ]

        Nothing ->
            pageContent


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.snapshotReady SnapshotReady
        , case ( model.preview, model.instructions ) of
            ( Just preview, _ ) ->
                Sub.map PreviewMsg (Preview.subscriptions preview)

            ( Nothing, Just instructions ) ->
                Sub.map InstructionsMsg (Instructions.subscriptions instructions)

            ( Nothing, Nothing ) ->
                Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
