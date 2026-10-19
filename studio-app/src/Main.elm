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
import Html exposing (Html)
import LDraw.Resolve exposing (PartCache)
import Studio.Preview as Preview
import Studio.Workspace as Workspace


{-| Flags from `initBricksRuntime`; the same record the viewer takes.
-}
type alias Flags =
    Workspace.Flags


type alias Model =
    { flags : Flags
    , workspace : Workspace.Model
    , preview : Maybe Preview.Model
    }


type Msg
    = WorkspaceMsg Workspace.Msg
    | PreviewMsg Preview.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( workspace, cmd ) =
            Workspace.init flags
    in
    ( { flags = flags, workspace = workspace, preview = Nothing }
    , Cmd.map WorkspaceMsg cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.preview ) of
        ( WorkspaceMsg Workspace.SimulateRequested, Nothing ) ->
            openPreview model

        ( WorkspaceMsg workspaceMsg, Nothing ) ->
            let
                ( workspace, cmd ) =
                    Workspace.update workspaceMsg model.workspace
            in
            ( { model | workspace = workspace }, Cmd.map WorkspaceMsg cmd )

        ( WorkspaceMsg (Workspace.WindowSized width height), Just _ ) ->
            -- The preview has its own listener; the workspace only needs to know
            -- the size for when it comes back.
            let
                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | workspace = workspace }, Cmd.none )

        ( WorkspaceMsg _, Just _ ) ->
            ( model, Cmd.none )

        ( PreviewMsg Preview.Leave, Just preview ) ->
            ( { model
                | workspace = Workspace.restore preview.viewport.camera model.workspace
                , preview = Nothing
              }
            , Cmd.none
            )

        ( PreviewMsg (Preview.WindowSized width height), Just preview ) ->
            -- Keep the workspace's idea of the window current for when it returns.
            let
                ( next, cmd ) =
                    Preview.update (Preview.WindowSized width height) preview

                ( workspace, _ ) =
                    Workspace.update (Workspace.WindowSized width height) model.workspace
            in
            ( { model | preview = Just next, workspace = workspace }, Cmd.map PreviewMsg cmd )

        ( PreviewMsg previewMsg, Just preview ) ->
            let
                ( next, cmd ) =
                    Preview.update previewMsg preview
            in
            ( { model | preview = Just next }, Cmd.map PreviewMsg cmd )

        ( PreviewMsg _, Nothing ) ->
            ( model, Cmd.none )


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


loadedParts : Workspace.Model -> PartCache
loadedParts workspace =
    workspace.viewport.partCache


view : Model -> Html Msg
view model =
    case model.preview of
        Just preview ->
            Html.map PreviewMsg (Preview.view preview)

        Nothing ->
            Html.map WorkspaceMsg (Workspace.view model.workspace)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.preview of
        Just preview ->
            Sub.batch
                [ Sub.map PreviewMsg (Preview.subscriptions preview)
                ]

        Nothing ->
            Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
