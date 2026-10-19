module Main exposing (Flags, Model, Msg, init, main)

{-| Palikkakehys Studio: the LDraw editor.

A thin shell over the studio's pages. Each page keeps its own `Model` and `Msg`
and this module only routes between them, so a page can grow without touching
the others (`WorkspaceMsg Workspace.Msg` today; the simulation preview joins as
another page).

-}

import Browser
import Html exposing (Html)
import Studio.Workspace as Workspace


{-| Flags from `initBricksRuntime`; the same record the viewer takes.
-}
type alias Flags =
    Workspace.Flags


type Page
    = WorkspacePage


type alias Model =
    { page : Page
    , workspace : Workspace.Model
    }


type Msg
    = WorkspaceMsg Workspace.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( workspace, cmd ) =
            Workspace.init flags
    in
    ( { page = WorkspacePage, workspace = workspace }
    , Cmd.map WorkspaceMsg cmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WorkspaceMsg workspaceMsg ->
            let
                ( workspace, cmd ) =
                    Workspace.update workspaceMsg model.workspace
            in
            ( { model | workspace = workspace }, Cmd.map WorkspaceMsg cmd )


view : Model -> Html Msg
view model =
    case model.page of
        WorkspacePage ->
            Html.map WorkspaceMsg (Workspace.view model.workspace)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        WorkspacePage ->
            Sub.map WorkspaceMsg (Workspace.subscriptions model.workspace)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
