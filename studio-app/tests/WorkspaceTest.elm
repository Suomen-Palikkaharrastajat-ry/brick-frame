module WorkspaceTest exposing (suite)

{-| Tests for the studio's workspace page: loading documents into the viewport
and what the page does with the viewport's events.
-}

import Expect
import LDraw.Document as Document
import Studio.Workspace as Workspace exposing (Msg(..))
import Test exposing (Test, describe, test)
import Viewport


flags : Workspace.Flags
flags =
    { ldrawBase = ""
    , ldrawFallbackBase = ""
    , defaultModelUrl = ""
    , initialHash = ""
    , maxRpm = 120
    , uiMode = "simulator"
    , controlsEnabled = True
    , initialMotorIndex = -1
    , initialRpm = 0
    , useWindowResize = False
    , ambientStrength = Nothing
    , lightStrength = Nothing
    , vibrance = Nothing
    , edgeWidth = Nothing
    , supportsPointerEvents = True
    , studioUrl = ""
    }


start : Workspace.Model
start =
    Workspace.init flags |> Tuple.first


{-| A file whose only part is not embedded, so the viewport has to fetch it.
-}
twoParts : String
twoParts =
    "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n1 14 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"


suite : Test
suite =
    describe "Studio.Workspace"
        [ test "starts with an empty document that is ready to show" <|
            \_ ->
                Expect.all
                    [ \m -> Expect.equal [] (Document.placements m.document)
                    , \m -> Expect.equal Viewport.Ready m.viewport.loadPhase
                    ]
                    start
        , test "opening a file replaces the document" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .document
                    |> Document.placements
                    |> List.map .id
                    |> Expect.equal [ 0, 1 ]
        , test "the viewport starts resolving the parts of the new document" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .viewport
                    |> .loadPhase
                    |> Expect.equal Viewport.ResolvingParts
        , test "a new document is empty again" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> Workspace.update NewDocument
                    |> Tuple.first
                    |> .document
                    |> Document.placements
                    |> Expect.equal []
        , test "the document comes back untouched after loading" <|
            \_ ->
                Workspace.update (FileTextReceived twoParts) start
                    |> Tuple.first
                    |> .document
                    |> .touched
                    |> Expect.equal (Document.clearTouched Document.empty |> .touched)
        , test "a file error is shown through the viewport" <|
            \_ ->
                Workspace.update (FileFailed "not LDraw") start
                    |> Tuple.first
                    |> .viewport
                    |> .errorMsg
                    |> Expect.equal (Just "not LDraw")
        , test "the help dialog toggles" <|
            \_ ->
                Workspace.update ToggleHelp start
                    |> Tuple.first
                    |> .helpOpen
                    |> Expect.equal True
        ]
