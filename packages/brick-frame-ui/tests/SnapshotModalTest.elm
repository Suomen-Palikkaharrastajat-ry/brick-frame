module SnapshotModalTest exposing (suite)

{-| Tests for UI.SnapshotModal state transitions and its rendered controls.
-}

import Expect
import Html.Attributes as HtmlAttr
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UI.SnapshotModal as SnapshotModal


type TestMsg
    = Save { dataUrl : String, filename : String }
    | Close


testActions : SnapshotModal.Actions TestMsg
testActions =
    { onSave = Save
    , onClose = Close
    }


suite : Test
suite =
    describe "UI.SnapshotModal"
        [ describe "State transitions"
            [ test "init starts in Rendering phase" <|
                \_ ->
                    SnapshotModal.init { filename = "test-render.png" }
                        |> .phase
                        |> Expect.equal SnapshotModal.Rendering
            , test "setReady transitions to Preview with hasSaved = False" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test-render.png" }
                                |> SnapshotModal.setReady
                                    { dataUrl = "data:image/png;base64,mock"
                                    , filename = "test-render.png"
                                    }
                    in
                    case modal.phase of
                        SnapshotModal.Preview data ->
                            Expect.all
                                [ \d -> Expect.equal "data:image/png;base64,mock" d.dataUrl
                                , \d -> Expect.equal "test-render.png" d.filename
                                , \d -> Expect.equal False d.hasSaved
                                ]
                                data

                        SnapshotModal.Rendering ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved records the save in the Preview phase" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }
                                |> SnapshotModal.markSaved
                    in
                    case modal.phase of
                        SnapshotModal.Preview data ->
                            Expect.equal True data.hasSaved

                        SnapshotModal.Rendering ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved is a no-op while Rendering" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                    in
                    Expect.equal modal (SnapshotModal.markSaved modal)
            ]
        , describe "Rendering view"
            [ test "shows the capturing message and a Close button without progress claims" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "role" "dialog") ]
                            , Query.has [ Selector.text "Capturing snapshot…" ]
                            , Query.hasNot [ Selector.text "Raytrac" ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            ]
            ]
        , describe "Preview view"
            [ test "shows the image and Save, with Close and no agreement checkbox" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,abc123", filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.tag "img", Selector.attribute (HtmlAttr.src "data:image/png;base64,abc123") ]
                            , Query.has [ Selector.text "render.png • PNG" ]
                            , Query.has [ Selector.tag "button", Selector.text "Save Image" ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.tag "input" ]
                            ]
            ]
        ]
