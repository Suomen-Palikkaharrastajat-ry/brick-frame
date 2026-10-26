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


readyModel : SnapshotModal.Model
readyModel =
    SnapshotModal.init { filename = "test.png" }
        |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }


suite : Test
suite =
    describe "UI.SnapshotModal"
        [ describe "State transitions"
            [ test "init starts in Rendering phase with no progress" <|
                \_ ->
                    SnapshotModal.init { filename = "test-render.png" }
                        |> (\modal -> ( modal.phase, modal.progress ))
                        |> Expect.equal ( SnapshotModal.Rendering, { fraction = 0, samples = 0 } )
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

                        _ ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved records the save in the Preview phase" <|
                \_ ->
                    case (SnapshotModal.markSaved readyModel).phase of
                        SnapshotModal.Preview data ->
                            Expect.equal True data.hasSaved

                        _ ->
                            Expect.fail "Expected Preview phase"
            , test "markSaved is a no-op while Rendering" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                    in
                    Expect.equal modal (SnapshotModal.markSaved modal)
            , test "setProgress records the fraction and sample count while Rendering" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setProgress { fraction = 0.42, samples = 42 }
                        |> .progress
                        |> Expect.equal { fraction = 0.42, samples = 42 }
            , test "setProgress clamps the fraction to the 0 to 1 range" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setProgress { fraction = 1.7, samples = -3 }
                        |> .progress
                        |> Expect.equal { fraction = 1, samples = 0 }
            , test "setProgress is ignored once the image is ready" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.setProgress { fraction = 0.5, samples = 10 }
                        |> Expect.equal readyModel
            , test "setFailed moves a rendering modal to Failed with the message" <|
                \_ ->
                    SnapshotModal.init { filename = "test.png" }
                        |> SnapshotModal.setFailed "WebGL context lost"
                        |> .phase
                        |> Expect.equal (SnapshotModal.Failed "WebGL context lost")
            , test "setFailed is ignored once the image is ready" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.setFailed "late error"
                        |> Expect.equal readyModel
            , test "isRendering is True only in the Rendering phase" <|
                \_ ->
                    Expect.all
                        [ \_ -> SnapshotModal.isRendering (SnapshotModal.init { filename = "a.png" }) |> Expect.equal True
                        , \_ -> SnapshotModal.isRendering readyModel |> Expect.equal False
                        , \_ ->
                            SnapshotModal.init { filename = "a.png" }
                                |> SnapshotModal.setFailed "boom"
                                |> SnapshotModal.isRendering
                                |> Expect.equal False
                        ]
                        ()
            ]
        , describe "Rendering view"
            [ test "shows the sample count, a progress bar and the honest time estimate" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setProgress { fraction = 0.42, samples = 42 }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "role" "dialog") ]
                            , Query.has [ Selector.text "Path tracing… 42 samples" ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "role" "progressbar") ]
                            , Query.has [ Selector.attribute (HtmlAttr.attribute "aria-valuenow" "42") ]
                            , Query.has [ Selector.text "Rendering a path-traced image; this takes about 30 seconds." ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.text "Raytrac" ]
                            , Query.hasNot [ Selector.tag "input" ]
                            ]
            , test "starts at zero samples before any progress report" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "Path tracing… 0 samples" ]
            ]
        , describe "Failed view"
            [ test "shows the error and a Close button, without Save" <|
                \_ ->
                    SnapshotModal.init { filename = "render.png" }
                        |> SnapshotModal.setFailed "Out of memory while tracing"
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Out of memory while tracing" ]
                            , Query.has [ Selector.tag "button", Selector.text "Close" ]
                            , Query.hasNot [ Selector.tag "button", Selector.text "Save Image" ]
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
            , test "describes the image as 1920 × 1080" <|
                \_ ->
                    readyModel
                        |> SnapshotModal.view testActions
                        |> Query.fromHtml
                        |> Query.has [ Selector.text "A 1920 × 1080 PNG of the current view" ]
            ]
        ]
