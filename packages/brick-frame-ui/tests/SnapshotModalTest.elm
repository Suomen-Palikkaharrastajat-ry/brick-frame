module SnapshotModalTest exposing (suite)

{-| Tests for UI.SnapshotModal lifecycle, state transitions, and continue gating.
-}

import Expect
import Html.Attributes as HtmlAttr
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UI.SnapshotModal as SnapshotModal


type TestMsg
    = Save { dataUrl : String, filename : String }
    | ToggleAgreed Bool
    | Continue


testActions : SnapshotModal.Actions TestMsg
testActions =
    { onSave = Save
    , onToggleAgreed = ToggleAgreed
    , onContinue = Continue
    }


suite : Test
suite =
    describe "UI.SnapshotModal"
        [ describe "State transitions and gating"
            [ test "init starts in Rendering phase and cannot continue" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test-render.png" }
                    in
                    Expect.all
                        [ \m -> Expect.equal SnapshotModal.Rendering m.phase
                        , \m -> Expect.equal False (SnapshotModal.canContinue m)
                        ]
                        modal
            , test "setReady transitions to Preview with hasSaved = False and hasAgreed = False" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test-render.png" }
                                |> SnapshotModal.setReady
                                    { dataUrl = "data:image/png;base64,mock"
                                    , filename = "test-render.png"
                                    }
                    in
                    Expect.all
                        [ \m -> Expect.equal False (SnapshotModal.canContinue m)
                        , \m ->
                            case m.phase of
                                SnapshotModal.Preview data ->
                                    Expect.all
                                        [ \d -> Expect.equal "data:image/png;base64,mock" d.dataUrl
                                        , \d -> Expect.equal "test-render.png" d.filename
                                        , \d -> Expect.equal False d.hasSaved
                                        , \d -> Expect.equal False d.hasAgreed
                                        ]
                                        data

                                SnapshotModal.Rendering ->
                                    Expect.fail "Expected Preview phase"
                        ]
                        modal
            , test "markSaved permits continuation" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }
                                |> SnapshotModal.markSaved
                    in
                    Expect.equal True (SnapshotModal.canContinue modal)
            , test "setAgreed permits continuation even when not saved" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }
                                |> SnapshotModal.setAgreed True
                    in
                    Expect.equal True (SnapshotModal.canContinue modal)
            , test "toggling agreed to False without saving prevents continuation" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "test.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,mock", filename = "test.png" }
                                |> SnapshotModal.setAgreed True
                                |> SnapshotModal.setAgreed False
                    in
                    Expect.equal False (SnapshotModal.canContinue modal)
            ]
        , describe "Rendering view"
            [ test "shows progress indicator in Rendering phase" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "render.png" }
                    in
                    SnapshotModal.view testActions modal
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.attribute (HtmlAttr.attribute "role" "dialog") ]
                            , Query.has [ Selector.text "Raytracing realistic rendering..." ]
                            , Query.has [ Selector.text "Please wait while the snapshot renders..." ]
                            , Query.has [ Selector.tag "button", Selector.text "Continue" ]
                            ]
            , test "shows preview image and enables Continue when agreed or saved" <|
                \_ ->
                    let
                        modal =
                            SnapshotModal.init { filename = "render.png" }
                                |> SnapshotModal.setReady { dataUrl = "data:image/png;base64,abc123", filename = "render.png" }
                                |> SnapshotModal.setAgreed True
                    in
                    SnapshotModal.view testActions modal
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.tag "img", Selector.attribute (HtmlAttr.src "data:image/png;base64,abc123") ]
                            , Query.has [ Selector.text "render.png • 1080p high quality PNG" ]
                            , Query.has [ Selector.text "Save Image" ]
                            , Query.has [ Selector.text "I have saved / reviewed the render" ]
                            , Query.has [ Selector.tag "button", Selector.text "Continue" ]
                            ]
            ]
        ]
