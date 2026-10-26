module CameraControlsTest exposing (suite)

{-| Tests for the studio's camera controls: the shared globe with its walk and
help buttons, the phone layout's palette/camera bottom sheet, and the fine-move
toggle.
-}

import Expect
import Html.Attributes as HtmlAttr
import Render.Camera as Camera
import Render.Style as Style
import Studio.CameraControls as CameraControls
import Studio.Layout exposing (Panel(..))
import Studio.Workspace as Workspace exposing (Msg(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


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
    , draft = Nothing
    , favorites = Nothing
    , recentParts = Nothing
    , playBaseUrl = Nothing
    , contraptionsBaseUrl = Nothing
    }


{-| A workspace in a window of this size, as the page sees it once measured.
-}
sized : Int -> Int -> Workspace.Model
sized width height =
    Workspace.init flags
        |> Tuple.first
        |> Workspace.update (WindowSized width height)
        |> Tuple.first


step : Msg -> Workspace.Model -> Workspace.Model
step msg model =
    Workspace.update msg model |> Tuple.first


phone : Workspace.Model
phone =
    sized 390 844


suite : Test
suite =
    describe "Studio camera controls"
        [ test "the shared controls carry the walk and help buttons, labelled" <|
            \_ ->
                CameraControls.view
                    { onPress = always ()
                    , onMove = always ()
                    , onRelease = ()
                    , onZoom = always ()
                    , onZoomDone = ()
                    , onReset = ()
                    , onWalk = ()
                    , onHelp = ()
                    , onRealistic = ()
                    , onSnapshot = ()
                    , noOp = ()
                    }
                    { camera = Camera.init, helpOpen = False, renderQuality = Style.Draft }
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Camera") ]
                        , Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Realistic rendering") ]
                            >> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-pressed" "false") ]
                        , Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Take snapshot") ]
                            >> Query.has [ Selector.class "touch-target" ]
                        , Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Street level view") ]
                            >> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-pressed" "false"), Selector.class "touch-target" ]
                        , Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Keyboard shortcuts") ]
                            >> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-pressed" "false") ]
                        ]
        , test "a wide window has the camera globe over the canvas and no bottom sheet" <|
            \_ ->
                sized 1280 800
                    |> Workspace.view
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.hasNot [ Selector.id "panel-sheet" ]
                        , Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Camera") ]
                        ]
        , test "a phone held upright opens on the palette, with no camera globe over the canvas" <|
            \_ ->
                phone
                    |> Workspace.view
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ Selector.id "panel-sheet" ]
                            >> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Parts") ]
                        , Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Camera controls") ]
                            >> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-pressed" "false"), Selector.class "touch-target" ]
                        ]
        , test "the camera toggle puts the camera controls in the bottom sheet, and back" <|
            \_ ->
                let
                    onCamera =
                        step ToggleCameraSheet phone

                    viewOf model =
                        Workspace.view model |> Query.fromHtml
                in
                Expect.all
                    [ \_ ->
                        viewOf onCamera
                            |> Query.find [ Selector.id "panel-sheet" ]
                            |> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Camera") ]
                    , \_ ->
                        viewOf onCamera
                            |> Query.find [ Selector.attribute (HtmlAttr.attribute "aria-label" "Camera controls") ]
                            |> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-pressed" "true") ]
                    , \_ ->
                        viewOf (step ToggleCameraSheet onCamera)
                            |> Query.find [ Selector.id "panel-sheet" ]
                            |> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Parts") ]
                    ]
                    ()
        , test "the palette tab leaves the camera controls for the palette" <|
            \_ ->
                step (OpenPanel (Just PartsPanel)) phone
                    |> Workspace.view
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "panel-sheet" ]
                    |> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Parts") ]
        , test "the phone's palette sheet cannot be closed, only switched" <|
            \_ ->
                step (OpenPanel Nothing) phone
                    |> Workspace.view
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "panel-sheet" ]
                    |> Query.has [ Selector.attribute (HtmlAttr.attribute "aria-label" "Parts") ]
        , test "the fine-move toggle is off at first and on after a press" <|
            \_ ->
                ( phone.fineMove, (step ToggleFineMove phone).fineMove )
                    |> Expect.equal ( False, True )
        ]
