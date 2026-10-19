module ViewportTest exposing (suite)

{-| Tests for the viewport component's state machine: what each input does to
the model and which events it reports to the host.
-}

import Dict
import Expect
import LDraw.Resolve as Resolve
import Render.Camera as Camera
import Render.Style as Style
import Set
import Test exposing (Test, describe, test)
import Viewport exposing (CameraMode(..), Event(..), HeldControl(..), LoadPhase(..), Msg(..))
import Viewport.Shortcuts as Shortcuts


config : Viewport.Config
config =
    { ldrawBase = ""
    , ldrawFallbackBase = ""
    , defaultModelUrl = ""
    , initialHash = ""
    , supportsPointerEvents = True
    , useWindowResize = False
    , renderStyle = Style.defaultStyle
    , partCache = Resolve.initCache
    }


idle : Viewport.Model
idle =
    Viewport.init config |> Tuple.first


keyEvent : String -> Shortcuts.KeyEvent
keyEvent key =
    { key = key
    , shift = False
    , ctrl = False
    , meta = False
    , alt = False
    , repeat = False
    , targetTag = "BODY"
    , targetEditable = False
    }


update : Msg -> Viewport.Model -> ( Viewport.Model, List Event )
update msg model =
    Viewport.update msg model
        |> (\( m, _, events ) -> ( m, events ))


suite : Test
suite =
    describe "Viewport"
        [ describe "init"
            [ test "an empty model URL leaves the viewport idle and auto-fitting" <|
                \_ ->
                    Expect.all
                        [ \m -> Expect.equal Idle m.loadPhase
                        , \m -> Expect.equal CameraAutoFit m.cameraMode
                        ]
                        idle
            , test "a model URL starts fetching it" <|
                \_ ->
                    Viewport.init { config | defaultModelUrl = "/m.ldr" }
                        |> Tuple.first
                        |> .loadPhase
                        |> Expect.equal (FetchingTopLevel "/m.ldr")
            , test "an explicit camera in the hash switches off auto-fit" <|
                \_ ->
                    Viewport.init { config | initialHash = "#az=1.5&d=400" }
                        |> Tuple.first
                        |> Expect.all
                            [ \m -> Expect.equal CameraManual m.cameraMode
                            , \m -> Expect.within (Expect.Absolute 0.0001) 1.5 m.camera.azimuth
                            , \m -> Expect.within (Expect.Absolute 0.0001) 400 m.camera.distance
                            ]
            ]
        , describe "input events"
            [ test "WindowResize stores the size" <|
                \_ ->
                    update (WindowResize 1024 768) idle
                        |> Tuple.first
                        |> Expect.all
                            [ \m -> Expect.equal 1024 m.width
                            , \m -> Expect.equal 768 m.height
                            ]
            , test "a wheel turn zooms and reports a settled camera" <|
                \_ ->
                    let
                        ( after, events ) =
                            update (Wheel 100) idle
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ CameraSettled ] events
                        , \_ -> Expect.equal CameraManual after.cameraMode
                        , \_ -> Expect.greaterThan idle.camera.distance after.camera.distance
                        ]
                        ()
            , test "press and release without moving is a click" <|
                \_ ->
                    idle
                        |> update (MouseDown 10 20 False)
                        |> Tuple.first
                        |> update (MouseUp 10 20)
                        |> Tuple.second
                        |> Expect.equal [ Clicked 10 20 ]
            , test "press, drag and release settles the camera instead" <|
                \_ ->
                    idle
                        |> update (MouseDown 10 20 False)
                        |> Tuple.first
                        |> update (MouseMove 60 20)
                        |> Tuple.first
                        |> update (MouseUp 60 20)
                        |> Tuple.second
                        |> Expect.equal [ CameraSettled ]
            , test "mouse input is ignored while a touch is down" <|
                \_ ->
                    let
                        touching =
                            idle
                                |> update (PointerTouchStart { id = 1, x = 5, y = 5 })
                                |> Tuple.first
                    in
                    update (MouseDown 10 20 False) touching
                        |> Tuple.first
                        |> Expect.equal touching
            , test "releasing a touch settles the camera" <|
                \_ ->
                    idle
                        |> update (PointerTouchStart { id = 1, x = 5, y = 5 })
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ CameraSettled ]
            , test "ending a touch that never started does nothing" <|
                \_ ->
                    update (PointerTouchEnd 9) idle
                        |> Tuple.second
                        |> Expect.equal []
            ]
        , describe "keyboard"
            [ test "a plain key press is handed to the host and held" <|
                \_ ->
                    let
                        ( after, events ) =
                            update (KeyPressed (keyEvent "w")) idle
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ KeyAccepted (keyEvent "w") ] events
                        , \_ -> Expect.equal True (Set.member "w" after.heldKeys)
                        ]
                        ()
            , test "typing into a form field is ignored" <|
                \_ ->
                    let
                        typing =
                            { key = "w"
                            , shift = False
                            , ctrl = False
                            , meta = False
                            , alt = False
                            , repeat = False
                            , targetTag = "INPUT"
                            , targetEditable = False
                            }
                    in
                    update (KeyPressed typing) idle
                        |> Expect.equal ( idle, [] )
            , test "auto-repeat holds the key but is not a new press" <|
                \_ ->
                    let
                        repeated =
                            keyEvent "w"
                    in
                    update (KeyPressed { repeated | repeat = True }) idle
                        |> Tuple.second
                        |> Expect.equal []
            , test "AllKeysReleased drops every held key" <|
                \_ ->
                    idle
                        |> update (KeyPressed (keyEvent "w"))
                        |> Tuple.first
                        |> update AllKeysReleased
                        |> Tuple.first
                        |> .heldKeys
                        |> Expect.equal Set.empty
            ]
        , describe "needsAnimationFrame"
            [ test "idle → False" <|
                \_ -> Viewport.needsAnimationFrame idle |> Expect.equal False
            , test "flattening geometry → True" <|
                \_ ->
                    Viewport.needsAnimationFrame { idle | loadPhase = FlatteningGeometry }
                        |> Expect.equal True
            , test "held rotate control → True" <|
                \_ ->
                    Viewport.needsAnimationFrame { idle | heldControl = HoldRotate 1 0 }
                        |> Expect.equal True
            , test "held movement key → True" <|
                \_ ->
                    Viewport.needsAnimationFrame { idle | heldKeys = Set.singleton "w" }
                        |> Expect.equal True
            , test "held speed modifier alone → False" <|
                \_ ->
                    -- Shift changes nothing on its own, so it must not keep the
                    -- render loop awake.
                    Viewport.needsAnimationFrame { idle | heldKeys = Set.singleton "Shift" }
                        |> Expect.equal False
            , test "the instanced renderer's settle countdown → True" <|
                \_ ->
                    Viewport.needsAnimationFrame { idle | refineFrames = 3 }
                        |> Expect.equal True
            ]
        , describe "loading"
            [ test "a file with nothing to fetch asks the host to analyse it" <|
                \_ ->
                    update (FileContentReceived "0 nothing to fetch\n") idle
                        |> Tuple.second
                        |> Expect.equal [ LoadStarted, PartsReady ]
            , test "beginFlatten requests the worker and starts the progress bar" <|
                \_ ->
                    let
                        ( ready, _ ) =
                            update (FileContentReceived "0 empty\n") idle

                        ( flattening, events ) =
                            Viewport.beginFlatten { lines = [], fallbackLines = [] } ready
                    in
                    Expect.all
                        [ \m -> Expect.equal FlatteningGeometry m.loadPhase
                        , \m -> Expect.equal 90 m.loadingProgressPct
                        , \_ ->
                            case events of
                                [ FlattenRequested payload ] ->
                                    Expect.equal True (String.contains "\"parentColor\"" payload)

                                _ ->
                                    Expect.fail "expected a single FlattenRequested"
                        ]
                        flattening
            , test "a part that is not cached is fetched before the host is asked" <|
                \_ ->
                    update (FileContentReceived "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat\n") idle
                        |> Tuple.second
                        |> Expect.equal [ LoadStarted ]
            , test "a worker failure falls back to local geometry" <|
                \_ ->
                    let
                        ( ready, _ ) =
                            update (FileContentReceived "0 empty\n") idle

                        ( flattening, _ ) =
                            Viewport.beginFlatten { lines = [], fallbackLines = [] } ready

                        ( done, events ) =
                            update (GeometryFlattenFailed "boom") flattening
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ ModelReady ] events
                        , \m -> Expect.equal Ready m.loadPhase
                        , \m -> Expect.equal (Just "Geometry worker failed; used local flatten. boom") m.errorMsg
                        ]
                        done
            , test "a rejected file reports the failure and shows the message" <|
                \_ ->
                    let
                        ( after, events ) =
                            update (FileLoadError "not an LDraw file") idle
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ LoadFailed "not an LDraw file" ] events
                        , \m -> Expect.equal (Just "not an LDraw file") m.errorMsg
                        ]
                        after
            , test "DismissError clears the message" <|
                \_ ->
                    update (FileLoadError "x") idle
                        |> Tuple.first
                        |> update DismissError
                        |> Tuple.first
                        |> .errorMsg
                        |> Expect.equal Nothing
            ]
        , describe "navigation"
            [ test "walk mode needs a loaded model" <|
                \_ ->
                    update ToggleWalkMode idle
                        |> Tuple.first
                        |> .camera
                        |> .navigation
                        |> Expect.equal Camera.Orbit
            ]
        ]
