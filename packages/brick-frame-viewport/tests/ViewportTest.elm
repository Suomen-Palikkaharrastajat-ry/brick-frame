module ViewportTest exposing (suite)

{-| Tests for the viewport component's state machine: what each input does to
the model and which events it reports to the host.
-}

import Dict
import Expect
import Http
import LDraw.Document as Document exposing (Document)
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4
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
    , targetRole = ""
    }


update : Msg -> Viewport.Model -> ( Viewport.Model, List Event )
update msg model =
    Viewport.update msg model
        |> (\( m, _, events ) -> ( m, events ))


suite : Test
suite =
    describe "Viewport"
        [ documentSuite
        , describe "init"
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
                        |> Expect.equal [ Clicked 10 20 False ]
            , test "a canvas placed away from the window origin reports canvas pixels" <|
                \_ ->
                    Viewport.setCanvasOrigin 300 56 idle
                        |> update (MouseDown 310 76 False)
                        |> Tuple.first
                        |> update (MouseUp 310 76)
                        |> Tuple.second
                        |> Expect.equal [ Clicked 10 20 False ]
            , test "TouchStart / TouchEnd reports Tapped in canvas pixels with setCanvasOrigin" <|
                \_ ->
                    let
                        touchConfig =
                            { config | supportsPointerEvents = False }

                        touchModel =
                            Viewport.init touchConfig
                                |> Tuple.first
                                |> Viewport.setCanvasOrigin 200 40
                    in
                    touchModel
                        |> update (TouchStart [ { id = 1, x = 215, y = 65 } ])
                        |> Tuple.first
                        |> update (TouchEnd [])
                        |> Tuple.second
                        |> Expect.equal [ Tapped 15 25, CameraSettled ]
            , test "multi-touch TouchMove translates points by setCanvasOrigin" <|
                \_ ->
                    let
                        touchConfig =
                            { config | supportsPointerEvents = False }

                        touchModel =
                            Viewport.init touchConfig
                                |> Tuple.first
                                |> Viewport.setCanvasOrigin 100 50

                        ( started, _ ) =
                            update (TouchStart [ { id = 1, x = 110, y = 70 }, { id = 2, x = 150, y = 90 } ]) touchModel

                        ( moved, _ ) =
                            update (TouchMove [ { id = 1, x = 120, y = 70 }, { id = 2, x = 160, y = 90 } ]) started

                        ( _, events ) =
                            update (TouchEnd []) moved
                    in
                    Expect.equal [ CameraSettled ] events
            , test "PointerTouchStart / PointerTouchEnd reports Tapped in canvas pixels with setCanvasOrigin" <|
                \_ ->
                    Viewport.setCanvasOrigin 300 56 idle
                        |> update (PointerTouchStart { id = 1, x = 310, y = 76 })
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ Tapped 10 20, CameraSettled ]
            , test "PointerTouchStart / PointerTouchMove settles camera after moving with setCanvasOrigin" <|
                \_ ->
                    Viewport.setCanvasOrigin 100 50 idle
                        |> update (PointerTouchStart { id = 1, x = 110, y = 60 })
                        |> Tuple.first
                        |> update (PointerTouchMove { id = 1, x = 210, y = 60 })
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ CameraSettled ]
            , test "CanvasPressed dynamically updates canvasOrigin from event" <|
                \_ ->
                    idle
                        |> update (CanvasPressed ( 250, 80 ) (MouseDown 260 100 False))
                        |> Tuple.first
                        |> update (MouseUp 260 100)
                        |> Tuple.second
                        |> Expect.equal [ Clicked 10 20 False ]
            , test "CanvasPressed with PointerTouchStart dynamically updates canvasOrigin" <|
                \_ ->
                    idle
                        |> update (CanvasPressed ( 150, 60 ) (PointerTouchStart { id = 1, x = 165, y = 85 }))
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ Tapped 15 25, CameraSettled ]
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
            , test "releasing a touch that moved settles the camera" <|
                \_ ->
                    idle
                        |> update (PointerTouchStart { id = 1, x = 5, y = 5 })
                        |> Tuple.first
                        |> update (PointerTouchMove { id = 1, x = 105, y = 5 })
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
        , describe "capturing a gesture"
            [ test "a mouse press reports where it started" <|
                \_ ->
                    update (MouseDown 10 20 False) idle
                        |> Tuple.second
                        |> Expect.equal [ PressStarted 10 20 ]
            , test "a release that did not start on the canvas is not a click" <|
                \_ ->
                    update (MouseUp 10 20) idle
                        |> Tuple.second
                        |> Expect.equal []
            , test "a shift-click says so" <|
                \_ ->
                    idle
                        |> update (MouseDown 10 20 True)
                        |> Tuple.first
                        |> update (MouseUp 10 20)
                        |> Tuple.second
                        |> Expect.equal [ Clicked 10 20 True ]
            , test "a captured mouse drag is reported and leaves the camera alone" <|
                \_ ->
                    let
                        pressed =
                            update (MouseDown 10 20 False) idle
                                |> Tuple.first
                                |> Viewport.captureDrag

                        ( moved, moveEvents ) =
                            update (MouseMove 50 60) pressed

                        ( released, upEvents ) =
                            update (MouseUp 50 60) moved
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ DragMoved 50 60 ] moveEvents
                        , \_ -> Expect.equal [ DragEnded 50 60 ] upEvents
                        , \_ -> Expect.within (Expect.Absolute 0.000001) pressed.camera.azimuth moved.camera.azimuth
                        , \_ -> Expect.equal False released.captured
                        ]
                        ()
            , test "after a captured drag the camera follows the mouse again" <|
                \_ ->
                    idle
                        |> update (MouseDown 10 20 False)
                        |> Tuple.first
                        |> Viewport.captureDrag
                        |> update (MouseUp 10 20)
                        |> Tuple.first
                        |> update (MouseDown 10 20 False)
                        |> Tuple.first
                        |> update (MouseMove 70 20)
                        |> Tuple.second
                        |> Expect.equal []
            , test "a tap with one finger is reported" <|
                \_ ->
                    idle
                        |> update (PointerTouchStart { id = 1, x = 30, y = 40 })
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ Tapped 30 40, CameraSettled ]
            , test "dragging a finger is not a tap" <|
                \_ ->
                    idle
                        |> update (PointerTouchStart { id = 1, x = 30, y = 40 })
                        |> Tuple.first
                        |> update (PointerTouchMove { id = 1, x = 130, y = 40 })
                        |> Tuple.first
                        |> update (PointerTouchEnd 1)
                        |> Tuple.second
                        |> Expect.equal [ CameraSettled ]
            , test "a finger press can be captured like a mouse press" <|
                \_ ->
                    let
                        ( down, downEvents ) =
                            update (PointerTouchStart { id = 1, x = 30, y = 40 }) idle

                        ( moved, moveEvents ) =
                            Viewport.captureDrag down
                                |> update (PointerTouchMove { id = 1, x = 90, y = 40 })

                        ( _, upEvents ) =
                            update (PointerTouchEnd 1) moved
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ PressStarted 30 40 ] downEvents
                        , \_ -> Expect.equal [ DragMoved 90 40 ] moveEvents
                        , \_ -> Expect.equal [ DragEnded 90 40 ] upEvents
                        ]
                        ()
            , test "a second finger ends a capture and becomes a camera gesture" <|
                \_ ->
                    let
                        captured =
                            update (PointerTouchStart { id = 1, x = 30, y = 40 }) idle
                                |> Tuple.first
                                |> Viewport.captureDrag
                    in
                    update (PointerTouchStart { id = 2, x = 80, y = 40 }) captured
                        |> Expect.all
                            [ \( _, events ) -> Expect.equal [ DragEnded 30 40 ] events
                            , \( m, _ ) -> Expect.equal False m.captured
                            ]
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
                            , targetRole = ""
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



-- ── Document mode ─────────────────────────────────────────────────────────────


brickLines : List LDrawLine
brickLines =
    [ Triangle { color = 16, p1 = vec3 -10 0 -10, p2 = vec3 10 0 -10, p3 = vec3 0 -20 0 } ]


studioConfig : Viewport.Config
studioConfig =
    { config
        | partCache =
            Dict.fromList [ ( "3001.dat", Loaded brickLines ) ]
    }


studio : Viewport.Model
studio =
    Viewport.init studioConfig |> Tuple.first


{-| A document with `n` bricks side by side, spaced so they do not overlap.
-}
bricks : Int -> Document
bricks n =
    List.range 0 (n - 1)
        |> List.foldl
            (\i doc ->
                Document.addPlacement "" { part = "3001.dat", color = 4, matrix = Mat4.makeTranslate3 (toFloat i * 40) 0 0 } doc
                    |> Tuple.first
            )
            Document.empty


loaded : Document -> Viewport.Model
loaded doc =
    Viewport.loadDocument doc studio
        |> (\( m, _, _ ) -> m)


sources : Viewport.Model -> List Int
sources model =
    model.instancedScene
        |> Maybe.map (\scene -> List.map .source scene.placements)
        |> Maybe.withDefault []


documentSuite : Test
documentSuite =
    describe "Viewport documents"
        [ test "loadDocument shows every placement through the instanced renderer" <|
            \_ ->
                let
                    ( model, _, events ) =
                        Viewport.loadDocument (bricks 3) studio
                in
                Expect.all
                    [ \_ -> Expect.equal [ LoadStarted, ModelReady ] events
                    , \m -> Expect.equal Ready m.loadPhase
                    , \m -> Expect.equal [ 0, 1, 2 ] (List.sort (sources m))
                    , \m -> Expect.equal Nothing m.scene
                    ]
                    model
        , test "an empty document is ready and has no bounds to frame" <|
            \_ ->
                loaded Document.empty
                    |> Expect.all
                        [ \m -> Expect.equal Ready m.loadPhase
                        , \m -> Expect.equal Nothing m.modelBounds
                        ]
        , test "adding a placement leaves the existing ones in place" <|
            \_ ->
                let
                    before =
                        loaded (bricks 2)

                    ( after, _, _ ) =
                        Viewport.applyDocument (bricks 3) before
                in
                Expect.equal [ 0, 1, 2 ] (List.sort (sources after))
        , test "removing a placement removes only that one" <|
            \_ ->
                let
                    ( after, _, _ ) =
                        Viewport.applyDocument (Document.removePlacement 1 (bricks 3)) (loaded (bricks 3))
                in
                Expect.equal [ 0, 2 ] (List.sort (sources after))
        , test "recolouring a placement changes its colour and nothing else" <|
            \_ ->
                let
                    ( after, _, _ ) =
                        Viewport.applyDocument
                            (Document.updatePlacement 1 (\p -> { p | color = 14 }) (bricks 3))
                            (loaded (bricks 3))

                    colours model =
                        model.instancedScene
                            |> Maybe.map (\scene -> scene.placements |> List.sortBy .source |> List.map (.color >> Math.Vector4.toRecord))
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (sources m)) 3
                    , \m ->
                        case colours m of
                            Just [ a, b, c ] ->
                                Expect.all
                                    [ \_ -> Expect.equal a c
                                    , \_ -> Expect.notEqual a b
                                    ]
                                    ()

                            _ ->
                                Expect.fail "expected three placements"
                    ]
                    after
        , test "moving a placement moves it" <|
            \_ ->
                let
                    ( after, _, _ ) =
                        Viewport.applyDocument
                            (Document.updatePlacement 0 (\p -> { p | matrix = Mat4.makeTranslate3 500 0 0 }) (bricks 1))
                            (loaded (bricks 1))
                in
                after.instancedScene
                    |> Maybe.map (\scene -> List.map (.center >> Vec3.getX) scene.placements)
                    |> Maybe.withDefault []
                    |> List.map (\x -> x > 400)
                    |> Expect.equal [ True ]
        , test "a part that is not cached is fetched, and appears once it arrives" <|
            \_ ->
                let
                    withNew =
                        Document.addPlacement "" { part = "9999.dat", color = 4, matrix = Mat4.identity } (bricks 1)
                            |> Tuple.first

                    ( waiting, _, _ ) =
                        Viewport.applyDocument withNew (loaded (bricks 1))

                    ( arrived, _, _ ) =
                        Viewport.update (PartLoaded "9999.dat" (Ok "3 16 0 0 0 10 0 0 0 0 10")) waiting
                            |> (\( m, c, e ) -> ( m, c, e ))
                in
                Expect.all
                    [ \_ -> Expect.equal (Just Loading) (Dict.get "9999.dat" waiting.partCache)
                    , \_ -> Expect.equal [ 0 ] (sources waiting)
                    , \_ -> Expect.equal [ 0, 1 ] (List.sort (sources arrived))
                    ]
                    ()
        , test "a part that fails to load does not block the document" <|
            \_ ->
                let
                    withNew =
                        Document.addPlacement "" { part = "9999.dat", color = 4, matrix = Mat4.identity } (bricks 1)
                            |> Tuple.first

                    ( waiting, _, _ ) =
                        Viewport.applyDocument withNew (loaded (bricks 1))

                    ( failed, _, _ ) =
                        Viewport.update (PartLoaded "9999.dat" (Err Http.NetworkError)) waiting
                in
                Expect.equal [ 0 ] (sources failed)
        , test "framing happens for the first content only" <|
            \_ ->
                let
                    ( first, _, _ ) =
                        Viewport.applyDocument (bricks 1) (loaded Document.empty)

                    ( second, _, _ ) =
                        Viewport.applyDocument (bricks 2) first
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing first.modelBounds
                    , \_ -> Expect.equal first.camera.target second.camera.target
                    ]
                    ()
        , test "pickSource finds the placement under the cursor" <|
            \_ ->
                let
                    model =
                        loaded (bricks 1)
                            |> (\m -> { m | width = 800, height = 600 })

                    framed =
                        Viewport.applyDocument (bricks 1) model |> (\( m, _, _ ) -> m)
                in
                ( Viewport.pickSource 400 300 framed, Viewport.pickSource 5 5 framed )
                    |> Expect.equal ( Just 0, Nothing )
        , test "pickSource prefers a smaller brick sitting on a large baseplate" <|
            \_ ->
                let
                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "base.dat", color = 2, matrix = Mat4.identity }
                            |> (\( d, _ ) -> Document.addPlacement "" { part = "brick.dat", color = 4, matrix = Mat4.identity } d)
                            |> Tuple.first

                    baseGeometry =
                        "3 16 -100 0 -100 100 0 -100 100 0 100\n3 16 -100 0 -100 100 0 100 -100 0 100"

                    brickGeometry =
                        "3 16 -5 0 -5 5 0 -5 5 0 5"

                    customCache =
                        Dict.fromList
                            [ ( "base.dat", Loaded (Parser.parseFile baseGeometry) )
                            , ( "brick.dat", Loaded (Parser.parseFile brickGeometry) )
                            ]

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                Expect.equal (Just 1) (Viewport.pickSource 400 300 model)
        , test "pickSource prefers smaller part on coplanar surface regardless of insertion order" <|
            \_ ->
                let
                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "brick.dat", color = 4, matrix = Mat4.identity }
                            |> (\( d, _ ) -> Document.addPlacement "" { part = "base.dat", color = 2, matrix = Mat4.identity } d)
                            |> Tuple.first

                    baseGeometry =
                        "3 16 -100 0 -100 100 0 -100 100 0 100\n3 16 -100 0 -100 100 0 100 -100 0 100"

                    brickGeometry =
                        "3 16 -5 0 -5 5 0 -5 5 0 5"

                    customCache =
                        Dict.fromList
                            [ ( "base.dat", Loaded (Parser.parseFile baseGeometry) )
                            , ( "brick.dat", Loaded (Parser.parseFile brickGeometry) )
                            ]

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                Expect.equal (Just 0) (Viewport.pickSource 400 300 model)
        , test "a ray inside a part's box that misses its triangles returns Nothing" <|
            \_ ->
                let
                    hollowGeometry =
                        "3 16 50 0 50 60 0 50 60 0 60\n2 16 -50 0 -50 60 0 60"

                    customCache =
                        Dict.fromList [ ( "hollow.dat", Loaded (Parser.parseFile hollowGeometry) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "hollow.dat", color = 4, matrix = Mat4.identity }
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                Expect.equal Nothing (Viewport.pickSource 400 300 model)
        , test "part seen through empty corner of nearer part's box is picked" <|
            \_ ->
                let
                    hollowGeometry =
                        "3 16 30 0 30 40 0 30 40 0 40\n2 16 -50 -50 -50 50 50 50"

                    solidGeometry =
                        "3 16 -20 0 -20 20 0 -20 0 0 20"

                    customCache =
                        Dict.fromList
                            [ ( "hollow.dat", Loaded (Parser.parseFile hollowGeometry) )
                            , ( "solid.dat", Loaded (Parser.parseFile solidGeometry) )
                            ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "hollow.dat", color = 1, matrix = Mat4.identity }
                            |> (\( d, _ ) -> Document.addPlacement "" { part = "solid.dat", color = 4, matrix = Mat4.identity } d)
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                Expect.equal (Just 1) (Viewport.pickSource 400 300 model)
        , test "mirrored or scaled placement matrix picks at the right depth" <|
            \_ ->
                let
                    triangleGeom =
                        "3 16 -10 0 -10 10 0 -10 0 0 10"

                    customCache =
                        Dict.fromList [ ( "tri.dat", Loaded (Parser.parseFile triangleGeom) ) ]

                    scaleMatrix =
                        Mat4.mul (Mat4.makeScale3 -2 2 2) (Mat4.makeTranslate3 0 0 -50)

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "tri.dat", color = 4, matrix = scaleMatrix }
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                case Viewport.pickPoint 400 300 model of
                    Just hit ->
                        Expect.all
                            [ \_ -> Expect.equal 0 hit.source
                            , \_ -> Expect.within (Expect.Absolute 1.0) 100.0 (Vec3.getZ hit.point)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "expected hit on scaled part"
        , test "eye inside a part's box ignores hits in front of the near plane" <|
            \_ ->
                let
                    boxGeom =
                        "3 16 -10 -10 0.05 10 -10 0.05 0 10 0.05\n2 16 -20 -20 -20 20 20 20"

                    customCache =
                        Dict.fromList [ ( "box.dat", Loaded (Parser.parseFile boxGeom) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "box.dat", color = 4, matrix = Mat4.identity }
                            |> Tuple.first

                    ( loadedModel, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }

                    insideCam =
                        let
                            c =
                                loadedModel.camera
                        in
                        { c | azimuth = 0, elevation = 0, distance = 400, target = vec3 0 0 -400 }

                    modelWithEyeInside =
                        { loadedModel | camera = insideCam }
                in
                Expect.equal Nothing (Viewport.pickSource 400 300 modelWithEyeInside)
        , test "picking matches LOD proxy boxes when placement is at Far LOD" <|
            \_ ->
                let
                    cornerGeom =
                        "3 16 0 0 0 1 0 0 0 1 0\n2 16 -5 -5 -5 5 5 5"

                    customCache =
                        Dict.fromList [ ( "small.dat", Loaded (Parser.parseFile cornerGeom) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "small.dat", color = 4, matrix = Mat4.identity }
                            |> Tuple.first

                    ( loadedModel, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }

                    farCam =
                        let
                            c =
                                loadedModel.camera
                        in
                        { c | distance = 10000.0 }

                    farModel =
                        { loadedModel | camera = farCam }
                in
                Expect.equal (Just 0) (Viewport.pickSource 400 300 farModel)
        , test "parts with edge lines only fall back to box entry so they remain pickable" <|
            \_ ->
                let
                    linesOnlyGeom =
                        "2 16 -10 0 0 10 0 0\n2 16 0 -10 0 0 10 0"

                    customCache =
                        Dict.fromList [ ( "cross.dat", Loaded (Parser.parseFile linesOnlyGeom) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "cross.dat", color = 4, matrix = Mat4.identity }
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                Expect.equal (Just 0) (Viewport.pickSource 400 300 model)
        , test "a part added while loading is fetched and appears when resolved" <|
            \_ ->
                let
                    docWithNew =
                        Document.addPlacement "" { part = "9999.dat", color = 4, matrix = Mat4.identity } (bricks 1)
                            |> Tuple.first

                    ( initialLoading, _, _ ) =
                        Viewport.loadDocument (bricks 1) { studio | partCache = Dict.empty }

                    ( waiting, _, _ ) =
                        Viewport.applyDocument docWithNew initialLoading

                    ( initialBrickLoaded, _, _ ) =
                        Viewport.update (PartLoaded "3001.dat" (Ok "3 16 0 0 0 10 0 0 0 0 10")) waiting

                    ( secondLoaded, _, _ ) =
                        Viewport.update (PartLoaded "9999.dat" (Ok "3 16 0 0 0 10 0 0 0 0 10")) initialBrickLoaded
                in
                Expect.equal [ 0, 1 ] (List.sort (sources secondLoaded))
        , test "a removed submodel is cleaned from partCache on loadDocument" <|
            \_ ->
                let
                    mpd1 =
                        Document.parse "0 FILE main.ldr\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 sub.ldr\n0 FILE sub.ldr\n3 16 0 0 0 10 0 0 0 0 10\n"

                    mpd2 =
                        Document.parse "0 FILE main.ldr\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"

                    ( loaded1, _, _ ) =
                        Viewport.loadDocument mpd1 studio

                    ( loaded2, _, _ ) =
                        Viewport.loadDocument mpd2 loaded1
                in
                Expect.all
                    [ \_ -> Expect.equal True (Dict.member "sub.ldr" loaded1.partCache)
                    , \_ -> Expect.equal False (Dict.member "sub.ldr" loaded2.partCache)
                    ]
                    ()
        , test "setActivePath exposes child placements as sources and filters pickSource to the active submodel" <|
            \_ ->
                let
                    mpd =
                        String.join "\n"
                            [ "0 FILE main.ldr"
                            , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                            , "1 16 100 0 0 1 0 0 0 1 0 0 0 1 sub.ldr"
                            , "0 FILE sub.ldr"
                            , "1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                            , ""
                            ]

                    doc =
                        Document.parse mpd

                    ( topLevel, _, _ ) =
                        Viewport.loadDocument doc { studio | width = 800, height = 600 }

                    insideSub =
                        Viewport.setActivePath [ 1 ] topLevel

                    backToTop =
                        Viewport.setActivePath [] insideSub
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0, 1 ] (List.sort (sources topLevel))
                    , \_ -> Expect.equal [ 0, 2 ] (List.sort (sources insideSub))
                    , \_ -> Expect.equal (Just (Set.singleton 2)) insideSub.activeSources
                    , \_ -> Expect.equal [ 0, 1 ] (List.sort (sources backToTop))
                    , \_ -> Expect.equal Nothing backToTop.activeSources
                    ]
                    ()
        , test "pickSurfaceExcluding hits placement when not excluded" <|
            \_ ->
                let
                    triangleGeom =
                        "3 16 -50 0 -50 50 0 -50 0 0 50\n2 16 -50 -50 -50 50 50 50"

                    customCache =
                        Dict.fromList [ ( "plate.dat", Loaded (Parser.parseFile triangleGeom) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "plate.dat", color = 4, matrix = Mat4.makeTranslate3 0 -24 0 }
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                case Viewport.pickSurfaceExcluding Set.empty 400 300 model of
                    Just hit ->
                        Expect.all
                            [ \_ -> Expect.equal 0 hit.source
                            , \_ -> Expect.within (Expect.Absolute 1.0) -24.0 hit.topY
                            ]
                            ()

                    Nothing ->
                        Expect.fail "expected hit on placement"
        , test "pickSurfaceExcluding falls back to ground plane (source = -1, topY = 0) when hit placement is excluded" <|
            \_ ->
                let
                    triangleGeom =
                        "3 16 -50 0 -50 50 0 -50 0 0 50\n2 16 -50 -50 -50 50 50 50"

                    customCache =
                        Dict.fromList [ ( "plate.dat", Loaded (Parser.parseFile triangleGeom) ) ]

                    doc =
                        Document.empty
                            |> Document.addPlacement "" { part = "plate.dat", color = 4, matrix = Mat4.makeTranslate3 0 -24 0 }
                            |> Tuple.first

                    ( model, _, _ ) =
                        Viewport.loadDocument doc { studio | partCache = customCache, width = 800, height = 600 }
                in
                case Viewport.pickSurfaceExcluding (Set.singleton 0) 400 300 model of
                    Just hit ->
                        Expect.all
                            [ \_ -> Expect.equal -1 hit.source
                            , \_ -> Expect.within (Expect.Absolute 1.0e-5) 0.0 hit.topY
                            , \_ -> Expect.within (Expect.Absolute 1.0e-5) 0.0 (Vec3.getY hit.point)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "expected ground plane hit when placement is excluded"
        , test "groundIntersection intersects horizontal plane at Y = 0" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        Viewport.loadDocument Document.empty { studio | width = 800, height = 600 }
                in
                case Viewport.groundIntersection 400 300 model of
                    Just pt ->
                        Expect.within (Expect.Absolute 1.0e-5) 0.0 (Vec3.getY pt)

                    Nothing ->
                        Expect.fail "expected ground intersection"
        ]
