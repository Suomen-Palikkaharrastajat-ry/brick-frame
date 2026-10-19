module AnimationFrameTest exposing (suite)

{-| Tests for `Main.needsAnimationFrame`, the predicate that gates the
`onAnimationFrame` subscription so idle models stop redrawing at 60 fps.
-}

import Expect
import Main exposing (Flags)
import Set
import Test exposing (Test, describe, test)
import Viewport exposing (HeldControl(..), LoadPhase(..))


{-| Flags that produce an idle baseline model: an empty `defaultModelUrl` leaves
`loadPhase = Idle`, so nothing is loading and no model is fetched.
-}
testFlags : Flags
testFlags =
    { ldrawBase = ""
    , ldrawFallbackBase = ""
    , defaultModelUrl = ""
    , initialHash = ""
    , maxRpm = 120
    , uiMode = "app"
    , controlsEnabled = True
    , initialMotorIndex = -1
    , initialRpm = 0
    , useWindowResize = False
    , ambientStrength = Nothing
    , lightStrength = Nothing
    , vibrance = Nothing
    , edgeWidth = Nothing
    , supportsPointerEvents = True
    }


idleModel : Main.Model
idleModel =
    Main.init testFlags |> Tuple.first


withViewport : (Viewport.Model -> Viewport.Model) -> Viewport.Model
withViewport change =
    change idleModel.viewport


suite : Test
suite =
    describe "Main.needsAnimationFrame"
        [ test "idle, stopped, no held control → False" <|
            \_ ->
                Main.needsAnimationFrame idleModel
                    |> Expect.equal False
        , test "playback running → True" <|
            \_ ->
                let
                    simulation =
                        idleModel.simulation

                    playback =
                        simulation.playback
                in
                Main.needsAnimationFrame
                    { idleModel | simulation = { simulation | playback = { playback | running = True } } }
                    |> Expect.equal True
        , test "flattening geometry → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | viewport = withViewport (\v -> { v | loadPhase = FlatteningGeometry }) }
                    |> Expect.equal True
        , test "held rotate control → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | viewport = withViewport (\v -> { v | heldControl = HoldRotate 1 0 }) }
                    |> Expect.equal True
        , test "held movement key → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | viewport = withViewport (\v -> { v | heldKeys = Set.singleton "w" }) }
                    |> Expect.equal True
        , test "held speed modifier alone → False" <|
            \_ ->
                -- Shift changes nothing on its own, so it must not keep the
                -- render loop awake.
                Main.needsAnimationFrame
                    { idleModel | viewport = withViewport (\v -> { v | heldKeys = Set.singleton "Shift" }) }
                    |> Expect.equal False
        ]
