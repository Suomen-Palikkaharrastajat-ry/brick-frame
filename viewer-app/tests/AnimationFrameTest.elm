module AnimationFrameTest exposing (suite)

{-| Tests for `Main.needsAnimationFrame`, the predicate that gates the
`onAnimationFrame` subscription so idle models stop redrawing at 60 fps.
-}

import Expect
import Main exposing (Flags, HeldControl(..), LoadPhase(..))
import Set
import Test exposing (Test, describe, test)


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
                    playback =
                        idleModel.playback
                in
                Main.needsAnimationFrame
                    { idleModel | playback = { playback | running = True } }
                    |> Expect.equal True
        , test "flattening geometry → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | loadPhase = FlatteningGeometry }
                    |> Expect.equal True
        , test "held rotate control → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | heldControl = HoldRotate 1 0 }
                    |> Expect.equal True
        , test "held movement key → True" <|
            \_ ->
                Main.needsAnimationFrame
                    { idleModel | heldKeys = Set.singleton "w" }
                    |> Expect.equal True
        , test "held speed modifier alone → False" <|
            \_ ->
                -- Shift changes nothing on its own, so it must not keep the
                -- render loop awake.
                Main.needsAnimationFrame
                    { idleModel | heldKeys = Set.singleton "Shift" }
                    |> Expect.equal False
        ]
