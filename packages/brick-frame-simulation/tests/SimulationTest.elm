module SimulationTest exposing (suite)

{-| Tests for the simulation component: analysis of a loaded model, the
playback clock and the motor controls.
-}

import Dict
import Expect
import Gear.Types exposing (GearSpec)
import LDraw.Resolve as Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Render.Camera as Camera
import Simulation exposing (Event(..), Msg(..))
import Test exposing (Test, describe, test)
import Time


specs : List GearSpec
specs =
    [ { partFile = "3647.dat", teeth = 8, pitchRadius = 10 }
    , { partFile = "4019.dat", teeth = 16, pitchRadius = 20 }
    ]


config : Simulation.Config
config =
    { gearSpecs = specs
    , lodCache = Resolve.initCache
    , maxRpm = 120
    , initialRpm = 60
    , requestedMotorIndex = Nothing
    }


part : String -> Float -> LDrawLine
part file x =
    SubFileRef { color = 4, transform = Mat4.makeTranslate3 x 0 0, file = file }


{-| An 8T and a 16T gear 30 LDU apart (their pitch radii sum), plus one brick.
-}
meshingPair : List LDrawLine
meshingPair =
    [ part "3647.dat" 0, part "4019.dat" 30, part "3001.dat" 100 ]


cache : Resolve.PartCache
cache =
    Dict.fromList
        [ ( "3647.dat", Loaded [] )
        , ( "4019.dat", Loaded [] )
        , ( "3001.dat", Loaded [] )
        ]


analysed : Simulation.Model
analysed =
    Simulation.init config
        |> Simulation.analyse { lines = meshingPair, cache = cache }
        |> Tuple.first


time : Int -> Time.Posix
time =
    Time.millisToPosix


suite : Test
suite =
    describe "Simulation"
        [ describe "init"
            [ test "the initial RPM becomes the motor speed" <|
                \_ ->
                    Simulation.init config
                        |> .motor
                        |> .speedRadPerSec
                        |> Expect.within (Expect.Absolute 0.0001) (2 * pi)
            , test "the initial RPM is clamped to the maximum" <|
                \_ ->
                    Simulation.init { config | initialRpm = 1000 }
                        |> .motor
                        |> .speedRadPerSec
                        |> Expect.within (Expect.Absolute 0.0001) (4 * pi)
            , test "nothing is detected before a model has been analysed" <|
                \_ ->
                    Simulation.init config
                        |> Expect.all
                            [ \m -> Expect.equal False m.simulationChecked
                            , \m -> Expect.equal False m.simulationAvailable
                            , \m -> Expect.equal Nothing m.gearGraph
                            ]
            ]
        , describe "analyse"
            [ test "detects the gears and picks the first as motor" <|
                \_ ->
                    Expect.all
                        [ \m -> Expect.equal True m.simulationAvailable
                        , \m -> Expect.equal True m.simulationChecked
                        , \m -> Expect.equal (Just 0) m.motor.drivingGearId
                        ]
                        analysed
            , test "leaves gears out of the static lines but keeps other parts" <|
                \_ ->
                    Simulation.analyse { lines = meshingPair, cache = cache } (Simulation.init config)
                        |> Tuple.second
                        |> Expect.all
                            [ \a -> Expect.equal 2 a.gearCount
                            , \a -> Expect.equal [ "3001.dat" ] (fileNames a.staticLines)
                            , \a -> Expect.equal [ "3001.dat" ] (fileNames a.fallbackLines)
                            ]
            , test "the requested motor index picks another gear" <|
                \_ ->
                    Simulation.init { config | requestedMotorIndex = Just 1 }
                        |> Simulation.analyse { lines = meshingPair, cache = cache }
                        |> Tuple.first
                        |> .motor
                        |> .drivingGearId
                        |> Expect.equal (Just 1)
            , test "an out-of-range motor index falls back to the first gear" <|
                \_ ->
                    Simulation.init { config | requestedMotorIndex = Just 9 }
                        |> Simulation.analyse { lines = meshingPair, cache = cache }
                        |> Tuple.first
                        |> .motor
                        |> .drivingGearId
                        |> Expect.equal (Just 0)
            , test "a model without gears is checked but unavailable" <|
                \_ ->
                    Simulation.init config
                        |> Simulation.analyse { lines = [ part "3001.dat" 0 ], cache = cache }
                        |> Tuple.first
                        |> Expect.all
                            [ \m -> Expect.equal True m.simulationChecked
                            , \m -> Expect.equal False m.simulationAvailable
                            ]
            ]
        , describe "playback"
            [ test "Play starts the clock and reports it" <|
                \_ ->
                    Simulation.update Play analysed
                        |> Expect.all
                            [ \( m, _ ) -> Expect.equal True m.playback.running
                            , \( _, events ) -> Expect.equal [ PlayStateChanged True 0 ] events
                            ]
            , test "Tick advances time only while running" <|
                \_ ->
                    let
                        started =
                            Simulation.update Play analysed |> Tuple.first

                        first =
                            Simulation.update (Tick (time 1000)) started |> Tuple.first

                        second =
                            Simulation.update (Tick (time 3000)) first |> Tuple.first
                    in
                    Expect.within (Expect.Absolute 0.0001) 2 second.playback.currentTime
            , test "Tick while paused does not move the clock" <|
                \_ ->
                    Simulation.update (Tick (time 5000)) analysed
                        |> Tuple.first
                        |> .playback
                        |> .currentTime
                        |> Expect.equal 0
            , test "gear angles follow the clock by the gear ratio" <|
                \_ ->
                    let
                        running =
                            Simulation.update Play analysed
                                |> Tuple.first
                                |> Simulation.update (Tick (time 0))
                                |> Tuple.first
                                |> Simulation.update (Tick (time 1000))
                                |> Tuple.first
                    in
                    ( Dict.get 0 running.gearAngles, Dict.get 1 running.gearAngles )
                        |> Expect.all
                            [ \( motor, _ ) -> Expect.within (Expect.Absolute 0.0001) (2 * pi) (Maybe.withDefault 0 motor)
                            , \( motor, driven ) ->
                                -- 8T drives 16T: half the angle, opposite way.
                                Expect.within (Expect.Absolute 0.0001)
                                    (-(Maybe.withDefault 0 motor) / 2)
                                    (Maybe.withDefault 0 driven)
                            ]
            , test "Stop rewinds and reports time zero" <|
                \_ ->
                    let
                        played =
                            Simulation.update Play analysed
                                |> Tuple.first
                                |> Simulation.update (Tick (time 0))
                                |> Tuple.first
                                |> Simulation.update (Tick (time 500))
                                |> Tuple.first
                    in
                    Simulation.update Stop played
                        |> Expect.all
                            [ \( m, _ ) -> Expect.equal False m.playback.running
                            , \( m, _ ) -> Expect.equal 0 m.playback.currentTime
                            , \( _, events ) -> Expect.equal [ PlayStateChanged False 0 ] events
                            ]
            , test "ToggleMotor alternates between Play and Pause" <|
                \_ ->
                    let
                        once =
                            Simulation.update ToggleMotor analysed |> Tuple.first

                        twice =
                            Simulation.update ToggleMotor once |> Tuple.first
                    in
                    ( once.playback.running, twice.playback.running )
                        |> Expect.equal ( True, False )
            ]
        , describe "motor"
            [ test "SetMotorGear changes the driving gear" <|
                \_ ->
                    Simulation.update (SetMotorGear 1) analysed
                        |> Tuple.first
                        |> .motor
                        |> .drivingGearId
                        |> Expect.equal (Just 1)
            , test "SetMotorSpeed is clamped to the maximum RPM" <|
                \_ ->
                    Simulation.update (SetMotorSpeed 500) analysed
                        |> Tuple.first
                        |> .motor
                        |> .speedRadPerSec
                        |> Expect.within (Expect.Absolute 0.0001) (4 * pi)
            , test "negative speeds run the motor backwards" <|
                \_ ->
                    Simulation.update (SetMotorSpeed -30) analysed
                        |> Tuple.first
                        |> .motor
                        |> .speedRadPerSec
                        |> Expect.within (Expect.Absolute 0.0001) -pi
            ]
        , describe "lifecycle"
            [ test "reset forgets the analysed model" <|
                \_ ->
                    Simulation.reset analysed
                        |> Expect.all
                            [ \m -> Expect.equal False m.simulationChecked
                            , \m -> Expect.equal Nothing m.gearGraph
                            , \m -> Expect.equal Dict.empty m.gearAngles
                            ]
            , test "markUnavailable is checked-but-unavailable" <|
                \_ ->
                    Simulation.markUnavailable analysed
                        |> Expect.all
                            [ \m -> Expect.equal True m.simulationChecked
                            , \m -> Expect.equal False m.simulationAvailable
                            , \m -> Expect.equal Nothing m.gearGraph
                            ]
            , test "needsAnimationFrame only while playing" <|
                \_ ->
                    ( Simulation.needsAnimationFrame analysed
                    , Simulation.needsAnimationFrame (Simulation.update Play analysed |> Tuple.first)
                    )
                        |> Expect.equal ( False, True )
            ]
        , describe "picking"
            [ test "nothing is picked before a model has been analysed" <|
                \_ ->
                    Simulation.pickGear Camera.init 800 600 400 300 (Simulation.init config)
                        |> Expect.equal Nothing
            ]
        ]


fileNames : List LDrawLine -> List String
fileNames lines =
    List.filterMap
        (\line ->
            case line of
                SubFileRef ref ->
                    Just ref.file

                _ ->
                    Nothing
        )
        lines
