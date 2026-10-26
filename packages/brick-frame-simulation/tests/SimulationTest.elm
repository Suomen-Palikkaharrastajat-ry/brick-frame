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
import Math.Vector3 as Vec3
import Render.Camera as Camera
import Render.Style as Style
import Set
import Simulation exposing (Event(..), Msg(..))
import Simulation.Meshes as Meshes
import Test exposing (Test, describe, test)
import Time
import WebGL


specs : List GearSpec
specs =
    [ { partFile = "3647.dat", teeth = 8, pitchRadius = 10 }
    , { partFile = "4019.dat", teeth = 16, pitchRadius = 20 }
    , { partFile = "3648.dat", teeth = 24, pitchRadius = 30 }
    , { partFile = "3648b.dat", teeth = 24, pitchRadius = 30 }
    , { partFile = "3649.dat", teeth = 40, pitchRadius = 50 }
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


{-| Two 24T gears 60 LDU apart along X, both turning about Z (their pitch radii
sum), with an axle along Z through the second one.
-}
drivenLines : List LDrawLine
drivenLines =
    [ gear24 0, gear24 60, axleAlongZ 60 0 ]


gear24 : Float -> LDrawLine
gear24 x =
    SubFileRef { color = 4, transform = Mat4.makeTranslate3 x 0 0, file = "3648.dat" }


{-| An axle along Z at (x, y, 0): its local X turned to Z.
-}
axleAlongZ : Float -> Float -> LDrawLine
axleAlongZ x y =
    SubFileRef
        { color = 0
        , transform = Mat4.mul (Mat4.makeTranslate3 x y 0) (Mat4.makeRotate (pi / 2) (Vec3.vec3 0 1 0))
        , file = "3705.dat"
        }


{-| A pin along Z at (x, y, 0): its local X turned to Z.
-}
pinAlongZ : Float -> Float -> LDrawLine
pinAlongZ x y =
    SubFileRef
        { color = 0
        , transform = Mat4.mul (Mat4.makeTranslate3 x y 0) (Mat4.makeRotate (pi / 2) (Vec3.vec3 0 1 0))
        , file = "2780.dat"
        }


drivenCache : Resolve.PartCache
drivenCache =
    Dict.fromList
        [ ( "3648.dat", Loaded [] )
        , ( "3648b.dat", Loaded [] )
        , ( "3649.dat", Loaded [] )
        , ( "3705.dat", Loaded [] )
        , ( "2780.dat", Loaded [] )
        ]


{-| The model with the driven layout, plus an axle 200 LDU away that nothing drives.
-}
drivenModel : Simulation.Model
drivenModel =
    Simulation.init config
        |> Simulation.analyse { lines = drivenLines ++ [ axleAlongZ 200 0 ], cache = drivenCache }
        |> Tuple.first


drivenPartsFor : List Int -> Simulation.Model -> List Simulation.DrivenPart
drivenPartsFor ids =
    Simulation.drivenPartsFor (Set.fromList ids)


drivenEntities : List Int -> Simulation.Model -> List WebGL.Entity
drivenEntities ids =
    Simulation.drivenEntitiesFor (Set.fromList ids) Camera.init Style.defaultStyle 1.0


originX : Simulation.DrivenPart -> Float
originX drivenPart =
    Vec3.getX (Mat4.transform drivenPart.transform (Vec3.vec3 0 0 0))


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


{-| An analysed model of the given lines, read at t = 1 s with the motor on gear 0.
-}
meshModel : List LDrawLine -> Simulation.Model
meshModel lines =
    Simulation.init config
        |> Simulation.analyse { lines = lines, cache = Dict.union drivenCache cache }
        |> Tuple.first
        |> Simulation.setTime 1.0


{-| A gear at (x, 0, 0), turned 180° about X when flipped, so its local +Z points the other way.
-}
gearAt : String -> Bool -> Float -> LDrawLine
gearAt file flipped x =
    let
        turn =
            if flipped then
                Mat4.makeRotate pi (Vec3.vec3 1 0 0)

            else
                Mat4.identity
    in
    SubFileRef { color = 4, transform = Mat4.mul (Mat4.makeTranslate3 x 0 0) turn, file = file }


signum : Float -> Float
signum x =
    if x < 0 then
        -1

    else
        1


{-| The sense, +1 or -1, in which a rotation by `angle` about `axis` turns as seen
along the world Z axis (where all test gears point along ±Z).
-}
visualSense : Float -> Vec3.Vec3 -> Float
visualSense angle axis =
    signum (angle * Vec3.getZ axis)


{-| The visual sense of a gear, from its rendered axis and its angle.
-}
gearSense : Simulation.Model -> Int -> Maybe Float
gearSense m gearId =
    Maybe.map2 visualSense
        (Dict.get gearId m.gearAngles)
        (Dict.get gearId m.gearMeshes |> Maybe.map .axis)


{-| The product of the visual senses of gears 0 and 1: -1 when they turn opposite ways, +1 when together.
-}
oppositeWays : Simulation.Model -> Float
oppositeWays m =
    Maybe.map2 (*) (gearSense m 0) (gearSense m 1)
        |> Maybe.withDefault 0


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
        , describe "host clock"
            [ test "setTime turns the gears by ratio x speed x t, meshing gears opposite ways" <|
                \_ ->
                    let
                        at =
                            Simulation.setTime 0.5 analysed
                    in
                    ( Dict.get 0 at.gearAngles, Dict.get 1 at.gearAngles )
                        |> Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.0001) 0.5 at.playback.currentTime
                            , \( motor, _ ) -> Expect.within (Expect.Absolute 0.0001) pi (Maybe.withDefault 0 motor)
                            , \( _, driven ) -> Expect.within (Expect.Absolute 0.0001) (-pi / 2) (Maybe.withDefault 0 driven)
                            ]
            , test "setTime is a pure function of the time" <|
                \_ ->
                    Simulation.setTime 0.3 (Simulation.setTime 7 analysed)
                        |> .gearAngles
                        |> Expect.equal (Simulation.setTime 0.3 analysed).gearAngles
            , test "rotatingGearIds lists the motor's gear train, not a lone gear" <|
                \_ ->
                    Simulation.init config
                        |> Simulation.analyse { lines = meshingPair ++ [ part "3647.dat" 500 ], cache = cache }
                        |> Tuple.first
                        |> Simulation.rotatingGearIds
                        |> Expect.equal (Set.fromList [ 0, 1 ])
            , test "rotatingGearIds leaves out gears without a loaded mesh" <|
                \_ ->
                    Simulation.init config
                        |> Simulation.analyse { lines = meshingPair, cache = Dict.remove "4019.dat" cache }
                        |> Tuple.first
                        |> Simulation.rotatingGearIds
                        |> Expect.equal (Set.fromList [ 0 ])
            ]
        , describe "true axes"
            [ test "a flipped gear is rendered about its own true axis" <|
                \_ ->
                    let
                        m =
                            meshModel [ gearAt "3648.dat" False 0, gearAt "4019.dat" True 50 ]
                    in
                    [ Dict.get 0 m.gearMeshes, Dict.get 1 m.gearMeshes ]
                        |> List.map (Maybe.map (\g -> signum (Vec3.getZ g.axis)))
                        |> Expect.equal [ Just -1, Just 1 ]
            , test "a meshing pair turns opposite ways on screen when one gear is flipped" <|
                \_ ->
                    oppositeWays (meshModel [ gearAt "3648.dat" False 0, gearAt "4019.dat" True 50 ])
                        |> Expect.equal -1
            , test "a same-orientation meshing pair still turns opposite ways" <|
                \_ ->
                    oppositeWays (meshModel [ gearAt "3648.dat" False 0, gearAt "4019.dat" False 50 ])
                        |> Expect.equal -1
            , test "a flipped co-axial gear turns with its partner" <|
                \_ ->
                    oppositeWays (meshModel [ gearAt "3648.dat" False 0, gearAt "3647.dat" True 0 ])
                        |> Expect.equal 1
            , test "a same-orientation co-axial gear turns with its partner" <|
                \_ ->
                    oppositeWays (meshModel [ gearAt "3648.dat" False 0, gearAt "3647.dat" False 0 ])
                        |> Expect.equal 1
            , test "a driven axle on a flipped gear turns with it" <|
                \_ ->
                    let
                        m =
                            meshModel [ gearAt "3648.dat" True 0, axleAlongZ 0 0 ]
                    in
                    case ( m.componentMeshes, Dict.get 0 m.gearAngles ) of
                        ( [ axle ], Just angle ) ->
                            Expect.equal (gearSense m 0) (Just (visualSense angle axle.axis))

                        _ ->
                            Expect.fail "expected one driven axle on the gear"
            ]
        , describe "driven parts"
            [ test "an axle threaded on a gear is driven by it; a free axle is not" <|
                \_ ->
                    drivenPartsFor [ 0, 1 ] drivenModel
                        |> Expect.all
                            [ \parts -> Expect.equal [ 1 ] (List.map .drivingGearId parts)
                            , \parts -> Expect.equal [ "3705.dat" ] (List.map .file parts)
                            , \parts -> Expect.equal [ 60 ] (List.map originX parts)
                            ]
            , test "the driven axle turns with its gear's angle" <|
                \_ ->
                    let
                        at =
                            Simulation.setTime 0.5 drivenModel
                    in
                    ( Dict.get 1 at.gearAngles, List.length (drivenEntities [ 1 ] at) )
                        |> Expect.all
                            [ \( angle, _ ) -> Expect.within (Expect.Absolute 0.0001) -pi (Maybe.withDefault 0 angle)
                            , \( _, count ) -> Expect.equal 2 count
                            ]
            , test "only parts of the given gears are drawn turning" <|
                \_ ->
                    drivenEntities [ 0 ] drivenModel
                        |> Expect.equal []
            , test "an axle 2 LDU off the gear's axis is driven, 4 LDU off is not" <|
                \_ ->
                    let
                        drivenCount y =
                            Simulation.init config
                                |> Simulation.analyse { lines = [ gear24 0, gear24 60, axleAlongZ 60 y ], cache = drivenCache }
                                |> Tuple.first
                                |> drivenPartsFor [ 0, 1 ]
                                |> List.length
                    in
                    Expect.equal ( 1, 0 ) ( drivenCount 2, drivenCount 4 )
            , test "a gear turned onto an X-axis axle on Brick Contraptions' axle line drives it" <|
                \_ ->
                    -- Contraptions adds gears and axles at Y -60 (its axle line); an axle
                    -- runs along X by default, and a gear turned to X slides onto it.
                    let
                        turnedGear =
                            SubFileRef
                                { color = 7
                                , transform = Mat4.mul (Mat4.makeTranslate3 40 -60 0) (Mat4.makeRotate (pi / 2) (Vec3.vec3 0 1 0))
                                , file = "3648.dat"
                                }

                        axleX =
                            SubFileRef { color = 0, transform = Mat4.makeTranslate3 40 -60 0, file = "3705.dat" }
                    in
                    Simulation.init config
                        |> Simulation.analyse { lines = [ turnedGear, axleX ], cache = drivenCache }
                        |> Tuple.first
                        |> drivenPartsFor [ 0 ]
                        |> List.map (\dp -> ( dp.drivingGearId, dp.file ))
                        |> Expect.equal [ ( 0, "3705.dat" ) ]
            , test "a driven part is only listed when its mesh has loaded" <|
                \_ ->
                    Simulation.init config
                        |> Simulation.analyse { lines = drivenLines, cache = Dict.remove "3705.dat" drivenCache }
                        |> Tuple.first
                        |> drivenPartsFor [ 0, 1 ]
                        |> Expect.equal []
            , test "a pin placed at (10, 10, 0) on a 24T gear is detected as a driven part" <|
                \_ ->
                    let
                        gear =
                            SubFileRef { color = 4, transform = Mat4.identity, file = "3648b.dat" }

                        pin =
                            pinAlongZ 10 10

                        m =
                            Simulation.init config
                                |> Simulation.analyse { lines = [ gear, pin ], cache = drivenCache }
                                |> Tuple.first
                    in
                    drivenPartsFor [ 0 ] m
                        |> List.map (\dp -> ( dp.drivingGearId, dp.file ))
                        |> Expect.equal [ ( 0, "2780.dat" ) ]
            , test "an off-center pin and axle on a 40T gear are detected as driven parts" <|
                \_ ->
                    let
                        gear =
                            SubFileRef { color = 4, transform = Mat4.identity, file = "3649.dat" }

                        axleOffCenter =
                            axleAlongZ 0 20

                        pinOffCenter =
                            pinAlongZ 10 30

                        m =
                            Simulation.init config
                                |> Simulation.analyse { lines = [ gear, axleOffCenter, pinOffCenter ], cache = drivenCache }
                                |> Tuple.first
                    in
                    drivenPartsFor [ 0 ] m
                        |> List.map (\dp -> ( dp.drivingGearId, dp.file ))
                        |> Expect.equal [ ( 0, "3705.dat" ), ( 0, "2780.dat" ) ]
            , test "at t > 0, drivenEntitiesFor produces the rendered pin entity orbiting the gear axis" <|
                \_ ->
                    let
                        gear =
                            SubFileRef { color = 4, transform = Mat4.identity, file = "3648b.dat" }

                        pin =
                            pinAlongZ 10 10

                        m =
                            Simulation.init config
                                |> Simulation.analyse { lines = [ gear, pin ], cache = drivenCache }
                                |> Tuple.first
                                |> Simulation.setTime 0.5
                    in
                    case m.componentMeshes of
                        [ pinMesh ] ->
                            let
                                angle =
                                    Dict.get 0 m.gearAngles |> Maybe.withDefault 0.0

                                rotMat =
                                    Meshes.rotationAround angle pinMesh.axis pinMesh.rotationCenter

                                orbitedPos =
                                    Mat4.transform rotMat pinMesh.center
                            in
                            { count = List.length (drivenEntities [ 0 ] m)
                            , rotCenter = pinMesh.rotationCenter
                            , radius = Vec3.distance pinMesh.center pinMesh.rotationCenter
                            , hasMoved = Vec3.distance orbitedPos pinMesh.center > 1.0
                            }
                                |> Expect.all
                                    [ \r -> Expect.equal 2 r.count
                                    , \r -> Expect.within (Expect.Absolute 0.0001) 0 (Vec3.length r.rotCenter)
                                    , \r -> Expect.within (Expect.Absolute 0.01) (sqrt 200) r.radius
                                    , \r -> Expect.equal True r.hasMoved
                                    ]

                        _ ->
                            Expect.fail "expected one component mesh for the pin"
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
