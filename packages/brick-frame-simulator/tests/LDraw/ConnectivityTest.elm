module LDraw.ConnectivityTest exposing (suite)

{-| Unit tests for LDraw.Connectivity: connection detection, snap transformation math,
and 3D SAT collision detection.
-}

import Expect
import LDraw.Connectivity as Connectivity
    exposing
        ( BoundingBox3D
        , ConnectionPoint
        , ConnectionType(..)
        , OBB
        , SnapCandidate
        , checkCollisions
        , effectiveEnvelope
        , extractConnections
        , isCompatible
        , isSnapColliding
        , isStudded
        , isUpright
        , knownCatalogPart
        , makeAxle
        , makeGear
        , snapEnterDistance
        , snapReleaseDistance
        , solveSnap
        , solveSnapCandidates
        , solveSnapWithHysteresis
        , solveSnapWithHysteresisAndPredicate
        , solveSnapWithPredicate
        , studOffset
        , testOBB
        , toOBB
        , transformDirection
        , transformPoint
        )
import LDraw.Types exposing (LDrawLine(..), SubFileData)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Connectivity"
        [ compatibilityTests
        , extractionTests
        , studLatticeTests
        , snapSolverTests
        , collisionSATTests
        ]


compatibilityTests : Test
compatibilityTests =
    describe "isCompatible"
        [ test "Stud meshes with AntiStud" <|
            \_ ->
                Expect.equal True (isCompatible Stud AntiStud)
        , test "AntiStud meshes with Stud" <|
            \_ ->
                Expect.equal True (isCompatible AntiStud Stud)
        , test "Stud does not mesh with Stud" <|
            \_ ->
                Expect.equal False (isCompatible Stud Stud)
        , test "Technic pin male meshes with female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicPinMale { length = 20.0, friction = True })
                        (TechnicPinFemale { length = 20.0 })
                    )
        , test "Technic axle male meshes with female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicAxleMale { length = 40.0 })
                        (TechnicAxleFemale { length = 20.0 })
                    )
        , test "Clip meshes with Bar" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (ClipMale { width = 4.0 })
                        (BarFemale { diameter = 4.0 })
                    )
        , test "Ball joint meshes with socket" <|
            \_ ->
                Expect.equal True (isCompatible BallMale BallSocketFemale)
        , test "Stud does not mesh with axle hole" <|
            \_ ->
                Expect.equal False
                    (isCompatible Stud (TechnicAxleFemale { length = 20.0 }))
        , test "Technic axle male meshes with Technic pin female" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicAxleMale { length = 40.0 })
                        (TechnicPinFemale { length = 20.0 })
                    )
        , test "Technic pin female meshes with Technic axle male" <|
            \_ ->
                Expect.equal True
                    (isCompatible
                        (TechnicPinFemale { length = 20.0 })
                        (TechnicAxleMale { length = 40.0 })
                    )
        ]


extractionTests : Test
extractionTests =
    describe "Connection extraction"
        [ test "extracts stud.dat as Stud with normal (0, -1, 0)" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 10 0 20
                            , file = "stud.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal Stud pt.connType
                            , \pt -> Expect.equal 10 (Vec3.getX pt.localPosition)
                            , \pt -> Expect.equal 0 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 20 (Vec3.getZ pt.localPosition)
                            , \pt -> Expect.equal -1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts stud4.dat as AntiStud at its open end with normal (0, 1, 0)" <|
            \_ ->
                let
                    -- As parts place it: flipped (Y scale -1) with its base at Y = 20,
                    -- so the tube runs down to the underside at Y = 24.
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.mul (Mat4.makeTranslate3 0 20 0) (Mat4.makeScale3 1 -1 1)
                            , file = "stud4.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal AntiStud pt.connType
                            , \pt -> Expect.equal 24 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts stud4od.dat as AntiStud at its open end with normal (0, 1, 0)" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.mul (Mat4.makeTranslate3 0 20 0) (Mat4.makeScale3 1 -1 1)
                            , file = "stud4od.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.all
                            [ \pt -> Expect.equal AntiStud pt.connType
                            , \pt -> Expect.equal 24 (Vec3.getY pt.localPosition)
                            , \pt -> Expect.equal 1 (Vec3.getY pt.normal)
                            ]
                            p

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts confric subfile as TechnicPinMale with friction = True" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.identity
                            , file = "confric5.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.equal (TechnicPinMale { length = 20.0, friction = True }) p.connType

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts connect.dat subfile as TechnicPinMale with friction = False" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.identity
                            , file = "connect.dat"
                            }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p ] ->
                        Expect.equal (TechnicPinMale { length = 20.0, friction = False }) p.connType

                    _ ->
                        Expect.fail ("Expected 1 connection point, got " ++ String.fromInt (List.length points))
        , test "extracts beamhole.dat and connhole.dat as TechnicPinFemale" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef { color = 16, transform = Mat4.identity, file = "beamhole.dat" }
                        , SubFileRef { color = 16, transform = Mat4.identity, file = "connhole.dat" }
                        ]

                    points =
                        extractConnections lines
                in
                case points of
                    [ p1, p2 ] ->
                        Expect.all
                            [ \_ -> Expect.equal (TechnicPinFemale { length = 20.0 }) p1.connType
                            , \_ -> Expect.equal (TechnicPinFemale { length = 20.0 }) p2.connType
                            ]
                            ()

                    _ ->
                        Expect.fail ("Expected 2 connection points, got " ++ String.fromInt (List.length points))
        , test "knownCatalogPart builds 3673 with frictionless pin connections" <|
            \_ ->
                case knownCatalogPart "3673.dat" of
                    Just conn ->
                        let
                            frictionlessPins =
                                List.filter (\c -> c.connType == TechnicPinMale { length = 10.0, friction = False }) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 2 (List.length frictionlessPins)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3673.dat was not found"
        , test "knownCatalogPart builds 6558 3L friction pin" <|
            \_ ->
                case knownCatalogPart "6558.dat" of
                    Just conn ->
                        let
                            frictionPins =
                                List.filter (\c -> c.connType == TechnicPinMale { length = 30.0, friction = True }) conn.connections

                            zPositions =
                                List.map (\c -> Vec3.getZ c.localPosition) conn.connections
                                    |> List.sort
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 2 (List.length frictionPins)
                            , \_ -> Expect.equal [ -15.0, 15.0 ] zPositions
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -5 -5 -15) box.min
                                            , \_ -> Expect.equal (vec3 5 5 15) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 6558.dat was not found"
        , test "knownCatalogPart builds 43093 with axle and pin ends" <|
            \_ ->
                case knownCatalogPart "43093.dat" of
                    Just conn ->
                        let
                            pins =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinMale { friction } ->
                                                friction == True

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            axles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleMale { length } ->
                                                length == 20.0

                                            _ ->
                                                False
                                    )
                                    conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length pins)
                            , \_ -> Expect.equal 1 (List.length axles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 43093.dat was not found"
        , test "knownCatalogPart builds 4274 with pin and stud ends" <|
            \_ ->
                case knownCatalogPart "4274.dat" of
                    Just conn ->
                        let
                            pins =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinMale { length, friction } ->
                                                length == 10.0 && friction == False

                                            _ ->
                                                False
                                    )
                                    conn.connections

                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn.connections)
                            , \_ -> Expect.equal 1 (List.length pins)
                            , \_ -> Expect.equal 1 (List.length studs)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 4274.dat was not found"
        , test "knownCatalogPart builds 32556 and 32556a 3L frictionless pins" <|
            \_ ->
                case ( knownCatalogPart "32556.dat", knownCatalogPart "32556a.dat" ) of
                    ( Just conn1, Just conn2 ) ->
                        let
                            frictionlessPins c =
                                List.filter (\p -> p.connType == TechnicPinMale { length = 30.0, friction = False }) c.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length conn1.connections)
                            , \_ -> Expect.equal 2 (List.length (frictionlessPins conn1))
                            , \_ -> Expect.equal 2 (List.length conn2.connections)
                            , \_ -> Expect.equal 2 (List.length (frictionlessPins conn2))
                            ]
                            ()

                    _ ->
                        Expect.fail "Catalog parts 32556.dat or 32556a.dat not found"
        , test "knownCatalogPart builds Brick 2x4 with 8 studs and 8 anti-studs" <|
            \_ ->
                case knownCatalogPart "3001.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 8 (List.length studs)
                            , \_ -> Expect.equal 8 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3001.dat was not found"
        , test "knownCatalogPart builds Brick 1x4 with 4 studs and 4 anti-studs" <|
            \_ ->
                case knownCatalogPart "3010.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 4 (List.length studs)
                            , \_ -> Expect.equal 4 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3010.dat was not found"
        , test "knownCatalogPart builds Plate 2x8 with 16 studs and 16 anti-studs" <|
            \_ ->
                case knownCatalogPart "3034.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 16 (List.length studs)
                            , \_ -> Expect.equal 16 (List.length antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3034.dat was not found"
        , test "knownCatalogPart builds Plate 2x6 (3795) with 12 studs and 12 anti-studs" <|
            \_ ->
                case knownCatalogPart "3795.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 12 (List.length studs)
                            , \_ -> Expect.equal 12 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -60 -4 -20) box.min
                                            , \_ -> Expect.equal (vec3 60 8 20) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 2x6"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3795.dat was not found"
        , test "knownCatalogPart builds Plate 4x4 (3031) with 16 studs and 16 anti-studs" <|
            \_ ->
                case knownCatalogPart "3031.dat" of
                    Just conn ->
                        let
                            studs =
                                List.filter (\c -> c.connType == Stud) conn.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 16 (List.length studs)
                            , \_ -> Expect.equal 16 (List.length antiStuds)
                            , \_ ->
                                case conn.boundingBoxes of
                                    [ box ] ->
                                        Expect.all
                                            [ \_ -> Expect.equal (vec3 -40 -4 -40) box.min
                                            , \_ -> Expect.equal (vec3 40 8 40) box.max
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "Expected 1 bounding box for Plate 4x4"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3031.dat was not found"
        , test "knownCatalogPart builds round plates (4073, 6141, 85861) and round brick (3062b)" <|
            \_ ->
                let
                    verifyRoundPart partName expectedHeight =
                        case knownCatalogPart partName of
                            Just conn ->
                                let
                                    studs =
                                        List.filter (\c -> c.connType == Stud) conn.connections

                                    antiStuds =
                                        List.filter (\c -> c.connType == AntiStud) conn.connections
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 1 (List.length studs)
                                    , \_ -> Expect.equal 1 (List.length antiStuds)
                                    , \_ ->
                                        case conn.boundingBoxes of
                                            [ box ] ->
                                                Expect.all
                                                    [ \_ -> Expect.equal (vec3 -10 -4 -10) box.min
                                                    , \_ -> Expect.equal (vec3 10 expectedHeight 10) box.max
                                                    ]
                                                    ()

                                            _ ->
                                                Expect.fail ("Expected 1 bounding box for " ++ partName)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog part " ++ partName ++ " was not found")
                in
                Expect.all
                    [ \_ -> verifyRoundPart "4073.dat" 8
                    , \_ -> verifyRoundPart "6141.dat" 8
                    , \_ -> verifyRoundPart "85861.dat" 8
                    , \_ -> verifyRoundPart "3062.dat" 24
                    , \_ -> verifyRoundPart "3062a.dat" 24
                    , \_ -> verifyRoundPart "3062b.dat" 24
                    ]
                    ()
        , test "makeAxle produces connection points at stud intervals and correct bounds" <|
            \_ ->
                let
                    axle4 =
                        makeAxle 4 "3705.dat"

                    xCoords =
                        List.map (\c -> Vec3.getX c.localPosition) axle4.connections
                in
                Expect.all
                    [ \_ -> Expect.equal 4 (List.length axle4.connections)
                    , \_ -> Expect.equal [ -30.0, -10.0, 10.0, 30.0 ] xCoords
                    , \_ ->
                        case axle4.boundingBoxes of
                            [ box ] ->
                                Expect.all
                                    [ \_ -> Expect.equal (vec3 -40 -4 -4) box.min
                                    , \_ -> Expect.equal (vec3 40 4 4) box.max
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box for axle"
                    ]
                    ()
        , test "makeGear produces central axle hole and bounds" <|
            \_ ->
                let
                    gear8 =
                        makeGear 10.0 20.0 "3647.dat"
                in
                Expect.all
                    [ \_ -> Expect.equal 1 (List.length gear8.connections)
                    , \_ ->
                        case gear8.connections of
                            [ c ] ->
                                Expect.all
                                    [ \_ -> Expect.equal (TechnicAxleFemale { length = 20.0 }) c.connType
                                    , \_ -> Expect.equal (vec3 0 0 0) c.localPosition
                                    , \_ -> Expect.equal (vec3 0 0 1) c.normal
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 connection point for gear"
                    ]
                    ()
        , test "40T gear (3649) includes axle holes and 12 pin holes" <|
            \_ ->
                case knownCatalogPart "3649.dat" of
                    Just gear40 ->
                        let
                            axleHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear40.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear40.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 41 (List.length gear40.connections)
                            , \_ -> Expect.equal 5 (List.length axleHoles)
                            , \_ -> Expect.equal 36 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3649.dat was not found"
        , test "24T gear (3648b) includes central axle hole and 4 pin holes" <|
            \_ ->
                case knownCatalogPart "3648b.dat" of
                    Just gear24 ->
                        let
                            axleHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicAxleFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear24.connections

                            pinHoles =
                                List.filter
                                    (\c ->
                                        case c.connType of
                                            TechnicPinFemale _ ->
                                                True

                                            _ ->
                                                False
                                    )
                                    gear24.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 13 (List.length gear24.connections)
                            , \_ -> Expect.equal 1 (List.length axleHoles)
                            , \_ -> Expect.equal 12 (List.length pinHoles)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 3648b.dat was not found"
        , test "knownCatalogPart supports classic and modern gears, axles, beams, and bushings" <|
            \_ ->
                let
                    partsToCheck =
                        [ "3647"
                        , "10928"
                        , "11955"
                        , "4019"
                        , "3648"
                        , "3648b"
                        , "3649"
                        , "32198"
                        , "32269"
                        , "6589"
                        , "32270"
                        , "4143"
                        , "3650b"
                        , "4716"
                        , "32905"
                        , "3704"
                        , "4519"
                        , "3705"
                        , "32073"
                        , "3706"
                        , "44294"
                        , "3707"
                        , "60485"
                        , "3737"
                        , "3708"
                        , "32062"
                        , "6587"
                        , "3700"
                        , "3701"
                        , "3702"
                        , "3703"
                        , "32523"
                        , "32316"
                        , "32524"
                        , "32449"
                        , "32525"
                        , "32009"
                        , "3713"
                        , "4265c"
                        ]

                    missing =
                        List.filter (\p -> knownCatalogPart p == Nothing) partsToCheck
                in
                Expect.equal [] missing
        , test "87087 has 1 up stud, 1 side stud facing -Z, and 1 anti-stud; 3070b has 1 anti-stud and no studs" <|
            \_ ->
                case ( knownCatalogPart "87087.dat", knownCatalogPart "3070b.dat" ) of
                    ( Just snot87087, Just tile1x1 ) ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) snot87087.connections

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) snot87087.connections

                            antiStuds87087 =
                                List.filter (\c -> c.connType == AntiStud) snot87087.connections

                            tileStuds =
                                List.filter (\c -> c.connType == Stud) tile1x1.connections

                            tileAntiStuds =
                                List.filter (\c -> c.connType == AntiStud) tile1x1.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 1 (List.length sideStuds)
                            , \_ -> Expect.equal 1 (List.length antiStuds87087)
                            , \_ -> Expect.equal 0 (List.length tileStuds)
                            , \_ -> Expect.equal 1 (List.length tileAntiStuds)
                            ]
                            ()

                    _ ->
                        Expect.fail "87087.dat or 3070b.dat not found in catalog"
        , test "4733 (Travis brick) has 1 top stud, 4 side studs facing -Z, +X, +Z, -X, and 1 bottom anti-stud" <|
            \_ ->
                case knownCatalogPart "4733.dat" of
                    Just travis ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) travis.connections

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && abs (Vec3.getY c.normal) < 0.3) travis.connections

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) travis.connections

                            frontSide =
                                List.filter (\c -> Vec3.getZ c.normal < -0.7) sideStuds

                            rightSide =
                                List.filter (\c -> Vec3.getX c.normal > 0.7) sideStuds

                            backSide =
                                List.filter (\c -> Vec3.getZ c.normal > 0.7) sideStuds

                            leftSide =
                                List.filter (\c -> Vec3.getX c.normal < -0.7) sideStuds
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 4 (List.length sideStuds)
                            , \_ -> Expect.equal 1 (List.length antiStuds)
                            , \_ -> Expect.equal 1 (List.length frontSide)
                            , \_ -> Expect.equal 1 (List.length rightSide)
                            , \_ -> Expect.equal 1 (List.length backSide)
                            , \_ -> Expect.equal 1 (List.length leftSide)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "4733.dat not found in catalog"
        , test "4070 (Headlight brick) has 1 top stud, 1 bottom anti-stud, 1 recessed front side stud at Z = -6, and 1 rear anti-stud at Z = 10" <|
            \_ ->
                case knownCatalogPart "4070.dat" of
                    Just headlight ->
                        let
                            upStuds =
                                List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) headlight.connections

                            bottomAntiStuds =
                                List.filter (\c -> c.connType == AntiStud && Vec3.getY c.normal > 0.7) headlight.connections

                            frontRecessedStud =
                                List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) headlight.connections

                            rearAntiStud =
                                List.filter (\c -> c.connType == AntiStud && Vec3.getZ c.normal > 0.7) headlight.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length upStuds)
                            , \_ -> Expect.equal 1 (List.length bottomAntiStuds)
                            , \_ -> Expect.equal 1 (List.length frontRecessedStud)
                            , \_ ->
                                case frontRecessedStud of
                                    [ s ] ->
                                        Expect.equal -6 (round (Vec3.getZ s.localPosition))

                                    _ ->
                                        Expect.fail "Expected exactly 1 front recessed stud"
                            , \_ -> Expect.equal 1 (List.length rearAntiStud)
                            , \_ ->
                                case rearAntiStud of
                                    [ a ] ->
                                        Expect.equal 10 (round (Vec3.getZ a.localPosition))

                                    _ ->
                                        Expect.fail "Expected exactly 1 rear anti-stud"
                            ]
                            ()

                    Nothing ->
                        Expect.fail "4070.dat not found in catalog"
        , test "catalog slope parts have expected stud counts, anti-stud counts, and bounding boxes" <|
            \_ ->
                let
                    checkSlope part expectedStuds expectedAntiStuds expectedMin expectedMax =
                        case knownCatalogPart part of
                            Just conn ->
                                let
                                    studCount =
                                        List.length (List.filter (\c -> c.connType == Stud) conn.connections)

                                    antiCount =
                                        List.length (List.filter (\c -> c.connType == AntiStud) conn.connections)

                                    box =
                                        List.head conn.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                in
                                Expect.all
                                    [ \_ -> Expect.equal expectedStuds studCount
                                    , \_ -> Expect.equal expectedAntiStuds antiCount
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMin) (Vec3.toRecord box.min)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMax) (Vec3.toRecord box.max)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog slope part not found: " ++ part)
                in
                Expect.all
                    [ \_ -> checkSlope "3040b.dat" 1 2 (vec3 -10 -4 -30) (vec3 10 24 10)
                    , \_ -> checkSlope "3039.dat" 2 4 (vec3 -20 -4 -30) (vec3 20 24 10)
                    , \_ -> checkSlope "3038.dat" 3 6 (vec3 -30 -4 -30) (vec3 30 24 10)
                    , \_ -> checkSlope "3037.dat" 4 8 (vec3 -40 -4 -30) (vec3 40 24 10)
                    , \_ -> checkSlope "3298.dat" 2 6 (vec3 -20 -4 -50) (vec3 20 24 10)
                    ]
                    ()
        , test "catalog bracket parts 36840, 36841, and 44728 have expected connection counts and bounding boxes" <|
            \_ ->
                let
                    checkBracket part expectedStuds expectedAntiStuds expectedSideStuds expectedMin expectedMax =
                        case knownCatalogPart part of
                            Just conn ->
                                let
                                    upStuds =
                                        List.filter (\c -> c.connType == Stud && Vec3.getY c.normal < -0.7) conn.connections

                                    sideStuds =
                                        List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7) conn.connections

                                    antiStuds =
                                        List.filter (\c -> c.connType == AntiStud) conn.connections

                                    box =
                                        List.head conn.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                in
                                Expect.all
                                    [ \_ -> Expect.equal expectedStuds (List.length upStuds)
                                    , \_ -> Expect.equal expectedSideStuds (List.length sideStuds)
                                    , \_ -> Expect.equal expectedAntiStuds (List.length antiStuds)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMin) (Vec3.toRecord box.min)
                                    , \_ -> Expect.equal (Vec3.toRecord expectedMax) (Vec3.toRecord box.max)
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail ("Catalog bracket part not found: " ++ part)
                in
                Expect.all
                    [ \_ -> checkBracket "36840.dat" 1 1 1 (vec3 -10 -12 -14) (vec3 10 8 10)
                    , \_ -> checkBracket "36841.dat" 1 1 1 (vec3 -10 -4 -14) (vec3 10 20 10)
                    , \_ -> checkBracket "44728.dat" 2 2 4 (vec3 -20 -4 -14) (vec3 20 40 10)
                    ]
                    ()
        ]


studLatticeTests : Test
studLatticeTests =
    let
        noLines _ =
            Nothing

        offsetOf name matrix =
            studOffset (Connectivity.extractPartConnectivity name noLines) matrix

        turnedY =
            Mat4.makeRotate (pi / 2) (vec3 0 1 0)

        rot0 =
            Mat4.identity

        rot90 =
            turnedY

        rot180 =
            Mat4.makeRotate pi (vec3 0 1 0)

        rot270 =
            Mat4.makeRotate (3 * pi / 2) (vec3 0 1 0)
    in
    describe "stud offset and the 60592 window"
        [ test "a 1x1 (odd on both axes) is offset 10 on both" <|
            \_ -> offsetOf "3005.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "a 2x4 (even on both axes) is not offset" <|
            \_ -> offsetOf "3001.dat" Mat4.identity |> Expect.equal { x = 0, z = 0 }
        , test "a 1x3 (odd on both axes) is offset 10 on both" <|
            \_ -> offsetOf "3622.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "a 1x2 has its odd axis along Z, and swaps it when turned 90°" <|
            \_ ->
                Expect.all
                    [ \_ -> offsetOf "3004.dat" Mat4.identity |> Expect.equal { x = 0, z = 10 }
                    , \_ -> offsetOf "3004.dat" turnedY |> Expect.equal { x = 10, z = 0 }
                    ]
                    ()
        , test "only the rotation counts: a translated part keeps its offset" <|
            \_ ->
                offsetOf "3004.dat" (Mat4.mul (Mat4.makeTranslate3 30 -24 7) turnedY)
                    |> Expect.equal { x = 10, z = 0 }
        , test "the window 60592 (2 x 1 studs) is offset 0 on X and 10 on Z" <|
            \_ -> offsetOf "60592.dat" Mat4.identity |> Expect.equal { x = 0, z = 10 }
        , test "the SNOT brick 87087 with side stud counts only its up-facing stud: { x = 10, z = 10 }" <|
            \_ -> offsetOf "87087.dat" Mat4.identity |> Expect.equal { x = 10, z = 10 }
        , test "Technic parts (no stud points) are never offset" <|
            \_ ->
                [ "32524.dat", "3705.dat", "3648.dat", "3647.dat", "6590.dat" ]
                    |> List.map (\name -> offsetOf name turnedY)
                    |> Expect.equal (List.repeat 5 { x = 0, z = 0 })
        , test "isStudded is true for bricks and false for Technic parts" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal True (isStudded (Connectivity.extractPartConnectivity "3001.dat" noLines))
                    , \_ -> Expect.equal False (isStudded (Connectivity.extractPartConnectivity "32524.dat" noLines))
                    , \_ -> Expect.equal False (isStudded (Connectivity.extractPartConnectivity "3648.dat" noLines))
                    ]
                    ()
        , test "60592 has an underside AntiStud at each of its two 2x1 footprint studs" <|
            \_ ->
                case knownCatalogPart "60592.dat" of
                    Just conn ->
                        let
                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) conn.connections
                        in
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length antiStuds)
                            , \_ -> Expect.equal [ 48, 48 ] (List.map (\c -> Vec3.getY c.localPosition) antiStuds)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Catalog part 60592.dat was not found"
        , test "studOffset across rotations (0°, 90°, 180°, 270°) for 1x1, 2x4, 1x2, 2x3, 2x6, 4x4" <|
            \_ ->
                Expect.all
                    [ -- 1x1 (odd on both axes): { x = 10, z = 10 } at all 4 rotations
                      \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot90)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot180)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3005.dat" rot270)
                    , -- 2x4 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3001.dat" rot270)
                    , -- 1x2 (even X, odd Z): swaps offset when turned 90° or 270°
                      \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3004.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3004.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3004.dat" rot180)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3004.dat" rot270)
                    , -- 2x3 (odd X, even Z): swaps offset when turned 90° or 270°
                      \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3002.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3002.dat" rot90)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3002.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3002.dat" rot270)
                    , -- 2x6 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3795.dat" rot270)
                    , -- 4x4 (even on both axes): { x = 0, z = 0 } at all 4 rotations
                      \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot0)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot90)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot180)
                    , \_ -> Expect.equal { x = 0, z = 0 } (offsetOf "3031.dat" rot270)
                    ]
                    ()
        , test "studOffset for tiles (3070b, 3069b) and slopes (3040b, 3039) across 0° and 90° rotations" <|
            \_ ->
                Expect.all
                    [ -- 1x1 tile 3070b: odd on both axes, offset { x = 10, z = 10 } at rot0 and rot90
                      \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3070b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3070b.dat" rot90)

                    -- 2x1 tile 3069b: 2 on X, 1 on Z at rot0, swaps at rot90
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3069b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3069b.dat" rot90)

                    -- 1x2 slope 3040b: single stud at origin, offset { x = 10, z = 10 }
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3040b.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 10 } (offsetOf "3040b.dat" rot90)

                    -- 2x2 slope 3039: 2 studs along X, 1 column along Z at rot0, swaps at rot90
                    , \_ -> Expect.equal { x = 0, z = 10 } (offsetOf "3039.dat" rot0)
                    , \_ -> Expect.equal { x = 10, z = 0 } (offsetOf "3039.dat" rot90)
                    ]
                    ()
        , test "isUpright reports True for yaw rotations and False for tipped/inverted parts" <|
            \_ ->
                let
                    conn2x4 =
                        Connectivity.extractPartConnectivity "3001.dat" noLines

                    conn1x1 =
                        Connectivity.extractPartConnectivity "3024.dat" noLines

                    pitch90 =
                        Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                    roll90 =
                        Mat4.makeRotate (pi / 2) (vec3 0 0 1)

                    upsideDown =
                        Mat4.makeRotate pi (vec3 1 0 0)
                in
                Expect.all
                    [ \_ -> Expect.equal True (isUpright conn2x4 rot0)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot90)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot180)
                    , \_ -> Expect.equal True (isUpright conn2x4 rot270)
                    , \_ -> Expect.equal True (isUpright conn1x1 rot0)
                    , \_ -> Expect.equal True (isUpright conn1x1 rot90)
                    , \_ -> Expect.equal False (isUpright conn2x4 pitch90)
                    , \_ -> Expect.equal False (isUpright conn2x4 roll90)
                    , \_ -> Expect.equal False (isUpright conn2x4 upsideDown)
                    , \_ -> Expect.equal False (isUpright conn1x1 pitch90)
                    , \_ -> Expect.equal False (isUpright conn1x1 upsideDown)
                    ]
                    ()
        ]


snapSolverTests : Test
snapSolverTests =
    describe "solveSnap"
        [ test "snaps dragged anti-stud onto target stud within 12 LDU" <|
            \_ ->
                let
                    -- Bottom brick at (0, 0, 0) with a top stud at (0, 0, 0) pointing (0, -1, 0)
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    -- Dragged brick with bottom anti-stud at local position (0, 24, 0), normal (0, 1, 0)
                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    -- Currently hovering at (2, -26, 3) (distance ≈ 3.6 LDU from (0, -24, 0))
                    currentHoverMat =
                        Mat4.makeTranslate3 2 -26 3
                in
                case solveSnap 12.0 0 currentHoverMat [ draggedAntiStud ] [ targetStud ] of
                    Just snap ->
                        let
                            -- Verify snapped anti-stud lands exactly on target stud at (0, 0, 0)
                            snappedAntiStudPos =
                                Mat4.transform snap.snapMatrix draggedAntiStud.localPosition
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.001 (Vec3.distance (vec3 0 0 0) snappedAntiStudPos)
                            , \_ -> Expect.lessThan 0.001 (Vec3.distance (vec3 0 -24 0) (Mat4.transform snap.snapMatrix (vec3 0 0 0)))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Snap candidate was not found"
        , test "snaps slope 3039 anti-studs onto target studs of a 2x2 brick" <|
            \_ ->
                let
                    brickConn =
                        Connectivity.extractPartConnectivity "3003.dat" (\_ -> Nothing)

                    targetStuds =
                        brickConn.connections
                            |> List.filter (\c -> c.connType == Stud)
                            |> List.map
                                (\c ->
                                    { placementId = 1
                                    , pointId = c.id
                                    , connType = c.connType
                                    , worldPosition = c.localPosition
                                    , worldNormal = c.normal
                                    , worldTangent = c.tangent
                                    }
                                )

                    slopeConn =
                        Connectivity.extractPartConnectivity "3039.dat" (\_ -> Nothing)

                    hoverMat =
                        Mat4.makeTranslate3 1 -22 11
                in
                case solveSnap 12.0 0 hoverMat slopeConn.connections targetStuds of
                    Just snap ->
                        let
                            snappedPos =
                                Mat4.transform snap.snapMatrix (vec3 0 0 0)
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.001 (abs (Vec3.getX snappedPos - 0))
                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.getY snappedPos - -24))
                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.getZ snappedPos - 10))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected snap candidate for slope on brick"
        , test "rotationIndex cycles orientation around connection normal by 90-degree steps" <|
            \_ ->
                let
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    currentHoverMat =
                        Mat4.makeTranslate3 0 -24 0

                    -- Step 1 = 90 degrees turn
                    snap90 =
                        solveSnap 12.0 1 currentHoverMat [ draggedAntiStud ] [ targetStud ]
                in
                case snap90 of
                    Just snap ->
                        let
                            -- In step 1 (90 deg), local X (1, 0, 0) rotates towards local Z (0, 0, 1) or (0, 0, -1)
                            transformedUnitX =
                                Vec3.sub
                                    (Mat4.transform snap.snapMatrix (vec3 1 24 0))
                                    (Mat4.transform snap.snapMatrix (vec3 0 24 0))
                        in
                        Expect.all
                            [ \_ -> Expect.lessThan 0.01 (abs (Vec3.getX transformedUnitX))
                            , \_ -> Expect.greaterThan 0.99 (abs (Vec3.getZ transformedUnitX))
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Snap 90 deg step failed"
        , test "does not snap beyond max distance" <|
            \_ ->
                let
                    targetStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    draggedAntiStud =
                        { id = 101
                        , connType = AntiStud
                        , localPosition = vec3 0 24 0
                        , normal = vec3 0 1 0
                        , tangent = vec3 1 0 0
                        }

                    -- 30 LDU away (> 12 LDU threshold)
                    farHoverMat =
                        Mat4.makeTranslate3 0 -54 0
                in
                Expect.equal Nothing (solveSnap 12.0 0 farHoverMat [ draggedAntiStud ] [ targetStud ])
        , test "snaps Technic axle into gear axle hole" <|
            \_ ->
                let
                    gearAxleHole =
                        { placementId = 2
                        , pointId = 1
                        , connType = TechnicAxleFemale { length = 20.0 }
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 0 1
                        , worldTangent = vec3 1 0 0
                        }

                    axleConn =
                        { id = 1
                        , connType = TechnicAxleMale { length = 80.0 }
                        , localPosition = vec3 0 0 0
                        , normal = vec3 0 0 1
                        , tangent = vec3 1 0 0
                        }

                    hoverMat =
                        Mat4.makeTranslate3 2 1 5
                in
                case solveSnap 12.0 0 hoverMat [ axleConn ] [ gearAxleHole ] of
                    Just snap ->
                        let
                            axlePos =
                                Mat4.transform snap.snapMatrix axleConn.localPosition
                        in
                        Expect.lessThan 0.001 (Vec3.distance (vec3 0 0 0) axlePos)

                    Nothing ->
                        Expect.fail "Axle snap into gear failed"
        , test "snaps Technic pin into 24T gear pin hole" <|
            \_ ->
                case ( knownCatalogPart "2780.dat", knownCatalogPart "3648b.dat" ) of
                    ( Just pinPart, Just gearPart ) ->
                        let
                            gearWorldPoints =
                                List.map
                                    (\c ->
                                        { placementId = 1
                                        , pointId = c.id
                                        , connType = c.connType
                                        , worldPosition = c.localPosition
                                        , worldNormal = c.normal
                                        , worldTangent = c.tangent
                                        }
                                    )
                                    gearPart.connections

                            -- Hover near pin hole at (10, 10, 0)
                            hoverMat =
                                Mat4.makeTranslate3 10 10 -8
                        in
                        case solveSnap 12.0 0 hoverMat pinPart.connections gearWorldPoints of
                            Just snap ->
                                let
                                    snappedPos =
                                        Mat4.transform snap.snapMatrix snap.draggedPoint.localPosition
                                in
                                Expect.lessThan 0.001 (Vec3.distance snap.targetPoint.worldPosition snappedPos)

                            Nothing ->
                                Expect.fail "Pin snap into 24T gear failed"

                    _ ->
                        Expect.fail "Parts 2780.dat or 3648b.dat not found"
        , test "snaps Technic axle into beam pin hole" <|
            \_ ->
                let
                    beamPinHole =
                        { placementId = 2
                        , pointId = 1
                        , connType = TechnicPinFemale { length = 20.0 }
                        , worldPosition = vec3 0 10 0
                        , worldNormal = vec3 0 0 1
                        , worldTangent = vec3 1 0 0
                        }

                    axleConn =
                        { id = 1
                        , connType = TechnicAxleMale { length = 80.0 }
                        , localPosition = vec3 -10 0 0
                        , normal = vec3 1 0 0
                        , tangent = vec3 0 1 0
                        }

                    hoverMat =
                        Mat4.makeTranslate3 2 11 3
                in
                case solveSnap 12.0 0 hoverMat [ axleConn ] [ beamPinHole ] of
                    Just snap ->
                        let
                            axlePos =
                                Mat4.transform snap.snapMatrix axleConn.localPosition
                        in
                        Expect.lessThan 0.001 (Vec3.distance (vec3 0 10 0) axlePos)

                    Nothing ->
                        Expect.fail "Axle snap into beam pin hole failed"
        , test "1x1 plate dragging near 2x2 plate on ground snaps on top (Y = -16) not below (Y = 0)" <|
            \_ ->
                case ( knownCatalogPart "3024.dat", knownCatalogPart "3022.dat" ) of
                    ( Just plate1x1, Just plate2x2 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -8 0

                            targetWorldPoints =
                                List.map (Connectivity.transformPoint targetMatrix 1) plate2x2.connections

                            -- Hovering along ground near the (10, -8, 10) stud position
                            hoverMatrix =
                                Mat4.makeTranslate3 10 -8 10
                        in
                        case solveSnap 12.0 0 hoverMatrix plate1x1.connections targetWorldPoints of
                            Just snap ->
                                let
                                    snappedOrigin =
                                        Mat4.transform snap.snapMatrix (vec3 0 0 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.equal -16 (round (Vec3.getY snappedOrigin))
                                    , \_ -> Expect.notEqual 0 (round (Vec3.getY snappedOrigin))
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Snap candidate was not found"

                    _ ->
                        Expect.fail "Catalog parts 3024.dat or 3022.dat not found"
        , test "1x1 tile dragging near 1x1 brick snaps on top and isSnapColliding is False" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "3005.dat" ) of
                    ( Just tile1x1, Just brick1x1 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            targetWorldPoints =
                                List.map (Connectivity.transformPoint targetMatrix 1) brick1x1.connections

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -30 0

                            maybeSnap =
                                solveSnap 12.0 0 hoverMatrix tile1x1.connections targetWorldPoints
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    snappedOrigin =
                                        Mat4.transform snap.snapMatrix (vec3 0 0 0)

                                    tileBox =
                                        List.head tile1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                            |> (\b -> effectiveEnvelope b tile1x1.connections)

                                    brickBox =
                                        List.head brick1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                            |> (\b -> effectiveEnvelope b brick1x1.connections)

                                    colliding =
                                        isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, brickBox ) ] snap
                                in
                                Expect.all
                                    [ \_ -> Expect.equal -32 (round (Vec3.getY snappedOrigin))
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "Tile snap candidate not found"

                    _ ->
                        Expect.fail "3070b.dat or 3005.dat not found in catalog"
        , test "solveSnap with an upright 3070b near the 87087 side stud returns a matrix with the tile's down axis along +Z (auto-tip)" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            -- Hovering upright in front of the side stud at (0, -14, -18)
                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            maybeSnap =
                                solveSnap 12.0 0 hoverMatrix tile1x1.connections sideStud
                        in
                        case maybeSnap of
                            Just snap ->
                                let
                                    -- Down direction of the tile (local (0, 1, 0)) transformed to world
                                    downDir =
                                        Connectivity.transformDirection snap.snapMatrix (vec3 0 1 0)

                                    tileBox =
                                        List.head tile1x1.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                            |> (\b -> effectiveEnvelope b tile1x1.connections)

                                    snotBox =
                                        List.head snot87087.boundingBoxes
                                            |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                            |> (\b -> effectiveEnvelope b snot87087.connections)

                                    colliding =
                                        isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, snotBox ) ] snap
                                in
                                Expect.all
                                    [ \_ -> Expect.greaterThan 0.7 (Vec3.getZ downDir)
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "SNOT side-stud snap candidate not found"

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        , test "solveSnap with a tile snaps cleanly onto each of the 4 side studs of 4733 without collision" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "4733.dat" ) of
                    ( Just tile1x1, Just travis ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            travisBox =
                                List.head travis.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b travis.connections)

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)

                            sideStuds =
                                List.filter (\c -> c.connType == Stud && abs (Vec3.getY c.normal) < 0.3) travis.connections
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            testSideStud targetStud =
                                let
                                    offsetPos =
                                        Vec3.add targetStud.worldPosition (Vec3.scale 8 targetStud.worldNormal)

                                    hoverMatrix =
                                        Mat4.makeTranslate offsetPos

                                    maybeSnap =
                                        solveSnap 16.0 0 hoverMatrix tile1x1.connections [ targetStud ]
                                in
                                case maybeSnap of
                                    Just snap ->
                                        let
                                            colliding =
                                                isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, travisBox ) ] snap
                                        in
                                        Expect.all
                                            [ \_ -> Expect.equal AntiStud snap.draggedPoint.connType
                                            , \_ -> Expect.equal False colliding
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail "Tile snap candidate not found on 4733 side stud"
                        in
                        Expect.all (List.map (\stud -> \_ -> testSideStud stud) sideStuds) ()

                    _ ->
                        Expect.fail "3070b.dat or 4733.dat not found in catalog"
        , test "4070 headlight brick rear anti-stud snaps onto a stud and front recessed stud receives a tile" <|
            \_ ->
                case ( knownCatalogPart "4070.dat", knownCatalogPart "87087.dat", knownCatalogPart "3070b.dat" ) of
                    ( Just headlight, Just brick87087, Just tile1x1 ) ->
                        let
                            baseMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            -- Target side stud on 87087 facing -Z at (0, -14, -10)
                            baseStud =
                                brick87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint baseMatrix 1)

                            -- Hovering 4070 upright so its rear anti-stud (+Z) faces baseStud (-Z)
                            hoverHeadlight =
                                Mat4.makeTranslate3 0 -24 -24

                            rearSnap =
                                solveSnap 16.0 0 hoverHeadlight headlight.connections baseStud

                            targetHeadlightMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            recessedStud =
                                headlight.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetHeadlightMatrix 1)

                            -- Tipped tile so its underside (+Z) faces front recessed stud (-Z)
                            tipTile =
                                Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                            hoverTile =
                                Mat4.mul (Mat4.makeTranslate3 0 -14 -16) tipTile

                            frontSnap =
                                solveSnap 16.0 0 hoverTile tile1x1.connections recessedStud

                            headlightBox =
                                List.head headlight.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b headlight.connections)

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)
                        in
                        case ( rearSnap, frontSnap ) of
                            ( Just rSnap, Just fSnap ) ->
                                let
                                    colliding =
                                        isSnapColliding 0.5 2 hoverTile [ ( 2, hoverTile, tileBox ) ] [ ( 1, targetHeadlightMatrix, headlightBox ) ] fSnap
                                in
                                Expect.all
                                    [ \_ -> Expect.equal 101 rSnap.draggedPoint.id
                                    , \_ -> Expect.equal AntiStud rSnap.draggedPoint.connType
                                    , \_ -> Expect.equal False colliding
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Failed to snap on headlight rear anti-stud or front recessed stud"

                    _ ->
                        Expect.fail "4070.dat, 87087.dat, or 3070b.dat not found in catalog"
        , test "4-step in-flight rotation on a side stud preserves SO(3) orthonormality and clean alignment without collisions" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            tileBox =
                                List.head tile1x1.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 0 -10, max = vec3 10 8 10 }
                                    |> (\b -> effectiveEnvelope b tile1x1.connections)

                            snotBox =
                                List.head snot87087.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 -10 -4 -10, max = vec3 10 24 10 }
                                    |> (\b -> effectiveEnvelope b snot87087.connections)

                            verifyStep idx =
                                case solveSnap 16.0 idx hoverMatrix tile1x1.connections sideStud of
                                    Just snap ->
                                        let
                                            c1 =
                                                transformDirection snap.snapMatrix (vec3 1 0 0)

                                            c2 =
                                                transformDirection snap.snapMatrix (vec3 0 1 0)

                                            c3 =
                                                transformDirection snap.snapMatrix (vec3 0 0 1)

                                            det =
                                                Vec3.dot (Vec3.cross c1 c2) c3

                                            colliding =
                                                isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, tileBox ) ] [ ( 1, targetMatrix, snotBox ) ] snap
                                        in
                                        Expect.all
                                            [ \_ -> Expect.lessThan 0.001 (abs (Vec3.length c1 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.length c2 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.length c3 - 1.0))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c1 c2))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c2 c3))
                                            , \_ -> Expect.lessThan 0.001 (abs (Vec3.dot c3 c1))
                                            , \_ -> Expect.lessThan 0.001 (abs (det - 1.0))
                                            , \_ -> Expect.equal False colliding
                                            ]
                                            ()

                                    Nothing ->
                                        Expect.fail ("Failed rotation step " ++ String.fromInt idx)
                        in
                        Expect.all (List.map (\idx -> \_ -> verifyStep idx) [ 0, 1, 2, 3 ]) ()

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        , test "solveSnapCandidates allows snapping to horizontal side studs near ground level" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just snot87087 ) ->
                        let
                            targetMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            sideStud =
                                snot87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint targetMatrix 1)

                            hoverMatrix =
                                Mat4.makeTranslate3 0 -14 -18

                            candidates =
                                solveSnapCandidates 16.0 0 hoverMatrix tile1x1.connections sideStud
                        in
                        Expect.equal False (List.isEmpty candidates)

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        ]


collisionSATTests : Test
collisionSATTests =
    describe "3D SAT collision detection"
        [ test "detects intersection between two overlapping 2x4 bricks" <|
            \_ ->
                let
                    box =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    matA =
                        Mat4.makeTranslate3 0 0 0

                    matB =
                        Mat4.makeTranslate3 10 5 5

                    obbA =
                        toOBB box matA

                    obbB =
                        toOBB box matB
                in
                Expect.equal True (testOBB 0.5 obbA obbB)
        , test "two stacked bricks with effective envelope do not collide (face-touching overlap 0)" <|
            \_ ->
                let
                    boxA =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    boxB =
                        { min = vec3 -40 0 -20, max = vec3 40 24 20 }

                    -- Brick A at y = 0..24
                    matA =
                        Mat4.makeTranslate3 0 0 0

                    -- Brick B stacked on top at y = -24..0
                    matB =
                        Mat4.makeTranslate3 0 -24 0

                    obbA =
                        toOBB boxA matA

                    obbB =
                        toOBB boxB matB
                in
                Expect.equal False (testOBB 0.5 obbA obbB)
        , test "vertical stacking plate-on-plate (3024 on 3022) with effectiveEnvelope does not collide" <|
            \_ ->
                case ( knownCatalogPart "3022.dat", knownCatalogPart "3024.dat" ) of
                    ( Just conn3022, Just conn3024 ) ->
                        case ( conn3022.boundingBoxes, conn3024.boundingBoxes ) of
                            ( [ box3022 ], [ box3024 ] ) ->
                                let
                                    env3022 =
                                        effectiveEnvelope box3022 conn3022.connections

                                    env3024 =
                                        effectiveEnvelope box3024 conn3024.connections

                                    -- Plate 3022 placed at y = 0..8
                                    mat3022 =
                                        Mat4.makeTranslate3 0 0 0

                                    -- Plate 3024 stacked on top: translated by y = -8 (occupies y = -8..0 in world)
                                    mat3024 =
                                        Mat4.makeTranslate3 10 -8 10

                                    obb3022 =
                                        toOBB env3022 mat3022

                                    obb3024 =
                                        toOBB env3024 mat3024

                                    rawObb3022 =
                                        toOBB box3022 mat3022

                                    rawObb3024 =
                                        toOBB box3024 mat3024
                                in
                                Expect.all
                                    [ \_ -> Expect.equal True (testOBB 0.5 rawObb3022 rawObb3024)
                                    , \_ -> Expect.equal False (testOBB 0.5 obb3022 obb3024)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box each for 3022 and 3024"

                    _ ->
                        Expect.fail "Catalog parts 3022.dat and 3024.dat must exist"
        , test "vertical stacking plate-on-brick (3024 on 3005) with effectiveEnvelope does not collide" <|
            \_ ->
                case ( knownCatalogPart "3005.dat", knownCatalogPart "3024.dat" ) of
                    ( Just conn3005, Just conn3024 ) ->
                        case ( conn3005.boundingBoxes, conn3024.boundingBoxes ) of
                            ( [ box3005 ], [ box3024 ] ) ->
                                let
                                    env3005 =
                                        effectiveEnvelope box3005 conn3005.connections

                                    env3024 =
                                        effectiveEnvelope box3024 conn3024.connections

                                    -- Brick 3005 placed at y = 0..24
                                    mat3005 =
                                        Mat4.makeTranslate3 0 0 0

                                    -- Plate 3024 stacked on top: translated by y = -8 (occupies y = -8..0 in world)
                                    mat3024 =
                                        Mat4.makeTranslate3 0 -8 0

                                    obb3005 =
                                        toOBB env3005 mat3005

                                    obb3024 =
                                        toOBB env3024 mat3024

                                    rawObb3005 =
                                        toOBB box3005 mat3005

                                    rawObb3024 =
                                        toOBB box3024 mat3024
                                in
                                Expect.all
                                    [ \_ -> Expect.equal True (testOBB 0.5 rawObb3005 rawObb3024)
                                    , \_ -> Expect.equal False (testOBB 0.5 obb3005 obb3024)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected 1 bounding box each for 3005 and 3024"

                    _ ->
                        Expect.fail "Catalog parts 3005.dat and 3024.dat must exist"
        , test "brick intersecting by 10 LDU inside another is flagged by checkCollisions" <|
            \_ ->
                let
                    box =
                        { min = vec3 -10 0 -10, max = vec3 10 24 10 }

                    dragged =
                        [ ( 10, Mat4.makeTranslate3 5 0 0, box ) ]

                    scene =
                        [ ( 1, Mat4.makeTranslate3 0 0 0, box ) ]

                    collisions =
                        checkCollisions 0.5 dragged scene
                in
                Expect.equal [ ( 10, 1 ) ] collisions
        , test "effectiveEnvelope trims top stud protrusion from y = -4 to 0" <|
            \_ ->
                let
                    rawBox =
                        { min = vec3 -40 -4 -20, max = vec3 40 24 20 }

                    studPoints =
                        [ { id = 1
                          , connType = Stud
                          , localPosition = vec3 0 0 0
                          , normal = vec3 0 -1 0
                          , tangent = vec3 1 0 0
                          }
                        ]

                    effective =
                        effectiveEnvelope rawBox studPoints
                in
                Expect.all
                    [ \b -> Expect.equal 0 (Vec3.getY b.min)
                    , \b -> Expect.equal 24 (Vec3.getY b.max)
                    ]
                    effective
        , test "effectiveEnvelope correctly trims multi-level stud bases using min for min faces and max for max faces" <|
            \_ ->
                let
                    box =
                        { min = vec3 -14 -8 -14, max = vec3 14 28 14 }

                    stud1 =
                        { id = 1
                        , connType = Stud
                        , localPosition = vec3 0 -4 0
                        , normal = vec3 0 -1 0
                        , tangent = vec3 1 0 0
                        }

                    stud2 =
                        { id = 2
                        , connType = Stud
                        , localPosition = vec3 0 0 0
                        , normal = vec3 0 -1 0
                        , tangent = vec3 1 0 0
                        }

                    env =
                        effectiveEnvelope box [ stud1, stud2 ]
                in
                Expect.equal -4 (round (Vec3.getY env.min))
        , describe "solveSnapWithHysteresis"
            [ test "enters snap within snapEnterDistance (12 LDU)" <|
                \_ ->
                    let
                        dragged =
                            [ { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 } ]

                        target =
                            [ { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 } ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance Nothing 0 (Mat4.makeTranslate3 8 0 0) dragged target
                    in
                    Expect.notEqual Nothing result
            , test "rejects snap at 15 LDU when not already snapped (enter = 12 LDU)" <|
                \_ ->
                    let
                        dragged =
                            [ { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 } ]

                        target =
                            [ { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 } ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance Nothing 0 (Mat4.makeTranslate3 15 0 0) dragged target
                    in
                    Expect.equal Nothing result
            , test "maintains latch at 15 LDU when already active (release = 18 LDU)" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 (Mat4.makeTranslate3 15 0 0) [ draggedPt ] [ targetPt ]
                    in
                    Expect.notEqual Nothing result
            , test "breaks latch when pulled beyond release distance (20 LDU > 18 LDU)" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 24 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 24 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 (Mat4.makeTranslate3 20 0 0) [ draggedPt ] [ targetPt ]
                    in
                    Expect.equal Nothing result
            , test "directional connections unlatch if rotated away even within hysteresis distance" <|
                \_ ->
                    let
                        draggedPt =
                            { id = 1, connType = AntiStud, localPosition = vec3 0 0 0, normal = vec3 0 1 0, tangent = vec3 1 0 0 }

                        targetPt =
                            { pointId = 10, placementId = 1, connType = Stud, worldPosition = vec3 0 0 0, worldNormal = vec3 0 -1 0, worldTangent = vec3 1 0 0 }

                        activeSnap =
                            solveSnap snapEnterDistance 0 (Mat4.makeTranslate3 5 0 0) [ draggedPt ] [ targetPt ]

                        -- Rotated 180 degrees around Z axis (normal inverted to 0, -1, 0, facing away from desired normal 0, 1, 0)
                        -- at distance 10 LDU (well within release distance 18 LDU)
                        rotatedMatrix =
                            Mat4.mul (Mat4.makeTranslate3 10 0 0) (Mat4.makeRotate (degrees 180) (vec3 0 0 1))

                        result =
                            solveSnapWithHysteresis snapEnterDistance snapReleaseDistance activeSnap 0 rotatedMatrix [ draggedPt ] [ targetPt ]
                    in
                    Expect.equal Nothing result
            ]
        , describe "collision-aware snapping and isSnapColliding"
            [ test "properly stacked 2x4 brick on another 2x4 brick does not collide (isSnapColliding = False)" <|
                \_ ->
                    let
                        brickA =
                            -- Base 2x4 brick at origin Y=0 (body Y in [0, 24], top studs at Y=0)
                            { id = 1
                            , matrix = Mat4.identity
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        draggedBrickB =
                            -- Dragged 2x4 brick
                            { id = 2
                            , matrix = Mat4.makeTranslate3 0 -24 0
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        targetStud =
                            { placementId = 1
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 -30 0 -10
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 -30 24 -10
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 -22 0) [ draggedAntiStud ] [ targetStud ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 draggedBrickB.id draggedBrickB.matrix [ ( draggedBrickB.id, draggedBrickB.matrix, draggedBrickB.box ) ] [ ( brickA.id, brickA.matrix, brickA.box ) ] snap
                            in
                            Expect.equal False colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "snapping brick inside another brick returns isSnapColliding = True" <|
                \_ ->
                    let
                        brickA =
                            -- Brick 1 at origin Y=0 (Y in [0, 24])
                            { id = 1
                            , matrix = Mat4.identity
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        brickB =
                            -- Brick 2 stacked on Brick 1 (Y in [-24, 0])
                            { id = 2
                            , matrix = Mat4.makeTranslate3 0 -24 0
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        draggedBrickC =
                            { id = 3
                            , matrix = Mat4.makeTranslate3 5 -20 5
                            , box = { min = vec3 -40 0 -10, max = vec3 40 24 10 }
                            }

                        -- Covered stud on Brick 1 at Y=0 pointing up
                        coveredStud =
                            { placementId = 1
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 -20 0) [ draggedAntiStud ] [ coveredStud ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                -- snapMatrix places Brick C at Y in [-24, 0], which is occupied by Brick B!
                                colliding =
                                    isSnapColliding 0.5 draggedBrickC.id draggedBrickC.matrix [ ( draggedBrickC.id, draggedBrickC.matrix, draggedBrickC.box ) ] [ ( brickA.id, brickA.matrix, brickA.box ), ( brickB.id, brickB.matrix, brickB.box ) ] snap
                            in
                            Expect.equal True colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "solveSnapWithPredicate falls back to second candidate when closest candidate collides" <|
                \_ ->
                    let
                        brickOccupied =
                            -- Occupies X in [0, 40], Y in [-24, 0]
                            ( 1, Mat4.makeTranslate3 20 -24 0, { min = vec3 -20 0 -10, max = vec3 20 24 10 } )

                        dragged =
                            -- 1x2 brick
                            ( 2, Mat4.makeTranslate3 8 -20 0, { min = vec3 -10 0 -10, max = vec3 10 24 10 } )

                        -- Candidate 1: at X = 10 (closer, distance ~ 2 LDU, but collides with brickOccupied at X in [0, 40])
                        targetStud1 =
                            { placementId = 3
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 10 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        -- Candidate 2: at X = -10 (farther, distance ~ 18 LDU from X=8, open space, no collision)
                        targetStud2 =
                            { placementId = 3
                            , pointId = 2
                            , connType = Stud
                            , worldPosition = vec3 -10 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        currentHover =
                            Mat4.makeTranslate3 8 -20 0

                        predicate snap =
                            not (isSnapColliding 0.5 2 currentHover [ dragged ] [ brickOccupied ] snap)

                        result =
                            solveSnapWithPredicate predicate 24.0 0 currentHover [ draggedAntiStud ] [ targetStud1, targetStud2 ]
                    in
                    case result of
                        Just snap ->
                            -- The solver skipped colliding targetStud1 and picked targetStud2!
                            Expect.equal 2 snap.targetPoint.pointId

                        Nothing ->
                            Expect.fail "Expected non-colliding candidate to be chosen"
            , test "solveSnapWithPredicate returns Nothing when all candidates collide" <|
                \_ ->
                    let
                        brickOccupied =
                            ( 1, Mat4.makeTranslate3 0 -24 0, { min = vec3 -20 0 -10, max = vec3 20 24 10 } )

                        dragged =
                            ( 2, Mat4.makeTranslate3 0 -20 0, { min = vec3 -10 0 -10, max = vec3 10 24 10 } )

                        targetStud1 =
                            { placementId = 3
                            , pointId = 1
                            , connType = Stud
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 -1 0
                            , worldTangent = vec3 1 0 0
                            }

                        draggedAntiStud =
                            { id = 101
                            , connType = AntiStud
                            , localPosition = vec3 0 24 0
                            , normal = vec3 0 1 0
                            , tangent = vec3 1 0 0
                            }

                        currentHover =
                            Mat4.makeTranslate3 0 -20 0

                        predicate snap =
                            not (isSnapColliding 0.5 2 currentHover [ dragged ] [ brickOccupied ] snap)

                        result =
                            solveSnapWithPredicate predicate 24.0 0 currentHover [ draggedAntiStud ] [ targetStud1 ]
                    in
                    Expect.equal Nothing result
            , test "isSnapColliding allows Technic pin lead part to insert into hole of targetPlacementId" <|
                \_ ->
                    let
                        beamTarget =
                            -- Target beam placementId = 10
                            { id = 10
                            , matrix = Mat4.identity
                            , box = { min = vec3 -10 -10 -50, max = vec3 10 10 50 }
                            }

                        draggedPin =
                            -- Dragged pin placementId = 20
                            { id = 20
                            , matrix = Mat4.makeTranslate3 0 0 -8
                            , box = { min = vec3 -5 -5 -10, max = vec3 5 5 10 }
                            }

                        holeTarget =
                            { placementId = 10
                            , pointId = 1
                            , connType = TechnicPinFemale { length = 10.0 }
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 0 1
                            , worldTangent = vec3 1 0 0
                            }

                        pinConn =
                            { id = 1
                            , connType = TechnicPinMale { length = 10.0, friction = True }
                            , localPosition = vec3 0 0 10
                            , normal = vec3 0 0 1
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 (Mat4.makeTranslate3 0 0 -8) [ pinConn ] [ holeTarget ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 draggedPin.id draggedPin.matrix [ ( draggedPin.id, draggedPin.matrix, draggedPin.box ) ] [ ( beamTarget.id, beamTarget.matrix, beamTarget.box ) ] snap
                            in
                            Expect.equal False colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate for Technic pin"
            , test "isSnapColliding flags collision if follower part in multi-part drag collides with targetPlacementId" <|
                \_ ->
                    let
                        beamTarget =
                            -- Target beam placementId = 10, extends from Z in [-50, 50]
                            { id = 10
                            , matrix = Mat4.identity
                            , box = { min = vec3 -10 -10 -50, max = vec3 10 10 50 }
                            }

                        leadPin =
                            { id = 20
                            , matrix = Mat4.identity
                            , box = { min = vec3 -5 -5 -10, max = vec3 5 5 10 }
                            }

                        -- Follower part is offset by Z = 10, placing it inside beamTarget's box!
                        followerPart =
                            { id = 21
                            , matrix = Mat4.makeTranslate3 0 0 10
                            , box = { min = vec3 -10 -10 -10, max = vec3 10 10 10 }
                            }

                        holeTarget =
                            { placementId = 10
                            , pointId = 1
                            , connType = TechnicPinFemale { length = 10.0 }
                            , worldPosition = vec3 0 0 0
                            , worldNormal = vec3 0 0 1
                            , worldTangent = vec3 1 0 0
                            }

                        pinConn =
                            { id = 1
                            , connType = TechnicPinMale { length = 10.0, friction = True }
                            , localPosition = vec3 0 0 0
                            , normal = vec3 0 0 1
                            , tangent = vec3 1 0 0
                            }

                        maybeSnap =
                            solveSnap 12.0 0 Mat4.identity [ pinConn ] [ holeTarget ]
                    in
                    case maybeSnap of
                        Just snap ->
                            let
                                colliding =
                                    isSnapColliding 0.5 leadPin.id leadPin.matrix [ ( leadPin.id, leadPin.matrix, leadPin.box ), ( followerPart.id, followerPart.matrix, followerPart.box ) ] [ ( beamTarget.id, beamTarget.matrix, beamTarget.box ) ] snap
                            in
                            Expect.equal True colliding

                        Nothing ->
                            Expect.fail "Expected snap candidate"
            , test "snapping Bracket 1x2 Down (99781) onto Brick 1x2 (3004) returns isSnapColliding = False" <|
                \_ ->
                    case ( knownCatalogPart "99781.dat", knownCatalogPart "3004.dat" ) of
                        ( Just bracket, Just brick ) ->
                            let
                                targetMatrix =
                                    Mat4.makeTranslate3 0 -24 0

                                targetWorldPoints =
                                    List.map (Connectivity.transformPoint targetMatrix 1) brick.connections

                                hoverMatrix =
                                    Mat4.makeTranslate3 0 -30 0

                                maybeSnap =
                                    solveSnap 12.0 0 hoverMatrix bracket.connections targetWorldPoints
                            in
                            case maybeSnap of
                                Just snap ->
                                    let
                                        bracketBox =
                                            List.head bracket.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -14, max = vec3 20 20 10 }
                                                |> (\b -> effectiveEnvelope b bracket.connections)

                                        brickBox =
                                            List.head brick.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -10, max = vec3 20 24 10 }
                                                |> (\b -> effectiveEnvelope b brick.connections)

                                        colliding =
                                            isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, bracketBox ) ] [ ( 1, targetMatrix, brickBox ) ] snap
                                    in
                                    Expect.equal False colliding

                                Nothing ->
                                    Expect.fail "Bracket snap candidate not found"

                        _ ->
                            Expect.fail "99781.dat or 3004.dat not found in catalog"
            , test "snapping Plate 1x2 onto side studs of Bracket 99781 returns isSnapColliding = False" <|
                \_ ->
                    case ( knownCatalogPart "3023.dat", knownCatalogPart "99781.dat" ) of
                        ( Just plate, Just bracket ) ->
                            let
                                targetMatrix =
                                    Mat4.makeTranslate3 0 -32 0

                                sideStuds =
                                    bracket.connections
                                        |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                        |> List.map (Connectivity.transformPoint targetMatrix 1)

                                hoverMatrix =
                                    Mat4.makeTranslate3 0 -22 -20

                                maybeSnap =
                                    solveSnap 16.0 0 hoverMatrix plate.connections sideStuds
                            in
                            case maybeSnap of
                                Just snap ->
                                    let
                                        plateBox =
                                            List.head plate.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -10, max = vec3 20 8 10 }
                                                |> (\b -> effectiveEnvelope b plate.connections)

                                        bracketBox =
                                            List.head bracket.boundingBoxes
                                                |> Maybe.withDefault { min = vec3 -20 -4 -14, max = vec3 20 20 10 }
                                                |> (\b -> effectiveEnvelope b bracket.connections)

                                        colliding =
                                            isSnapColliding 0.5 2 hoverMatrix [ ( 2, hoverMatrix, plateBox ) ] [ ( 1, targetMatrix, bracketBox ) ] snap
                                    in
                                    Expect.equal False colliding

                                Nothing ->
                                    Expect.fail "Plate onto bracket side stud snap candidate not found"

                        _ ->
                            Expect.fail "3023.dat or 99781.dat not found in catalog"
            ]
        ]
