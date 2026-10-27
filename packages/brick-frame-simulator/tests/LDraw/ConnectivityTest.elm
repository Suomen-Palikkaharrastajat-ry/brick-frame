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
        , test "extracts stud4.dat as AntiStud with normal (0, 1, 0)" <|
            \_ ->
                let
                    lines =
                        [ SubFileRef
                            { color = 16
                            , transform = Mat4.makeTranslate3 0 24 0
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
            ]
        ]
