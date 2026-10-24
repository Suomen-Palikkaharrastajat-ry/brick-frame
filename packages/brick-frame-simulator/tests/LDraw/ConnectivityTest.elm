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
        , knownCatalogPart
        , snapEnterDistance
        , snapReleaseDistance
        , solveSnap
        , solveSnapWithHysteresis
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
        ]
