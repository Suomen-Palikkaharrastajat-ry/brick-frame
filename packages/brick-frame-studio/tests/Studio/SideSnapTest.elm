module Studio.SideSnapTest exposing (suite)

{-| Unit tests for Studio.SideSnap: ray reach, ordering nearest to eye first,
and approachMatrix for SNOT magnetic snapping.
-}

import Expect
import LDraw.Connectivity as Connectivity
    exposing
        ( ConnectionPoint
        , ConnectionType(..)
        , WorldConnectionPoint
        , knownCatalogPart
        )
import LDraw.Parser as Parser
import LDraw.Types exposing (LDrawLine)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Studio.SideSnap as SideSnap
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.SideSnap"
        [ targetsNearRayTests
        , approachMatrixTests
        , roundPlateOnSideStudTests
        , multiAntiStudSelectionTests
        , fourStepRotationTests
        ]


targetsNearRayTests : Test
targetsNearRayTests =
    describe "targetsNearRay"
        [ test "filters out vertical connection points (top studs and bottom anti-studs)" <|
            \_ ->
                let
                    ray =
                        { origin = vec3 0 0 -100
                        , direction = vec3 0 0 1
                        }

                    topStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 -1 0
                        , worldTangent = vec3 1 0 0
                        }

                    sideStud =
                        { placementId = 2
                        , pointId = 2
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }
                in
                SideSnap.targetsNearRay ray 16.0 [ topStud, sideStud ]
                    |> List.map .pointId
                    |> Expect.equal [ 2 ]
        , test "includes side points within reach and excludes those outside reach" <|
            \_ ->
                let
                    ray =
                        { origin = vec3 0 0 -100
                        , direction = vec3 0 0 1
                        }

                    nearSideStud =
                        { placementId = 1
                        , pointId = 1
                        , connType = Stud
                        , worldPosition = vec3 10 0 0
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }

                    farSideStud =
                        { placementId = 2
                        , pointId = 2
                        , connType = Stud
                        , worldPosition = vec3 30 0 0
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }
                in
                SideSnap.targetsNearRay ray 16.0 [ nearSideStud, farSideStud ]
                    |> List.map .pointId
                    |> Expect.equal [ 1 ]
        , test "orders targets nearest to the eye first along the ray" <|
            \_ ->
                let
                    ray =
                        { origin = vec3 0 0 -100
                        , direction = vec3 0 0 1
                        }

                    closerTarget =
                        { placementId = 1
                        , pointId = 10
                        , connType = Stud
                        , worldPosition = vec3 0 0 -50
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }

                    fartherTarget =
                        { placementId = 2
                        , pointId = 20
                        , connType = Stud
                        , worldPosition = vec3 0 0 50
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }
                in
                SideSnap.targetsNearRay ray 16.0 [ fartherTarget, closerTarget ]
                    |> List.map .pointId
                    |> Expect.equal [ 10, 20 ]
        ]


approachMatrixTests : Test
approachMatrixTests =
    describe "approachMatrix"
        [ test "translates lead matrix so nearest compatible point meets target" <|
            \_ ->
                case knownCatalogPart "3070b.dat" of
                    Just tile1x1 ->
                        let
                            leadMatrix =
                                Mat4.makeTranslate3 50 -20 30

                            target =
                                { placementId = 1
                                , pointId = 1
                                , connType = Stud
                                , worldPosition = vec3 0 -14 -10
                                , worldNormal = vec3 0 0 -1
                                , worldTangent = vec3 1 0 0
                                }
                        in
                        case SideSnap.approachMatrix leadMatrix tile1x1.connections target of
                            Just approach ->
                                let
                                    tileAntiStud =
                                        tile1x1.connections
                                            |> List.head
                                            |> Maybe.withDefault
                                                { id = 100
                                                , connType = AntiStud
                                                , localPosition = vec3 0 8 0
                                                , normal = vec3 0 1 0
                                                , tangent = vec3 1 0 0
                                                }

                                    placedPoint =
                                        Mat4.transform approach tileAntiStud.localPosition
                                in
                                Expect.lessThan 0.001 (Vec3.distance placedPoint target.worldPosition)

                            Nothing ->
                                Expect.fail "approachMatrix returned Nothing for compatible tile"

                    Nothing ->
                        Expect.fail "3070b.dat not found in catalog"
        , test "returns Nothing if dragged part has no compatible connection point" <|
            \_ ->
                let
                    studOnlyPart =
                        [ { id = 1
                          , connType = Stud
                          , localPosition = vec3 0 0 0
                          , normal = vec3 0 -1 0
                          , tangent = vec3 1 0 0
                          }
                        ]

                    studTarget =
                        { placementId = 1
                        , pointId = 2
                        , connType = Stud
                        , worldPosition = vec3 0 0 0
                        , worldNormal = vec3 0 0 -1
                        , worldTangent = vec3 1 0 0
                        }
                in
                Expect.equal Nothing (SideSnap.approachMatrix Mat4.identity studOnlyPart studTarget)
        ]


{-| The connection-bearing lines of 85861 (Plate 1 x 1 Round with Open Stud), as
its LDraw file has them: an open stud on top, and a flipped stud4 tube from
Y = 3 down to the underside at Y = 8.
-}
roundPlateLines : String -> Maybe (List LDrawLine)
roundPlateLines name =
    if name == "85861.dat" then
        Just
            (Parser.parseFile
                (String.join "\n"
                    [ "1 16 0 3 0 -1 0 0 0 -1.25 0 0 0 1 stud4.dat"
                    , "1 16 0 0 0 10 0 0 0 3 0 0 0 10 4-4cyli.dat"
                    , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud2a.dat"
                    ]
                )
            )

    else
        Nothing


roundPlateOnSideStudTests : Test
roundPlateOnSideStudTests =
    let
        plate =
            Connectivity.extractPartConnectivity "85861.dat" roundPlateLines
    in
    describe "a 1 x 1 round plate on a brick's side stud"
        [ test "the plate's anti-stud is on its underside, facing down" <|
            \_ ->
                plate.connections
                    |> List.filter (\c -> c.connType == AntiStud)
                    |> List.map (\c -> ( Vec3.toRecord c.localPosition, Vec3.toRecord c.normal ))
                    |> Expect.equal [ ( { x = 0, y = 8, z = 0 }, { x = 0, y = 1, z = 0 } ) ]
        , test "the plate's box is a plate's: from its stud top to its underside" <|
            \_ ->
                plate.boundingBoxes
                    |> List.map (\b -> ( Vec3.toRecord b.min, Vec3.toRecord b.max ))
                    |> Expect.equal [ ( { x = -10, y = -4, z = -10 }, { x = 10, y = 8, z = 10 } ) ]
        , test "the plate snaps onto 87087's side stud without colliding with the brick" <|
            \_ ->
                case knownCatalogPart "87087.dat" of
                    Just brick ->
                        let
                            brickMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            target =
                                brick.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint brickMatrix 1)
                                    |> List.head

                            -- Tipped so its underside faces the stud (+Z), a little off it
                            leadMatrix =
                                Mat4.mul (Mat4.makeTranslate3 3 -12 -30) (Mat4.makeRotate (pi / 2) (vec3 1 0 0))

                            envelope conn =
                                List.head conn.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                    |> (\box -> Connectivity.effectiveEnvelope box conn.connections)

                            snapOnto tp =
                                SideSnap.approachMatrix leadMatrix plate.connections tp
                                    |> Maybe.map (\approach -> Connectivity.solveSnapCandidates 24 0 approach plate.connections [ tp ])
                                    |> Maybe.andThen List.head
                                    |> Maybe.map (\snap -> ( tp, snap ))
                        in
                        case Maybe.andThen snapOnto target of
                            Just ( tp, snap ) ->
                                let
                                    antiStud =
                                        Mat4.transform snap.snapMatrix (vec3 0 8 0)
                                in
                                Expect.all
                                    [ \_ -> Expect.equal AntiStud snap.draggedPoint.connType
                                    , \_ -> Expect.lessThan 0.001 (Vec3.distance antiStud tp.worldPosition)
                                    , \_ ->
                                        Connectivity.isSnapColliding 0.5
                                            0
                                            leadMatrix
                                            [ ( 0, leadMatrix, envelope plate ) ]
                                            [ ( 1, brickMatrix, envelope brick ) ]
                                            snap
                                            |> Expect.equal False
                                    ]
                                    ()

                            Nothing ->
                                Expect.fail "the plate found no snap on the side stud"

                    Nothing ->
                        Expect.fail "87087.dat not found in catalog"
        ]


multiAntiStudSelectionTests : Test
multiAntiStudSelectionTests =
    describe "multi-anti-stud selection on side studs"
        [ test "approachMatrix selects the closer anti-stud on a 1 x 2 tile" <|
            \_ ->
                case ( knownCatalogPart "3069b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x2, Just brick87087 ) ->
                        let
                            brickMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            targetSideStud =
                                brick87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint brickMatrix 1)
                                    |> List.head
                                    |> Maybe.withDefault
                                        { placementId = 1
                                        , pointId = 200
                                        , connType = Stud
                                        , worldPosition = vec3 0 -14 -10
                                        , worldNormal = vec3 0 0 -1
                                        , worldTangent = vec3 0 1 0
                                        }

                            antiStuds =
                                List.filter (\c -> c.connType == AntiStud) tile1x2.connections

                            antiStud1 =
                                List.filter (\c -> Vec3.getX c.localPosition < 0) antiStuds
                                    |> List.head

                            antiStud2 =
                                List.filter (\c -> Vec3.getX c.localPosition > 0) antiStuds
                                    |> List.head

                            tipMatrix =
                                Mat4.makeRotate (pi / 2) (vec3 1 0 0)

                            leadMatrixA =
                                Mat4.mul (Mat4.makeTranslate3 10 -14 -18) tipMatrix

                            leadMatrixB =
                                Mat4.mul (Mat4.makeTranslate3 -10 -14 -18) tipMatrix
                        in
                        case ( antiStud1, antiStud2 ) of
                            ( Just as1, Just as2 ) ->
                                let
                                    approachA =
                                        SideSnap.approachMatrix leadMatrixA tile1x2.connections targetSideStud

                                    approachB =
                                        SideSnap.approachMatrix leadMatrixB tile1x2.connections targetSideStud
                                in
                                case ( approachA, approachB ) of
                                    ( Just matA, Just matB ) ->
                                        let
                                            placedAs1 =
                                                Mat4.transform matA as1.localPosition

                                            placedAs2 =
                                                Mat4.transform matB as2.localPosition
                                        in
                                        Expect.all
                                            [ \_ -> Expect.lessThan 0.001 (Vec3.distance placedAs1 targetSideStud.worldPosition)
                                            , \_ -> Expect.lessThan 0.001 (Vec3.distance placedAs2 targetSideStud.worldPosition)
                                            ]
                                            ()

                                    _ ->
                                        Expect.fail "approachMatrix returned Nothing for 1x2 tile"

                            _ ->
                                Expect.fail "3069b.dat missing anti-studs"

                    _ ->
                        Expect.fail "3069b.dat or 87087.dat not found in catalog"
        ]


fourStepRotationTests : Test
fourStepRotationTests =
    describe "4-step in-flight rotation on side studs"
        [ test "rotationIndex 0, 1, 2, 3 produce valid SO(3) rotation matrices and clean alignment without collisions" <|
            \_ ->
                case ( knownCatalogPart "3070b.dat", knownCatalogPart "87087.dat" ) of
                    ( Just tile1x1, Just brick87087 ) ->
                        let
                            brickMatrix =
                                Mat4.makeTranslate3 0 -24 0

                            targetSideStud =
                                brick87087.connections
                                    |> List.filter (\c -> c.connType == Stud && Vec3.getZ c.normal < -0.7)
                                    |> List.map (Connectivity.transformPoint brickMatrix 1)
                                    |> List.head

                            envelope conn =
                                List.head conn.boundingBoxes
                                    |> Maybe.withDefault { min = vec3 0 0 0, max = vec3 0 0 0 }
                                    |> (\box -> Connectivity.effectiveEnvelope box conn.connections)

                            brickBox =
                                envelope brick87087

                            tileBox =
                                envelope tile1x1

                            leadMatrix =
                                Mat4.mul (Mat4.makeTranslate3 0 -14 -18) (Mat4.makeRotate (pi / 2) (vec3 1 0 0))

                            testRotation rotIdx =
                                case targetSideStud of
                                    Just target ->
                                        let
                                            snapCandidates =
                                                SideSnap.approachMatrix leadMatrix tile1x1.connections target
                                                    |> Maybe.map (\approach -> Connectivity.solveSnapCandidates 24 rotIdx approach tile1x1.connections [ target ])
                                                    |> Maybe.andThen List.head
                                        in
                                        case snapCandidates of
                                            Just snap ->
                                                let
                                                    mat =
                                                        snap.snapMatrix

                                                    c1 =
                                                        Connectivity.transformDirection mat (vec3 1 0 0)

                                                    c2 =
                                                        Connectivity.transformDirection mat (vec3 0 1 0)

                                                    c3 =
                                                        Connectivity.transformDirection mat (vec3 0 0 1)

                                                    len1 =
                                                        Vec3.length c1

                                                    len2 =
                                                        Vec3.length c2

                                                    len3 =
                                                        Vec3.length c3

                                                    dot12 =
                                                        abs (Vec3.dot c1 c2)

                                                    dot23 =
                                                        abs (Vec3.dot c2 c3)

                                                    dot31 =
                                                        abs (Vec3.dot c3 c1)

                                                    det =
                                                        Vec3.dot (Vec3.cross c1 c2) c3

                                                    colliding =
                                                        Connectivity.isSnapColliding 0.5
                                                            0
                                                            leadMatrix
                                                            [ ( 0, leadMatrix, tileBox ) ]
                                                            [ ( 1, brickMatrix, brickBox ) ]
                                                            snap
                                                in
                                                Expect.all
                                                    [ \_ -> Expect.lessThan 0.001 (abs (len1 - 1.0))
                                                    , \_ -> Expect.lessThan 0.001 (abs (len2 - 1.0))
                                                    , \_ -> Expect.lessThan 0.001 (abs (len3 - 1.0))
                                                    , \_ -> Expect.lessThan 0.001 dot12
                                                    , \_ -> Expect.lessThan 0.001 dot23
                                                    , \_ -> Expect.lessThan 0.001 dot31
                                                    , \_ -> Expect.lessThan 0.001 (abs (det - 1.0))
                                                    , \_ -> Expect.equal False colliding
                                                    ]
                                                    ()

                                            Nothing ->
                                                Expect.fail ("Failed to find snap candidate for rotationIndex " ++ String.fromInt rotIdx)

                                    Nothing ->
                                        Expect.fail "Missing target side stud"
                        in
                        Expect.all (List.map (\idx -> \_ -> testRotation idx) [ 0, 1, 2, 3 ]) ()

                    _ ->
                        Expect.fail "3070b.dat or 87087.dat not found in catalog"
        ]
