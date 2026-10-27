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
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Studio.SideSnap as SideSnap
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.SideSnap"
        [ targetsNearRayTests
        , approachMatrixTests
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
