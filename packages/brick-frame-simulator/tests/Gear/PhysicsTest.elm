module Gear.PhysicsTest exposing (suite)

{-| Unit tests for gear-ratio propagation and angle sampling.
-}

import Array
import Dict
import Expect
import Gear.Physics
    exposing
        ( KinematicLinkage
        , angleAt
        , crankSliderPosition
        , crankSliderRodAngle
        , fourBarAngles
        , propagate
        , propagateWithLinkages
        )
import Gear.Types exposing (GearGraph, GearId, GearInstance, GearSpec)
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)



-- ── Helpers ───────────────────────────────────────────────────────────────────


spec : Int -> Float -> GearSpec
spec teeth pitchRadius =
    { partFile = "test.dat", teeth = teeth, pitchRadius = pitchRadius }


makeInstance : GearId -> Int -> Float -> Gear.Types.GearInstance
makeInstance id teeth pr =
    { id = id
    , spec = spec teeth pr
    , color = 16
    , worldPosition = vec3 0 0 0
    , worldMatrix = Mat4.identity
    }


{-| Build a simple linear chain: gear 0 → gear 1 → gear 2.
-}
chainGraph : Int -> Int -> Int -> GearGraph
chainGraph t0 t1 t2 =
    { instances =
        Array.fromList
            [ makeInstance 0 t0 16
            , makeInstance 1 t1 24
            , makeInstance 2 t2 16
            ]
    , connections =
        Dict.fromList
            [ ( 0, [ 1 ] )
            , ( 1, [ 0, 2 ] )
            , ( 2, [ 1 ] )
            ]
    , rigidAxles = Dict.empty
    }


twoGearGraph : Int -> Int -> GearGraph
twoGearGraph t0 t1 =
    { instances =
        Array.fromList
            [ makeInstance 0 t0 16
            , makeInstance 1 t1 24
            ]
    , connections =
        Dict.fromList
            [ ( 0, [ 1 ] )
            , ( 1, [ 0 ] )
            ]
    , rigidAxles = Dict.empty
    }


isolatedGraph : GearGraph
isolatedGraph =
    { instances =
        Array.fromList
            [ makeInstance 0 8 16
            , makeInstance 1 16 24
            ]
    , connections = Dict.empty
    , rigidAxles = Dict.empty
    }



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Gear.Physics"
        [ describe "propagate — basic"
            [ test "motor gear maps to motorAngle" <|
                \_ ->
                    propagate (twoGearGraph 8 16) 0 1.0
                        |> angleAt
                        |> (\f -> f 0)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "disconnected gear is absent from result" <|
                \_ ->
                    propagate isolatedGraph 0 1.0
                        |> Dict.member 1
                        |> Expect.equal False
            , test "empty graph returns only motor gear" <|
                \_ ->
                    let
                        emptyGraph =
                            { instances = Array.fromList [ makeInstance 0 8 16 ]
                            , connections = Dict.empty
                            , rigidAxles = Dict.empty
                            }

                        result =
                            propagate emptyGraph 0 2.0
                    in
                    Expect.equal (Dict.fromList [ ( 0, 2.0 ) ]) result
            ]
        , describe "propagate — gear ratios"
            [ test "8T driving 16T: neighbour angle is -0.5 × motor" <|
                \_ ->
                    propagate (twoGearGraph 8 16) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            , test "16T driving 8T: neighbour angle is -2 × motor" <|
                \_ ->
                    propagate (twoGearGraph 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -2.0
            , test "equal gears (8T → 8T): neighbour angle is -1 × motor" <|
                \_ ->
                    propagate (twoGearGraph 8 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -1.0
            , test "worm (1T) driving 24T: neighbour angle is +1/24 × motor (no sign inversion)" <|
                \_ ->
                    propagate (twoGearGraph 1 24) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) (1 / 24)
            ]
        , describe "propagate — 3-gear chain (8T → 16T → 8T)"
            [ test "gear 0 ratio = 1.0 (motor)" <|
                \_ ->
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 0)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "gear 1 ratio = -0.5" <|
                \_ ->
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            , test "gear 2 ratio = +1.0 (two sign inversions, same tooth count as gear 0)" <|
                \_ ->
                    -- gear 1 angle = -0.5; gear 2 = -0.5 * -(16/8) = +1.0
                    -- Net: same speed as motor, positive direction (two inversions cancel)
                    propagate (chainGraph 8 16 8) 0 1.0
                        |> angleAt
                        |> (\f -> f 2)
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.0
            , test "all three gears are in the result" <|
                \_ ->
                    let
                        result =
                            propagate (chainGraph 8 16 8) 0 1.0
                    in
                    Expect.equal 3 (Dict.size result)
            ]
        , describe "angleAt"
            [ test "returns correct angle for present key" <|
                \_ ->
                    angleAt (Dict.fromList [ ( 0, 1.5 ) ]) 0
                        |> Expect.within (Expect.Absolute 1.0e-6) 1.5
            , test "returns 0.0 for absent key" <|
                \_ ->
                    angleAt Dict.empty 99
                        |> Expect.within (Expect.Absolute 1.0e-6) 0.0
            ]
        , describe "propagate — rigid axle"
            [ test "gear on same axle rotates at the same angle" <|
                \_ ->
                    let
                        rigidGraph =
                            { instances =
                                Array.fromList
                                    [ makeInstance 0 8 10
                                    , makeInstance 1 8 10
                                    ]
                            , connections = Dict.empty
                            , rigidAxles =
                                Dict.fromList
                                    [ ( 0, [ 1 ] )
                                    , ( 1, [ 0 ] )
                                    ]
                            }
                    in
                    propagate rigidGraph 0 2.5
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) 2.5
            , test "rigid coupling chains through to a meshing gear" <|
                \_ ->
                    -- Gear 0 (motor, 8T) is rigid-coupled to gear 1 (8T),
                    -- and gear 1 meshes with gear 2 (16T).
                    -- Expected angle for gear 2: motor * 1 * -(8/16) = -0.5.
                    let
                        mixedGraph =
                            { instances =
                                Array.fromList
                                    [ makeInstance 0 8 10
                                    , makeInstance 1 8 10
                                    , makeInstance 2 16 20
                                    ]
                            , connections =
                                Dict.fromList
                                    [ ( 1, [ 2 ] )
                                    , ( 2, [ 1 ] )
                                    ]
                            , rigidAxles =
                                Dict.fromList
                                    [ ( 0, [ 1 ] )
                                    , ( 1, [ 0 ] )
                                    ]
                            }
                    in
                    propagate mixedGraph 0 1.0
                        |> angleAt
                        |> (\f -> f 2)
                        |> Expect.within (Expect.Absolute 1.0e-6) -0.5
            ]
        , describe "propagate — signed ratios from gear geometry"
            [ test "same-orientation 8T driving 24T along X: -1/3 (unchanged rule)" <|
                \_ ->
                    let
                        d =
                            (8 + 24) / (2 * pi)

                        g =
                            geometryGraph
                                [ placedGear "3647.dat" 0 8 Mat4.identity
                                , placedGear "3648.dat" 1 24 (Mat4.makeTranslate3 d 0 0)
                                ]
                    in
                    propagate g 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) (-1 / 3)
            , test "one gear flipped (16T flipped, 24T upright): ratio sign is positive" <|
                \_ ->
                    let
                        d =
                            (16 + 24) / (2 * pi)

                        g =
                            geometryGraph
                                [ placedGear "4019.dat" 0 16 Mat4.identity
                                , placedGear "3648.dat" 1 24 (Mat4.mul (Mat4.makeTranslate3 d 0 0) flipX)
                                ]
                    in
                    propagate g 0 1.0
                        |> angleAt
                        |> (\f -> f 1)
                        |> Expect.within (Expect.Absolute 1.0e-6) (2 / 3)
            , test "crown 24T with spur 16T: sign is the product of the two axis flips" <|
                \_ ->
                    let
                        spurDistance =
                            16 / (2 * pi)

                        expectation ( ss, sc ) () =
                            let
                                g =
                                    geometryGraph
                                        [ placedGear "3650b.dat" 0 24 (flipIf (sc < 0) Mat4.identity)
                                        , placedGear "4019.dat" 1 16 (Mat4.mul (Mat4.makeTranslate3 0 0 spurDistance) (Mat4.makeRotate (toFloat ss * pi / 2) (vec3 0 1 0)))
                                        ]
                            in
                            propagate g 0 1.0
                                |> angleAt
                                |> (\f -> f 1)
                                |> Expect.within (Expect.Absolute 1.0e-6) (1.5 * toFloat (ss * sc))
                    in
                    Expect.all
                        (List.map expectation [ ( 1, 1 ), ( 1, -1 ), ( -1, 1 ), ( -1, -1 ) ])
                        ()
            ]
        , describe "Kinematic Linkages"
            [ test "crank-slider calculates exact position at top/bottom dead center and quadrature" <|
                \_ ->
                    let
                        r =
                            10.0

                        l =
                            30.0

                        -- At theta = 0: x = r + l = 40
                        x0 =
                            crankSliderPosition r l 0.0

                        -- At theta = pi: x = l - r = 20
                        xPi =
                            crankSliderPosition r l pi

                        -- At theta = pi/2: x = sqrt(30^2 - 10^2) = sqrt(800) ≈ 28.284
                        xQuad =
                            crankSliderPosition r l (pi / 2)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 40.0 x0
                        , \_ -> Expect.within (Expect.Absolute 0.001) 20.0 xPi
                        , \_ -> Expect.within (Expect.Absolute 0.001) (sqrt 800) xQuad
                        ]
                        ()
            , test "connecting rod angle tilts and aligns with stroke" <|
                \_ ->
                    let
                        r =
                            10.0

                        l =
                            20.0

                        phi0 =
                            crankSliderRodAngle r l 0.0

                        phiQuad =
                            crankSliderRodAngle r l (pi / 2)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0.0 phi0
                        , \_ -> Expect.within (Expect.Absolute 0.001) (asin -0.5) phiQuad
                        ]
                        ()
            , test "propagateWithLinkages updates slider positions from driven gears" <|
                \_ ->
                    let
                        graph =
                            twoGearGraph 8 16

                        linkage : KinematicLinkage
                        linkage =
                            { id = 101
                            , drivingGearId = 1
                            , crankRadius = 10.0
                            , rodLength = 30.0
                            , crankOffset = 0.0
                            }

                        -- Motor at gear 0 rotates 2.0 rad. Gear 1 (ratio -8/16 = -0.5) rotates -1.0 rad.
                        result =
                            propagateWithLinkages graph [ linkage ] 0 2.0
                    in
                    case Dict.get 101 result.linkagePositions of
                        Just state ->
                            let
                                expectedX =
                                    crankSliderPosition 10.0 30.0 -1.0
                            in
                            Expect.within (Expect.Absolute 0.001) expectedX state.sliderX

                        Nothing ->
                            Expect.fail "expected linkage state"
            , test "crank-slider clamps gracefully when crank radius exceeds rod length (r > l)" <|
                \_ ->
                    let
                        pos =
                            crankSliderPosition 25.0 10.0 (pi / 2)

                        rodAngle =
                            crankSliderRodAngle 25.0 10.0 (pi / 2)
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0.0 pos
                        , \_ -> Expect.within (Expect.Absolute 0.001) -(pi / 2) rodAngle
                        ]
                        ()
            , test "fourBarAngles solves closed-loop coupler and rocker angles" <|
                \_ ->
                    let
                        links =
                            { ground = 40.0, crank = 15.0, coupler = 40.0, rocker = 15.0 }

                        theta =
                            pi / 3
                    in
                    case fourBarAngles links theta of
                        Just ( couplerAngle, rockerAngle ) ->
                            let
                                bx =
                                    links.crank * cos theta

                                by =
                                    links.crank * sin theta

                                cxFromCoupler =
                                    bx + links.coupler * cos couplerAngle

                                cyFromCoupler =
                                    by + links.coupler * sin couplerAngle

                                cxFromRocker =
                                    links.ground + links.rocker * cos rockerAngle

                                cyFromRocker =
                                    links.rocker * sin rockerAngle
                            in
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.001) cxFromRocker cxFromCoupler
                                , \_ -> Expect.within (Expect.Absolute 0.001) cyFromRocker cyFromCoupler
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected valid four-bar linkage solution"
            , test "fourBarAngles returns Nothing when linkage cannot close (triangle inequality violated)" <|
                \_ ->
                    fourBarAngles { ground = 100.0, crank = 10.0, coupler = 20.0, rocker = 20.0 } 0.0
                        |> Expect.equal Nothing
            ]
        ]



-- ── Geometry helpers ──────────────────────────────────────────────────────────


flipX : Mat4.Mat4
flipX =
    Mat4.makeRotate pi (vec3 1 0 0)


flipIf : Bool -> Mat4.Mat4 -> Mat4.Mat4
flipIf flipped m =
    if flipped then
        Mat4.mul m flipX

    else
        m


{-| A gear with pitch radius `teeth / (2·pi)` at the origin of its matrix.
-}
placedGear : String -> GearId -> Int -> Mat4.Mat4 -> GearInstance
placedGear partFile id teeth m =
    { id = id
    , spec = { partFile = partFile, teeth = teeth, pitchRadius = toFloat teeth / (2 * pi) }
    , color = 16
    , worldPosition = Mat4.transform m (vec3 0 0 0)
    , worldMatrix = m
    }


{-| A graph in which every gear meshes with every other one.
-}
geometryGraph : List GearInstance -> GearGraph
geometryGraph gears =
    let
        ids =
            List.map .id gears
    in
    { instances = Array.fromList gears
    , connections =
        Dict.fromList
            (List.map (\g -> ( g.id, List.filter (\i -> i /= g.id) ids )) gears)
    , rigidAxles = Dict.empty
    }
