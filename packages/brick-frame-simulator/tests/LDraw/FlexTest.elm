module LDraw.FlexTest exposing (suite)

{-| Unit tests for cubic Bezier spline generation, parallel transport frames,
and belt/chain wrap routing.
-}

import Expect
import LDraw.Flex as Flex exposing (BeltPulley, FlexKind(..), SplineConfig)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Flex"
        [ describe "Cubic Bezier curve"
            [ test "interpolates start and end control points" <|
                \_ ->
                    let
                        c0 =
                            vec3 0 0 0

                        c1 =
                            vec3 10 10 0

                        c2 =
                            vec3 20 10 0

                        c3 =
                            vec3 30 0 0

                        pStart =
                            Flex.cubicBezier c0 c1 c2 c3 0.0

                        pEnd =
                            Flex.cubicBezier c0 c1 c2 c3 1.0
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getX pStart)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 30 (Vec3.getX pEnd)
                        ]
                        ()
            , test "tangent direction at endpoints matches control vectors" <|
                \_ ->
                    let
                        c0 =
                            vec3 0 0 0

                        c1 =
                            vec3 0 10 0

                        c2 =
                            vec3 20 10 0

                        c3 =
                            vec3 20 0 0

                        tan0 =
                            Flex.cubicBezierTangent c0 c1 c2 c3 0.0

                        tan1 =
                            Flex.cubicBezierTangent c0 c1 c2 c3 1.0
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getX tan0)
                        , \_ -> Expect.greaterThan 0.0 (Vec3.getY tan0)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getX tan1)
                        , \_ -> Expect.lessThan 0.0 (Vec3.getY tan1)
                        ]
                        ()
            , test "straight line curve length matches distance" <|
                \_ ->
                    let
                        pointAt t =
                            vec3 (t * 50) 0 0

                        len =
                            Flex.curveLength pointAt 20
                    in
                    Expect.within (Expect.Absolute 0.01) 50.0 len
            ]
        , describe "Procedural spline routing"
            [ test "evaluates spline into discretized frames" <|
                \_ ->
                    let
                        config : SplineConfig
                        config =
                            { startPoint = vec3 0 0 0
                            , startTangent = vec3 0 1 0
                            , endPoint = vec3 40 40 0
                            , endTangent = vec3 1 0 0
                            , slack = 1.0
                            , segmentPitch = 4.0
                            }

                        route =
                            Flex.evaluateSpline config
                    in
                    Expect.all
                        [ \_ -> Expect.greaterThan 40.0 route.totalLength
                        , \_ -> Expect.greaterThan 10 route.segmentCount
                        , \_ -> Expect.equal (route.segmentCount + 1) (List.length route.frames)
                        ]
                        ()
            , test "parallel transport frames maintain orthonormality without twist" <|
                \_ ->
                    let
                        config : SplineConfig
                        config =
                            { startPoint = vec3 0 0 0
                            , startTangent = vec3 0 0 1
                            , endPoint = vec3 30 20 30
                            , endTangent = vec3 1 0 0
                            , slack = 1.0
                            , segmentPitch = 5.0
                            }

                        route =
                            Flex.evaluateSpline config

                        checkOrthonormal frame =
                            let
                                dotTN =
                                    Vec3.dot frame.tangent frame.normal

                                dotTB =
                                    Vec3.dot frame.tangent frame.binormal

                                dotNB =
                                    Vec3.dot frame.normal frame.binormal
                            in
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.01) 0 dotTN
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 dotTB
                                , \_ -> Expect.within (Expect.Absolute 0.01) 0 dotNB
                                , \_ -> Expect.within (Expect.Absolute 0.01) 1 (Vec3.length frame.tangent)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 1 (Vec3.length frame.normal)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 1 (Vec3.length frame.binormal)
                                ]
                                ()
                    in
                    Expect.all (List.map (\f -> \_ -> checkOrthonormal f) route.frames) ()
            , test "frame matrices position segments at curve coordinates" <|
                \_ ->
                    let
                        config : SplineConfig
                        config =
                            { startPoint = vec3 10 0 0
                            , startTangent = vec3 0 1 0
                            , endPoint = vec3 50 0 0
                            , endTangent = vec3 0 -1 0
                            , slack = 1.0
                            , segmentPitch = 10.0
                            }

                        route =
                            Flex.evaluateSpline config

                        firstFrame =
                            List.head route.frames

                        lastFrame =
                            List.head (List.reverse route.frames)
                    in
                    case ( firstFrame, lastFrame ) of
                        ( Just f0, Just fn ) ->
                            let
                                p0 =
                                    Mat4.transform f0.matrix (vec3 0 0 0)

                                pn =
                                    Mat4.transform fn.matrix (vec3 0 0 0)
                            in
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.01) 10 (Vec3.getX p0)
                                , \_ -> Expect.within (Expect.Absolute 0.01) 50 (Vec3.getX pn)
                                ]
                                ()

                        _ ->
                            Expect.fail "expected frames"
            ]
        , describe "Belt and chain wrap routing"
            [ test "routes chain loop wrapped around two pulleys" <|
                \_ ->
                    let
                        p1 : BeltPulley
                        p1 =
                            { center = vec3 0 0 0, radius = 16.0, axis = vec3 0 1 0 }

                        p2 : BeltPulley
                        p2 =
                            { center = vec3 64 0 0, radius = 16.0, axis = vec3 0 1 0 }

                        chain =
                            Flex.routeBeltOrChain p1 p2 16.0
                    in
                    Expect.all
                        [ -- Expected length: 2 * 64 + 2 * pi * 16 ≈ 128 + 100.53 ≈ 228.53 LDU
                          \_ -> Expect.within (Expect.Absolute 2.0) 228.5 chain.totalLength
                        , \_ -> Expect.greaterThan 10 chain.segmentCount
                        , \_ -> Expect.equal (chain.segmentCount + 1) (List.length chain.frames)
                        ]
                        ()
            , test "LSynth emitter produces begin, end, and segment placements" <|
                \_ ->
                    let
                        p1 : BeltPulley
                        p1 =
                            { center = vec3 0 0 0, radius = 10.0, axis = vec3 0 1 0 }

                        p2 : BeltPulley
                        p2 =
                            { center = vec3 40 0 0, radius = 10.0, axis = vec3 0 1 0 }

                        route =
                            Flex.routeBeltOrChain p1 p2 16.0

                        lines =
                            Flex.generateLSynthLines "3711.dat" 7 route
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just "0 SYNTH BEGIN 3711.dat") (List.head lines)
                        , \_ -> Expect.equal (Just "0 SYNTH END") (List.head (List.reverse lines))
                        , \_ -> Expect.greaterThan 3 (List.length lines)
                        ]
                        ()
            ]
        ]
