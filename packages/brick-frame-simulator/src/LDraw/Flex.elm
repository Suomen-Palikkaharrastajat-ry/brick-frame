module LDraw.Flex exposing
    ( BeltPulley
    , CurveFrame
    , CurveSample
    , FlexKind(..)
    , FlexRoute
    , SplineConfig
    , cubicBezier
    , cubicBezierTangent
    , curveLength
    , defaultPitch
    , evaluateSpline
    , generateLSynthLines
    , generatePlacements
    , parallelTransportFrames
    , routeBeltOrChain
    )

{-| Procedural spline routing and segment placement for flexible parts.

Supports cubic Bezier curve evaluation, parallel transport frames (rotation-minimizing
Bishop frames) to prevent twisting, chain/belt pulley routing, and LSynth emission.

-}

import LDraw.Document exposing (Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| The kind of flexible element being generated.
-}
type FlexKind
    = PneumaticHose4mm
    | RibbedHose
    | FlexAxle
    | TechnicChain
    | RubberBelt


{-| Default segment pitch in LDU (LDraw Units).
-}
defaultPitch : FlexKind -> Float
defaultPitch kind =
    case kind of
        PneumaticHose4mm ->
            4.0

        RibbedHose ->
            4.0

        FlexAxle ->
            16.0

        TechnicChain ->
            16.0

        RubberBelt ->
            4.0


{-| Configuration for cubic Bezier spline generation between two anchors.
-}
type alias SplineConfig =
    { startPoint : Vec3
    , startTangent : Vec3
    , endPoint : Vec3
    , endTangent : Vec3
    , slack : Float
    , segmentPitch : Float
    }


{-| A sample point along a curve before parallel transport frame evaluation.
-}
type alias CurveSample =
    { point : Vec3
    , tangent : Vec3
    , t : Float
    , distance : Float
    }


{-| An oriented reference frame along the curve.
-}
type alias CurveFrame =
    { point : Vec3
    , tangent : Vec3
    , normal : Vec3
    , binormal : Vec3
    , matrix : Mat4
    , t : Float
    , distance : Float
    }


{-| Evaluated curve route with discretized frames and total length.
-}
type alias FlexRoute =
    { totalLength : Float
    , segmentCount : Int
    , frames : List CurveFrame
    }


{-| Pulley specification for belt or chain wrap routing.
-}
type alias BeltPulley =
    { center : Vec3
    , radius : Float
    , axis : Vec3
    }


{-| Evaluate a cubic Bezier curve at parameter `t` in [0, 1].
-}
cubicBezier : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
cubicBezier c0 c1 c2 c3 t =
    let
        u =
            1 - t

        tt =
            t * t

        uu =
            u * u

        uuu =
            uu * u

        ttt =
            tt * t

        p0 =
            Vec3.scale uuu c0

        p1 =
            Vec3.scale (3 * uu * t) c1

        p2 =
            Vec3.scale (3 * u * tt) c2

        p3 =
            Vec3.scale ttt c3
    in
    Vec3.add p0 (Vec3.add p1 (Vec3.add p2 p3))


{-| Evaluate the tangent velocity vector of a cubic Bezier curve at parameter `t`.
-}
cubicBezierTangent : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Vec3
cubicBezierTangent c0 c1 c2 c3 t =
    let
        u =
            1 - t

        d0 =
            Vec3.sub c1 c0

        d1 =
            Vec3.sub c2 c1

        d2 =
            Vec3.sub c3 c2

        term0 =
            Vec3.scale (3 * u * u) d0

        term1 =
            Vec3.scale (6 * u * t) d1

        term2 =
            Vec3.scale (3 * t * t) d2
    in
    Vec3.add term0 (Vec3.add term1 term2)


{-| Numerical integration of curve length using fine subdivision.
-}
curveLength : (Float -> Vec3) -> Int -> Float
curveLength pointAt samples =
    let
        count =
            max 10 samples

        step =
            1.0 / toFloat count

        sumDistance i acc prevPoint =
            if i > count then
                acc

            else
                let
                    curr =
                        pointAt (toFloat i * step)

                    d =
                        Vec3.distance prevPoint curr
                in
                sumDistance (i + 1) (acc + d) curr
    in
    sumDistance 1 0.0 (pointAt 0.0)


{-| Evaluate cubic spline route and parallel transport frames.
-}
evaluateSpline : SplineConfig -> FlexRoute
evaluateSpline config =
    let
        span =
            Vec3.distance config.startPoint config.endPoint

        alpha =
            (span / 3.0) * max 0.1 config.slack

        t0Len =
            Vec3.length config.startTangent

        t1Len =
            Vec3.length config.endTangent

        unitT0 =
            if t0Len < 0.0001 then
                vec3 0 0 1

            else
                Vec3.scale (1 / t0Len) config.startTangent

        unitT1 =
            if t1Len < 0.0001 then
                vec3 0 0 1

            else
                Vec3.scale (1 / t1Len) config.endTangent

        c0 =
            config.startPoint

        c1 =
            Vec3.add config.startPoint (Vec3.scale alpha unitT0)

        c2 =
            Vec3.sub config.endPoint (Vec3.scale alpha unitT1)

        c3 =
            config.endPoint

        evalPoint =
            cubicBezier c0 c1 c2 c3

        evalTangent t =
            let
                vel =
                    cubicBezierTangent c0 c1 c2 c3 t

                len =
                    Vec3.length vel
            in
            if len < 0.0001 then
                vec3 0 0 1

            else
                Vec3.scale (1 / len) vel

        totalLen =
            curveLength evalPoint 64

        pitch =
            max 1.0 config.segmentPitch

        segCount =
            max 1 (round (totalLen / pitch))

        sampleStep =
            1.0 / toFloat segCount

        samples =
            List.range 0 segCount
                |> List.map
                    (\i ->
                        let
                            t =
                                toFloat i * sampleStep

                            pt =
                                evalPoint t

                            tangent =
                                evalTangent t

                            dist =
                                toFloat i * (totalLen / toFloat segCount)
                        in
                        { point = pt, tangent = tangent, t = t, distance = dist }
                    )

        initialNormal =
            pickInitialNormal (evalTangent 0.0)
    in
    { totalLength = totalLen
    , segmentCount = segCount
    , frames = parallelTransportFrames samples initialNormal
    }


pickInitialNormal : Vec3 -> Vec3
pickInitialNormal tangent =
    let
        up =
            if abs (Vec3.getY tangent) < 0.9 then
                vec3 0 1 0

            else
                vec3 1 0 0

        cross =
            Vec3.cross tangent up

        len =
            Vec3.length cross
    in
    if len < 0.0001 then
        vec3 1 0 0

    else
        Vec3.scale (1 / len) cross


{-| Compute rotation-minimizing parallel transport frames along a sample list.
-}
parallelTransportFrames : List CurveSample -> Vec3 -> List CurveFrame
parallelTransportFrames samples initialNormal =
    case samples of
        [] ->
            []

        sample0 :: rest ->
            let
                norm0Len =
                    Vec3.length initialNormal

                n0 =
                    if norm0Len < 0.0001 then
                        vec3 1 0 0

                    else
                        Vec3.scale (1 / norm0Len) initialNormal

                b0 =
                    Vec3.cross sample0.tangent n0

                frame0 =
                    { point = sample0.point
                    , tangent = sample0.tangent
                    , normal = n0
                    , binormal = b0
                    , matrix = frameMatrix sample0.point n0 b0 sample0.tangent
                    , t = sample0.t
                    , distance = sample0.distance
                    }

                step sample ( prevFrame, acc ) =
                    let
                        axis =
                            Vec3.cross prevFrame.tangent sample.tangent

                        axisLen =
                            Vec3.length axis

                        nextNormal =
                            if axisLen < 0.00001 then
                                prevFrame.normal

                            else
                                let
                                    unitAxis =
                                        Vec3.scale (1 / axisLen) axis

                                    cosPhi =
                                        clamp -1.0 1.0 (Vec3.dot prevFrame.tangent sample.tangent)

                                    sinPhi =
                                        clamp -1.0 1.0 axisLen

                                    crossTerm =
                                        Vec3.scale sinPhi (Vec3.cross unitAxis prevFrame.normal)

                                    dotTerm =
                                        Vec3.scale ((1 - cosPhi) * Vec3.dot unitAxis prevFrame.normal) unitAxis

                                    rotated =
                                        Vec3.add (Vec3.scale cosPhi prevFrame.normal) (Vec3.add crossTerm dotTerm)

                                    rotLen =
                                        Vec3.length rotated
                                in
                                if rotLen < 0.0001 then
                                    prevFrame.normal

                                else
                                    Vec3.scale (1 / rotLen) rotated

                        nextBinormal =
                            Vec3.cross sample.tangent nextNormal

                        currFrame =
                            { point = sample.point
                            , tangent = sample.tangent
                            , normal = nextNormal
                            , binormal = nextBinormal
                            , matrix = frameMatrix sample.point nextNormal nextBinormal sample.tangent
                            , t = sample.t
                            , distance = sample.distance
                            }
                    in
                    ( currFrame, currFrame :: acc )

                ( _, framesRev ) =
                    List.foldl step ( frame0, [ frame0 ] ) rest
            in
            List.reverse framesRev


{-| Construct a 4x4 matrix from origin and orthogonal basis vectors.
Local X = normal, Local Y = binormal, Local Z = tangent.
-}
frameMatrix : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Mat4
frameMatrix pos n b t =
    Mat4.fromRecord
        { m11 = Vec3.getX n
        , m21 = Vec3.getY n
        , m31 = Vec3.getZ n
        , m41 = 0
        , m12 = Vec3.getX b
        , m22 = Vec3.getY b
        , m32 = Vec3.getZ b
        , m42 = 0
        , m13 = Vec3.getX t
        , m23 = Vec3.getY t
        , m33 = Vec3.getZ t
        , m43 = 0
        , m14 = Vec3.getX pos
        , m24 = Vec3.getY pos
        , m34 = Vec3.getZ pos
        , m44 = 1
        }


{-| Compute belt or chain routing wrapped around two pulleys with tangential paths.
-}
routeBeltOrChain : BeltPulley -> BeltPulley -> Float -> FlexRoute
routeBeltOrChain p1 p2 pitch =
    let
        centerDelta =
            Vec3.sub p2.center p1.center

        dist =
            Vec3.length centerDelta

        safeDist =
            max 1.0 dist

        r1 =
            p1.radius

        r2 =
            p2.radius

        axisLen =
            Vec3.length p1.axis

        unitAxis =
            if axisLen < 0.0001 then
                vec3 0 1 0

            else
                Vec3.scale (1 / axisLen) p1.axis

        unitDir =
            Vec3.scale (1 / safeDist) centerDelta

        perp =
            Vec3.cross unitAxis unitDir

        diffR =
            r1 - r2

        sinBeta =
            if safeDist > abs diffR then
                diffR / safeDist

            else
                0.0

        cosBeta =
            sqrt (max 0.0 (1.0 - sinBeta * sinBeta))

        t1Top =
            Vec3.add p1.center (Vec3.add (Vec3.scale (-r1 * sinBeta) unitDir) (Vec3.scale (r1 * cosBeta) perp))

        t2Top =
            Vec3.add p2.center (Vec3.add (Vec3.scale (-r2 * sinBeta) unitDir) (Vec3.scale (r2 * cosBeta) perp))

        t2Bot =
            Vec3.add p2.center (Vec3.add (Vec3.scale (-r2 * sinBeta) unitDir) (Vec3.scale (-r2 * cosBeta) perp))

        t1Bot =
            Vec3.add p1.center (Vec3.add (Vec3.scale (-r1 * sinBeta) unitDir) (Vec3.scale (-r1 * cosBeta) perp))

        straightLen =
            Vec3.distance t1Top t2Top

        topTan =
            Vec3.scale (1 / max 0.001 straightLen) (Vec3.sub t2Top t1Top)

        botTan =
            Vec3.scale (1 / max 0.001 straightLen) (Vec3.sub t1Bot t2Bot)

        arc2Len =
            r2 * (pi - 2 * asin sinBeta)

        arc1Len =
            r1 * (pi + 2 * asin sinBeta)

        totalLength =
            2 * straightLen + arc1Len + arc2Len

        safePitch =
            max 1.0 pitch

        segCount =
            max 2 (round (totalLength / safePitch))

        sampleStep =
            1.0 / toFloat segCount

        pointAroundLoop t =
            let
                d =
                    t * totalLength
            in
            if d < straightLen then
                let
                    frac =
                        d / straightLen
                in
                ( Vec3.add t1Top (Vec3.scale (frac * straightLen) topTan), topTan )

            else if d < straightLen + arc2Len then
                let
                    frac =
                        (d - straightLen) / max 0.001 arc2Len

                    angle =
                        frac * (pi - 2 * asin sinBeta)

                    rot =
                        Mat4.makeRotate angle unitAxis

                    v0 =
                        Vec3.sub t2Top p2.center

                    vRot =
                        Mat4.transform rot v0

                    tangent =
                        Vec3.cross unitAxis (Vec3.scale (1 / r2) vRot)
                in
                ( Vec3.add p2.center vRot, tangent )

            else if d < 2 * straightLen + arc2Len then
                let
                    frac =
                        (d - straightLen - arc2Len) / straightLen
                in
                ( Vec3.add t2Bot (Vec3.scale (frac * straightLen) botTan), botTan )

            else
                let
                    frac =
                        (d - 2 * straightLen - arc2Len) / max 0.001 arc1Len

                    angle =
                        frac * (pi + 2 * asin sinBeta)

                    rot =
                        Mat4.makeRotate angle unitAxis

                    v0 =
                        Vec3.sub t1Bot p1.center

                    vRot =
                        Mat4.transform rot v0

                    tangent =
                        Vec3.cross unitAxis (Vec3.scale (1 / r1) vRot)
                in
                ( Vec3.add p1.center vRot, tangent )

        samples =
            List.range 0 segCount
                |> List.map
                    (\i ->
                        let
                            t =
                                toFloat i * sampleStep

                            ( pt, tangent ) =
                                pointAroundLoop t

                            distAlong =
                                toFloat i * (totalLength / toFloat segCount)
                        in
                        { point = pt, tangent = tangent, t = t, distance = distAlong }
                    )
    in
    { totalLength = totalLength
    , segmentCount = segCount
    , frames = parallelTransportFrames samples unitAxis
    }


{-| Generate LSynth synthetic directive lines and child subparts.
-}
generateLSynthLines : String -> Int -> FlexRoute -> List String
generateLSynthLines partName color route =
    let
        header =
            "0 SYNTH BEGIN " ++ partName

        footer =
            "0 SYNTH END"

        lineForFrame frame =
            let
                p =
                    frame.point

                m =
                    Mat4.toRecord frame.matrix

                formatF v =
                    String.fromFloat (toFloat (round (v * 1000)) / 1000)
            in
            String.join " "
                [ "1"
                , String.fromInt color
                , formatF (Vec3.getX p)
                , formatF (Vec3.getY p)
                , formatF (Vec3.getZ p)
                , formatF m.m11
                , formatF m.m12
                , formatF m.m13
                , formatF m.m21
                , formatF m.m22
                , formatF m.m23
                , formatF m.m31
                , formatF m.m32
                , formatF m.m33
                , partName
                ]
    in
    [ header ] ++ List.map lineForFrame route.frames ++ [ footer ]


{-| Generate concrete `Placement` entries along a flexible route.
-}
generatePlacements : Int -> String -> Int -> FlexRoute -> List Placement
generatePlacements startId partName color route =
    route.frames
        |> List.indexedMap
            (\idx frame ->
                { id = startId + idx
                , part = partName
                , fileText = partName
                , color = color
                , matrix = frame.matrix
                , extra = []
                }
            )
