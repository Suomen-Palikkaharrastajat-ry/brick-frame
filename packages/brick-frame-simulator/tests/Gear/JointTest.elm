module Gear.JointTest exposing (suite)

{-| Unit tests for hinge axis detection, joint identification, pivot transforms,
and kinematic linkage alignment.
-}

import Expect
import Gear.Joint as Joint exposing (JointType(..))
import LDraw.Document exposing (Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (Test, describe, test)


makePlacement : Int -> String -> Mat4 -> Placement
makePlacement id part matrix =
    { id = id
    , part = part
    , fileText = part
    , color = 16
    , matrix = matrix
    , extra = []
    }


suite : Test
suite =
    describe "Gear.Joint"
        [ describe "Part categorization"
            [ test "identifies turntable parts" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (Joint.isTurntablePart "4014.dat")
                        , \_ -> Expect.equal True (Joint.isTurntablePart "4016.dat")
                        , \_ -> Expect.equal True (Joint.isTurntablePart "parts/61485.dat")
                        , \_ -> Expect.equal False (Joint.isTurntablePart "3001.dat")
                        ]
                        ()
            , test "identifies hinge parts" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (Joint.isHingePart "3937.dat")
                        , \_ -> Expect.equal True (Joint.isHingePart "3938.dat")
                        , \_ -> Expect.equal True (Joint.isHingePart "44301.dat")
                        , \_ -> Expect.equal False (Joint.isHingePart "3001.dat")
                        ]
                        ()
            , test "identifies pin parts" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (Joint.isPinPart "2780.dat")
                        , \_ -> Expect.equal True (Joint.isPinPart "3673.dat")
                        , \_ -> Expect.equal True (Joint.isPinPart "pin.dat")
                        , \_ -> Expect.equal False (Joint.isPinPart "3001.dat")
                        ]
                        ()
            , test "identifies ball and socket parts" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (Joint.isBallSocketPart "towball.dat")
                        , \_ -> Expect.equal True (Joint.isBallSocketPart "socket.dat")
                        , \_ -> Expect.equal True (Joint.isBallSocketPart "2736.dat")
                        , \_ -> Expect.equal False (Joint.isBallSocketPart "3001.dat")
                        ]
                        ()
            ]
        , describe "Hinge axis detection"
            [ test "turntable axis aligns with transformed local Y" <|
                \_ ->
                    let
                        p =
                            makePlacement 0 "4014.dat" Mat4.identity

                        axis =
                            Joint.hingeAxis p
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getX axis)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 1 (Vec3.getY axis)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getZ axis)
                        ]
                        ()
            , test "hinge brick axis aligns with transformed local X" <|
                \_ ->
                    let
                        p =
                            makePlacement 0 "3937.dat" Mat4.identity

                        axis =
                            Joint.hingeAxis p
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 1 (Vec3.getX axis)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getY axis)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getZ axis)
                        ]
                        ()
            , test "rotated hinge transforms its rotation axis accurately" <|
                \_ ->
                    let
                        rot =
                            Mat4.makeRotate (pi / 2) (vec3 0 1 0)

                        p =
                            makePlacement 0 "3937.dat" rot

                        axis =
                            Joint.hingeAxis p
                    in
                    -- Rotating local X by 90 deg around Y points to -Z in right-handed convention
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX axis)
                        , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getY axis)
                        , \_ -> Expect.within (Expect.Absolute 0.01) -1 (Vec3.getZ axis)
                        ]
                        ()
            ]
        , describe "Angle snapping"
            [ test "snaps angles to nearest step" <|
                \_ ->
                    let
                        step =
                            degrees 15

                        input =
                            degrees 17
                    in
                    Joint.snapAngle step input
                        |> Expect.within (Expect.Absolute 0.0001) (degrees 15)
            , test "default snap for click hinges is 22.5 deg" <|
                \_ ->
                    Joint.defaultSnapAngle "3937.dat"
                        |> Expect.within (Expect.Absolute 0.0001) (degrees 22.5)
            ]
        , describe "Pivot transforms"
            [ test "rotates a point around pivot axis" <|
                \_ ->
                    let
                        pivot =
                            vec3 10 0 0

                        axis =
                            vec3 0 1 0

                        transform =
                            Joint.pivotTransform pivot axis (pi / 2)

                        original =
                            vec3 20 0 0

                        result =
                            Mat4.transform transform original
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.001) 10 (Vec3.getX result)
                        , \_ -> Expect.within (Expect.Absolute 0.001) 0 (Vec3.getY result)
                        , \_ -> Expect.within (Expect.Absolute 0.001) -10 (Vec3.getZ result)
                        ]
                        ()
            ]
        , describe "Joint detection between paired parts"
            [ test "detects turntable revolute joint" <|
                \_ ->
                    let
                        p1 =
                            makePlacement 0 "4014.dat" Mat4.identity

                        p2 =
                            makePlacement 1 "4016.dat" (Mat4.makeTranslate (vec3 0 10 0))

                        joint =
                            Joint.detectJointsBetween p1 p2
                    in
                    case joint of
                        Just j ->
                            case j.jointType of
                                RevoluteJoint { limits } ->
                                    Expect.all
                                        [ \_ -> Expect.equal 0 j.parentPlacementId
                                        , \_ -> Expect.equal 1 j.childPlacementId
                                        , \_ -> Expect.equal Nothing limits
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "expected RevoluteJoint"

                        Nothing ->
                            Expect.fail "expected turntable joint to be detected"
            , test "detects hinge pair revolute joint with angular limits" <|
                \_ ->
                    let
                        p1 =
                            makePlacement 0 "3937.dat" Mat4.identity

                        p2 =
                            makePlacement 1 "3938.dat" (Mat4.makeTranslate (vec3 0 0 8))

                        joint =
                            Joint.detectJointsBetween p1 p2
                    in
                    case joint of
                        Just j ->
                            case j.jointType of
                                RevoluteJoint { limits } ->
                                    Expect.all
                                        [ \_ -> Expect.equal 0 j.parentPlacementId
                                        , \_ -> Expect.equal 1 j.childPlacementId
                                        , \_ -> Expect.notEqual Nothing limits
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "expected RevoluteJoint"

                        Nothing ->
                            Expect.fail "expected hinge joint to be detected"
            , test "detects ball and socket spherical joint" <|
                \_ ->
                    let
                        p1 =
                            makePlacement 0 "towball.dat" Mat4.identity

                        p2 =
                            makePlacement 1 "socket.dat" (Mat4.makeTranslate (vec3 0 2 0))

                        joint =
                            Joint.detectJointsBetween p1 p2
                    in
                    case joint of
                        Just j ->
                            case j.jointType of
                                SphericalJoint { coneLimit } ->
                                    Expect.greaterThan 0.0 coneLimit

                                _ ->
                                    Expect.fail "expected SphericalJoint"

                        Nothing ->
                            Expect.fail "expected ball-and-socket joint to be detected"
            ]
        , describe "Linkage alignment solver"
            [ test "calculates exact rotation angle to align moving pin with target hole" <|
                \_ ->
                    let
                        pivot =
                            vec3 0 0 0

                        axis =
                            vec3 0 1 0

                        movingPin =
                            vec3 10 0 0

                        targetHole =
                            vec3 0 0 -10

                        angle =
                            Joint.alignLinkageAngle pivot axis movingPin targetHole

                        transformed =
                            Mat4.transform (Joint.pivotTransform pivot axis angle) movingPin
                    in
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.01) (pi / 2) angle
                        , \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX transformed)
                        , \_ -> Expect.within (Expect.Absolute 0.01) -10 (Vec3.getZ transformed)
                        ]
                        ()
            , test "solves closed-loop coupler distance" <|
                \_ ->
                    let
                        pivot =
                            vec3 0 0 0

                        axis =
                            vec3 0 1 0

                        crankPin =
                            vec3 10 0 0

                        rockerHole =
                            vec3 30 0 0

                        couplerLen =
                            20.0

                        solutions =
                            Joint.alignLinkageCoupler pivot axis crankPin rockerHole couplerLen
                    in
                    case solutions of
                        Just ( a1, a2 ) ->
                            let
                                pRot =
                                    Mat4.transform (Joint.pivotTransform pivot axis a1) crankPin

                                dist =
                                    Vec3.distance pRot rockerHole
                            in
                            Expect.within (Expect.Absolute 0.01) couplerLen dist

                        Nothing ->
                            Expect.fail "expected coupler solution"
            ]
        ]
