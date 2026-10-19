module Gear.Joint exposing
    ( JointType(..)
    , MechanicalJoint
    , alignLinkage
    , alignLinkageAngle
    , alignLinkageCoupler
    , defaultSnapAngle
    , detectJoints
    , detectJointsBetween
    , hingeAxis
    , isBallSocketPart
    , isHingePart
    , isPinPart
    , isTurntablePart
    , pivotTransform
    , snapAngle
    )

{-| Mechanical joint and hinge detection, pivot transforms, and linkage alignment.

Detects rotational degrees of freedom along hinge, pin, turntable, and ball-and-socket
connection axes. Provides analytical linkage solvers for closing 1-DOF kinematic loops.

-}

import Dict exposing (Dict)
import LDraw.Document exposing (Placement)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| The kinematic degree of freedom and constraints of a physical joint.
-}
type JointType
    = RevoluteJoint { axis : Vec3, limits : Maybe ( Float, Float ) }
    | PrismaticJoint { axis : Vec3, travel : ( Float, Float ) }
    | SphericalJoint { coneLimit : Float }


{-| A detected mechanical joint between two placements in a model.
-}
type alias MechanicalJoint =
    { id : Int
    , parentPlacementId : Int
    , childPlacementId : Int
    , pivotWorld : Vec3
    , jointType : JointType
    , currentAngle : Float
    }


{-| Normalise part file names for robust comparison.
-}
normalisePart : String -> String
normalisePart name =
    let
        lower =
            String.toLower name

        withoutDir =
            case String.indices "/" lower of
                [] ->
                    lower

                indices ->
                    case List.reverse indices of
                        lastSlash :: _ ->
                            String.dropLeft (lastSlash + 1) lower

                        [] ->
                            lower
    in
    withoutDir


{-| Whether a part name represents a turntable component.
-}
isTurntablePart : String -> Bool
isTurntablePart name =
    let
        p =
            normalisePart name
    in
    List.member p
        [ "4014.dat"
        , "4016.dat"
        , "61485.dat"
        , "18938.dat"
        , "3680.dat"
        , "3679.dat"
        , "99009.dat"
        , "99010.dat"
        , "4014"
        , "4016"
        , "61485"
        , "18938"
        , "3680"
        , "3679"
        , "99009"
        , "99010"
        ]


{-| Whether a part name represents a hinge base or finger.
-}
isHingePart : String -> Bool
isHingePart name =
    let
        p =
            normalisePart name
    in
    List.member p
        [ "3937.dat"
        , "3938.dat"
        , "44301.dat"
        , "44302.dat"
        , "2429.dat"
        , "2430.dat"
        , "4275.dat"
        , "4276.dat"
        , "4315.dat"
        , "44568.dat"
        , "44570.dat"
        , "3937"
        , "3938"
        , "44301"
        , "44302"
        , "2429"
        , "2430"
        , "4275"
        , "4276"
        , "4315"
        , "44568"
        , "44570"
        ]


{-| Whether a part represents a Technic pin or pinhole joint primitive.
-}
isPinPart : String -> Bool
isPinPart name =
    let
        p =
            normalisePart name
    in
    List.member p
        [ "pin.dat"
        , "pinhole.dat"
        , "2780.dat"
        , "3673.dat"
        , "3749.dat"
        , "4274.dat"
        , "11214.dat"
        , "15100.dat"
        , "4459.dat"
        , "6558.dat"
        , "32054.dat"
        , "32556.dat"
        , "pin"
        , "pinhole"
        , "2780"
        , "3673"
        , "3749"
        , "4274"
        , "11214"
        , "15100"
        , "4459"
        , "6558"
        , "32054"
        , "32556"
        ]


{-| Whether a part represents a ball-and-socket joint primitive.
-}
isBallSocketPart : String -> Bool
isBallSocketPart name =
    let
        p =
            normalisePart name
    in
    List.member p
        [ "towball.dat"
        , "socket.dat"
        , "2736.dat"
        , "32235.dat"
        , "6628.dat"
        , "53585.dat"
        , "60176.dat"
        , "15459.dat"
        , "92013.dat"
        , "towball"
        , "socket"
        , "2736"
        , "32235"
        , "6628"
        , "53585"
        , "60176"
        , "15459"
        , "92013"
        ]


{-| Extract placement origin from its matrix.
-}
placementPosition : Placement -> Vec3
placementPosition p =
    Mat4.transform p.matrix (vec3 0 0 0)


{-| Compute the world-space unit rotation axis for a placement.
-}
hingeAxis : Placement -> Vec3
hingeAxis placement =
    let
        part =
            normalisePart placement.part

        localAxis =
            if isTurntablePart part then
                -- Turntable rotates around local Y (normal to disc)
                vec3 0 1 0

            else if isHingePart part then
                -- Standard hinge bricks rotate around local X (barrel direction)
                vec3 1 0 0

            else
                -- Technic pins rotate around local Z
                vec3 0 0 1

        origin =
            Mat4.transform placement.matrix (vec3 0 0 0)

        axisTip =
            Mat4.transform placement.matrix localAxis

        delta =
            Vec3.sub axisTip origin

        len =
            Vec3.length delta
    in
    if len < 0.0001 then
        vec3 0 1 0

    else
        Vec3.scale (1 / len) delta


{-| Default angle snapping step in radians.
-}
defaultSnapAngle : String -> Float
defaultSnapAngle partName =
    let
        p =
            normalisePart partName
    in
    if isTurntablePart p then
        degrees 15

    else if isHingePart p then
        degrees 22.5

    else
        degrees 15


{-| Round an angle in radians to the nearest multiple of `stepRadians`.
-}
snapAngle : Float -> Float -> Float
snapAngle stepRadians angle =
    if stepRadians <= 0 then
        angle

    else
        toFloat (round (angle / stepRadians)) * stepRadians


{-| Rotation transform around an arbitrary pivot point and unit axis.
-}
pivotTransform : Vec3 -> Vec3 -> Float -> Mat4
pivotTransform pivot axis radians =
    let
        len =
            Vec3.length axis

        unitAxis =
            if len < 0.0001 then
                vec3 0 1 0

            else
                Vec3.scale (1 / len) axis
    in
    Mat4.mul
        (Mat4.mul (Mat4.makeTranslate pivot) (Mat4.makeRotate radians unitAxis))
        (Mat4.makeTranslate (Vec3.negate pivot))


{-| Detect if two placements form a valid mechanical joint.
-}
detectJointsBetween : Placement -> Placement -> Maybe MechanicalJoint
detectJointsBetween p1 p2 =
    let
        pos1 =
            placementPosition p1

        pos2 =
            placementPosition p2

        dist =
            Vec3.distance pos1 pos2

        name1 =
            normalisePart p1.part

        name2 =
            normalisePart p2.part

        axis1 =
            hingeAxis p1

        axis2 =
            hingeAxis p2

        axesParallel =
            abs (Vec3.dot axis1 axis2) >= 0.9
    in
    if isTurntablePair name1 name2 && dist <= 26.0 then
        let
            pivot =
                Vec3.scale 0.5 (Vec3.add pos1 pos2)
        in
        Just
            { id = 0
            , parentPlacementId = p1.id
            , childPlacementId = p2.id
            , pivotWorld = pivot
            , jointType = RevoluteJoint { axis = axis1, limits = Nothing }
            , currentAngle = 0.0
            }

    else if isHingePair name1 name2 && dist <= 32.0 && axesParallel then
        let
            pivot =
                Vec3.scale 0.5 (Vec3.add pos1 pos2)
        in
        Just
            { id = 0
            , parentPlacementId = p1.id
            , childPlacementId = p2.id
            , pivotWorld = pivot
            , jointType = RevoluteJoint { axis = axis1, limits = Just ( -pi / 2, pi / 2 ) }
            , currentAngle = 0.0
            }

    else if isBallSocketPair name1 name2 && dist <= 16.0 then
        Just
            { id = 0
            , parentPlacementId = p1.id
            , childPlacementId = p2.id
            , pivotWorld = pos1
            , jointType = SphericalJoint { coneLimit = pi / 4 }
            , currentAngle = 0.0
            }

    else if isPinPair name1 name2 && dist <= 28.0 && axesParallel then
        let
            pivot =
                Vec3.scale 0.5 (Vec3.add pos1 pos2)
        in
        Just
            { id = 0
            , parentPlacementId = p1.id
            , childPlacementId = p2.id
            , pivotWorld = pivot
            , jointType = RevoluteJoint { axis = axis1, limits = Nothing }
            , currentAngle = 0.0
            }

    else
        Nothing


isTurntablePair : String -> String -> Bool
isTurntablePair a b =
    let
        matches ( x, y ) =
            (a == x && b == y) || (a == y && b == x)
    in
    List.any matches
        [ ( "4014.dat", "4016.dat" )
        , ( "4014", "4016" )
        , ( "61485.dat", "18938.dat" )
        , ( "61485", "18938" )
        , ( "3680.dat", "3679.dat" )
        , ( "3680", "3679" )
        , ( "99009.dat", "99010.dat" )
        , ( "99009", "99010" )
        ]


isHingePair : String -> String -> Bool
isHingePair a b =
    let
        matches ( x, y ) =
            (a == x && b == y) || (a == y && b == x)
    in
    List.any matches
        [ ( "3937.dat", "3938.dat" )
        , ( "3937", "3938" )
        , ( "44301.dat", "44302.dat" )
        , ( "44301", "44302" )
        , ( "2429.dat", "2430.dat" )
        , ( "2429", "2430" )
        , ( "4275.dat", "4276.dat" )
        , ( "4275", "4276" )
        , ( "4315.dat", "4276.dat" )
        , ( "44568.dat", "44570.dat" )
        ]


isBallSocketPair : String -> String -> Bool
isBallSocketPair a b =
    let
        matches ( x, y ) =
            (a == x && b == y) || (a == y && b == x)
    in
    List.any matches
        [ ( "towball.dat", "socket.dat" )
        , ( "towball", "socket" )
        , ( "2736.dat", "32235.dat" )
        , ( "2736", "32235" )
        , ( "2736.dat", "6628.dat" )
        , ( "53585.dat", "60176.dat" )
        , ( "15459.dat", "92013.dat" )
        ]


isPinPair : String -> String -> Bool
isPinPair a b =
    (isPinPart a && isPinPart b) || (a == "pin.dat" && b == "pinhole.dat") || (a == "pinhole.dat" && b == "pin.dat")


{-| Detect all mechanical joints between placements.
-}
detectJoints : List Placement -> List MechanicalJoint
detectJoints placements =
    let
        indexed =
            List.indexedMap Tuple.pair placements

        pairs =
            List.concatMap
                (\( i, p1 ) ->
                    List.filterMap
                        (\( j, p2 ) ->
                            if i < j then
                                detectJointsBetween p1 p2

                            else
                                Nothing
                        )
                        indexed
                )
                indexed
    in
    List.indexedMap (\idx joint -> { joint | id = idx }) pairs


{-| Compute the exact angle in radians to rotate around `(pivot, axis)` to bring
`movingPoint` into directional alignment with `targetPoint`.
-}
alignLinkageAngle : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float
alignLinkageAngle pivot axis movingPoint targetPoint =
    let
        axisLen =
            Vec3.length axis

        u =
            if axisLen < 0.0001 then
                vec3 0 1 0

            else
                Vec3.scale (1 / axisLen) axis

        pm =
            Vec3.sub movingPoint pivot

        pt =
            Vec3.sub targetPoint pivot

        vm =
            Vec3.sub pm (Vec3.scale (Vec3.dot pm u) u)

        vt =
            Vec3.sub pt (Vec3.scale (Vec3.dot pt u) u)

        lenM =
            Vec3.length vm

        lenT =
            Vec3.length vt
    in
    if lenM < 0.0001 || lenT < 0.0001 then
        0.0

    else
        let
            nm =
                Vec3.scale (1 / lenM) vm

            nt =
                Vec3.scale (1 / lenT) vt

            cosA =
                clamp -1.0 1.0 (Vec3.dot nm nt)

            crossVec =
                Vec3.cross nm nt

            sinA =
                Vec3.dot crossVec u
        in
        atan2 sinA cosA


{-| Calculate the angle solutions required to close a linkage of known coupler length.
Returns `Just (angle1, angle2)` if a geometric solution exists, or `Nothing` if the
target cannot be reached.
-}
alignLinkageCoupler : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Float -> Maybe ( Float, Float )
alignLinkageCoupler pivot axis movingPoint targetPoint couplerLength =
    let
        axisLen =
            Vec3.length axis

        u =
            if axisLen < 0.0001 then
                vec3 0 1 0

            else
                Vec3.scale (1 / axisLen) axis

        pm =
            Vec3.sub movingPoint pivot

        pt =
            Vec3.sub targetPoint pivot

        hM =
            Vec3.dot pm u

        hT =
            Vec3.dot pt u

        deltaH =
            abs (hM - hT)
    in
    if couplerLength < deltaH then
        Nothing

    else
        let
            rpSquared =
                couplerLength * couplerLength - deltaH * deltaH

            rp =
                sqrt (max 0 rpSquared)

            vm =
                Vec3.sub pm (Vec3.scale hM u)

            vt =
                Vec3.sub pt (Vec3.scale hT u)

            lm =
                Vec3.length vm

            lt =
                Vec3.length vt
        in
        if lm < 0.0001 || lt < 0.0001 then
            Nothing

        else
            let
                cosGamma =
                    (lm * lm + lt * lt - rp * rp) / (2 * lm * lt)
            in
            if cosGamma < -1.0 || cosGamma > 1.0 then
                Nothing

            else
                let
                    gamma =
                        acos (clamp -1.0 1.0 cosGamma)

                    baseTheta =
                        alignLinkageAngle pivot axis movingPoint targetPoint
                in
                Just ( baseTheta + gamma, baseTheta - gamma )


{-| Returns the pivot transform matrix that swings `movingPoint` into alignment with `targetPoint`.
-}
alignLinkage : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Mat4
alignLinkage pivot axis movingPoint targetPoint =
    let
        angle =
            alignLinkageAngle pivot axis movingPoint targetPoint
    in
    pivotTransform pivot axis angle
