module Studio.Transform exposing
    ( Euler
    , Placement3
    , eulerDegrees
    , isRigid
    , position
    , withEuler
    , withPosition
    )

{-| Reading and writing a placement's matrix in the terms an inspector shows:
a position and three turn angles in degrees.

The angles follow `R = Rz · Ry · Rx` — turn about X first, then Y, then Z — with
the right-handed rotations `Render.Camera` and `Studio.Edit.Rotate` use. A matrix
that scales or mirrors cannot be described by angles alone; `isRigid` says
whether it can.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Turn angles in degrees about each axis.
-}
type alias Euler =
    { x : Float
    , y : Float
    , z : Float
    }


{-| A position and rotation, as the inspector edits them.
-}
type alias Placement3 =
    { position : Vec3
    , rotation : Euler
    }


{-| Where the matrix puts the origin.
-}
position : Mat4 -> Vec3
position matrix =
    Mat4.transform matrix (vec3 0 0 0)


{-| The same matrix moved to a new position, rotation unchanged.
-}
withPosition : Vec3 -> Mat4 -> Mat4
withPosition target matrix =
    let
        m =
            Mat4.toRecord matrix
    in
    Mat4.fromRecord { m | m14 = Vec3.getX target, m24 = Vec3.getY target, m34 = Vec3.getZ target }


{-| Whether the 3×3 part is a pure rotation: orthonormal, determinant +1. Parts
with scale or mirroring return `False`, and their angles are not meaningful.
-}
isRigid : Mat4 -> Bool
isRigid matrix =
    let
        m =
            Mat4.toRecord matrix

        dot a b c d e f =
            a * d + b * e + c * f

        tolerance =
            1.0e-4

        det =
            m.m11 * (m.m22 * m.m33 - m.m23 * m.m32) - m.m12 * (m.m21 * m.m33 - m.m23 * m.m31) + m.m13 * (m.m21 * m.m32 - m.m22 * m.m31)
    in
    abs (dot m.m11 m.m21 m.m31 m.m11 m.m21 m.m31 - 1)
        < tolerance
        && abs (dot m.m12 m.m22 m.m32 m.m12 m.m22 m.m32 - 1)
        < tolerance
        && abs (dot m.m13 m.m23 m.m33 m.m13 m.m23 m.m33 - 1)
        < tolerance
        && abs (dot m.m11 m.m21 m.m31 m.m12 m.m22 m.m32)
        < tolerance
        && abs (dot m.m11 m.m21 m.m31 m.m13 m.m23 m.m33)
        < tolerance
        && abs (dot m.m12 m.m22 m.m32 m.m13 m.m23 m.m33)
        < tolerance
        && abs (det - 1)
        < tolerance


{-| The turn angles of a rigid matrix, in degrees.

Where turning about Y by ±90° makes X and Z coincide (gimbal lock) the split
between them is arbitrary; Z is taken as zero.

-}
eulerDegrees : Mat4 -> Euler
eulerDegrees matrix =
    let
        m =
            Mat4.toRecord matrix

        sinY =
            clamp -1 1 -m.m31

        y =
            asin sinY

        ( x, z ) =
            if abs sinY > 0.999999 then
                ( atan2 -m.m23 m.m22, 0 )

            else
                ( atan2 m.m32 m.m33, atan2 m.m21 m.m11 )
    in
    { x = clean (degrees_ x), y = clean (degrees_ y), z = clean (degrees_ z) }


{-| The matrix with its rotation replaced by `euler` (degrees) and its position kept.
-}
withEuler : Euler -> Mat4 -> Mat4
withEuler euler matrix =
    let
        rotation =
            Mat4.makeRotate (degrees euler.z) (vec3 0 0 1)
                |> (\rz -> Mat4.mul rz (Mat4.makeRotate (degrees euler.y) (vec3 0 1 0)))
                |> (\rzy -> Mat4.mul rzy (Mat4.makeRotate (degrees euler.x) (vec3 1 0 0)))

        r =
            Mat4.toRecord rotation

        p =
            position matrix
    in
    Mat4.fromRecord
        { r
            | m14 = Vec3.getX p
            , m24 = Vec3.getY p
            , m34 = Vec3.getZ p
        }
        |> roundEntries


degrees_ : Float -> Float
degrees_ radians =
    radians * 180 / pi


{-| Angles are shown to two decimals; anything closer to a whole number than
that is whole, so 90° reads 90 rather than 89.99999.
-}
clean : Float -> Float
clean value =
    let
        rounded =
            toFloat (round (value * 100)) / 100
    in
    if rounded == 0 then
        0

    else
        rounded


roundEntries : Mat4 -> Mat4
roundEntries matrix =
    let
        m =
            Mat4.toRecord matrix

        r v =
            toFloat (round (v * 1000000)) / 1000000
    in
    Mat4.fromRecord
        { m11 = r m.m11
        , m21 = r m.m21
        , m31 = r m.m31
        , m41 = 0
        , m12 = r m.m12
        , m22 = r m.m22
        , m32 = r m.m32
        , m42 = 0
        , m13 = r m.m13
        , m23 = r m.m23
        , m33 = r m.m33
        , m43 = 0
        , m14 = r m.m14
        , m24 = r m.m24
        , m34 = r m.m34
        , m44 = 1
        }
