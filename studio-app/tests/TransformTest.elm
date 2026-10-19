module TransformTest exposing (suite)

{-| Tests for the inspector's position and angle conversions.
-}

import Expect
import Fuzz
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Studio.Transform as Transform
import Test exposing (Test, describe, fuzz3, test)


near : Float -> Float -> Float -> Expect.Expectation
near tolerance expected actual =
    Expect.within (Expect.Absolute tolerance) expected actual


suite : Test
suite =
    describe "Studio.Transform"
        [ describe "position"
            [ test "reads the translation" <|
                \_ ->
                    Transform.position (Mat4.makeTranslate3 4 -8 12)
                        |> Vec3.toRecord
                        |> Expect.equal { x = 4, y = -8, z = 12 }
            , test "withPosition moves the origin and keeps the rotation" <|
                \_ ->
                    let
                        turned =
                            Transform.withEuler { x = 0, y = 90, z = 0 } (Mat4.makeTranslate3 1 2 3)

                        moved =
                            Transform.withPosition (vec3 10 20 30) turned
                    in
                    Expect.all
                        [ \_ -> Expect.equal { x = 10, y = 20, z = 30 } (Vec3.toRecord (Transform.position moved))
                        , \_ -> Expect.equal (Transform.eulerDegrees turned) (Transform.eulerDegrees moved)
                        ]
                        ()
            ]
        , describe "eulerDegrees"
            [ test "the identity has no turn" <|
                \_ ->
                    Transform.eulerDegrees Mat4.identity
                        |> Expect.equal { x = 0, y = 0, z = 0 }
            , test "a quarter turn about each axis reads as 90" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal { x = 90, y = 0, z = 0 } (Transform.eulerDegrees (Mat4.makeRotate (degrees 90) (vec3 1 0 0)))
                        , \_ -> Expect.equal { x = 0, y = 90, z = 0 } (Transform.eulerDegrees (Mat4.makeRotate (degrees 90) (vec3 0 1 0)))
                        , \_ -> Expect.equal { x = 0, y = 0, z = 90 } (Transform.eulerDegrees (Mat4.makeRotate (degrees 90) (vec3 0 0 1)))
                        ]
                        ()
            , test "gimbal lock still gives angles that rebuild the matrix" <|
                \_ ->
                    let
                        locked =
                            Mat4.makeRotate (degrees 90) (vec3 0 1 0)

                        rebuilt =
                            Transform.withEuler (Transform.eulerDegrees locked) locked
                    in
                    sameRotation locked rebuilt
            ]
        , describe "withEuler"
            [ fuzz3 (Fuzz.floatRange -180 180) (Fuzz.floatRange -89 89) (Fuzz.floatRange -180 180) "angles survive a round trip" <|
                \x y z ->
                    let
                        angles =
                            { x = toFloat (round x), y = toFloat (round y), z = toFloat (round z) }

                        back =
                            Transform.eulerDegrees (Transform.withEuler angles Mat4.identity)
                    in
                    Expect.all
                        [ \_ -> near 0.02 angles.x back.x
                        , \_ -> near 0.02 angles.y back.y
                        , \_ -> near 0.02 angles.z back.z
                        ]
                        ()
            , test "keeps the position" <|
                \_ ->
                    Transform.withEuler { x = 30, y = 45, z = 60 } (Mat4.makeTranslate3 7 8 9)
                        |> Transform.position
                        |> Vec3.toRecord
                        |> Expect.equal { x = 7, y = 8, z = 9 }
            , test "the result is rigid" <|
                \_ ->
                    Transform.withEuler { x = 30, y = 45, z = 60 } Mat4.identity
                        |> Transform.isRigid
                        |> Expect.equal True
            ]
        , describe "isRigid"
            [ test "a mirrored matrix is not" <|
                \_ ->
                    Transform.isRigid (Mat4.makeScale3 -1 1 1) |> Expect.equal False
            , test "a scaled matrix is not" <|
                \_ ->
                    Transform.isRigid (Mat4.makeScale3 2 2 2) |> Expect.equal False
            , test "a translation is" <|
                \_ ->
                    Transform.isRigid (Mat4.makeTranslate3 5 5 5) |> Expect.equal True
            ]
        ]


sameRotation : Mat4.Mat4 -> Mat4.Mat4 -> Expect.Expectation
sameRotation a b =
    let
        ra =
            Mat4.toRecord a

        rb =
            Mat4.toRecord b
    in
    Expect.all
        [ \_ -> near 0.0001 ra.m11 rb.m11
        , \_ -> near 0.0001 ra.m12 rb.m12
        , \_ -> near 0.0001 ra.m13 rb.m13
        , \_ -> near 0.0001 ra.m21 rb.m21
        , \_ -> near 0.0001 ra.m22 rb.m22
        , \_ -> near 0.0001 ra.m23 rb.m23
        , \_ -> near 0.0001 ra.m31 rb.m31
        , \_ -> near 0.0001 ra.m32 rb.m32
        , \_ -> near 0.0001 ra.m33 rb.m33
        ]
        ()
