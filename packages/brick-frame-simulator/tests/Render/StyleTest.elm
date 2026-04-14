module Render.StyleTest exposing (suite)

{-| Tests for lightweight render style tuning helpers.
-}

import Expect
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Style as Style
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Render.Style"
        [ test "clampVibrance bounds to conservative range" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.0001) -0.5 (Style.clampVibrance -2)
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0.5 (Style.clampVibrance 2)
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0.2 (Style.clampVibrance 0.2)
                    ]
                    ()
        , test "clampStyle normalizes and bounds values" <|
            \_ ->
                let
                    clamped =
                        Style.clampStyle
                            { lightDirection = vec3 0 0 0
                            , ambientStrength = 3
                            , lightStrength = 5
                            , specularStrength = -1
                            , specularPower = 100
                            , rimStrength = -2
                            , rimPower = 0.01
                            , vibrance = 99
                            , edgeColor = vec3 -1 0.4 2
                            , edgeWidth = 99
                            }
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.0001) 1 clamped.ambientStrength
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 1 clamped.lightStrength
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0 clamped.specularStrength
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 64 clamped.specularPower
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0 clamped.rimStrength
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0.1 clamped.rimPower
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0.5 clamped.vibrance
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0 (Vec3.getX clamped.edgeColor)
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 0.4 (Vec3.getY clamped.edgeColor)
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 1 (Vec3.getZ clamped.edgeColor)
                    , \_ -> Expect.notEqual 0 (Vec3.length clamped.lightDirection)
                    , \_ -> Expect.within (Expect.Absolute 0.0001) 8 clamped.edgeWidth
                    ]
                    ()
        , test "default style remains within expected range" <|
            \_ ->
                let
                    style =
                        Style.defaultStyle
                in
                Expect.all
                    [ \_ -> Expect.atMost 1 style.ambientStrength
                    , \_ -> Expect.atLeast 0 style.ambientStrength
                    , \_ -> Expect.atMost 1 style.lightStrength
                    , \_ -> Expect.atLeast 0 style.lightStrength
                    , \_ -> Expect.atMost 0.5 style.vibrance
                    , \_ -> Expect.atLeast -0.5 style.vibrance
                    , \_ -> Expect.atLeast 0.5 style.edgeWidth
                    , \_ -> Expect.atMost 8 style.edgeWidth
                    ]
                    ()
        ]
