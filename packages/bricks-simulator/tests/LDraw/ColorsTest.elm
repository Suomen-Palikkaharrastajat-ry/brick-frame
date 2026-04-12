module LDraw.ColorsTest exposing (suite)

{-| Unit tests for LDraw color resolution behavior.
-}

import Expect
import LDraw.Colors exposing (resolveColor)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Colors.resolveColor"
        [ test "Studio color -1 inherits parent color like 16" <|
            \_ ->
                Expect.equal
                    (resolveColor 4 16)
                    (resolveColor 4 -1)
        , test "supports additional known LEGO colors from shared catalog" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal { r = 0.878, g = 0.4, b = 0.573, alpha = 0.5 } (resolveColor 15 45)
                    , \_ -> Expect.equal { r = 0.584, g = 0.106, b = 0.624, alpha = 1.0 } (resolveColor 15 69)
                    , \_ -> Expect.equal { r = 0.067, g = 0.067, b = 0.067, alpha = 1.0 } (resolveColor 15 256)
                    , \_ -> Expect.equal { r = 1.0, g = 1.0, b = 0.902, alpha = 1.0 } (resolveColor 15 329)
                    , \_ -> Expect.equal { r = 0.671, g = 0.376, b = 0.071, alpha = 1.0 } (resolveColor 15 366)
                    ]
                    ()
        , test "unknown colors still fallback to magenta for visibility" <|
            \_ ->
                Expect.equal
                    { r = 1.0, g = 0.0, b = 1.0, alpha = 1.0 }
                    (resolveColor 15 9999)
        ]
