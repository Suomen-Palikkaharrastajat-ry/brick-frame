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
        ]
