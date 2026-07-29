module LDraw.ColorsTest exposing (suite)

{-| Unit tests for LDraw color resolution behavior.

Exact RGB values come from the generated `LDraw.ColorData` table, which is
derived from the official `LDConfig.ldr`. These tests therefore assert
*coverage* and *special-code behavior* rather than pinning every channel, so
that a future LDraw colour revision does not break the suite.

-}

import Expect
import LDraw.Colors exposing (resolveColor)
import Test exposing (Test, describe, test)


magenta : { r : Float, g : Float, b : Float, alpha : Float }
magenta =
    { r = 1.0, g = 0.0, b = 1.0, alpha = 1.0 }


black : { r : Float, g : Float, b : Float, alpha : Float }
black =
    { r = 0.0, g = 0.0, b = 0.0, alpha = 1.0 }


expectKnown : Int -> Expect.Expectation
expectKnown code =
    resolveColor 15 code
        |> Expect.notEqual magenta


suite : Test
suite =
    describe "LDraw.Colors.resolveColor"
        [ test "Studio color -1 inherits parent color like 16" <|
            \_ ->
                Expect.equal
                    (resolveColor 4 16)
                    (resolveColor 4 -1)
        , test "edge colors 24 and Studio -2 both render black" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal black (resolveColor 15 24)
                    , \_ -> Expect.equal black (resolveColor 15 -2)
                    ]
                    ()
        , test "Studio 100000-offset codes resolve to the base color" <|
            \_ ->
                Expect.equal
                    (resolveColor 15 70)
                    (resolveColor 15 100070)
        , test "covers the full LDConfig catalog, not a hand-kept subset" <|
            \_ ->
                -- Codes 33/34/54/297 were absent from the old 92-entry table and
                -- rendered ~181 parts of the sample Studio model magenta.
                Expect.all
                    [ \_ -> expectKnown 33 -- Trans_Dark_Blue
                    , \_ -> expectKnown 34 -- Trans_Green
                    , \_ -> expectKnown 54 -- Trans_Neon_Yellow
                    , \_ -> expectKnown 297 -- Pearl_Gold
                    , \_ -> expectKnown 45 -- Trans_Dark_Pink
                    , \_ -> expectKnown 69 -- Bright_Reddish_Lilac
                    , \_ -> expectKnown 256 -- Rubber_Black
                    , \_ -> expectKnown 329 -- Glow_In_Dark_White
                    , \_ -> expectKnown 366 -- Earth_Orange
                    , \_ -> expectKnown 371 -- Medium_Tan
                    ]
                    ()
        , test "transparent colors carry LDConfig alpha" <|
            \_ ->
                (resolveColor 15 33).alpha
                    |> Expect.lessThan 1.0
        , test "unknown colors still fallback to magenta for visibility" <|
            \_ ->
                Expect.equal
                    magenta
                    (resolveColor 15 999999)
        ]
