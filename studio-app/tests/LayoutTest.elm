module LayoutTest exposing (suite)

{-| Tests for the studio's layout arithmetic.
-}

import Expect
import Studio.Layout as Layout exposing (Mode(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.Layout"
        [ test "a wide window gets two side panes and a canvas between them" <|
            \_ ->
                Layout.compute 1400 800
                    |> Expect.equal { mode = Wide, canvasWidth = 800, canvasHeight = 800 }
        , test "a phone gets the window above its tab bar for the canvas" <|
            \_ ->
                Layout.compute 390 780
                    |> Expect.equal { mode = Narrow, canvasWidth = 390, canvasHeight = 780 - Layout.tabBarHeight }
        , test "the switch happens at 960 pixels" <|
            \_ ->
                ( .mode (Layout.compute 959 600), .mode (Layout.compute 960 600) )
                    |> Expect.equal ( Narrow, Wide )
        , test "the canvas never goes negative" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.atLeast 1 (Layout.compute 960 600).canvasWidth
                    , \_ -> Expect.atLeast 1 (Layout.compute 300 10).canvasHeight
                    ]
                    ()
        ]
