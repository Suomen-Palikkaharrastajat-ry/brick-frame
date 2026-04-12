module SmokeTest exposing (suite)

{-| Minimal smoke test ensuring the Elm test harness executes.
-}

import Expect
import Test exposing (Test, describe, test)
import UI.Theme as Theme


suite : Test
suite =
    describe "light theme tokens"
        [ test "test harness is wired" <|
            \_ ->
                Expect.pass
        , test "app background uses CSS variable" <|
            \_ ->
                Expect.equal "var(--color-bg-page)" Theme.appBackground
        , test "panel styles are light-mode surfaces" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal "var(--color-bg-page)" Theme.panelSurface
                    , \_ -> Expect.equal "var(--color-bg-subtle)" Theme.panelSubtleBackground
                    ]
                    ()
        ]
