module SmokeTest exposing (suite)

{-| Minimal smoke test ensuring the Elm test harness executes.
-}

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "template"
        [ test "test harness is wired" <|
            \_ ->
                Expect.pass
        ]
