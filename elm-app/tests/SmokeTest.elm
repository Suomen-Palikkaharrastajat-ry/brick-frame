module SmokeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "template"
        [ test "test harness is wired" <|
            \_ ->
                Expect.pass
        ]
