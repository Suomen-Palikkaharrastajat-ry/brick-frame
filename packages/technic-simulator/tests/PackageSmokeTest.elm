module PackageSmokeTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "technic-simulator package"
        [ test "test harness is wired" <|
            \_ ->
                Expect.equal True True
        ]
