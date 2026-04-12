module PackageSmokeTest exposing (suite)

{-| Basic smoke test to ensure the package test harness runs.
-}

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "bricks-simulator package"
        [ test "test harness is wired" <|
            \_ ->
                Expect.equal True True
        ]
