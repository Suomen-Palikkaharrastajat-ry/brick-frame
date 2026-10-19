module LodDataTest exposing (suite)

{-| Guards that the generated `Data.lodParts` stays in step with
`Data.embeddedParts`, so distance-based LOD selection can always find a
low-detail mesh for every embedded gear part.
-}

import Data
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Data.lodParts coverage"
        [ test "every embedded part has an LOD counterpart" <|
            \_ ->
                let
                    missing =
                        Dict.keys Data.embeddedParts
                            |> List.filter (\key -> not (Dict.member key Data.lodParts))
                in
                Expect.equal [] missing
        ]
