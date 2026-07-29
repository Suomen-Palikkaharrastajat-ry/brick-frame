module Gear.LodTest exposing (suite)

{-| Tests for distance-based LOD selection, focused on the threshold and the
hysteresis band. Distances are expressed via the exposed threshold functions so
the tests stay valid if the tuning constants change.
-}

import Expect
import Gear.Lod as Lod exposing (Lod(..))
import Test exposing (Test, describe, test)


pr : Float
pr =
    10.0


suite : Test
suite =
    describe "Gear.Lod.chooseLod"
        [ test "Full stays Full when nearer than the simplify distance" <|
            \_ ->
                Lod.chooseLod Full (Lod.simplifyDistance pr - 1) pr
                    |> Expect.equal Full
        , test "Full switches to Simplified when beyond the simplify distance" <|
            \_ ->
                Lod.chooseLod Full (Lod.simplifyDistance pr + 1) pr
                    |> Expect.equal Simplified
        , test "Simplified holds inside the hysteresis band (no flicker)" <|
            \_ ->
                -- Between detailDistance and simplifyDistance: a Full gear here
                -- would stay Full, and a Simplified gear stays Simplified.
                let
                    midBand =
                        (Lod.detailDistance pr + Lod.simplifyDistance pr) / 2
                in
                Expect.all
                    [ \_ -> Lod.chooseLod Simplified midBand pr |> Expect.equal Simplified
                    , \_ -> Lod.chooseLod Full midBand pr |> Expect.equal Full
                    ]
                    ()
        , test "Simplified switches back to Full when nearer than the detail distance" <|
            \_ ->
                Lod.chooseLod Simplified (Lod.detailDistance pr - 1) pr
                    |> Expect.equal Full
        , test "detail distance is strictly inside the simplify distance" <|
            \_ ->
                Lod.detailDistance pr
                    |> Expect.lessThan (Lod.simplifyDistance pr)
        , test "a larger gear stays Full where a smaller one simplifies" <|
            \_ ->
                let
                    smallPr =
                        10.0

                    largePr =
                        30.0

                    -- Distance that simplifies the small gear but is still within
                    -- the large gear's simplify distance.
                    distance =
                        Lod.simplifyDistance smallPr + 1
                in
                Expect.all
                    [ \_ -> Lod.chooseLod Full distance smallPr |> Expect.equal Simplified
                    , \_ -> Lod.chooseLod Full distance largePr |> Expect.equal Full
                    ]
                    ()
        ]
