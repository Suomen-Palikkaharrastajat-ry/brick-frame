module BuildGroupsTest exposing (suite)

{-| Unit tests for the palette's build groups: sizes, clutter and shelving.
-}

import Expect
import Studio.BuildGroups as BuildGroups exposing (Tier(..))
import Test exposing (Test, describe, test)


part : String -> String -> String -> { file : String, description : String, category : String }
part file description category =
    { file = file, description = description, category = category }


suite : Test
suite =
    describe "Studio.BuildGroups"
        [ describe "sizeKey"
            [ test "reads a two-part size" <|
                \_ -> Expect.equal [ 1, 2 ] (BuildGroups.sizeKey "Brick 1 x 2")
            , test "reads a fractional height" <|
                \_ -> Expect.equal [ 1, 2, 0.667 ] (BuildGroups.sizeKey "Slope Brick 31 1 x 2 x 0.667")
            , test "stops at a mixed fraction" <|
                \_ -> Expect.equal [ 2, 2, 1 ] (BuildGroups.sizeKey "Brick 2 x 2 x 1 1/3")
            , test "skips a number that is not part of a size" <|
                \_ -> Expect.equal [ 2, 1 ] (BuildGroups.sizeKey "Slope Brick 45 2 x 1")
            , test "sorts parts without a size last" <|
                \_ -> Expect.equal [ 1.0e9 ] (BuildGroups.sizeKey "Plant Flower")
            ]
        , describe "isClutter"
            [ test "prints are clutter, case aside" <|
                \_ -> Expect.equal True (BuildGroups.isClutter (part "3069bp64.dat" "Tile 1 x 2 with Map pattern" "Tile"))
            , test "another system named mid-description is clutter" <|
                \_ -> Expect.equal True (BuildGroups.isClutter (part "x.dat" "Wheel 4 x 2 Znap" "Wheel"))
            , test "third-party parts are clutter" <|
                \_ -> Expect.equal True (BuildGroups.isClutter (part "x.dat" "|BuWizz 2.0 Motor" "Electric"))
            , test "a part still being worked on is not clutter" <|
                \_ -> Expect.equal False (BuildGroups.isClutter (part "95188.dat" "Brick 6 x 6 Corner Round (Needs Work)" "Brick"))
            ]
        , describe "classify"
            [ test "a classic curated into two groups is in both, with a place in each" <|
                \_ ->
                    let
                        shelving =
                            BuildGroups.classify (part "98138.dat" "Tile 1 x 1 Round with Groove" "Tile")
                    in
                    Expect.all
                        [ \s -> Expect.equal [ "tiles", "round" ] s.groups
                        , \s -> Expect.equal 2 (List.length s.classics)
                        , \s -> Expect.equal Basic s.tier
                        ]
                        shelving
            , test "a Technic part is tier Other" <|
                \_ -> Expect.equal Other (BuildGroups.classify (part "3647.dat" "Technic Gear 8 Tooth" "Technic")).tier
            , test "clutter is on no shelf" <|
                \_ -> Expect.equal [] (BuildGroups.classify (part "3011.dat" "Duplo Brick 2 x 4" "Duplo")).groups
            ]
        ]
