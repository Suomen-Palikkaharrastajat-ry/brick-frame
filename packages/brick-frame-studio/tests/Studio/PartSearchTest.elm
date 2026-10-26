module Studio.PartSearchTest exposing (suite)

{-| Tests for Studio.PartSearch: a blank query matches nothing, sizes match whole
("2 x 4" is not "12 x 4"), and an exact part number ranks first.
-}

import Expect
import Studio.PartIndex as PartIndex exposing (Part)
import Studio.PartSearch as PartSearch
import Test exposing (Test, describe, test)


catalogue : List Part
catalogue =
    [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
    , PartIndex.makePart "3002.dat" "Brick 2 x 3" "Brick" ""
    , PartIndex.makePart "4000.dat" "Brick 12 x 4" "Brick" ""
    , PartIndex.makePart "3024.dat" "Plate 1 x 1" "Plate" ""
    , PartIndex.makePart "3710.dat" "Plate 1 x 4" "Plate" ""
    ]


files : String -> List String
files query =
    PartSearch.matching 10 query catalogue
        |> List.map .file


suite : Test
suite =
    describe "Studio.PartSearch"
        [ test "a blank query matches nothing" <|
            \_ ->
                Expect.equal [] (files "   ")
        , test "a size matches whole: 2 x 4 does not find 12 x 4" <|
            \_ ->
                Expect.equal [ "3001.dat" ] (files "2 x 4")
        , test "2x4 and 2 x4 are read as 2 x 4" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal [ "3001.dat" ] (files "2x4")
                    , \_ -> Expect.equal [ "3001.dat" ] (files "2 x4")
                    ]
                    ()
        , test "every word must match: brick plate finds nothing" <|
            \_ ->
                Expect.equal [] (files "brick plate")
        , test "a part number ranks first" <|
            \_ ->
                Expect.equal (Just "3024.dat") (List.head (files "3024"))
        , test "the limit caps the results" <|
            \_ ->
                Expect.equal 1 (List.length (PartSearch.matching 1 "plate" catalogue))
        ]
