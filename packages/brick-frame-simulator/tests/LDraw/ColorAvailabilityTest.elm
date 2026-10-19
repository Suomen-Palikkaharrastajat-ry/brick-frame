module LDraw.ColorAvailabilityTest exposing (suite)

{-| Tests for LDraw.ColorAvailability.
-}

import Expect
import LDraw.ColorAvailability as ColorAvailability exposing (ColorAvailability(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.ColorAvailability"
        [ describe "checkAvailability"
            [ test "3001 in Red (4) is InProduction" <|
                \_ ->
                    case ColorAvailability.checkAvailability "3001.dat" 4 of
                        InProduction data ->
                            Expect.all
                                [ \_ -> Expect.equal 1958 data.yearIntroduced
                                , \_ -> Expect.greaterThan 0 data.setFrequency
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected InProduction"
            , test "3001 in Light Gray (7) is OutOfProduction" <|
                \_ ->
                    case ColorAvailability.checkAvailability "3001.dat" 7 of
                        OutOfProduction data ->
                            Expect.all
                                [ \_ -> Expect.equal 1958 data.yearIntroduced
                                , \_ -> Expect.equal 2004 data.yearDiscontinued
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected OutOfProduction"
            , test "3001 in rare/unproduced color (999) is Unofficial" <|
                \_ ->
                    ColorAvailability.checkAvailability "3001.dat" 999
                        |> Expect.equal Unofficial
            , test "technic gear 3647 in LBG (71) is InProduction" <|
                \_ ->
                    case ColorAvailability.checkAvailability "3647.dat" 71 of
                        InProduction data ->
                            Expect.equal 2004 data.yearIntroduced

                        _ ->
                            Expect.fail "Expected InProduction"
            , test "technic gear 3647 in Red (4) is Unofficial" <|
                \_ ->
                    ColorAvailability.checkAvailability "3647.dat" 4
                        |> Expect.equal Unofficial
            ]
        , describe "isOfficial"
            [ test "3001 in Red is official" <|
                \_ ->
                    ColorAvailability.isOfficial "3001.dat" 4
                        |> Expect.equal True
            , test "3001 in Light Gray is official (retired)" <|
                \_ ->
                    ColorAvailability.isOfficial "3001.dat" 7
                        |> Expect.equal True
            , test "3001 in 999 is not official" <|
                \_ ->
                    ColorAvailability.isOfficial "3001.dat" 999
                        |> Expect.equal False
            ]
        , describe "parseDimensionsFromDescription"
            [ test "parses Brick 2 x 4" <|
                \_ ->
                    ColorAvailability.parseDimensionsFromDescription "Brick  2 x  4"
                        |> Expect.equal ( 2, 4, 3 )
            , test "parses Plate 1 x 2" <|
                \_ ->
                    ColorAvailability.parseDimensionsFromDescription "Plate 1 x 2"
                        |> Expect.equal ( 1, 2, 1 )
            , test "defaults for parts without dimensions" <|
                \_ ->
                    ColorAvailability.parseDimensionsFromDescription "Minifig Torso"
                        |> Expect.equal ( 1, 1, 1 )
            ]
        , describe "isGearOrAxle"
            [ test "detects gear" <|
                \_ ->
                    ColorAvailability.isGearOrAxle "Technic" "Technic Gear 8 Tooth"
                        |> Expect.equal True
            , test "detects axle" <|
                \_ ->
                    ColorAvailability.isGearOrAxle "Technic" "Technic Axle 4"
                        |> Expect.equal True
            , test "does not flag normal brick" <|
                \_ ->
                    ColorAvailability.isGearOrAxle "Brick" "Brick 2 x 4"
                        |> Expect.equal False
            ]
        , describe "getMetadata"
            [ test "constructs complete part metadata" <|
                \_ ->
                    let
                        meta =
                            ColorAvailability.getMetadata "3001.dat" "Brick 2 x 4" "Brick"
                    in
                    Expect.all
                        [ \_ -> Expect.equal "3001.dat" meta.part
                        , \_ -> Expect.equal "Brick 2 x 4" meta.officialName
                        , \_ -> Expect.equal "Brick" meta.category
                        , \_ -> Expect.equal ( 2, 4, 3 ) meta.dimensions
                        , \_ -> Expect.equal False meta.isGearOrAxle
                        , \_ -> Expect.notEqual Nothing meta.weightGrams
                        ]
                        ()
            ]
        ]
