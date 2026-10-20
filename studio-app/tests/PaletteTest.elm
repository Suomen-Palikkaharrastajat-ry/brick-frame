module PaletteTest exposing (suite)

{-| Unit tests for visual palette shelves, favorites, recents, and color availability filtering.
-}

import Expect
import Json.Decode as Decode
import Set
import Studio.Palette as Palette exposing (ColorFilterMode(..), Shelf(..))
import Studio.PartIndex as PartIndex exposing (PartIndex)
import Test exposing (Test, describe, test)


indexJson : String
indexJson =
    """{"version":1,"parts":[
      ["3001.dat","Brick 2 x 4","Brick"],
      ["3002.dat","Brick 2 x 3","Brick"],
      ["3003.dat","Brick 2 x 2","Brick","square"],
      ["3023.dat","Plate 1 x 2","Plate"],
      ["3647.dat","Technic Gear 8 Tooth","Technic","cog, gear wheel"],
      ["4019.dat","Technic Gear 16 Tooth","Technic","cog"]
    ]}"""


testIndex : PartIndex
testIndex =
    case Decode.decodeString PartIndex.decoder indexJson of
        Ok i ->
            i

        Err _ ->
            { parts = [], categories = [] }


files : List PartIndex.Part -> List String
files =
    List.map .file


baseState : Palette.State
baseState =
    Palette.init


suite : Test
suite =
    describe "Studio.Palette"
        [ describe "Smart Shelves"
            [ test "All shelf returns all matching parts" <|
                \_ ->
                    Palette.filterParts 10 baseState 4 testIndex
                        |> files
                        |> Expect.equal [ "3001.dat", "3002.dat", "3003.dat", "3023.dat", "3647.dat", "4019.dat" ]
            , test "Favorites shelf filters only starred parts" <|
                \_ ->
                    let
                        state =
                            { baseState
                                | shelf = FavoritesShelf
                                , favorites = Set.fromList [ "3001.dat", "3647.dat" ]
                            }
                    in
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3001.dat", "3647.dat" ]
            , test "Recent shelf preserves recently used order" <|
                \_ ->
                    let
                        state =
                            { baseState
                                | shelf = RecentShelf
                                , recentParts = [ "3647.dat", "3023.dat", "3001.dat" ]
                            }
                    in
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3647.dat", "3023.dat", "3001.dat" ]
            , test "ColorMatched shelf filters parts officially produced in active colour" <|
                \_ ->
                    let
                        state =
                            { baseState | shelf = ColorMatchedShelf }
                    in
                    -- Color 4 is Red: 3001, 3002, 3003, 3023 are official in Red, but gear 3647/4019 are not official in Red
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3001.dat", "3002.dat", "3003.dat", "3023.dat" ]
            ]
        , describe "ColorFilterMode"
            [ test "OfficialOnly hides parts that are unofficial in the active colour" <|
                \_ ->
                    let
                        state =
                            { baseState | colorFilterMode = OfficialOnly }
                    in
                    -- Red (4): 3647 is unofficial, so it should be excluded
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> List.member "3647.dat"
                        |> Expect.equal False
            , test "ShowAllColors includes unofficial parts" <|
                \_ ->
                    let
                        state =
                            { baseState | colorFilterMode = ShowAllColors }
                    in
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> List.member "3647.dat"
                        |> Expect.equal True
            ]
        , describe "Shelf searching"
            [ test "query filters within Favorites shelf" <|
                \_ ->
                    let
                        state =
                            { baseState
                                | shelf = FavoritesShelf
                                , favorites = Set.fromList [ "3001.dat", "3002.dat", "3647.dat" ]
                                , query = "gear"
                            }
                    in
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3647.dat" ]
            , test "category filters within Recent shelf" <|
                \_ ->
                    let
                        state =
                            { baseState
                                | shelf = RecentShelf
                                , recentParts = [ "3001.dat", "3023.dat", "3647.dat" ]
                                , category = Just "Plate"
                            }
                    in
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3023.dat" ]
            ]
        ]
