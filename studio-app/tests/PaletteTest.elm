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


{-| A catalogue with classics, oddities, prints and Duplo mixed together, in
file order as the real index is.
-}
groupIndex : PartIndex
groupIndex =
    let
        parts =
            [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
            , PartIndex.makePart "3004.dat" "Brick 1 x 2" "Brick" ""
            , PartIndex.makePart "3005.dat" "Brick 1 x 1" "Brick" ""
            , PartIndex.makePart "3011.dat" "Duplo Brick 2 x 4" "Duplo" ""
            , PartIndex.makePart "3023b.dat" "Plate 1 x 2" "Plate" ""
            , PartIndex.makePart "3023bp00.dat" "Plate 1 x 2 with Black and Medium Blue Mechanical Eyes Pattern" "Plate" ""
            , PartIndex.makePart "3245c.dat" "Brick 1 x 2 x 2 without Understud" "Brick" ""
            , PartIndex.makePart "3647.dat" "Technic Gear 8 Tooth" "Technic" "cog"
            ]
    in
    { parts = parts, categories = PartIndex.categories parts }


crossIndex : PartIndex
crossIndex =
    let
        parts =
            [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
            , PartIndex.makePart "3020.dat" "Plate 2 x 4" "Plate" ""
            , PartIndex.makePart "3647.dat" "Technic Gear 8 Tooth" "Technic" ""
            , PartIndex.makePart "35442.dat" "Plate Gear 6 Tooth 2 x 2" "Plate" ""
            , PartIndex.makePart "4593.dat" "Hinge Control Stick" "Hinge" "gear shift, lever"
            , PartIndex.makePart "51739.dat" "Wing 2 x 4" "Wing" ""
            ]
    in
    { parts = parts, categories = PartIndex.categories parts }


boundaryIndex : PartIndex
boundaryIndex =
    let
        parts =
            [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
            , PartIndex.makePart "2352.dat" "Windscreen 2 x 12 x 4" "Windscreen" ""
            , PartIndex.makePart "3811.dat" "Baseplate 32 x 48" "Baseplate" ""
            ]
    in
    { parts = parts, categories = PartIndex.categories parts }


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
            [ test "All shelf under Everything returns all matching parts, building parts first" <|
                \_ ->
                    Palette.filterParts 10 { baseState | group = Nothing } 4 testIndex
                        |> files
                        |> Expect.equal [ "3003.dat", "3002.dat", "3001.dat", "3023.dat", "3647.dat", "4019.dat" ]
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
                            { baseState | shelf = ColorMatchedShelf, group = Nothing }
                    in
                    -- Color 4 is Red: 3001, 3002, 3003, 3023 are official in Red, but gear 3647/4019 are not official in Red
                    Palette.filterParts 10 state 4 testIndex
                        |> files
                        |> Expect.equal [ "3003.dat", "3002.dat", "3001.dat", "3023.dat" ]
            ]
        , describe "Build groups"
            [ test "the palette opens on Bricks, classics in curated order" <|
                \_ ->
                    Palette.filterParts 10 baseState 4 groupIndex
                        |> files
                        |> Expect.equal [ "3005.dat", "3004.dat", "3001.dat", "3245c.dat" ]
            , test "Technic is a group of its own" <|
                \_ ->
                    Palette.filterParts 10 { baseState | group = Just "technic" } 4 groupIndex
                        |> files
                        |> Expect.equal [ "3647.dat" ]
            , test "prints and Duplo are never shelved" <|
                \_ ->
                    Palette.filterParts 10 { baseState | group = Just "plates" } 4 groupIndex
                        |> files
                        |> Expect.equal [ "3023b.dat" ]
            , test "a search ignores the group and ranks building parts before the rest" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "1 x 2" } 4 groupIndex
                        |> files
                        |> Expect.equal [ "3004.dat", "3245c.dat", "3023b.dat", "3023bp00.dat" ]
            , test "a search ties across groups in group order: brick before plate" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "2x4" } 4 crossIndex
                        |> files
                        |> Expect.equal [ "3001.dat", "3020.dat", "51739.dat" ]
            , test "a classic leads its search, then description matches, then keyword-only ones" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "gear" } 4 crossIndex
                        |> files
                        |> Expect.equal [ "3647.dat", "35442.dat", "4593.dat" ]
            , test "an exact part number leads, typed in any case" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "3023B.DAT" } 4 groupIndex
                        |> files
                        |> List.head
                        |> Expect.equal (Just "3023b.dat")
            , test "a size does not match inside a larger number" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "2 x 4" } 4 boundaryIndex
                        |> files
                        |> Expect.equal [ "3001.dat" ]
            , test "half-spaced sizes are read whole" <|
                \_ ->
                    ( Palette.filterParts 10 { baseState | query = "brick 2 x4" } 4 boundaryIndex |> files
                    , Palette.filterParts 10 { baseState | query = "brick 2x 4" } 4 boundaryIndex |> files
                    )
                        |> Expect.equal ( [ "3001.dat" ], [ "3001.dat" ] )
            , test "Favourites ignore the build group" <|
                \_ ->
                    Palette.filterParts 10 { baseState | shelf = FavoritesShelf, favorites = Set.fromList [ "3647.dat" ] } 4 groupIndex
                        |> files
                        |> Expect.equal [ "3647.dat" ]
            , test "sizes typed as 2x4 match LDraw's 2 x 4" <|
                \_ ->
                    Palette.filterParts 10 { baseState | query = "brick 2x4" } 4 groupIndex
                        |> files
                        |> Expect.equal [ "3001.dat", "3011.dat" ]
            ]
        , describe "ColorFilterMode"
            [ test "OfficialOnly hides parts that are unofficial in the active colour" <|
                \_ ->
                    let
                        state =
                            { baseState | colorFilterMode = OfficialOnly, group = Nothing }
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
                            { baseState | colorFilterMode = ShowAllColors, group = Nothing }
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
