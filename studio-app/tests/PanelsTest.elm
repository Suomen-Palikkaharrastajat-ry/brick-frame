module PanelsTest exposing (suite)

{-| Tests for the studio's side panels: the part catalogue and its search, the
colour swatches, the structure tree and the inspector's fields.
-}

import Expect
import Json.Decode as Decode
import LDraw.Document as Document
import Set
import Studio.Colours as Colours
import Studio.Inspector as Inspector
import Studio.PartIndex as PartIndex exposing (PartIndex)
import Studio.Tree as Tree exposing (Row(..))
import Test exposing (Test, describe, test)


indexJson : String
indexJson =
    """{"version":1,"parts":[
      ["3001.dat","Brick 2 x 4","Brick"],
      ["3002.dat","Brick 2 x 3","Brick"],
      ["3003.dat","Brick 2 x 2","Brick","square"],
      ["3023.dat","Plate 1 x 2","Plate"],
      ["3647.dat","Technic Gear 8 Tooth","Technic","cog, gear wheel"],
      ["4019.dat","Technic Gear 16 Tooth","Technic","cog"],
      ["30001.dat","Brick 1 x 1 with Hole","Brick"]
    ]}"""


index : PartIndex
index =
    case Decode.decodeString PartIndex.decoder indexJson of
        Ok i ->
            i

        Err _ ->
            { parts = [], categories = [] }


files : List PartIndex.Part -> List String
files =
    List.map .file


document : Document.Document
document =
    Document.parse
        (String.join "\n"
            [ "0 FILE main.ldr"
            , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
            , "0 STEP"
            , "1 14 40 0 0 1 0 0 0 1 0 0 0 1 3023.dat"
            , "1 1 80 0 0 1 0 0 0 1 0 0 0 1 3023.dat"
            , "0 STEP"
            , "1 2 120 0 0 1 0 0 0 1 0 0 0 1 sub.ldr"
            , "0 FILE sub.ldr"
            , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
            ]
        )


suite : Test
suite =
    describe "Studio side panels"
        [ describe "PartIndex"
            [ test "reads every part, with or without keywords" <|
                \_ -> Expect.equal 7 (List.length index.parts)
            , test "rejects an index of an unknown version" <|
                \_ ->
                    Decode.decodeString PartIndex.decoder """{"version":2,"parts":[]}"""
                        |> Result.toMaybe
                        |> Expect.equal Nothing
            , test "counts parts per category, biggest first" <|
                \_ ->
                    Expect.equal [ ( "Brick", 4 ), ( "Technic", 2 ), ( "Plate", 1 ) ] index.categories
            ]
        , describe "PartIndex.search"
            [ test "matches words in the description in any order" <|
                \_ ->
                    PartIndex.search 10 "2 brick" Nothing index
                        |> files
                        |> List.sort
                        |> Expect.equal [ "3001.dat", "3002.dat", "3003.dat" ]
            , test "every word has to match" <|
                \_ ->
                    PartIndex.search 10 "brick 2 x 4" Nothing index
                        |> files
                        |> Expect.equal [ "3001.dat" ]
            , test "finds parts by keyword" <|
                \_ ->
                    PartIndex.search 10 "cog" Nothing index
                        |> files
                        |> List.sort
                        |> Expect.equal [ "3647.dat", "4019.dat" ]
            , test "an exact part number comes first" <|
                \_ ->
                    PartIndex.search 10 "3023" Nothing index
                        |> files
                        |> List.head
                        |> Expect.equal (Just "3023.dat")
            , test "a number typed without .dat still finds the file" <|
                \_ ->
                    PartIndex.search 10 "3647.dat" Nothing index
                        |> files
                        |> Expect.equal [ "3647.dat" ]
            , test "parts whose number starts with the query come before looser matches" <|
                \_ ->
                    PartIndex.search 10 "300" Nothing index
                        |> files
                        |> Expect.equal [ "3001.dat", "3002.dat", "3003.dat", "30001.dat" ]
            , test "a category narrows the search" <|
                \_ ->
                    PartIndex.search 10 "" (Just "Technic") index
                        |> files
                        |> List.sort
                        |> Expect.equal [ "3647.dat", "4019.dat" ]
            , test "the limit caps the list" <|
                \_ ->
                    PartIndex.search 2 "" Nothing index
                        |> List.length
                        |> Expect.equal 2
            , test "case does not matter" <|
                \_ ->
                    PartIndex.search 10 "TECHNIC GEAR" Nothing index
                        |> List.length
                        |> Expect.equal 2
            , test "nothing matches nothing" <|
                \_ ->
                    PartIndex.search 10 "zzzz" Nothing index
                        |> Expect.equal []
            ]
        , describe "Colours"
            [ test "special colours are not offered" <|
                \_ ->
                    Colours.all
                        |> List.filter (\s -> s.code == 16 || s.code == 24)
                        |> Expect.equal []
            , test "css turns 0–1 channels into rgba()" <|
                \_ ->
                    Colours.css { r = 1, g = 0.5, b = 0, alpha = 0.5 }
                        |> Expect.equal "rgba(255,128,0,0.5)"
            , test "css clamps out-of-range channels" <|
                \_ ->
                    Colours.css { r = 2, g = -1, b = 0, alpha = 1 }
                        |> Expect.equal "rgba(255,0,0,1)"
            ]
        , describe "Tree"
            [ test "lists the submodels, and the main one's parts grouped by step" <|
                \_ ->
                    Tree.rows document
                        |> List.map describe_
                        |> Expect.equal
                            [ "submodel main.ldr 4"
                            , "part 0 3001.dat"
                            , "step 2"
                            , "part 1 3023.dat"
                            , "part 2 3023.dat"
                            , "step 3"
                            , "part 3 sub.ldr"
                            , "submodel sub.ldr 1"
                            ]
            , test "a plain model is one unnamed submodel" <|
                \_ ->
                    Tree.rows (Document.parse "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n")
                        |> List.map describe_
                        |> Expect.equal [ "submodel Model 1", "part 0 3001.dat" ]
            , test "a long list is cut short and says how much is missing" <|
                \_ ->
                    List.range 1 350
                        |> List.map (\i -> "1 4 " ++ String.fromInt i ++ " 0 0 1 0 0 0 1 0 0 0 1 3001.dat")
                        |> String.join "\n"
                        |> Document.parse
                        |> Tree.rows
                        |> List.reverse
                        |> List.head
                        |> Maybe.map describe_
                        |> Expect.equal (Just "more 50")
            ]
        , describe "Inspector.fields"
            [ test "nothing selected has no fields" <|
                \_ ->
                    Inspector.fields Set.empty document |> Expect.equal Nothing
            , test "ids that are not in the document have no fields" <|
                \_ ->
                    Inspector.fields (Set.fromList [ 99 ]) document |> Expect.equal Nothing
            , test "a single part shows its position, rotation and colour" <|
                \_ ->
                    case Inspector.fields (Set.fromList [ 1 ]) document of
                        Just f ->
                            Expect.all
                                [ \_ -> Expect.equal 1 f.count
                                , \_ -> Expect.equal 40 f.position.x
                                , \_ -> Expect.equal [ 14 ] f.colours
                                , \_ -> Expect.equal [ "3023.dat" ] f.files
                                , \_ -> Expect.equal (Just { x = 0, y = 0, z = 0 }) f.rotation
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected fields"
            , test "several parts share their centre and show no rotation" <|
                \_ ->
                    case Inspector.fields (Set.fromList [ 0, 1 ]) document of
                        Just f ->
                            Expect.all
                                [ \_ -> Expect.equal 2 f.count
                                , \_ -> Expect.equal 20 f.position.x
                                , \_ -> Expect.equal Nothing f.rotation
                                , \_ -> Expect.equal [ 4, 14 ] f.colours
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected fields"
            ]
        ]


describe_ : Row -> String
describe_ row =
    case row of
        SubmodelRow { name, parts } ->
            "submodel " ++ name ++ " " ++ String.fromInt parts

        StepRow n ->
            "step " ++ String.fromInt n

        PartRow { id, file } ->
            "part " ++ String.fromInt id ++ " " ++ file

        MoreRow n ->
            "more " ++ String.fromInt n
