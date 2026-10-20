module LDraw.DocumentTest exposing (suite)

{-| Round-trip tests for `LDraw.Document` and `LDraw.Writer`.
-}

import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Fixtures as Fixtures
import LDraw.Format as Format
import LDraw.Parser as Parser
import LDraw.Types exposing (LDrawLine)
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Document / LDraw.Writer"
        [ describe "parse >> write >> parse is identity"
            [ roundTrip "smile.ldr" Fixtures.smile
            , roundTrip "wheeler.ldr" Fixtures.wheeler
            , roundTrip "MPD with submodels" Fixtures.mpdModel
            , roundTrip "Studio type 11 + META" Fixtures.studioModel
            ]
        , describe "write is stable after one normalising pass"
            [ stable "smile.ldr" Fixtures.smile
            , stable "wheeler.ldr" Fixtures.wheeler
            , stable "MPD" Fixtures.mpdModel
            , stable "Studio" Fixtures.studioModel
            ]
        , describe "verbatim lines"
            [ test "META, comments and blank lines survive" <|
                \_ ->
                    Fixtures.studioModel
                        |> Document.parse
                        |> Writer.writeDocument
                        |> String.contains "0 !CUSTOM studio-metadata keep me"
                        |> Expect.equal True
            , test "Studio type 11 tokens are written back" <|
                \_ ->
                    Fixtures.studioModel
                        |> Document.parse
                        |> Writer.writeDocument
                        |> String.contains "11 4 uid-7 0 3 -40 -14 45.21 -1 0 0 0 1 0 0 0 -1 3647.dat"
                        |> Expect.equal True
            , test "tiny floats do not become exponents" <|
                \_ ->
                    Fixtures.studioModel
                        |> Document.parse
                        |> Writer.writeDocument
                        |> String.contains "e-"
                        |> Expect.equal False
            ]
        , describe "MPD structure"
            [ test "submodel order and names are kept" <|
                \_ ->
                    Document.parse Fixtures.mpdModel
                        |> .submodels
                        |> List.map .name
                        |> Expect.equal [ "Main.ldr", "Sub Group.ldr" ]
            , test "plain LDR has one unnamed submodel and no FILE header" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.wheeler
                    in
                    Expect.all
                        [ \d -> Expect.equal False (Document.isMultiFile d)
                        , \d -> Expect.equal False (String.contains "0 FILE" (Writer.writeDocument d))
                        ]
                        doc
            , test "placement ids are unique and sequential" <|
                \_ ->
                    Document.parse Fixtures.mpdModel
                        |> Document.placements
                        |> List.map .id
                        |> Expect.equal [ 0, 1, 2, 3 ]
            , test "MPD with leading blank line drops phantom unnamed submodel" <|
                \_ ->
                    let
                        doc =
                            Document.parse "\n0 FILE main.ldr\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"
                    in
                    Expect.all
                        [ \d -> Expect.equal "main.ldr" (Document.mainSubmodel d).name
                        , \d -> Expect.equal 1 (List.length (Document.mainPlacements d))
                        , \d -> Expect.equal [ "main.ldr" ] (List.map .name d.submodels)
                        , \d -> Expect.equal False (String.contains "0 FILE \n" (Writer.writeDocument d))
                        ]
                        doc
            , test "MPD with leading comment preserves comment in first named submodel" <|
                \_ ->
                    let
                        doc =
                            Document.parse "0 // Preamble\n0 FILE main.ldr\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"
                    in
                    Expect.all
                        [ \d -> Expect.equal "main.ldr" (Document.mainSubmodel d).name
                        , \d -> Expect.equal 1 (List.length (Document.mainPlacements d))
                        , \d -> Expect.equal [ "main.ldr" ] (List.map .name d.submodels)
                        , \d -> Expect.equal True (String.contains "0 // Preamble" (Writer.writeDocument d))
                        , \d -> Expect.equal False (String.contains "0 FILE \n" (Writer.writeDocument d))
                        ]
                        doc
            ]
        , describe "toLines"
            [ test "matches the existing parser for the main model" <|
                \_ ->
                    Fixtures.wheeler
                        |> Document.parse
                        |> Document.toLines
                        |> List.map describeLine
                        |> Expect.equal (Parser.parseFile Fixtures.wheeler |> List.map describeLine)
            ]
        , describe "editing"
            [ test "addPlacement returns a fresh id and appends to the main model" <|
                \_ ->
                    let
                        ( doc, id ) =
                            Document.parse Fixtures.mpdModel
                                |> Document.addPlacement "Main.ldr" { part = "3648.dat", color = 4, matrix = Mat4.identity }
                    in
                    Expect.all
                        [ \_ -> Expect.equal 4 id
                        , \d -> Expect.equal 5 (List.length (Document.placements d))
                        , \d -> Expect.equal True (String.contains "3648.dat" (Writer.writeDocument d))
                        ]
                        doc
            , test "removePlacement drops only that placement" <|
                \_ ->
                    Document.parse Fixtures.mpdModel
                        |> Document.removePlacement 1
                        |> Document.placements
                        |> List.map .id
                        |> Expect.equal [ 0, 2, 3 ]
            , test "updatePlacement changes colour and keeps position in file" <|
                \_ ->
                    Document.parse Fixtures.mpdModel
                        |> Document.updatePlacement 1 (\p -> { p | color = 14 })
                        |> Document.getPlacement 1
                        |> Maybe.map .color
                        |> Expect.equal (Just 14)
            ]
        , describe "Submodel Grouping and Exploding"
            [ test "groupIntoSubmodel groups selected placements at their shared center" <|
                \_ ->
                    let
                        rawModel =
                            String.join "\n"
                                [ "1 4 10 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 30 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , ""
                                ]

                        doc =
                            Document.parse rawModel

                        grouped =
                            Document.groupIntoSubmodel "assembly.ldr" [ 0, 1 ] doc

                        mainPlacements =
                            Document.mainPlacements grouped

                        submodelPlacements =
                            Document.placementsOf "assembly.ldr" grouped
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length mainPlacements)
                        , \_ -> Expect.equal (Just "assembly.ldr") (List.head mainPlacements |> Maybe.map .fileText)
                        , \_ -> Expect.equal 2 (List.length submodelPlacements)
                        , \_ ->
                            case List.head mainPlacements of
                                Just p ->
                                    let
                                        pos =
                                            Mat4.transform p.matrix (vec3 0 0 0)
                                    in
                                    Expect.within (Expect.Absolute 0.001) 20 (Vec3.getX pos)

                                Nothing ->
                                    Expect.fail "missing group placement"
                        ]
                        ()
            , test "explodeSubmodel inlines constituent placements preserving positions" <|
                \_ ->
                    let
                        rawModel =
                            String.join "\n"
                                [ "1 4 10 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 30 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , ""
                                ]

                        doc =
                            Document.parse rawModel

                        grouped =
                            Document.groupIntoSubmodel "assembly.ldr" [ 0, 1 ] doc

                        exploded =
                            Document.explodeSubmodel 2 grouped

                        finalPlacements =
                            Document.mainPlacements exploded
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 (List.length finalPlacements)
                        , \_ -> Expect.equal False (List.any (\s -> s.name == "assembly.ldr") exploded.submodels)
                        , \_ ->
                            let
                                xCoords =
                                    finalPlacements
                                        |> List.map (\p -> Vec3.getX (Mat4.transform p.matrix (vec3 0 0 0)))
                            in
                            Expect.equal [ 10, 30 ] (List.map round xCoords)
                        ]
                        ()
            , test "explodeSubmodel preserves submodel when another instance exists" <|
                \_ ->
                    let
                        mpd =
                            String.join "\n"
                                [ "0 FILE main.ldr"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 sub.ldr"
                                , "1 16 50 0 0 1 0 0 0 1 0 0 0 1 sub.ldr"
                                , "0 FILE sub.ldr"
                                , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , ""
                                ]

                        doc =
                            Document.parse mpd

                        exploded =
                            Document.explodeSubmodel 0 doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (List.any (\s -> s.name == "sub.ldr") exploded.submodels)
                        , \_ -> Expect.equal 2 (List.length (Document.mainPlacements exploded))
                        ]
                        ()
            , test "locatePlacement identifies submodel name and index" <|
                \_ ->
                    let
                        doc =
                            Document.parse Fixtures.mpdModel
                    in
                    case Document.locatePlacement 2 doc of
                        Just ( subName, idx, _ ) ->
                            Expect.all
                                [ \_ -> Expect.equal "Sub Group.ldr" subName
                                , \_ -> Expect.equal 2 idx
                                ]
                                ()

                        Nothing ->
                            Expect.fail "placement 2 not found"
            ]
        ]


roundTrip : String -> String -> Test
roundTrip label text =
    test label <|
        \_ ->
            let
                once =
                    Document.parse text
            in
            Document.parse (Writer.writeDocument once)
                |> summary
                |> Expect.equal (summary once)


stable : String -> String -> Test
stable label text =
    test label <|
        \_ ->
            let
                once =
                    Writer.writeDocument (Document.parse text)
            in
            Writer.writeDocument (Document.parse once)
                |> Expect.equal once


{-| Comparable view of a document (`Mat4` is compared through its record).
-}
summary : Document -> List ( String, List String )
summary doc =
    doc.submodels
        |> List.map
            (\s ->
                ( s.name
                , List.map
                    (\e ->
                        case e of
                            Document.PlacementEntry p ->
                                String.join "|"
                                    [ String.fromInt p.id
                                    , p.part
                                    , p.fileText
                                    , String.fromInt p.color
                                    , String.join "," p.extra
                                    , Debug.toString (Mat4.toRecord p.matrix)
                                    ]

                            Document.Raw text ->
                                "raw:" ++ text
                    )
                    s.entries
                )
            )


describeLine : LDrawLine -> String
describeLine =
    Format.lineToText
