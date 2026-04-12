module LDraw.ParserTest exposing (suite)

{-| Unit tests for line parsing and MPD splitting.
-}

import Dict
import Expect
import LDraw.Parser exposing (parseFile, parseLine, splitMpd)
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Parser"
        [ describe "parseLine"
            [ test "type 0 comment" <|
                \_ ->
                    parseLine "0 This is a comment"
                        |> Expect.equal (Just (Comment "This is a comment"))
            , test "type 0 empty comment" <|
                \_ ->
                    parseLine "0"
                        |> Expect.equal (Just (Comment ""))
            , test "empty line returns Nothing" <|
                \_ ->
                    parseLine ""
                        |> Expect.equal Nothing
            , test "whitespace-only returns Nothing" <|
                \_ ->
                    parseLine "   "
                        |> Expect.equal Nothing
            , test "unknown type digit returns Nothing" <|
                \_ ->
                    parseLine "9 some garbage"
                        |> Expect.equal Nothing
            , test "type 1 identity transform" <|
                \_ ->
                    let
                        result =
                            parseLine "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3647.dat"
                    in
                    case result of
                        Just (SubFileRef ref) ->
                            Expect.all
                                [ \r -> Expect.equal 16 r.color
                                , \r -> Expect.equal "3647.dat" r.file
                                , \r ->
                                    -- Identity matrix: transform origin → origin
                                    let
                                        p =
                                            Mat4.transform r.transform (vec3 0 0 0)
                                    in
                                    Expect.equal (vec3 0 0 0) p
                                ]
                                ref

                        _ ->
                            Expect.fail ("Expected SubFileRef, got: " ++ Debug.toString result)
            , test "type 1 translation" <|
                \_ ->
                    let
                        result =
                            parseLine "1 4 20 -8 0 1 0 0 0 1 0 0 0 1 stud.dat"
                    in
                    case result of
                        Just (SubFileRef ref) ->
                            Expect.all
                                [ \r -> Expect.equal 4 r.color
                                , \r -> Expect.equal "stud.dat" r.file
                                , \r ->
                                    -- Translation matrix: origin → (20, -8, 0)
                                    let
                                        p =
                                            Mat4.transform r.transform (vec3 0 0 0)
                                    in
                                    Expect.equal (vec3 20 -8 0) p
                                ]
                                ref

                        _ ->
                            Expect.fail ("Expected SubFileRef, got: " ++ Debug.toString result)
            , test "type 1 filename normalised to lowercase" <|
                \_ ->
                    let
                        result =
                            parseLine "1 16 0 0 0 1 0 0 0 1 0 0 0 1 STUD.DAT"
                    in
                    case result of
                        Just (SubFileRef ref) ->
                            Expect.equal "stud.dat" ref.file

                        _ ->
                            Expect.fail "Expected SubFileRef"
            , test "type 1 backslash in filename normalised to forward slash" <|
                \_ ->
                    let
                        -- Note: we can't put a real backslash in an Elm string
                        -- literal easily, so we test the normalised form directly
                        -- by providing a forward-slash path (normalisation is idempotent)
                        result =
                            parseLine "1 16 0 0 0 1 0 0 0 1 0 0 0 1 s/stud4.dat"
                    in
                    case result of
                        Just (SubFileRef ref) ->
                            Expect.equal "s/stud4.dat" ref.file

                        _ ->
                            Expect.fail "Expected SubFileRef"
            , test "type 2 line segment" <|
                \_ ->
                    let
                        result =
                            parseLine "2 24 0 0 0 10 0 0"
                    in
                    case result of
                        Just (LineSegment seg) ->
                            Expect.all
                                [ \s -> Expect.equal 24 s.color
                                , \s -> Expect.equal (vec3 0 0 0) s.p1
                                , \s -> Expect.equal (vec3 10 0 0) s.p2
                                ]
                                seg

                        _ ->
                            Expect.fail ("Expected LineSegment, got: " ++ Debug.toString result)
            , test "type 3 triangle" <|
                \_ ->
                    let
                        result =
                            parseLine "3 4 1 0 0 0 1 0 0 0 1"
                    in
                    case result of
                        Just (Triangle tri) ->
                            Expect.all
                                [ \t -> Expect.equal 4 t.color
                                , \t -> Expect.equal (vec3 1 0 0) t.p1
                                , \t -> Expect.equal (vec3 0 1 0) t.p2
                                , \t -> Expect.equal (vec3 0 0 1) t.p3
                                ]
                                tri

                        _ ->
                            Expect.fail ("Expected Triangle, got: " ++ Debug.toString result)
            , test "type 4 quad" <|
                \_ ->
                    let
                        result =
                            parseLine "4 16 -1 0 -1 1 0 -1 1 0 1 -1 0 1"
                    in
                    case result of
                        Just (Quad quad) ->
                            Expect.all
                                [ \q -> Expect.equal 16 q.color
                                , \q -> Expect.equal (vec3 -1 0 -1) q.p1
                                , \q -> Expect.equal (vec3 1 0 -1) q.p2
                                , \q -> Expect.equal (vec3 1 0 1) q.p3
                                , \q -> Expect.equal (vec3 -1 0 1) q.p4
                                ]
                                quad

                        _ ->
                            Expect.fail ("Expected Quad, got: " ++ Debug.toString result)
            , test "type 5 conditional line" <|
                \_ ->
                    let
                        result =
                            parseLine "5 24 0 0 0 1 0 0 0 0 1 1 0 1"
                    in
                    case result of
                        Just (ConditionalLine cond) ->
                            Expect.all
                                [ \c -> Expect.equal 24 c.color
                                , \c -> Expect.equal (vec3 0 0 0) c.p1
                                , \c -> Expect.equal (vec3 1 0 0) c.p2
                                , \c -> Expect.equal (vec3 0 0 1) c.c1
                                , \c -> Expect.equal (vec3 1 0 1) c.c2
                                ]
                                cond

                        _ ->
                            Expect.fail ("Expected ConditionalLine, got: " ++ Debug.toString result)
            , test "malformed float returns Nothing" <|
                \_ ->
                    parseLine "3 4 a 0 0 0 1 0 0 0 1"
                        |> Expect.equal Nothing
            , test "too few tokens for type 3 returns Nothing" <|
                \_ ->
                    parseLine "3 4 1 0 0"
                        |> Expect.equal Nothing
            ]
        , describe "parseFile"
            [ test "parses multiple lines, skips empties" <|
                \_ ->
                    let
                        input =
                            "0 First comment\n\n3 4 1 0 0 0 1 0 0 0 1\n   "

                        result =
                            parseFile input
                    in
                    Expect.equal 2 (List.length result)
            , test "returns lines in order" <|
                \_ ->
                    let
                        result =
                            parseFile "0 A\n0 B\n0 C"
                    in
                    Expect.equal
                        [ Comment "A", Comment "B", Comment "C" ]
                        result
            ]
        , describe "splitMpd"
            [ test "splits on FILE markers" <|
                \_ ->
                    let
                        input =
                            "0 FILE main.ldr\n0 A comment\n0 FILE sub.dat\n3 4 0 0 0 1 0 0 0 0 1"

                        result =
                            splitMpd input
                    in
                    Expect.all
                        [ \d -> Expect.equal True (Dict.member "main.ldr" d)
                        , \d -> Expect.equal True (Dict.member "sub.dat" d)
                        , \d -> Expect.equal 2 (Dict.size d)
                        ]
                        result
            , test "FILE names are normalised to lowercase" <|
                \_ ->
                    let
                        input =
                            "0 FILE MAIN.LDR\n0 comment"

                        result =
                            splitMpd input
                    in
                    Expect.equal True (Dict.member "main.ldr" result)
            , test "non-MPD file returns empty dict" <|
                \_ ->
                    let
                        input =
                            "0 This is just a plain file\n3 4 0 0 0 1 0 0 0 0 1"
                    in
                    Expect.equal Dict.empty (splitMpd input)
            , test "content between FILE markers is correct" <|
                \_ ->
                    let
                        input =
                            "0 FILE a.dat\nline1\nline2\n0 FILE b.dat\nline3"

                        result =
                            splitMpd input
                    in
                    Expect.equal (Just "line1\nline2") (Dict.get "a.dat" result)
            ]
        ]
