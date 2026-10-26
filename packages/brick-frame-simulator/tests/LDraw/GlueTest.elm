module LDraw.GlueTest exposing (suite)

{-| Tests for glued placements: the `0 !PALIKKA GLUED` line kept in front of a
placement, through parsing, writing and every document edit that could strand it.
-}

import Expect
import LDraw.Document as Document exposing (Document)
import LDraw.Steps as Steps
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Set
import Test exposing (Test, describe, test)


{-| A 3001 brick at `x`, as a type 1 line.
-}
brick : Int -> String
brick x =
    "1 4 " ++ String.fromInt x ++ " 0 0 1 0 0 0 1 0 0 0 1 3001.dat"


{-| Placement 0 is glued, 1 and 2 are not.
-}
gluedText : String
gluedText =
    String.join "\n"
        [ "0 Test model"
        , "0 !PALIKKA GLUED"
        , brick 0
        , brick 40
        , brick 80
        ]


{-| The glued set as a sorted list, for comparisons.
-}
glued : Document -> List Int
glued doc =
    Set.toList (Document.gluedIds doc)


suite : Test
suite =
    describe "LDraw.Document glue"
        [ test "glue survives a write and parse round trip" <|
            \_ ->
                let
                    written =
                        Writer.writeDocument (Document.parse gluedText)
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (glued (Document.parse gluedText))
                    , \_ -> Expect.equal [ 0 ] (glued (Document.parse written))
                    , \_ -> Expect.equal True (String.contains "0 !PALIKKA GLUED\n1 " written)
                    ]
                    ()
        , test "isGlued reports the glued placement only" <|
            \_ ->
                let
                    doc =
                        Document.parse gluedText
                in
                Expect.equal [ True, False, False ]
                    [ Document.isGlued 0 doc, Document.isGlued 1 doc, Document.isGlued 2 doc ]
        , test "setGlued True then False round trips to the original text" <|
            \_ ->
                let
                    plain =
                        Document.parse (String.replace "0 !PALIKKA GLUED\n" "" gluedText)

                    gluedOnce =
                        Document.setGlued True 1 plain
                in
                Expect.all
                    [ \_ -> Expect.equal [ 1 ] (glued gluedOnce)
                    , \_ -> Expect.equal (Writer.writeDocument plain) (Writer.writeDocument (Document.setGlued False 1 gluedOnce))
                    ]
                    ()
        , test "setGlued is idempotent" <|
            \_ ->
                let
                    once =
                        Document.setGlued True 1 (Document.parse gluedText)

                    twice =
                        Document.setGlued True 1 once
                in
                Expect.equal (Writer.writeDocument once) (Writer.writeDocument twice)
        , test "setGlued False on an unglued placement changes nothing" <|
            \_ ->
                let
                    doc =
                        Document.parse gluedText
                in
                Expect.equal (Writer.writeDocument doc) (Writer.writeDocument (Document.setGlued False 1 doc))
        , test "setGlued on an unknown id changes nothing" <|
            \_ ->
                let
                    doc =
                        Document.parse gluedText
                in
                Expect.equal (Writer.writeDocument doc) (Writer.writeDocument (Document.setGlued True 99 doc))
        , test "removing a neighbour keeps the glue line of the glued placement" <|
            \_ ->
                let
                    doc =
                        Document.removePlacement 1 (Document.parse gluedText)
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (glued doc)
                    , \_ -> Expect.equal True (String.contains "0 !PALIKKA GLUED\n1 " (Writer.writeDocument doc))
                    ]
                    ()
        , test "removing the glued placement removes its glue line too" <|
            \_ ->
                let
                    written =
                        Writer.writeDocument (Document.removePlacement 0 (Document.parse gluedText))
                in
                Expect.equal False (String.contains "GLUED" written)
        , test "a placement added after a glued one is not glued" <|
            \_ ->
                let
                    ( doc, newId ) =
                        Document.addPlacement "" { part = "3001.dat", color = 4, matrix = Mat4.makeTranslate (vec3 120 0 0) } (Document.parse gluedText)
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (glued doc)
                    , \_ -> Expect.equal False (Document.isGlued newId doc)
                    ]
                    ()
        , test "grouping glued placements keeps them glued inside the new submodel" <|
            \_ ->
                let
                    grouped =
                        Document.groupIntoSubmodel "group.ldr" [ 0, 1 ] (Document.parse gluedText)

                    written =
                        Writer.writeDocument grouped
                in
                Expect.all
                    [ \_ -> Expect.equal True (Set.member 0 (Document.gluedIds grouped))
                    , \_ -> Expect.equal True (String.contains "0 !PALIKKA GLUED\n1 " written)

                    -- One glue line for the glued member inside the group, one for the group reference.
                    , \_ -> Expect.equal 2 (count "GLUED" written)
                    ]
                    ()
        , test "exploding a glued submodel placement drops its glue" <|
            \_ ->
                let
                    grouped =
                        Document.groupIntoSubmodel "group.ldr" [ 0, 1 ] (Document.parse gluedText)

                    -- The group reference is the last placement added.
                    groupRef =
                        Document.placements grouped
                            |> List.map .id
                            |> List.maximum
                            |> Maybe.withDefault -1

                    exploded =
                        Document.explodeSubmodel groupRef grouped
                in
                Expect.equal 1 (Set.size (Document.gluedIds exploded))
        , test "autoStepDocument keeps the glue line in front of its placement" <|
            \_ ->
                let
                    stepped =
                        Steps.autoStepDocument 1 (Document.parse gluedText)
                in
                Expect.all
                    [ \_ -> Expect.equal True (String.contains "0 STEP\n0 !PALIKKA GLUED\n1 " (Writer.writeDocument stepped))
                    , \_ -> Expect.equal [ 0 ] (glued stepped)
                    ]
                    ()
        ]


{-| How many times `needle` occurs in `haystack`.
-}
count : String -> String -> Int
count needle haystack =
    List.length (String.indexes needle haystack)
