module EditTest exposing (suite)

{-| Tests for `Studio.Edit`: every command must be undone exactly by the
inverse `apply` returns, and the history must walk back and forth without
losing or inventing steps.
-}

import Expect exposing (Expectation)
import LDraw.Document as Document exposing (Document)
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Studio.Edit as Edit exposing (Axis(..), Command(..))
import Test exposing (Test, describe, test)


source : String
source =
    String.join "\n"
        [ "0 Test model"
        , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "0 // a comment between parts"
        , "1 14 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 2 80 0 -24 1 0 0 0 1 0 0 0 1 3003.dat"
        , ""
        ]


doc : Document
doc =
    Document.parse source |> Document.clearTouched


text : Document -> String
text =
    Writer.writeDocument


positionOf : Int -> Document -> Maybe ( Float, Float, Float )
positionOf id d =
    Document.getPlacement id d
        |> Maybe.map (\p -> Mat4.transform p.matrix (vec3 0 0 0) |> Vec3.toRecord |> (\v -> ( v.x, v.y, v.z )))


near : ( Float, Float, Float ) -> Maybe ( Float, Float, Float ) -> Expectation
near ( x, y, z ) actual =
    case actual of
        Just ( ax, ay, az ) ->
            Expect.all
                [ \_ -> Expect.within (Expect.Absolute 0.0001) x ax
                , \_ -> Expect.within (Expect.Absolute 0.0001) y ay
                , \_ -> Expect.within (Expect.Absolute 0.0001) z az
                ]
                ()

        Nothing ->
            Expect.fail "placement is missing"


suite : Test
suite =
    describe "Studio.Edit"
        [ describe "Translate"
            [ test "moves the named placements only" <|
                \_ ->
                    let
                        ( moved, _ ) =
                            Edit.apply (Translate [ 1 ] (vec3 20 0 -8)) doc
                    in
                    Expect.all
                        [ \d -> near ( 60, 0, -8 ) (positionOf 1 d)
                        , \d -> near ( 0, 0, 0 ) (positionOf 0 d)
                        ]
                        moved
            , test "is undone by its inverse" <|
                \_ ->
                    let
                        ( moved, inverse ) =
                            Edit.apply (Translate [ 0, 1 ] (vec3 20 4 -8)) doc

                        ( back, _ ) =
                            Edit.apply inverse moved
                    in
                    Expect.all
                        [ \d -> near ( 0, 0, 0 ) (positionOf 0 d)
                        , \d -> near ( 40, 0, 0 ) (positionOf 1 d)
                        ]
                        back
            ]
        , describe "Rotate"
            [ test "a quarter turn about Y carries a point about the pivot" <|
                \_ ->
                    let
                        ( turned, _ ) =
                            Edit.apply (Rotate [ 1 ] { pivot = vec3 0 0 0, axis = AxisY, radians = pi / 2 }) doc
                    in
                    case positionOf 1 turned of
                        Just ( x, y, z ) ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.0001) 0 x
                                , \_ -> Expect.within (Expect.Absolute 0.0001) 0 y
                                , \_ -> Expect.within (Expect.Absolute 0.0001) 40 (abs z)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "missing"
            , test "four quarter turns come back to where they started" <|
                \_ ->
                    List.range 1 4
                        |> List.foldl
                            (\_ d -> Edit.apply (Rotate [ 1 ] { pivot = vec3 0 0 0, axis = AxisY, radians = pi / 2 }) d |> Tuple.first)
                            doc
                        |> positionOf 1
                        |> near ( 40, 0, 0 )
            , test "twenty-four 15° turns leave no drift in the written file" <|
                \_ ->
                    List.range 1 24
                        |> List.foldl
                            (\_ d -> Edit.apply (Rotate [ 1 ] { pivot = vec3 0 0 0, axis = AxisZ, radians = degrees 15 }) d |> Tuple.first)
                            doc
                        |> text
                        |> Expect.equal source
            , test "turns about the pivot, not the origin" <|
                \_ ->
                    -- Placement 1 sits 20 to the right of a pivot at x = 20: a
                    -- quarter turn about Y puts it 20 in front of the pivot.
                    let
                        ( turned, _ ) =
                            Edit.apply (Rotate [ 1 ] { pivot = vec3 20 0 0, axis = AxisY, radians = pi / 2 }) doc
                    in
                    case positionOf 1 turned of
                        Just ( x, y, z ) ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.0001) 20 x
                                , \_ -> Expect.within (Expect.Absolute 0.0001) 0 y
                                , \_ -> Expect.within (Expect.Absolute 0.0001) 20 (abs z)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "missing"
            , test "is undone by its inverse" <|
                \_ ->
                    let
                        ( turned, inverse ) =
                            Edit.apply (Rotate [ 0, 1 ] { pivot = vec3 20 0 0, axis = AxisX, radians = 1.1 }) doc
                    in
                    Edit.apply inverse turned
                        |> Tuple.first
                        |> positionOf 1
                        |> near ( 40, 0, 0 )
            ]
        , describe "Recolor"
            [ test "sets colours and undoes them" <|
                \_ ->
                    let
                        ( painted, inverse ) =
                            Edit.apply (Recolor [ ( 0, 1 ), ( 2, 7 ) ]) doc

                        colours d =
                            List.map .color (Document.placements d)
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ 1, 14, 7 ] (colours painted)
                        , \_ -> Expect.equal [ 4, 14, 2 ] (colours (Edit.apply inverse painted |> Tuple.first))
                        ]
                        ()
            ]
        , describe "SetMatrix"
            [ test "replaces matrices and undoes them" <|
                \_ ->
                    let
                        ( set, inverse ) =
                            Edit.apply (SetMatrix [ ( 1, Mat4.makeTranslate3 7 8 9 ) ]) doc
                    in
                    Expect.all
                        [ \_ -> near ( 7, 8, 9 ) (positionOf 1 set)
                        , \_ -> near ( 40, 0, 0 ) (positionOf 1 (Edit.apply inverse set |> Tuple.first))
                        , \_ -> near ( 0, 0, 0 ) (positionOf 0 set)
                        ]
                        ()
            ]
        , describe "Add and Remove"
            [ test "Add appends placements and returns the ids to remove" <|
                \_ ->
                    let
                        ( added, inverse ) =
                            Edit.apply (Add [ { part = "3004.dat", color = 1, matrix = Mat4.identity } ]) doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ 0, 1, 2, 3 ] (List.map .id (Document.placements added))
                        , \_ -> Expect.equal (Remove [ 3 ]) inverse
                        ]
                        ()
            , test "Remove then undo restores the file exactly, comments and all" <|
                \_ ->
                    let
                        ( removed, inverse ) =
                            Edit.apply (Remove [ 1 ]) doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ 0, 2 ] (List.map .id (Document.placements removed))
                        , \_ -> Expect.equal source (text (Edit.apply inverse removed |> Tuple.first))
                        ]
                        ()
            , test "removing several keeps their order when restored" <|
                \_ ->
                    let
                        ( removed, inverse ) =
                            Edit.apply (Remove [ 2, 0 ]) doc
                    in
                    Edit.apply inverse removed
                        |> Tuple.first
                        |> text
                        |> Expect.equal source
            , test "removing a placement that is not there does nothing" <|
                \_ ->
                    Edit.apply (Remove [ 99 ]) doc
                        |> Tuple.first
                        |> text
                        |> Expect.equal source
            , test "a restored placement keeps its id and new ones do not collide" <|
                \_ ->
                    let
                        ( removed, inverse ) =
                            Edit.apply (Remove [ 2 ]) doc

                        ( restored, _ ) =
                            Edit.apply inverse removed

                        ( added, _ ) =
                            Edit.apply (Add [ { part = "3004.dat", color = 1, matrix = Mat4.identity } ]) restored
                    in
                    Expect.equal [ 0, 1, 2, 3 ] (List.map .id (Document.placements added))
            ]
        , describe "Group"
            [ test "is one step and undoes in reverse order" <|
                \_ ->
                    let
                        ( done, inverse ) =
                            Edit.apply
                                (Group
                                    [ Translate [ 0 ] (vec3 10 0 0)
                                    , Remove [ 0 ]
                                    , Add [ { part = "3004.dat", color = 1, matrix = Mat4.identity } ]
                                    ]
                                )
                                doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ 1, 2, 3 ] (List.map .id (Document.placements done))
                        , \_ -> Expect.equal source (text (Edit.apply inverse done |> Tuple.first))
                        ]
                        ()
            ]
        , describe "History"
            [ test "undo and redo walk back and forth" <|
                \_ ->
                    let
                        ( d1, h1 ) =
                            Edit.record (Translate [ 0 ] (vec3 10 0 0)) doc Edit.emptyHistory

                        ( d2, h2 ) =
                            Edit.record (Translate [ 0 ] (vec3 0 5 0)) d1 h1

                        ( u1, h3 ) =
                            Edit.undo d2 h2

                        ( u2, h4 ) =
                            Edit.undo u1 h3

                        ( r1, _ ) =
                            Edit.redo u2 h4
                    in
                    Expect.all
                        [ \_ -> near ( 10, 5, 0 ) (positionOf 0 d2)
                        , \_ -> near ( 10, 0, 0 ) (positionOf 0 u1)
                        , \_ -> near ( 0, 0, 0 ) (positionOf 0 u2)
                        , \_ -> near ( 10, 0, 0 ) (positionOf 0 r1)
                        ]
                        ()
            , test "a new edit discards what could have been redone" <|
                \_ ->
                    let
                        ( d1, h1 ) =
                            Edit.record (Translate [ 0 ] (vec3 10 0 0)) doc Edit.emptyHistory

                        ( u, h2 ) =
                            Edit.undo d1 h1

                        ( _, h3 ) =
                            Edit.record (Translate [ 0 ] (vec3 0 0 3)) u h2
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Edit.canRedo h2)
                        , \_ -> Expect.equal False (Edit.canRedo h3)
                        ]
                        ()
            , test "undo and redo with nothing to do change nothing" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal ( doc, Edit.emptyHistory ) (Edit.undo doc Edit.emptyHistory)
                        , \_ -> Expect.equal ( doc, Edit.emptyHistory ) (Edit.redo doc Edit.emptyHistory)
                        ]
                        ()
            , test "pushApplied remembers an edit that already happened" <|
                \_ ->
                    let
                        ( moved, inverse ) =
                            Edit.apply (Translate [ 1 ] (vec3 20 0 0)) doc

                        history =
                            Edit.pushApplied inverse Edit.emptyHistory
                    in
                    Edit.undo moved history
                        |> Tuple.first
                        |> positionOf 1
                        |> near ( 40, 0, 0 )
            , test "only the most recent 200 steps are kept" <|
                \_ ->
                    List.range 1 250
                        |> List.foldl
                            (\_ ( d, h ) -> Edit.record (Translate [ 0 ] (vec3 1 0 0)) d h)
                            ( doc, Edit.emptyHistory )
                        |> Tuple.second
                        |> .undo
                        |> List.length
                        |> Expect.equal 200
            ]
        , describe "helpers"
            [ test "snap rounds to the nearest step" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.0001) 20 (Edit.snap 20 13)
                        , \_ -> Expect.within (Expect.Absolute 0.0001) 0 (Edit.snap 20 9)
                        , \_ -> Expect.within (Expect.Absolute 0.0001) -8 (Edit.snap 8 -5)
                        , \_ -> Expect.within (Expect.Absolute 0.0001) 3.3 (Edit.snap 0 3.3)
                        ]
                        ()
            , test "the selection centre is the mean of the positions" <|
                \_ ->
                    Edit.selectionCenter [ 0, 1 ] doc
                        |> Maybe.map (Vec3.toRecord >> (\v -> ( v.x, v.y, v.z )))
                        |> Expect.equal (Just ( 20, 0, 0 ))
            , test "an empty selection has no centre" <|
                \_ -> Expect.equal Nothing (Edit.selectionCenter [] doc)
            ]
        , describe "submodels"
            [ test "AddIn appends to the named submodel and undo removes them" <|
                \_ ->
                    let
                        groupedDoc =
                            Document.groupIntoSubmodel "sub.ldr" [ 0, 1 ] doc

                        newPart =
                            { part = "3004.dat", color = 1, matrix = Mat4.identity }

                        ( added, inverse ) =
                            Edit.apply (Edit.AddIn "sub.ldr" [ newPart ]) groupedDoc

                        ( undone, _ ) =
                            Edit.apply inverse added
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length (Document.placementsOf "sub.ldr" groupedDoc) + 1) (List.length (Document.placementsOf "sub.ldr" added))
                        , \_ -> Expect.equal (List.length (Document.placementsOf "sub.ldr" groupedDoc)) (List.length (Document.placementsOf "sub.ldr" undone))
                        ]
                        ()
            , test "GroupIntoSubmodel and undo round-trip" <|
                \_ ->
                    let
                        ( grouped, inverse ) =
                            Edit.apply (Edit.GroupIntoSubmodel "sub.ldr" [ 0, 1 ]) doc

                        ( restored, _ ) =
                            Edit.apply inverse grouped
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Document.submodelByName "sub.ldr" grouped /= Nothing)
                        , \_ -> Expect.equal (text doc) (text restored)
                        ]
                        ()
            , test "ExplodeSubmodel and undo round-trip" <|
                \_ ->
                    let
                        grouped =
                            Document.groupIntoSubmodel "sub.ldr" [ 0, 1 ] doc

                        submodelPlacementId =
                            grouped.nextId - 1

                        ( exploded, inverse ) =
                            Edit.apply (Edit.ExplodeSubmodel submodelPlacementId) grouped

                        ( restored, _ ) =
                            Edit.apply inverse exploded
                    in
                    Expect.all
                        [ \_ -> Expect.equal Nothing (Document.getPlacement submodelPlacementId exploded)
                        , \_ -> Expect.equal (text grouped) (text restored)
                        ]
                        ()
            , test "Remove and undo in a non-main submodel restores the placement in that submodel" <|
                \_ ->
                    let
                        grouped =
                            Document.groupIntoSubmodel "sub.ldr" [ 0, 1 ] doc

                        subParts =
                            Document.placementsOf "sub.ldr" grouped

                        targetId =
                            subParts |> List.head |> Maybe.map .id |> Maybe.withDefault 0

                        ( removed, inverse ) =
                            Edit.apply (Edit.Remove [ targetId ]) grouped

                        ( restored, _ ) =
                            Edit.apply inverse removed
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length subParts - 1) (List.length (Document.placementsOf "sub.ldr" removed))
                        , \_ -> Expect.equal (List.length subParts) (List.length (Document.placementsOf "sub.ldr" restored))
                        , \_ -> Expect.equal True (Document.getPlacement targetId restored /= Nothing)
                        ]
                        ()
            ]
        ]
