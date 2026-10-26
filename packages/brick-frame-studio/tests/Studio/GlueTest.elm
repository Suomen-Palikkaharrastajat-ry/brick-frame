module Studio.GlueTest exposing (suite)

{-| Tests for glued parts in Studio.Core and Studio.Edit: a glued part can be
selected but not moved, turned, raised, lowered, nudged or deleted, and gluing
is one undo step.
-}

import Expect
import LDraw.Document as Document
import LDraw.Writer as Writer
import Math.Matrix4 as Mat4
import Math.Vector3 exposing (vec3)
import Set
import Studio.Core as Core
import Studio.Edit as Edit exposing (Axis(..), Command(..))
import Studio.Shortcuts as StudioShortcuts exposing (TurnDirection(..))
import Test exposing (Test, describe, test)


{-| Placement 0 is glued; 1 and 2 are free, 40 LDU apart along X.
-}
boardText : String
boardText =
    String.join "\n"
        [ "0 !PALIKKA GLUED"
        , "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 4 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 4 80 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        ]


{-| A studio model holding `text`, with nothing selected.
-}
modelWith : String -> Core.Model
modelWith text =
    let
        config =
            { ldrawBase = "/ldraw"
            , ldrawFallbackBase = ""
            , defaultModelUrl = ""
            , initialHash = ""
            , supportsPointerEvents = True
            , document = Document.parse text
            , gridStep = Just 20
            , rotateStep = Just 90
            , snapDistance = Just 24.0
            , collisionMode = Nothing
            }

        ( model, _ ) =
            Core.init config
    in
    model


{-| The model with `ids` selected.
-}
select : List Int -> Core.Model -> Core.Model
select ids model =
    let
        ( selected, _, _ ) =
            Core.update (Core.Select (Set.fromList ids)) model
    in
    selected


{-| The document as text, for "nothing changed" comparisons.
-}
written : Core.Model -> String
written model =
    Writer.writeDocument model.document


{-| A placement's fingerprint (part, colour and position), for "did not move".
-}
fingerprintOf : Int -> Core.Model -> Maybe String
fingerprintOf id model =
    Document.getPlacement id model.document
        |> Maybe.map Document.fingerprint


{-| Ids of the placements left in the model, in file order.
-}
idsOf : Core.Model -> List Int
idsOf model =
    Core.getPlacements model |> List.map .id


nudgeRight : Core.Msg
nudgeRight =
    Core.NudgeSelection (StudioShortcuts.Nudge { direction = StudioShortcuts.Right, fine = False })


suite : Test
suite =
    describe "Studio glued parts"
        [ coreSuite
        , editSuite
        ]


coreSuite : Test
coreSuite =
    describe "Studio.Core glued parts"
        [ test "a glued part can be selected and reports as glued" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)
                in
                Expect.all
                    [ \m -> Expect.equal True (Core.selectionGlued m)
                    , \m -> Expect.equal True (Core.isGlued 0 m)
                    , \m -> Expect.equal False (Core.isGlued 1 m)
                    , \m -> Expect.equal 1 (Core.selectionCount m)
                    ]
                    model
        , test "rotating a glued-only selection changes nothing" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( rotated, _, _ ) =
                        Core.rotateSelection AxisY 1 model
                in
                Expect.equal (written model) (written rotated)
        , test "turning a glued-only selection changes nothing" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( turned, _, _ ) =
                        Core.turnSelected TurnLeft model
                in
                Expect.equal (written model) (written turned)
        , test "raising and lowering a glued-only selection change nothing" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( raised, _, _ ) =
                        Core.raiseSelected model

                    ( lowered, _, _ ) =
                        Core.lowerSelected model
                in
                Expect.all
                    [ \_ -> Expect.equal (written model) (written raised)
                    , \_ -> Expect.equal (written model) (written lowered)
                    ]
                    ()
        , test "nudging a glued-only selection changes nothing" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( nudged, _, _ ) =
                        Core.update nudgeRight model
                in
                Expect.equal (written model) (written nudged)
        , test "deleting a glued-only selection changes nothing and keeps the selection" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( deleted, _, _ ) =
                        Core.deleteSelection model
                in
                Expect.all
                    [ \_ -> Expect.equal (written model) (written deleted)
                    , \_ -> Expect.equal True (Core.selectionGlued deleted)
                    ]
                    ()
        , test "a mixed selection moves only the unglued parts" <|
            \_ ->
                let
                    model =
                        select [ 0, 1 ] (modelWith boardText)

                    ( nudged, _, _ ) =
                        Core.update nudgeRight model
                in
                Expect.all
                    [ \_ -> Expect.equal (fingerprintOf 0 model) (fingerprintOf 0 nudged)
                    , \_ -> Expect.notEqual (fingerprintOf 1 model) (fingerprintOf 1 nudged)
                    ]
                    ()
        , test "deleting a mixed selection removes only the unglued parts" <|
            \_ ->
                let
                    model =
                        select [ 0, 1 ] (modelWith boardText)

                    ( deleted, _, _ ) =
                        Core.deleteSelection model
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0, 2 ] (idsOf deleted)
                    , \_ -> Expect.equal True (Core.isGlued 0 deleted)
                    ]
                    ()
        , test "setGlued glues the selection as one undo step" <|
            \_ ->
                let
                    model =
                        select [ 1, 2 ] (modelWith boardText)

                    ( glued, _, _ ) =
                        Core.setGlued True model

                    ( unglued, _, _ ) =
                        Core.undo glued
                in
                Expect.all
                    [ \_ -> Expect.equal [ True, True, True ] [ Core.isGlued 0 glued, Core.isGlued 1 glued, Core.isGlued 2 glued ]
                    , \_ -> Expect.equal [ True, False, False ] [ Core.isGlued 0 unglued, Core.isGlued 1 unglued, Core.isGlued 2 unglued ]
                    , \_ -> Expect.equal False (Core.canUndo unglued)
                    ]
                    ()
        , test "setGlued False unglues the selected glued part, which then moves" <|
            \_ ->
                let
                    model =
                        select [ 0 ] (modelWith boardText)

                    ( unglued, _, _ ) =
                        Core.setGlued False model

                    ( nudged, _, _ ) =
                        Core.update nudgeRight unglued
                in
                Expect.all
                    [ \_ -> Expect.equal False (Core.isGlued 0 unglued)
                    , \_ -> Expect.notEqual (fingerprintOf 0 unglued) (fingerprintOf 0 nudged)
                    ]
                    ()
        , test "setGlueEditable False makes setGlued a no-op in both directions" <|
            \_ ->
                let
                    base =
                        Core.setGlueEditable False (select [ 1 ] (modelWith boardText))

                    ( gluedAttempt, _, gluedEvents ) =
                        Core.setGlued True base

                    ( unglueAttempt, _, unglueEvents ) =
                        Core.setGlued False (select [ 0 ] base)
                in
                Expect.all
                    [ \_ -> Expect.equal False (Core.isGlued 1 gluedAttempt)
                    , \_ -> Expect.equal [] gluedEvents
                    , \_ -> Expect.equal True (Core.isGlued 0 unglueAttempt)
                    , \_ -> Expect.equal [] unglueEvents
                    ]
                    ()
        , test "undo cannot unglue a part while glue is not editable" <|
            \_ ->
                let
                    ( glued, _, _ ) =
                        Core.setGlued True (select [ 1 ] (modelWith boardText))

                    locked =
                        Core.setGlueEditable False glued

                    ( undone, _, _ ) =
                        Core.undo locked
                in
                Expect.equal True (Core.isGlued 1 undone)
        , test "clearUnglued keeps the glued parts and drops the rest" <|
            \_ ->
                let
                    ( cleared, _, _ ) =
                        Core.clearUnglued (modelWith boardText)
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (idsOf cleared)
                    , \_ -> Expect.equal True (Core.isGlued 0 cleared)
                    ]
                    ()
        ]


editSuite : Test
editSuite =
    describe "Studio.Edit glued parts"
        [ test "removing a glued part and undoing the removal restores the glue" <|
            \_ ->
                let
                    doc =
                        Document.parse boardText

                    ( removed, inverse ) =
                        Edit.apply (Remove [ 0 ]) doc

                    ( restored, _ ) =
                        Edit.apply inverse removed
                in
                Expect.all
                    [ \_ -> Expect.equal (Writer.writeDocument doc) (Writer.writeDocument restored)
                    , \_ -> Expect.equal [ 0 ] (Set.toList (Document.gluedIds restored))
                    ]
                    ()
        , test "duplicating a glued part gives an unglued copy" <|
            \_ ->
                let
                    doc =
                        Document.parse boardText

                    copyOf =
                        Document.getPlacement 0 doc
                            |> Maybe.map (\p -> { part = p.part, color = p.color, matrix = Mat4.makeTranslate (vec3 0 0 40) })
                in
                case copyOf of
                    Just spec ->
                        let
                            ( copied, _ ) =
                                Edit.apply (Add [ spec ]) doc

                            newId =
                                List.maximum (List.map .id (Document.placements copied))
                                    |> Maybe.withDefault -1
                        in
                        Expect.all
                            [ \_ -> Expect.equal True (Document.isGlued 0 copied)
                            , \_ -> Expect.equal False (Document.isGlued newId copied)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "placement 0 is missing"
        , test "undoing a SetGlued step restores the previous glue" <|
            \_ ->
                let
                    doc =
                        Document.parse boardText

                    ( glued, inverse ) =
                        Edit.apply (SetGlued [ ( 1, True ) ]) doc

                    ( unglued, _ ) =
                        Edit.apply inverse glued
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0, 1 ] (Set.toList (Document.gluedIds glued))
                    , \_ -> Expect.equal (Writer.writeDocument doc) (Writer.writeDocument unglued)
                    ]
                    ()
        ]
