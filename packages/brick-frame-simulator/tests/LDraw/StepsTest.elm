module LDraw.StepsTest exposing (suite)

{-| Unit tests for `LDraw.Steps`: step breakdown, delta callouts,
indexing, auto-partitioning, and camera framing bounds.
-}

import Expect
import LDraw.Document as Document
import LDraw.Steps as Steps exposing (CalloutItem, Step)
import Math.Vector3 as Vec3
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "LDraw.Steps"
        [ describe "step breakdown and grouping"
            [ test "splits placements on 0 STEP directives" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "0 Test Model"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "1 4 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                , "0 STEP"
                                , "1 1 60 0 0 1 0 0 0 1 0 0 0 1 3004.dat"
                                ]

                        doc =
                            Document.parse ldr

                        steps =
                            Steps.fromDocument doc
                    in
                    Expect.all
                        [ \s -> Expect.equal 3 (Steps.stepCount s)
                        , \s -> Expect.equal [ 1, 2, 3 ] (List.map .index s)
                        , \s -> Expect.equal [ 1, 2, 1 ] (List.map (\st -> List.length st.placements) s)
                        , \s -> Expect.equal [ 1, 3, 4 ] (List.map (\st -> List.length st.accumulatedPlacements) s)
                        ]
                        steps
            , test "ignores leading, trailing, and consecutive 0 STEP directives" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "0 STEP"
                                , "0 STEP"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "0 STEP"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "0 STEP"
                                ]

                        doc =
                            Document.parse ldr

                        steps =
                            Steps.fromDocument doc
                    in
                    Expect.all
                        [ \s -> Expect.equal 2 (Steps.stepCount s)
                        , \s -> Expect.equal [ 1, 2 ] (List.map .index s)
                        , \s -> Expect.equal [ 1, 1 ] (List.map (\st -> List.length st.placements) s)
                        ]
                        steps
            , test "treats document without 0 STEP as a single step 1" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "0 No step markers"
                                , "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                ]

                        doc =
                            Document.parse ldr

                        steps =
                            Steps.fromDocument doc
                    in
                    Expect.all
                        [ \s -> Expect.equal 1 (Steps.stepCount s)
                        , \s -> Expect.equal (Just 1) (List.head (List.map .index s))
                        , \s -> Expect.equal [ 2 ] (List.map (\st -> List.length st.placements) s)
                        ]
                        steps
            , test "empty document produces empty step list" <|
                \_ ->
                    let
                        doc =
                            Document.parse "0 Just a comment\n"

                        steps =
                            Steps.fromDocument doc
                    in
                    Expect.equal [] steps
            , test "recognises 0 !STEP and lowercase variations" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 !step"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "0 STEP // comment"
                                , "1 1 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                ]

                        steps =
                            Steps.fromDocument (Document.parse ldr)
                    in
                    Expect.equal 3 (Steps.stepCount steps)
            , test "hasStepMarkers detects presence or absence" <|
                \_ ->
                    let
                        withSteps =
                            Document.parse "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n0 STEP\n"

                        withoutSteps =
                            Document.parse "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat\n"
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Steps.hasStepMarkers withSteps)
                        , \_ -> Expect.equal False (Steps.hasStepMarkers withoutSteps)
                        ]
                        ()
            ]
        , describe "delta part callout calculation"
            [ test "aggregates quantities for identical part and color" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 20 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 14 60 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "1 4 80 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                ]

                        doc =
                            Document.parse ldr

                        steps =
                            Steps.fromDocument doc

                        firstStep =
                            List.head steps
                    in
                    case firstStep of
                        Just step ->
                            let
                                expected =
                                    [ { part = "3001.dat", color = 4, count = 3 }
                                    , { part = "3001.dat", color = 14, count = 1 }
                                    , { part = "3002.dat", color = 4, count = 1 }
                                    ]
                            in
                            Expect.equal expected step.callout

                        Nothing ->
                            Expect.fail "Expected at least one step"
            , test "stepDeltaCallout matches step callout" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "1 2 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                ]

                        steps =
                            Steps.fromDocument (Document.parse ldr)
                    in
                    case Steps.stepAtIndex 2 steps of
                        Just step2 ->
                            Expect.equal [ { part = "3002.dat", color = 2, count = 1 } ] (Steps.stepDeltaCallout step2)

                        Nothing ->
                            Expect.fail "Step 2 not found"
            ]
        , describe "step indexing and navigation queries"
            [ test "stepAtIndex finds correct step" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "1 2 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "0 STEP"
                                , "1 3 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                ]

                        steps =
                            Steps.fromDocument (Document.parse ldr)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Just 1) (Maybe.map .index (Steps.stepAtIndex 1 steps))
                        , \_ -> Expect.equal (Just 2) (Maybe.map .index (Steps.stepAtIndex 2 steps))
                        , \_ -> Expect.equal (Just 3) (Maybe.map .index (Steps.stepAtIndex 3 steps))
                        , \_ -> Expect.equal Nothing (Steps.stepAtIndex 0 steps)
                        , \_ -> Expect.equal Nothing (Steps.stepAtIndex 4 steps)
                        ]
                        ()
            , test "documentAtStep produces document with accumulated placements up to index" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "0 Header comment"
                                , "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "1 2 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                , "0 STEP"
                                , "1 3 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                ]

                        doc =
                            Document.parse ldr

                        docStep1 =
                            Steps.documentAtStep doc 1

                        docStep2 =
                            Steps.documentAtStep doc 2
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (List.length (Document.placements docStep1))
                        , \_ -> Expect.equal 2 (List.length (Document.placements docStep2))
                        , \_ -> Expect.equal [ "3001.dat" ] (List.map .part (Document.placements docStep1))
                        , \_ -> Expect.equal [ "3001.dat", "3002.dat" ] (List.map .part (Document.placements docStep2))
                        ]
                        ()
            ]
        , describe "autoPartition and autoStepDocument"
            [ test "autoPartition chunks placements into steps of N" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "1 2 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                    , "1 3 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                    , "1 4 60 0 0 1 0 0 0 1 0 0 0 1 3004.dat"
                                    , "1 5 80 0 0 1 0 0 0 1 0 0 0 1 3005.dat"
                                    ]
                                )

                        steps =
                            Steps.autoPartition 2 (Document.placements doc)
                    in
                    Expect.all
                        [ \s -> Expect.equal 3 (Steps.stepCount s)
                        , \s -> Expect.equal [ 2, 2, 1 ] (List.map (\st -> List.length st.placements) s)
                        , \s -> Expect.equal [ 2, 4, 5 ] (List.map (\st -> List.length st.accumulatedPlacements) s)
                        ]
                        steps
            , test "autoStepDocument inserts 0 STEP directives into document" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 1 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "1 2 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                    , "1 3 40 0 0 1 0 0 0 1 0 0 0 1 3003.dat"
                                    ]
                                )

                        steppedDoc =
                            Steps.autoStepDocument 1 doc

                        steps =
                            Steps.fromDocument steppedDoc
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Steps.hasStepMarkers steppedDoc)
                        , \_ -> Expect.equal 3 (Steps.stepCount steps)
                        ]
                        ()
            ]
        , describe "camera framing bounds"
            [ test "stepBounds calculates center and radius" <|
                \_ ->
                    let
                        doc =
                            Document.parse
                                (String.join "\n"
                                    [ "1 16 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    , "1 16 100 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                    ]
                                )

                        bounds =
                            Steps.stepBounds (Document.placements doc)
                    in
                    case bounds of
                        Just b ->
                            Expect.all
                                [ \_ -> Expect.equal 50 (round (Vec3.getX b.center))
                                , \_ -> Expect.equal 0 (round (Vec3.getY b.center))
                                , \_ -> Expect.equal 0 (round (Vec3.getZ b.center))
                                , \_ -> Expect.greaterThan 50 b.radius
                                ]
                                ()

                        Nothing ->
                            Expect.fail "Expected valid bounds"
            , test "stepBounds on empty list returns Nothing" <|
                \_ ->
                    Expect.equal Nothing (Steps.stepBounds [])
            ]
        , describe "exportHtml"
            [ test "generates HTML with step cards and callout chips" <|
                \_ ->
                    let
                        ldr =
                            String.join "\n"
                                [ "1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
                                , "0 STEP"
                                , "1 14 20 0 0 1 0 0 0 1 0 0 0 1 3002.dat"
                                ]

                        steps =
                            Steps.fromDocument (Document.parse ldr)

                        html =
                            Steps.exportHtml "My Test Model" steps
                    in
                    Expect.all
                        [ \h -> Expect.equal True (String.contains "Building Instructions — My Test Model" h)
                        , \h -> Expect.equal True (String.contains "Step 1" h)
                        , \h -> Expect.equal True (String.contains "Step 2" h)
                        , \h -> Expect.equal True (String.contains "3001.dat" h)
                        , \h -> Expect.equal True (String.contains "3002.dat" h)
                        , \h -> Expect.equal True (String.contains "Red" h)
                        , \h -> Expect.equal True (String.contains "Yellow" h)
                        ]
                        html
            ]
        ]
