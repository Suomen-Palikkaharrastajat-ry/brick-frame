module PlayTest exposing (suite)

{-| Unit tests for the Brick Play application.
-}

import Expect
import Main exposing (Flags, Model, Msg(..), init, remainingCount, update)
import Set
import Studio.Core as Core
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Brick Play"
        [ test "initial inventory has 10 bricks available" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                Expect.equal (remainingCount model) 10
        , test "AddBrick decrements inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update AddBrick model0
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 9
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 1
                    ]
                    model1
        , test "Adding bricks stops at inventory limit of 10" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    model10 =
                        List.foldl (\_ m -> Tuple.first (update AddBrick m)) model0 (List.range 1 10)

                    model11 =
                        Tuple.first (update AddBrick model10)
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 0
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 10
                    ]
                    model11
        , test "Undo increments inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update AddBrick model0

                    ( modelUndone, _ ) =
                        update Undo model1
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelUndone
        , test "Delete restores inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update AddBrick model0

                    -- model1 has the newly added brick selected
                    ( modelDeleted, _ ) =
                        update Delete model1
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelDeleted
        , test "Reset restores inventory count to 10" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    model3 =
                        List.foldl (\_ m -> Tuple.first (update AddBrick m)) model0 (List.range 1 3)

                    ( modelReset, _ ) =
                        update Reset model3
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelReset
        , describe "Kit Initialization and Multi-item Inventory"
            [ test "initializes inventory from parts in hash" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:14:2,3002.dat:4:3" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 2 (List.length m.inventory)
                        , \m -> Expect.equal 5 (remainingCount m)
                        ]
                        model
            , test "AddPart decrements only that part's inventory and respects limit" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:14:2,3002.dat:4:3" }

                        ( model0, _ ) =
                            init flags

                        item1 =
                            { part = "3001.dat", color = 14, count = 2 }

                        ( model1, _ ) =
                            update (AddPart item1) model0

                        ( model2, _ ) =
                            update (AddPart item1) model1

                        ( model3, _ ) =
                            update (AddPart item1) model2
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (Main.remainingForItem model1 item1)
                        , \_ -> Expect.equal 0 (Main.remainingForItem model2 item1)
                        , \_ -> Expect.equal 0 (Main.remainingForItem model3 item1)
                        , \_ -> Expect.equal 2 (List.length (Core.getPlacements model3.studio))
                        ]
                        ()
            , test "initializes document when modelText is present in hash" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#model=1%2014%200%200%200%201%200%200%200%201%200%200%200%201%203001.dat&parts=3001.dat:14:5" }

                        ( model, _ ) =
                            init flags
                    in
                    Expect.all
                        [ \m -> Expect.equal 1 (List.length (Core.getPlacements m.studio))
                        , \m -> Expect.equal 4 (remainingCount m)
                        ]
                        model
            ]
        ]


testFlags : Flags
testFlags =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = ""
    , supportsPointerEvents = True
    }
