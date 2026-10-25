module Studio.CoreTest exposing (suite)

{-| Tests for the Studio.Core TEA component.
-}

import Expect
import LDraw.Document as Document
import Math.Vector3 as Vec3 exposing (vec3)
import Set
import Studio.Core as Core
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Shortcuts as StudioShortcuts exposing (Tool(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.Core"
        [ test "initializes with empty document and default state" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model, _ ) =
                        Core.init config
                in
                Expect.all
                    [ \m -> Expect.equal (Core.getPlacements m) []
                    , \m -> Expect.equal (Core.hasSelection m) False
                    , \m -> Expect.equal (Core.selectionCount m) 0
                    , \m -> Expect.equal (Core.canUndo m) False
                    , \m -> Expect.equal (Core.canRedo m) False
                    , \m -> Expect.equal m.tool MoveTool
                    ]
                    model
        , test "addPart adds placement and records undo" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, events1 ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0
                in
                Expect.all
                    [ \m -> Expect.equal (List.length (Core.getPlacements m)) 1
                    , \m -> Expect.equal (Core.hasSelection m) True
                    , \m -> Expect.equal (Core.canUndo m) True
                    , \_ -> Expect.equal (List.any isDocChanged events1) True
                    ]
                    model1
        , test "undo and redo step through edits" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( modelUndone, _, _ ) =
                        Core.undo model1

                    ( modelRedone, _, _ ) =
                        Core.redo modelUndone
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length (Core.getPlacements modelUndone)) 0
                    , \_ -> Expect.equal (Core.canRedo modelUndone) True
                    , \_ -> Expect.equal (List.length (Core.getPlacements modelRedone)) 1
                    ]
                    ()
        , test "deleteSelection removes part and supports undo" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    ( modelDeleted, _, _ ) =
                        Core.deleteSelection model1

                    ( modelRestored, _, _ ) =
                        Core.undo modelDeleted
                in
                Expect.all
                    [ \_ -> Expect.equal (List.length (Core.getPlacements modelDeleted)) 0
                    , \_ -> Expect.equal (List.length (Core.getPlacements modelRestored)) 1
                    ]
                    ()
        , test "clearSelection and selectAll work as expected" <|
            \_ ->
                let
                    config =
                        { ldrawBase = "/ldraw"
                        , ldrawFallbackBase = ""
                        , defaultModelUrl = ""
                        , initialHash = ""
                        , supportsPointerEvents = True
                        , document = Document.empty
                        , gridStep = Just 20
                        , rotateStep = Just 90
                        , snapDistance = Just 24.0
                        , collisionMode = Nothing
                        }

                    ( model0, _ ) =
                        Core.init config

                    ( model1, _, _ ) =
                        Core.addPart "3001.dat" 14 (vec3 0 0 0) model0

                    cleared =
                        Core.clearSelection model1

                    selectedAll =
                        Core.selectAll cleared
                in
                Expect.all
                    [ \_ -> Expect.equal (Core.hasSelection cleared) False
                    , \_ -> Expect.equal (Core.selectionCount selectedAll) 1
                    ]
                    ()
        ]


isDocChanged : Core.Event -> Bool
isDocChanged ev =
    case ev of
        Core.DocumentChanged _ ->
            True

        _ ->
            False
