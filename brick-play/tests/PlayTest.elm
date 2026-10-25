module PlayTest exposing (suite)

{-| Unit tests for the Brick Play application.
-}

import Dict
import Expect
import Html.Attributes as Attr
import Main exposing (Flags, Model, Msg(..), helpRows, init, kitItemName, remainingCount, thumbnailRequests, update, view)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Set
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Viewport


suite : Test
suite =
    describe "Brick Play"
        [ menuTests
        , test "initial inventory has 10 bricks available" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                Expect.equal (remainingCount model) 10
        , test "adding a brick decrements inventory count" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0
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
                        List.foldl (\_ m -> Tuple.first (addFirst m)) model0 (List.range 1 10)

                    model11 =
                        Tuple.first (addFirst model10)
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
                        addFirst model0

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
                        addFirst model0

                    -- model1 has the newly added brick selected
                    ( modelDeleted, _ ) =
                        update Delete model1
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelDeleted
        , test "Clear restores inventory count to 10" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    model3 =
                        List.foldl (\_ m -> Tuple.first (addFirst m)) model0 (List.range 1 3)

                    ( modelCleared, _ ) =
                        update Clear model3
                in
                Expect.all
                    [ \m -> Expect.equal (remainingCount m) 10
                    , \m -> Expect.equal (List.length (Core.getPlacements m.studio)) 0
                    ]
                    modelCleared
        , test "no axis gizmo: arrows and rings are off from the start" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                Expect.equal Core.NoGizmo model.studio.gizmoStyle
        , test "the action bar edits the selected brick, one undo step each" <|
            \_ ->
                let
                    ( added, _ ) =
                        init testFlags |> Tuple.first |> addFirst

                    matrixOf m =
                        Core.getPlacements m.studio |> List.map .matrix

                    run msg =
                        update msg added |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnLeft)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TurnRight)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run (TurnSelection TipForward)))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run RaiseSelection))
                    , \_ -> Expect.notEqual (matrixOf added) (matrixOf (run LowerSelection))
                    , \_ -> Expect.equal 0 (List.length (Core.getPlacements (run Delete).studio))
                    , \_ -> Expect.equal True (Core.canUndo (run RaiseSelection).studio)
                    , \_ -> Expect.equal (matrixOf added) (matrixOf (run RaiseSelection |> update Undo |> Tuple.first))
                    ]
                    ()
        , test "ToggleHelp opens and closes the help and sets the studio help flag" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update ToggleHelp model0

                    ( model2, _ ) =
                        update ToggleHelp model1
                in
                Expect.all
                    [ \_ -> Expect.equal ( False, False ) ( model0.helpOpen, model0.studio.helpVisible )
                    , \_ -> Expect.equal ( True, True ) ( model1.helpOpen, model1.studio.helpVisible )
                    , \_ -> Expect.equal ( False, False ) ( model2.helpOpen, model2.studio.helpVisible )
                    ]
                    ()
        , test "WindowResized reaches the viewport so picking uses the real canvas size" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update (WindowResized 705 813) model0
                in
                Expect.equal
                    ( ( 705, 813 ), ( 705, 813 ) )
                    ( ( model1.studio.viewport.width, model1.studio.viewport.height )
                    , ( model1.windowWidth, model1.windowHeight )
                    )
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
            , test "pieces added from the kit never overlap; the first lands in the middle" <|
                \_ ->
                    let
                        flags =
                            { testFlags | initialHash = "#parts=3001.dat:19:3,3003.dat:4:2" }

                        ( model0, _ ) =
                            init flags

                        items =
                            [ { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3001.dat", color = 19, count = 3 }
                            , { part = "3003.dat", color = 4, count = 2 }
                            , { part = "3003.dat", color = 4, count = 2 }
                            ]

                        final =
                            List.foldl (\item m -> Tuple.first (update (AddPart item) m)) model0 items

                        placed =
                            Core.getPlacements final.studio
                                |> List.map (\p -> ( p.part, Mat4.transform p.matrix (vec3 0 0 0) ))

                        -- Plan-view half sizes: 3001 is 2 x 4 studs, 3003 2 x 2.
                        half part =
                            if part == "3001.dat" then
                                ( 40, 20 )

                            else
                                ( 20, 20 )

                        apart ( pa, a ) ( pb, b ) =
                            let
                                ( ax, az ) =
                                    half pa

                                ( bx, bz ) =
                                    half pb
                            in
                            abs (Vec3.getX a - Vec3.getX b) >= ax + bx - 0.5 || abs (Vec3.getZ a - Vec3.getZ b) >= az + bz - 0.5

                        pairs =
                            List.indexedMap (\i a -> List.indexedMap (\j b -> ( i < j, a, b )) placed) placed
                                |> List.concat
                                |> List.filter (\( keep, _, _ ) -> keep)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 5 (List.length placed)
                        , \_ -> Expect.equal True (List.all (\( _, a, b ) -> apart a b) pairs)
                        , \_ ->
                            List.head placed
                                |> Maybe.map (\( _, p ) -> ( Vec3.getX p, Vec3.getZ p ))
                                |> Expect.equal (Just ( 0, 0 ))
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
        , thumbnailTests
        , mouseTests
        ]


mouseTests : Test
mouseTests =
    let
        ( model0, _ ) =
            init testFlags

        send msg model =
            update (StudioMsg (Core.ViewportMsg msg)) model |> Tuple.first

        camera m =
            m.studio.viewport.camera

        rows =
            List.concatMap Tuple.second helpRows
    in
    describe "Mouse and ground"
        [ test "uses DragPartsOrPan with one-finger orbit on touch" <|
            \_ -> Expect.equal (Core.DragPartsOrPan Core.OneFingerOrbits) model0.studio.dragMode
        , test "left-drag on empty canvas pans: the target moves, the angle does not" <|
            \_ ->
                let
                    pressed =
                        send (Viewport.MouseButtonDown 100 100 0 False False) model0

                    moved =
                        send (Viewport.MouseMove 180 140) pressed
                in
                Expect.all
                    [ \_ -> Expect.notEqual (camera pressed).target (camera moved).target
                    , \_ -> Expect.within (Expect.Absolute 1.0e-9) (camera pressed).azimuth (camera moved).azimuth
                    ]
                    ()
        , test "right-drag orbits" <|
            \_ ->
                let
                    pressed =
                        send (Viewport.MouseButtonDown 100 100 2 False False) model0

                    moved =
                        send (Viewport.MouseMove 180 140) pressed
                in
                Expect.greaterThan 1.0e-6 (abs ((camera pressed).azimuth - (camera moved).azimuth))
        , test "help lists drag-empty-space pan and right-drag orbit" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal True (List.member ( "Drag empty space", "Pan" ) rows)
                    , \_ -> Expect.equal True (List.member ( "Right-drag", "Orbit" ) rows)
                    ]
                    ()
        , test "a newly added brick rests on the ground (origin Y = -24)" <|
            \_ ->
                addFirst model0
                    |> Tuple.first
                    |> .studio
                    |> Core.getPlacements
                    |> List.head
                    |> Maybe.map (\p -> Vec3.getY (Mat4.transform p.matrix (vec3 0 0 0)))
                    |> Expect.equal (Just -24)
        ]


kitFlags : Flags
kitFlags =
    { testFlags | initialHash = "#parts=3001.dat:14:4,3004.dat:4:4,3001.dat:14:1,3001.dat:1:2" }


thumbnailTests : Test
thumbnailTests =
    describe "Kit thumbnails"
        [ test "init asks for one thumbnail per distinct part and colour, in kit order" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                in
                thumbnailRequests model.inventory model.thumbnails
                    |> List.map (\r -> ( r.file, r.color ))
                    |> Expect.equal [ ( "3001.dat", 14 ), ( "3004.dat", 4 ), ( "3001.dat", 1 ) ]
        , test "requests carry the part colour as rgb from the colour table" <|
            \_ ->
                let
                    ( model, _ ) =
                        init testFlags
                in
                case thumbnailRequests model.inventory model.thumbnails of
                    [ request ] ->
                        Expect.all
                            [ \r -> Expect.equal 3 (List.length r.rgb)
                            , \r -> Expect.notEqual [ 0, 0, 0 ] r.rgb
                            ]
                            request

                    other ->
                        Expect.fail ("expected one request, got " ++ String.fromInt (List.length other))
        , test "thumbnailLoaded stores the image under file and colour" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                            |> Tuple.first
                            |> update (ThumbnailLoaded { file = "3004.dat", color = 4, url = "data:image/png;base64,AA", title = "Brick  1 x  2" })
                in
                Expect.all
                    [ \m -> Expect.equal 1 (Dict.size m.thumbnails)
                    , \m ->
                        thumbnailRequests m.inventory m.thumbnails
                            |> List.map .file
                            |> Expect.equal [ "3001.dat", "3001.dat" ]
                    ]
                    model
        , test "a loaded title names the card; the part number stands in before" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init kitFlags

                    item =
                        { part = "3004.dat", color = 4, count = 4 }

                    ( model1, _ ) =
                        update (ThumbnailLoaded { file = "3004.dat", color = 4, url = "data:,", title = "Brick 1 x 2" }) model0
                in
                Expect.equal ( "3004", "Brick 1 x 2" ) ( kitItemName model0 item, kitItemName model1 item )
        , test "a thumbnail in another colour does not name the item" <|
            \_ ->
                let
                    ( model, _ ) =
                        init kitFlags
                            |> Tuple.first
                            |> update (ThumbnailLoaded { file = "3001.dat", color = 1, url = "data:,", title = "Brick 2 x 4" })
                in
                Expect.equal "3001" (kitItemName model { part = "3001.dat", color = 14, count = 4 })
        ]


testFlags : Flags
testFlags =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = ""
    , supportsPointerEvents = True
    }


{-| Add the first kit item to the studio, as a tap on the dock does.
-}
addFirst : Model -> ( Model, Cmd Msg )
addFirst model =
    case model.inventory of
        first :: _ ->
            update (AddPart first) model

        [] ->
            ( model, Cmd.none )


{-| The top-right menu: it opens and closes, Escape closes it before it
deselects anything, and its items send the messages their buttons used to.
-}
menuTests : Test
menuTests =
    describe "Top-right menu"
        [ test "the menu opens, closes, and pauses the editing keys while open" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        update ToggleMenu model0

                    ( model2, _ ) =
                        update CloseMenu model1
                in
                Expect.equal
                    [ ( False, False ), ( True, True ), ( False, False ) ]
                    [ ( model0.menuOpen, model0.studio.helpVisible )
                    , ( model1.menuOpen, model1.studio.helpVisible )
                    , ( model2.menuOpen, model2.studio.helpVisible )
                    ]
        , test "Escape closes an open menu and keeps the selection" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    selected =
                        Tuple.first (addFirst model0)
                            |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))

                    ( menuOpen, _ ) =
                        update ToggleMenu selected

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") menuOpen
                in
                Expect.equal [ True, False, True ]
                    [ Core.hasSelection selected.studio
                    , afterEscape.menuOpen
                    , Core.hasSelection afterEscape.studio
                    ]
        , test "Escape with the menu closed still deselects" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    selected =
                        Tuple.first (addFirst model0)
                            |> (\m -> Tuple.first (update (StudioMsg (Core.SelectOnly 0)) m))

                    ( afterEscape, _ ) =
                        update (pressKey "Escape") selected
                in
                Expect.equal False (Core.hasSelection afterEscape.studio)
        , test "Undo from the menu undoes the last add, as the toolbar button did" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( model1, _ ) =
                        addFirst model0

                    ( model2, _ ) =
                        update (MenuPicked Undo) model1
                in
                Expect.equal [ 0, 10 ]
                    [ List.length (Core.getPlacements model2.studio), remainingCount model2 ]
        , test "the visible ? button toggles the shortcut list, as the ? key does" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags
                in
                Query.fromHtml (view model0)
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Keyboard shortcuts (?)") ]
                    |> Event.simulate Event.click
                    |> Event.expect ToggleHelp
        , test "the open menu lists its five actions with their shortcuts" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags

                    ( opened, _ ) =
                        update ToggleMenu model0
                in
                Query.fromHtml (view opened)
                    |> Expect.all
                        [ Query.findAll [ Selector.attribute (Attr.attribute "role" "menuitem") ] >> Query.count (Expect.equal 5)
                        , Query.has [ Selector.text "Ctrl+Shift+Z" ]
                        ]
        , test "the kit cards show their count on the image and name it in the accessible label" <|
            \_ ->
                let
                    ( model0, _ ) =
                        init testFlags
                in
                Query.fromHtml (view model0)
                    |> Expect.all
                        [ Query.has [ Selector.text "10/10" ]
                        , Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add 3001 (Yellow), 10 of 10 left") ]
                        ]
        ]


{-| A key press delivered to the studio the way the browser's keydown reaches it.
-}
pressKey : String -> Msg
pressKey key =
    StudioMsg
        (Core.ViewportMsg
            (Viewport.KeyPressed
                { key = key
                , ctrl = False
                , meta = False
                , alt = False
                , shift = False
                , repeat = False
                , targetTag = "BODY"
                , targetEditable = False
                , targetRole = ""
                }
            )
        )
