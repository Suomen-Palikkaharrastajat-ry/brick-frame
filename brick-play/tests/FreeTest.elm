module FreeTest exposing (suite)

{-| Tests for free building and puzzles in Brick Play: which mode a hash picks,
the free dock (colour row, tabs, search, unlimited cards), gluing with the Glue
button and `L`, the locks of a puzzle, and the two share links.
-}

import Expect
import Free.Catalog as FreeCatalog
import Html.Attributes as Attr
import LDraw.Document as Document
import LDraw.Writer as Writer
import Link
import Main exposing (Flags, Model, Msg(..), helpRows, init, remainingCount, remainingForItem, shareHash, update, view)
import Set
import Studio.Core as Core
import Studio.PartIndex as PartIndex
import Studio.Shortcuts exposing (TurnDirection(..))
import Studio.UrlPayload as UrlPayload
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url
import Viewport


suite : Test
suite =
    describe "Brick Play free building and puzzles"
        [ modeTests
        , freeDockTests
        , glueTests
        , puzzleLockTests
        , shareTests
        , helpTests
        ]


{-| Flags for a hash, which may start with `#`.
-}
flagsFor : String -> Flags
flagsFor hash =
    { ldrawBase = "/ldraw"
    , ldrawFallbackBase = ""
    , initialHash = hash
    , supportsPointerEvents = True
    }


{-| The model opened from a hash.
-}
open : String -> Model
open hash =
    Tuple.first (init (flagsFor hash))


{-| The model with one more free part added (and selected, as a dock tap does).
-}
addFree : String -> Int -> Model -> Model
addFree file colour model =
    Tuple.first (update (AddFreePart { file = file, color = colour, name = file }) model)


{-| The model after a key press, delivered the way the browser's keydown reaches it.
-}
press : String -> Model -> Model
press key model =
    Tuple.first
        (update
            (StudioMsg
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
            )
            model
        )


{-| Ids of the placements, in file order.
-}
placementIds : Model -> List Int
placementIds model =
    Core.getPlacements model.studio |> List.map .id


{-| The glued placement ids.
-}
gluedIds : Model -> List Int
gluedIds model =
    List.filter (\id -> Core.isGlued id model.studio) (placementIds model)


{-| A board with placement 0 glued and 1 free, both 3001 yellow bricks.
-}
gluedBoard : String
gluedBoard =
    String.join "\n"
        [ "0 !PALIKKA GLUED"
        , "1 14 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        , "1 14 40 0 0 1 0 0 0 1 0 0 0 1 3001.dat"
        ]


{-| A free build: a red 2 x 4 glued, three more red 2 x 4s and two yellow ones.
-}
freeBuild : Model
freeBuild =
    open "#free=1"
        |> addFree "3001.dat" 4
        |> update ToggleGlue
        |> Tuple.first
        |> addFree "3001.dat" 4
        |> addFree "3001.dat" 4
        |> addFree "3001.dat" 4
        |> addFree "3001.dat" 14
        |> addFree "3001.dat" 14


{-| A catalogue for the search stub: a 2 x 4, a 12 x 4 that must not match "2 x 4",
and a plate.
-}
catalogue : PartIndex.PartIndex
catalogue =
    { parts =
        [ PartIndex.makePart "3001.dat" "Brick 2 x 4" "Brick" ""
        , PartIndex.makePart "4000.dat" "Brick 12 x 4" "Brick" ""
        , PartIndex.makePart "3024.dat" "Plate 1 x 1" "Plate" ""
        ]
    , categories = []
    }


modeTests : Test
modeTests =
    describe "The mode the hash picks"
        [ test "no hash is free building, with an empty board and no kit" <|
            \_ ->
                let
                    model =
                        open ""
                in
                Expect.all
                    [ \m -> Expect.equal True m.free
                    , \m -> Expect.equal 0 (List.length (Core.getPlacements m.studio))
                    , \m -> Expect.equal 0 (remainingCount m)
                    ]
                    model
        , test "#free=1 is free building, and keeps every part of its board" <|
            \_ ->
                let
                    model =
                        open ("#free=1&model=" ++ Url.percentEncode gluedBoard)
                in
                Expect.all
                    [ \m -> Expect.equal True m.free
                    , \m -> Expect.equal 2 (List.length (Core.getPlacements m.studio))
                    , \m -> Expect.equal [ 0 ] (gluedIds m)
                    ]
                    model
        , test "a hash with a kit and no free=1 is a puzzle, counted from its kit" <|
            \_ ->
                let
                    model =
                        open "#parts=3001.dat:14:2"
                in
                Expect.all
                    [ \m -> Expect.equal False m.free
                    , \m -> Expect.equal 2 (remainingCount m)
                    ]
                    model
        , test "a bare model link is a puzzle with the default kit of 10" <|
            \_ ->
                let
                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard)
                in
                -- The default kit is 10 yellow bricks; the board's unglued one is counted.
                Expect.all
                    [ \m -> Expect.equal False m.free
                    , \m -> Expect.equal 9 (remainingCount m)
                    ]
                    model
        ]


freeDockTests : Test
freeDockTests =
    describe "The free dock"
        [ test "a card is unlimited: tapping it again adds another part" <|
            \_ ->
                let
                    model =
                        open ""
                            |> addFree "3001.dat" 14
                            |> addFree "3001.dat" 14
                            |> addFree "3001.dat" 14
                            |> addFree "3001.dat" 14
                            |> addFree "3001.dat" 14
                            |> addFree "3001.dat" 14
                in
                Expect.equal 6 (List.length (Core.getPlacements model.studio))
        , test "the Bricks tab is first, with a 2 x 4 card that adds it in the paint colour" <|
            \_ ->
                Query.fromHtml (view (open ""))
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Add Brick 2 x 4 (Yellow)") ]
                    |> Event.simulate Event.click
                    |> Event.expect (AddFreePart { file = "3001.dat", color = 14, name = "Brick 2 x 4" })
        , test "the colour row sets the colour of the next part" <|
            \_ ->
                let
                    model =
                        Tuple.first (update (SetPaintColour 4) (open ""))
                in
                Query.fromHtml (view model)
                    |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Add Brick 2 x 4 (Red)") ]
                    |> Event.simulate Event.click
                    |> Event.expect (AddFreePart { file = "3001.dat", color = 4, name = "Brick 2 x 4" })
        , test "exactly one colour swatch is pressed, the paint colour" <|
            \_ ->
                Query.fromHtml (view (open ""))
                    |> Query.findAll [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    |> Query.count (Expect.equal 1)
        , test "the Plates tab lists plates" <|
            \_ ->
                let
                    model =
                        Tuple.first (update (SetFreeTab FreeCatalog.PlatesTab) (open ""))
                in
                Query.fromHtml (view model)
                    |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add Plate 1 x 1 (Yellow)") ]
        , test "the four tabs are Bricks, Plates, Slopes & tiles and Technic" <|
            \_ ->
                Query.fromHtml (view (open ""))
                    |> Query.findAll [ Selector.attribute (Attr.attribute "role" "tab") ]
                    |> Query.count (Expect.equal 4)
        , test "a search waits for the part library while it loads" <|
            \_ ->
                let
                    model =
                        Tuple.first (update (SearchInput "2 x 4") (open ""))
                in
                Query.fromHtml (view model)
                    |> Query.has [ Selector.text "Loading the part library…" ]
        , test "a search over the loaded library lists its matches, and only them" <|
            \_ ->
                let
                    model =
                        open ""
                            |> update (SearchLoaded (Ok catalogue))
                            |> Tuple.first
                            |> update (SearchInput "2 x 4")
                            |> Tuple.first
                in
                Expect.all
                    [ \_ ->
                        Query.fromHtml (view model)
                            |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add Brick 2 x 4 (Yellow)") ]
                    , \_ ->
                        Query.fromHtml (view model)
                            |> Query.hasNot [ Selector.attribute (Attr.attribute "aria-label" "Add Brick 12 x 4 (Yellow)") ]
                    ]
                    ()
        , test "a search with no match says so" <|
            \_ ->
                let
                    model =
                        open ""
                            |> update (SearchLoaded (Ok catalogue))
                            |> Tuple.first
                            |> update (SearchInput "zebra")
                            |> Tuple.first
                in
                Query.fromHtml (view model)
                    |> Query.has [ Selector.text "No parts match \"zebra\"." ]
        , test "a blank search keeps the tab's cards" <|
            \_ ->
                let
                    model =
                        open ""
                            |> update (SearchLoaded (Ok catalogue))
                            |> Tuple.first
                            |> update (SearchInput "   ")
                            |> Tuple.first
                in
                Query.fromHtml (view model)
                    |> Query.has [ Selector.attribute (Attr.attribute "aria-label" "Add Brick 1 x 1 (Yellow)") ]
        ]


glueTests : Test
glueTests =
    describe "Glue"
        [ test "the Glue button glues the selection, and a second press unglues it" <|
            \_ ->
                let
                    glued =
                        addFree "3001.dat" 14 (open "")
                            |> update ToggleGlue
                            |> Tuple.first

                    unglued =
                        update ToggleGlue glued |> Tuple.first
                in
                Expect.equal ( [ True, False ], True, False )
                    ( [ Core.selectionGlued glued.studio, Core.selectionGlued unglued.studio ]
                    , List.all (\id -> Core.isGlued id glued.studio) (placementIds glued)
                    , List.any (\id -> Core.isGlued id unglued.studio) (placementIds unglued)
                    )
        , test "gluing is one undo step" <|
            \_ ->
                let
                    glued =
                        addFree "3001.dat" 14 (open "")
                            |> update ToggleGlue
                            |> Tuple.first

                    undone =
                        update Undo glued |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (gluedIds glued)
                    , \_ -> Expect.equal [] (gluedIds undone)
                    , \_ -> Expect.equal 1 (List.length (placementIds undone))
                    ]
                    ()
        , test "L glues and unglues the selection, as the Glue button does" <|
            \_ ->
                let
                    glued =
                        addFree "3001.dat" 14 (open "") |> press "l"

                    unglued =
                        press "l" glued
                in
                Expect.equal ( [ 0 ], [] ) ( gluedIds glued, gluedIds unglued )
        , test "the action bar says Glued for a glued selection, and its move buttons are off" <|
            \_ ->
                let
                    model =
                        addFree "3001.dat" 14 (open "") |> press "l"

                    view_ =
                        Query.fromHtml (view model)
                in
                Expect.all
                    [ \_ -> view_ |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Glued") ] |> Query.has [ Selector.attribute (Attr.attribute "aria-pressed" "true") ]
                    , \_ -> view_ |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Turn left") ] |> Query.has [ Selector.disabled True ]
                    , \_ -> view_ |> Query.find [ Selector.attribute (Attr.attribute "aria-label" "Delete") ] |> Query.has [ Selector.disabled True ]
                    ]
                    ()
        , test "a glued part does not turn with the action bar or the keys" <|
            \_ ->
                let
                    glued =
                        addFree "3001.dat" 14 (open "") |> press "l"

                    turned =
                        update (TurnSelection TurnLeft) glued |> Tuple.first

                    nudged =
                        press "ArrowLeft" glued

                    before =
                        Writer.writeDocument glued.studio.document
                in
                Expect.all
                    [ \_ -> Expect.equal before (Writer.writeDocument turned.studio.document)
                    , \_ -> Expect.equal before (Writer.writeDocument nudged.studio.document)
                    ]
                    ()
        , test "a glued part stays where it is when a new part is added beside it" <|
            \_ ->
                let
                    model =
                        open "#free=1"
                            |> addFree "3001.dat" 14
                            |> press "l"
                            |> addFree "3001.dat" 14
                in
                Expect.equal [ 0 ] (gluedIds model)
        ]


puzzleLockTests : Test
puzzleLockTests =
    describe "Puzzles lock their glued parts"
        [ test "a puzzle has no Glue button and L does nothing" <|
            \_ ->
                let
                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                            |> press "l"
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (gluedIds model)
                    , \_ -> Query.fromHtml (view model) |> Query.hasNot [ Selector.attribute (Attr.attribute "aria-label" "Glue") ]
                    , \_ -> Query.fromHtml (view model) |> Query.hasNot [ Selector.attribute (Attr.attribute "aria-label" "Glued") ]
                    ]
                    ()
        , test "a glued part's colour does not change in a puzzle" <|
            \_ ->
                let
                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                            |> update (StudioMsg (Core.Select (Set.fromList [ 0 ])))
                            |> Tuple.first
                            |> update (StudioMsg (Core.SetColorSelection 1))
                            |> Tuple.first
                in
                Expect.equal [ 14, 14 ] (List.map .color (Core.getPlacements model.studio))
        , test "a glued part's colour does not change in free building either" <|
            \_ ->
                let
                    model =
                        open ("#free=1&model=" ++ Url.percentEncode gluedBoard)
                            |> update (StudioMsg (Core.Select (Set.fromList [ 0 ])))
                            |> Tuple.first
                            |> update (StudioMsg (Core.SetColorSelection 1))
                            |> Tuple.first
                in
                Expect.equal [ 14, 14 ] (List.map .color (Core.getPlacements model.studio))
        , test "an unglued part's colour still changes" <|
            \_ ->
                let
                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                            |> update (StudioMsg (Core.Select (Set.fromList [ 1 ])))
                            |> Tuple.first
                            |> update (StudioMsg (Core.SetColorSelection 1))
                            |> Tuple.first
                in
                Expect.equal [ 14, 1 ] (List.map .color (Core.getPlacements model.studio))
        ]


shareTests : Test
shareTests =
    describe "Share links"
        [ test "a free build shares a puzzle: glued parts stay, the rest are counts grouped and summed" <|
            \_ ->
                let
                    payload =
                        UrlPayload.decodeHash (shareHash freeBuild)

                    shared =
                        Document.parse (Maybe.withDefault "" payload.modelText)
                in
                Expect.all
                    [ \_ -> Expect.equal False payload.free
                    , \_ -> Expect.equal [ { part = "3001.dat", color = 4, count = 3 }, { part = "3001.dat", color = 14, count = 2 } ] payload.parts
                    , \_ -> Expect.equal 1 (List.length (Document.placements shared))
                    , \_ -> Expect.equal 1 (Set.size (Document.gluedIds shared))
                    ]
                    ()
        , test "the shared free build opens as a puzzle: glued part locked, kit counted without it" <|
            \_ ->
                let
                    puzzle =
                        open (shareHash freeBuild)
                in
                Expect.all
                    [ \m -> Expect.equal False m.free
                    , \m -> Expect.equal 3 (remainingForItem m { part = "3001.dat", color = 4, count = 3 })
                    , \m -> Expect.equal 2 (remainingForItem m { part = "3001.dat", color = 14, count = 2 })
                    , \m -> Expect.equal 1 (List.length (gluedIds m))
                    ]
                    puzzle
        , test "a puzzle share keeps the whole state: glued, placed and the remaining counts" <|
            \_ ->
                let
                    kit =
                        { part = "3001.dat", color = 14, count = 5 }

                    board =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                            |> update (AddPart kit)
                            |> Tuple.first

                    reopened =
                        open (shareHash board)

                    summary m =
                        ( remainingForItem m kit, List.length (placementIds m), gluedIds m )
                in
                Expect.equal (summary board) (summary reopened)
        , test "a puzzle's glued parts do not count against its kit" <|
            \_ ->
                let
                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                in
                Expect.equal 4 (remainingCount model)
        , test "#free=1 reload keeps free building and every part, glued ones too" <|
            \_ ->
                let
                    reopened =
                        open (Link.freeHash freeBuild.studio.document)
                in
                Expect.all
                    [ \m -> Expect.equal True m.free
                    , \m -> Expect.equal 6 (List.length (placementIds m))
                    , \m -> Expect.equal [ 0 ] (gluedIds m)
                    ]
                    reopened
        , test "Clear in free building removes everything, glued parts included" <|
            \_ ->
                let
                    cleared =
                        update Clear freeBuild |> Tuple.first
                in
                Expect.equal 0 (List.length (placementIds cleared))
        , test "Clear in a puzzle keeps the glued parts and puts the rest back in the kit" <|
            \_ ->
                let
                    kit =
                        { part = "3001.dat", color = 14, count = 5 }

                    model =
                        open ("#model=" ++ Url.percentEncode gluedBoard ++ "&parts=3001.dat:14:5")
                            |> update (AddPart kit)
                            |> Tuple.first
                            |> update (AddPart kit)
                            |> Tuple.first

                    cleared =
                        update Clear model |> Tuple.first
                in
                Expect.all
                    [ \_ -> Expect.equal [ 0 ] (gluedIds cleared)
                    , \_ -> Expect.equal 1 (List.length (placementIds cleared))
                    , \_ -> Expect.equal 5 (remainingForItem cleared kit)
                    ]
                    ()
        ]


helpTests : Test
helpTests =
    let
        rowKeys model =
            helpRows model
                |> List.concatMap Tuple.second
                |> List.map Tuple.first
    in
    describe "The shortcut list"
        [ test "free building lists L for Glue" <|
            \_ ->
                List.member "L  (action bar: Glue)" (rowKeys (open ""))
                    |> Expect.equal True
        , test "a puzzle does not list L" <|
            \_ ->
                List.member "L  (action bar: Glue)" (rowKeys (open "#parts=3001.dat:14:2"))
                    |> Expect.equal False
        ]
