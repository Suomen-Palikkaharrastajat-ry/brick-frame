module HamburgerMenuTest exposing (suite)

{-| Tests for the shared top-left menu (`UI.HamburgerMenu`) and the compact
picture card (`UI.PartCard.compactView`).
-}

import Expect
import Html.Attributes as Attr
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import UI.HamburgerMenu as HamburgerMenu
import UI.PartCard as PartCard


attr : String -> String -> Selector.Selector
attr name value =
    Selector.attribute (Attr.attribute name value)


type Msg
    = Toggle
    | Close
    | Pick String


menuConfig : Bool -> HamburgerMenu.Config Msg
menuConfig open =
    { id = "test-menu"
    , open = open
    , onToggle = Toggle
    , onClose = Close
    , items =
        [ { label = "Undo", shortcut = "Ctrl+Z", enabled = True, onSelect = Pick "undo" }
        , { label = "Redo", shortcut = "", enabled = False, onSelect = Pick "redo" }
        ]
    }


cardConfig : Maybe Msg -> PartCard.CompactConfig Msg
cardConfig onClick =
    { imageUrl = Nothing
    , tooltip = "Brick 2 x 4 (3001.dat, Yellow)"
    , ariaLabel = "Add Brick 2 x 4 (Yellow), 3 of 4 left"
    , badge = Just { text = "3/4", muted = False }
    , onClick = onClick
    , attributes = []
    }


suite : Test
suite =
    describe "UI.HamburgerMenu and compact part cards"
        [ describe "HamburgerMenu"
            [ test "closed: the hamburger reports collapsed and no menu is drawn" <|
                \_ ->
                    let
                        html =
                            Query.fromHtml (HamburgerMenu.view (menuConfig False))
                    in
                    Expect.all
                        [ \_ -> html |> Query.find [ attr "aria-haspopup" "menu", attr "aria-expanded" "false" ] |> Query.has [ attr "aria-label" "Menu" ]
                        , \_ -> html |> Query.findAll [ attr "role" "menu" ] |> Query.count (Expect.equal 0)
                        ]
                        ()
            , test "open: the panel lists each item as a menuitem with its shortcut" <|
                \_ ->
                    let
                        html =
                            Query.fromHtml (HamburgerMenu.view (menuConfig True))
                    in
                    Expect.all
                        [ \_ -> html |> Query.find [ attr "aria-expanded" "true" ] |> Query.has []
                        , \_ -> html |> Query.findAll [ attr "role" "menuitem" ] |> Query.count (Expect.equal 2)
                        , \_ -> html |> Query.find [ attr "role" "menu" ] |> Query.has [ Selector.text "Ctrl+Z" ]
                        ]
                        ()
            , test "a disabled item cannot be chosen" <|
                \_ ->
                    Query.fromHtml (HamburgerMenu.view (menuConfig True))
                        |> Query.find [ attr "role" "menuitem", Selector.containing [ Selector.text "Redo" ] ]
                        |> Query.has [ Selector.disabled True ]
            , test "choosing an item sends its message" <|
                \_ ->
                    Query.fromHtml (HamburgerMenu.view (menuConfig True))
                        |> Query.find [ attr "role" "menuitem", Selector.containing [ Selector.text "Undo" ] ]
                        |> Event.simulate Event.click
                        |> Event.expect (Pick "undo")
            , test "the hamburger sends the toggle message" <|
                \_ ->
                    Query.fromHtml (HamburgerMenu.view (menuConfig False))
                        |> Query.find [ attr "aria-label" "Menu" ]
                        |> Event.simulate Event.click
                        |> Event.expect Toggle
            , test "the backdrop sends the close message while open" <|
                \_ ->
                    Query.fromHtml (HamburgerMenu.view (menuConfig True))
                        |> Query.find [ Selector.tag "div", attr "aria-hidden" "true" ]
                        |> Event.simulate Event.click
                        |> Event.expect Close
            ]
        , describe "compact part card"
            [ test "the card is named by its accessible label and tooltip, with its count on the image" <|
                \_ ->
                    Query.fromHtml (PartCard.compactView (cardConfig (Just (Pick "add"))))
                        |> Expect.all
                            [ Query.has [ attr "aria-label" "Add Brick 2 x 4 (Yellow), 3 of 4 left", attr "title" "Brick 2 x 4 (3001.dat, Yellow)" ]
                            , Query.has [ Selector.text "3/4" ]
                            ]
            , test "a card with nothing left is disabled and sends nothing" <|
                \_ ->
                    Query.fromHtml (PartCard.compactView (cardConfig Nothing))
                        |> Query.has [ Selector.disabled True ]
            , test "a card sends its message when pressed" <|
                \_ ->
                    Query.fromHtml (PartCard.compactView (cardConfig (Just (Pick "add"))))
                        |> Event.simulate Event.click
                        |> Event.expect (Pick "add")
            ]
        ]
