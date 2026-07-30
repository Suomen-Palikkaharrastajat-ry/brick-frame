module ShortcutsTest exposing (suite)

{-| Unit tests for keyboard shortcut mapping.

The typing guard is the one that matters most: the keydown subscription is
document-global, so a regression there silently hijacks form fields on any page
embedding the viewer.

-}

import Expect
import Set
import Test exposing (Test, describe, test)
import UI.Shortcuts as Shortcuts exposing (Action(..))


{-| A plain key press on the canvas, with no modifiers held.
-}
press : String -> Shortcuts.KeyEvent
press key =
    { key = key
    , shift = False
    , ctrl = False
    , meta = False
    , alt = False
    , repeat = False
    , targetTag = "CANVAS"
    , targetEditable = False
    }


{-| A key press whose target is some other element on the host page.
-}
pressOn : String -> String -> Shortcuts.KeyEvent
pressOn targetTag key =
    let
        event =
            press key
    in
    { event | targetTag = targetTag }


{-| A key press with one field overridden.
-}
pressWith : (Shortcuts.KeyEvent -> Shortcuts.KeyEvent) -> String -> Shortcuts.KeyEvent
pressWith override key =
    override (press key)


orbiting : { helpVisible : Bool, walking : Bool }
orbiting =
    { helpVisible = False, walking = False }


action : String -> Maybe Action
action key =
    Shortcuts.actionFor orbiting (press key)


suite : Test
suite =
    describe "UI.Shortcuts"
        [ describe "discrete shortcuts"
            [ test "space toggles playback, in both spellings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal (Just TogglePlayback) (action " ")
                        , \_ -> Expect.equal (Just TogglePlayback) (action "Spacebar")
                        ]
                        ()
            , test "view controls" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal (Just FitView) (action "f")
                        , \_ -> Expect.equal (Just FitView) (action "Home")
                        , \_ -> Expect.equal (Just ToggleWalk) (action "g")
                        , \_ -> Expect.equal (Just ToggleHelp) (action "?")
                        ]
                        ()
            , test "zoom accepts the shifted and unshifted spellings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal (Just ZoomIn) (action "+")
                        , \_ -> Expect.equal (Just ZoomIn) (action "=")
                        , \_ -> Expect.equal (Just ZoomOut) (action "-")
                        ]
                        ()
            , test "the number row selects view presets" <|
                \_ ->
                    List.map action [ "1", "2", "3", "4", "5", "6" ]
                        |> Expect.equal
                            [ Just PresetFront
                            , Just PresetBack
                            , Just PresetLeft
                            , Just PresetRight
                            , Just PresetTop
                            , Just PresetIsometric
                            ]
            , test "unbound keys do nothing" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (action "z")
                        , \_ -> Expect.equal Nothing (action "F5")
                        , \_ -> Expect.equal Nothing (action "7")
                        ]
                        ()
            , test "shifted letters still match" <|
                \_ ->
                    -- Holding Shift to move faster reports "W", not "w".
                    Expect.equal (Just ToggleWalk) (action "G")
            ]
        , describe "escape unwinds one layer at a time"
            [ test "closes the help overlay first" <|
                \_ ->
                    Shortcuts.actionFor { helpVisible = True, walking = True } (press "Escape")
                        |> Expect.equal (Just CloseHelp)
            , test "then leaves walk mode" <|
                \_ ->
                    Shortcuts.actionFor { helpVisible = False, walking = True } (press "Escape")
                        |> Expect.equal (Just LeaveWalk)
            , test "and does nothing when there is nothing to leave" <|
                \_ ->
                    Expect.equal Nothing (action "Escape")
            ]
        , describe "typing guard"
            [ test "ignores keys typed into form fields" <|
                \_ ->
                    -- Regression: the docs playground has type="url" inputs in the
                    -- same document as the viewer, so a space in the model URL box
                    -- used to toggle playback.
                    [ "INPUT", "TEXTAREA", "SELECT", "OPTION" ]
                        |> List.map
                            (\tag -> Shortcuts.actionFor orbiting (pressOn tag " "))
                        |> Expect.equal [ Nothing, Nothing, Nothing, Nothing ]
            , test "ignores keys aimed at a focused button" <|
                \_ ->
                    -- The browser already activates a focused button on Space;
                    -- handling it again toggled playback twice.
                    Shortcuts.actionFor orbiting (pressOn "BUTTON" " ")
                        |> Expect.equal Nothing
            , test "ignores contenteditable regions" <|
                \_ ->
                    Shortcuts.actionFor orbiting (pressWith (\e -> { e | targetEditable = True }) "g")
                        |> Expect.equal Nothing
            , test "tag matching is case-insensitive" <|
                \_ ->
                    Shortcuts.actionFor orbiting (pressOn "input" " ")
                        |> Expect.equal Nothing
            , test "leaves browser and host shortcuts alone" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal Nothing (Shortcuts.actionFor orbiting (pressWith (\e -> { e | ctrl = True }) "f"))
                        , \_ -> Expect.equal Nothing (Shortcuts.actionFor orbiting (pressWith (\e -> { e | meta = True }) "f"))
                        , \_ -> Expect.equal Nothing (Shortcuts.actionFor orbiting (pressWith (\e -> { e | alt = True }) "f"))
                        ]
                        ()
            , test "shift is not treated as a browser modifier" <|
                \_ ->
                    Shortcuts.actionFor orbiting (pressWith (\e -> { e | shift = True }) "?")
                        |> Expect.equal (Just ToggleHelp)
            ]
        , describe "held movement"
            [ test "WASD map to the expected axes" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal 1 (Shortcuts.movementFor (Set.singleton "w")).forward
                        , \_ -> Expect.equal -1 (Shortcuts.movementFor (Set.singleton "s")).forward
                        , \_ -> Expect.equal 1 (Shortcuts.movementFor (Set.singleton "d")).right
                        , \_ -> Expect.equal -1 (Shortcuts.movementFor (Set.singleton "a")).right
                        , \_ -> Expect.equal 1 (Shortcuts.movementFor (Set.singleton "e")).up
                        , \_ -> Expect.equal -1 (Shortcuts.movementFor (Set.singleton "q")).up
                        ]
                        ()
            , test "opposing keys cancel instead of jittering" <|
                \_ ->
                    (Shortcuts.movementFor (Set.fromList [ "w", "s" ])).forward
                        |> Expect.equal 0
            , test "combinations move diagonally" <|
                \_ ->
                    let
                        movement =
                            Shortcuts.movementFor (Set.fromList [ "w", "d" ])
                    in
                    Expect.all
                        [ \m -> Expect.equal 1 m.forward
                        , \m -> Expect.equal 1 m.right
                        ]
                        movement
            , test "arrows drive the look axes" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal 1 (Shortcuts.lookFor (Set.singleton "ArrowLeft")).azimuth
                        , \_ -> Expect.equal -1 (Shortcuts.lookFor (Set.singleton "ArrowRight")).azimuth
                        , \_ -> Expect.equal 1 (Shortcuts.lookFor (Set.singleton "ArrowUp")).elevation
                        , \_ -> Expect.equal -1 (Shortcuts.lookFor (Set.singleton "ArrowDown")).elevation
                        ]
                        ()
            , test "shift multiplies the speed" <|
                \_ ->
                    Shortcuts.speedMultiplier (Set.fromList [ "w", "Shift" ])
                        |> Expect.greaterThan (Shortcuts.speedMultiplier (Set.singleton "w"))
            ]
        , describe "animation frame gating"
            [ test "movement and look keys need frames" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal True (Shortcuts.hasContinuousInput (Set.singleton "w"))
                        , \_ -> Expect.equal True (Shortcuts.hasContinuousInput (Set.singleton "ArrowUp"))
                        ]
                        ()
            , test "the speed modifier alone does not" <|
                \_ ->
                    -- Holding Shift changes nothing on its own; waking the render
                    -- loop for it would keep the app busy for no reason.
                    Shortcuts.hasContinuousInput (Set.singleton "Shift")
                        |> Expect.equal False
            , test "cancelled movement does not" <|
                \_ ->
                    Shortcuts.hasContinuousInput (Set.fromList [ "a", "d" ])
                        |> Expect.equal False
            , test "nothing held does not" <|
                \_ ->
                    Shortcuts.hasContinuousInput Set.empty
                        |> Expect.equal False
            ]
        , describe "key normalisation"
            [ test "letters fold to lower case" <|
                \_ ->
                    Expect.equal "w" (Shortcuts.normalizeKey "W")
            , test "named keys keep their capitalisation" <|
                \_ ->
                    Expect.all
                        [ \_ -> Expect.equal "ArrowUp" (Shortcuts.normalizeKey "ArrowUp")
                        , \_ -> Expect.equal "Escape" (Shortcuts.normalizeKey "Escape")
                        , \_ -> Expect.equal "Shift" (Shortcuts.normalizeKey "Shift")
                        ]
                        ()
            ]
        , describe "help reference"
            [ test "documents every group with at least one row" <|
                \_ ->
                    Shortcuts.helpRows
                        |> List.all (\( _, rows ) -> not (List.isEmpty rows))
                        |> Expect.equal True
            ]
        ]
