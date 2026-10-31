module Studio.ShortcutsTest exposing (suite)

{-| Tests for the studio's pure keyboard mapping in `Studio.Shortcuts`.
-}

import Expect
import Studio.Edit exposing (Axis(..))
import Studio.Shortcuts as Shortcuts exposing (Action(..), EditKey(..), Nudge(..), NudgeDirection(..), Tool(..), TurnDirection(..))
import Test exposing (Test, describe, test)
import Viewport.Shortcuts exposing (KeyEvent)


suite : Test
suite =
    describe "Studio.Shortcuts"
        [ describe "editKeyFor"
            [ test "W A S D move away, left, toward and right" <|
                \_ ->
                    Expect.equal
                        [ Just (MoveBy (Nudge { direction = Away, fine = False }))
                        , Just (MoveBy (Nudge { direction = Left, fine = False }))
                        , Just (MoveBy (Nudge { direction = Toward, fine = False }))
                        , Just (MoveBy (Nudge { direction = Right, fine = False }))
                        ]
                        (List.map (editKey selected) [ "w", "a", "s", "d" ])
            , test "Q E and PageUp PageDown lower and raise" <|
                \_ ->
                    Expect.equal
                        [ Just (MoveBy (Nudge { direction = Down, fine = False }))
                        , Just (MoveBy (Nudge { direction = Up, fine = False }))
                        , Just (MoveBy (Nudge { direction = Up, fine = False }))
                        , Just (MoveBy (Nudge { direction = Down, fine = False }))
                        ]
                        (List.map (editKey selected) [ "q", "e", "PageUp", "PageDown" ])
            , test "Shift makes a move fine" <|
                \_ ->
                    Expect.equal (Just (MoveBy (Nudge { direction = Right, fine = True })))
                        (Shortcuts.editKeyFor selected (press "D" |> withShift))
            , test "arrows turn about the vertical axis and tip forward and back" <|
                \_ ->
                    Expect.equal
                        [ Just (RotateBy { direction = TurnLeft, fortyFive = False })
                        , Just (RotateBy { direction = TurnRight, fortyFive = False })
                        , Just (RotateBy { direction = TipForward, fortyFive = False })
                        , Just (RotateBy { direction = TipBackward, fortyFive = False })
                        ]
                        (List.map (editKey selected) [ "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown" ])
            , test "Shift turns by 45 degrees" <|
                \_ ->
                    Expect.equal (Just (RotateBy { direction = TipForward, fortyFive = True }))
                        (Shortcuts.editKeyFor selected (press "ArrowUp" |> withShift))
            , test "nothing without a selection" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing, Nothing ]
                        (List.map (\k -> Shortcuts.editKeyFor { selected | hasSelection = False } (press k))
                            [ "w", "ArrowLeft", "PageUp" ]
                        )
            , test "nothing while walking or with help open" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing ]
                        [ Shortcuts.editKeyFor { selected | walking = True } (press "d")
                        , Shortcuts.editKeyFor { selected | helpVisible = True } (press "ArrowRight")
                        ]
            , test "nothing in a text field or on a focused widget" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing, Nothing ]
                        [ Shortcuts.editKeyFor selected (pressOn "INPUT" "d")
                        , Shortcuts.editKeyFor selected (pressWith (\e -> { e | targetEditable = True }) "ArrowLeft")
                        , Shortcuts.editKeyFor selected (pressOn "BUTTON" "ArrowLeft" |> withRole "radio")
                        ]
            , test "Ctrl and Alt chords are left alone" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing ]
                        [ Shortcuts.editKeyFor selected (press "d" |> withCtrl)
                        , Shortcuts.editKeyFor selected (press "ArrowUp" |> withAlt)
                        ]
            ]
        , describe "actionFor"
            [ test "T and M both select the move tool" <|
                \_ ->
                    Expect.equal [ Just (UseTool MoveTool), Just (UseTool MoveTool) ]
                        [ Shortcuts.actionFor selected (press "t")
                        , Shortcuts.actionFor selected (press "m")
                        ]
            , test "X, Y and Z turn the selection" <|
                \_ ->
                    Expect.equal (Just (RotateSelection AxisX 1))
                        (Shortcuts.actionFor selected (press "x"))
            , test "Ctrl Z is ignored in a text field" <|
                \_ ->
                    Expect.equal Nothing
                        (Shortcuts.actionFor selected (pressOn "INPUT" "z" |> withCtrl))
            , test "nothing edits the document while help is visible" <|
                \_ ->
                    let
                        helpOpen =
                            { selected | helpVisible = True }
                    in
                    Expect.equal (List.repeat 8 Nothing)
                        (List.map (Shortcuts.actionFor helpOpen)
                            [ press "Delete"
                            , press "Backspace"
                            , press "x"
                            , press "t"
                            , press "["
                            , press "p"
                            , press "z" |> withCtrl
                            , press "d" |> withCtrl
                            ]
                        )
            , test "Escape while help is visible does not clear the selection" <|
                \_ ->
                    Expect.equal Nothing
                        (Shortcuts.actionFor { selected | helpVisible = True } (press "Escape"))
            , test "Escape while help is visible closes an open menu only" <|
                \_ ->
                    Expect.equal (Just CloseMenu)
                        (Shortcuts.actionFor { selected | helpVisible = True, menuOpen = True } (press "Escape"))
            , test "Ctrl C maps to CopySelection and Ctrl V maps to PasteClipboard" <|
                \_ ->
                    Expect.equal [ Just CopySelection, Just PasteClipboard ]
                        [ Shortcuts.actionFor selected (press "c" |> withCtrl)
                        , Shortcuts.actionFor selected (press "v" |> withCtrl)
                        ]
            , test "Cmd C and Cmd V map to CopySelection and PasteClipboard" <|
                \_ ->
                    Expect.equal [ Just CopySelection, Just PasteClipboard ]
                        [ Shortcuts.actionFor selected (press "c" |> withMeta)
                        , Shortcuts.actionFor selected (press "v" |> withMeta)
                        ]
            , test "Ctrl C and Ctrl V are ignored in a text field" <|
                \_ ->
                    Expect.equal [ Nothing, Nothing ]
                        [ Shortcuts.actionFor selected (pressOn "INPUT" "c" |> withCtrl)
                        , Shortcuts.actionFor selected (pressOn "INPUT" "v" |> withCtrl)
                        ]
            ]
        , describe "turnRotation"
            [ test "a plain turn uses the turn step about the vertical axis" <|
                \_ ->
                    Expect.equal ( AxisY, degrees 90 )
                        (Shortcuts.turnRotation { azimuth = 0, rotateStepDegrees = 90 }
                            { direction = TurnRight, fortyFive = False }
                        )
            , test "Shift turns by 45 degrees, the other way for a left turn" <|
                \_ ->
                    Expect.equal ( AxisY, -(degrees 45) )
                        (Shortcuts.turnRotation { azimuth = 0, rotateStepDegrees = 90 }
                            { direction = TurnLeft, fortyFive = True }
                        )
            ]
        , describe "helpRows"
            [ test "lists the move and turn keys" <|
                \_ ->
                    let
                        keys =
                            Shortcuts.helpRows
                                |> List.concatMap Tuple.second
                                |> List.map Tuple.first
                    in
                    Expect.equal True
                        (List.all (\k -> List.member k keys) [ "W A S D", "← →", "↑ ↓", "Esc", "T or M" ])
            , test "helpRows includes Copy and Paste" <|
                \_ ->
                    let
                        editRows =
                            Shortcuts.helpRows
                                |> List.filter (\( group, _ ) -> group == "Edit")
                                |> List.concatMap Tuple.second
                    in
                    Expect.equal True
                        (List.member ( "Ctrl C", "Copy" ) editRows
                            && List.member ( "Ctrl V", "Paste" ) editRows
                        )
            ]
        ]


selected : Shortcuts.Context
selected =
    { hasSelection = True
    , helpVisible = False
    , walking = False
    , inSubmodel = False
    , menuOpen = False
    }


editKey : Shortcuts.Context -> String -> Maybe EditKey
editKey context key =
    Shortcuts.editKeyFor context (press key)


press : String -> KeyEvent
press key =
    pressOn "CANVAS" key


pressOn : String -> String -> KeyEvent
pressOn tag key =
    { key = key
    , shift = False
    , ctrl = False
    , meta = False
    , alt = False
    , repeat = False
    , targetTag = tag
    , targetEditable = False
    , targetRole = ""
    }


pressWith : (KeyEvent -> KeyEvent) -> String -> KeyEvent
pressWith change key =
    change (press key)


withShift : KeyEvent -> KeyEvent
withShift event =
    { event | shift = True }


withCtrl : KeyEvent -> KeyEvent
withCtrl event =
    { event | ctrl = True }


withMeta : KeyEvent -> KeyEvent
withMeta event =
    { event | meta = True }


withAlt : KeyEvent -> KeyEvent
withAlt event =
    { event | alt = True }


withRole : String -> KeyEvent -> KeyEvent
withRole role event =
    { event | targetRole = role }
