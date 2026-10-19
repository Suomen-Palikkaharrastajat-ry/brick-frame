module Studio.Shortcuts exposing (Action(..), Context, Tool(..), actionFor, helpRows)

{-| The studio's keyboard shortcuts, on top of the viewport's.

Pure, like `Viewport.Shortcuts`, and guarded the same way: nothing fires while
the user is typing in a form field, and chords with Ctrl, Cmd or Alt only count
where they are listed here (undo, redo, duplicate, select all), so the browser's
own shortcuts keep working.

The viewport owns W A S D, the arrows, F, G, `?`, `+ -` and the digits; the keys
below avoid them.

-}

import Studio.Edit exposing (Axis(..))
import Viewport.Shortcuts as ViewportShortcuts exposing (KeyEvent)


{-| What a click on the canvas does, and what the on-screen tools are.
-}
type Tool
    = SelectTool
    | MoveTool
    | RotateTool


type Action
    = Undo
    | Redo
    | DeleteSelection
    | DuplicateSelection
    | SelectAll
    | ClearSelection
    | UseTool Tool
    | RotateSelection Axis Int
    | NextGridStep
    | NextRotateStep


{-| What `Escape` needs to know to back out of the right layer.
-}
type alias Context =
    { hasSelection : Bool
    , helpVisible : Bool
    , walking : Bool
    }


{-| The studio action for a key press, if any.
-}
actionFor : Context -> KeyEvent -> Maybe Action
actionFor context event =
    if ViewportShortcuts.isTextEntryTarget event then
        Nothing

    else if event.ctrl || event.meta then
        case ViewportShortcuts.normalizeKey event.key of
            "z" ->
                Just
                    (if event.shift then
                        Redo

                     else
                        Undo
                    )

            "y" ->
                Just Redo

            "d" ->
                Just DuplicateSelection

            "a" ->
                Just SelectAll

            _ ->
                Nothing

    else if event.alt then
        Nothing

    else
        case ViewportShortcuts.normalizeKey event.key of
            "Delete" ->
                Just DeleteSelection

            "Backspace" ->
                Just DeleteSelection

            "Escape" ->
                if context.hasSelection && not context.helpVisible && not context.walking then
                    Just ClearSelection

                else
                    Nothing

            "v" ->
                Just (UseTool SelectTool)

            "m" ->
                Just (UseTool MoveTool)

            "r" ->
                Just (UseTool RotateTool)

            "x" ->
                Just (RotateSelection AxisX (direction event))

            "y" ->
                Just (RotateSelection AxisY (direction event))

            "z" ->
                Just (RotateSelection AxisZ (direction event))

            "]" ->
                Just NextGridStep

            "[" ->
                Just NextGridStep

            "." ->
                Just NextRotateStep

            "," ->
                Just NextRotateStep

            _ ->
                Nothing


direction : KeyEvent -> Int
direction event =
    if event.shift then
        -1

    else
        1


{-| The studio's rows for the `?` dialog, after the viewport's.
-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Edit"
      , [ ( "Click", "Select (Shift: add or remove)" )
        , ( "V M R", "Select / move / rotate tool" )
        , ( "Drag a handle", "Move along that axis" )
        , ( "X Y Z", "Turn the selection (Shift: the other way)" )
        , ( "Delete", "Remove the selection" )
        , ( "Ctrl D", "Duplicate" )
        , ( "Ctrl A", "Select all" )
        , ( "Esc", "Clear the selection" )
        ]
      )
    , ( "History"
      , [ ( "Ctrl Z", "Undo" )
        , ( "Ctrl Shift Z / Ctrl Y", "Redo" )
        ]
      )
    , ( "Grid"
      , [ ( "[ ]", "Move step: 20, 8, 4 or 1 LDU" )
        , ( ", .", "Turn step: 90° or 15°" )
        ]
      )
    ]
