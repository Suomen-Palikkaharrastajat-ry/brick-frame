module Studio.Shortcuts exposing (Action(..), Context, Nudge(..), NudgeDirection(..), Tool(..), actionFor, helpRows, nudgeFor)

{-| The studio's keyboard shortcuts, on top of the viewport's.

Pure, like `Viewport.Shortcuts`, and guarded the same way: nothing fires while
the user is typing in a form field, and chords with Ctrl, Cmd or Alt only count
where they are listed here (undo, redo, duplicate, select all), so the browser's
own shortcuts keep working.

The viewport owns W A S D, the arrows, F, G, `?`, `+ -` and the digits; the keys
below avoid them. The one exception is the arrows while something is selected:
then they nudge the selection (`nudgeFor`), the way every editor does, and the
host keeps those presses away from the camera.

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
    | GroupSubmodel
    | ExplodeSubmodel
    | EnterSubmodel
    | ExitSubmodel
    | UseTool Tool
    | RotateSelection Axis Int
    | NextGridStep
    | CoarserGridStep
    | NextRotateStep
    | OpenFile
    | FocusSearch
    | ToggleBOM
    | OpenInstructions
    | Simulate
    | CloseMenu


{-| A keyboard nudge of the selection, relative to the view: `Right` is towards
the right of the screen, `Away` into it, `Up` against gravity. `fine` moves one
LDU instead of the grid step.
-}
type Nudge
    = Nudge { direction : NudgeDirection, fine : Bool }


type NudgeDirection
    = Left
    | Right
    | Away
    | Toward
    | Up
    | Down


{-| What `Escape` needs to know to back out of the right layer.
-}
type alias Context =
    { hasSelection : Bool
    , helpVisible : Bool
    , walking : Bool
    , inSubmodel : Bool
    , menuOpen : Bool
    }


{-| The studio action for a key press, if any.
-}
actionFor : Context -> KeyEvent -> Maybe Action
actionFor context event =
    if ViewportShortcuts.isTextEntryTarget event || ViewportShortcuts.isWidgetKey event then
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

            "o" ->
                Just OpenFile

            "g" ->
                Just
                    (if event.shift then
                        ExplodeSubmodel

                     else
                        GroupSubmodel
                    )

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

            "Enter" ->
                if context.hasSelection && not context.helpVisible && not context.walking then
                    Just EnterSubmodel

                else
                    Nothing

            "Escape" ->
                if context.menuOpen then
                    Just CloseMenu

                else if context.hasSelection && not context.helpVisible && not context.walking then
                    Just ClearSelection

                else if context.inSubmodel && not context.helpVisible && not context.walking then
                    Just ExitSubmodel

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
                Just CoarserGridStep

            "[" ->
                Just NextGridStep

            "/" ->
                Just FocusSearch

            "b" ->
                Just ToggleBOM

            "i" ->
                Just OpenInstructions

            "p" ->
                Just Simulate

            "." ->
                Just NextRotateStep

            "," ->
                Just NextRotateStep

            _ ->
                Nothing


{-| The nudge an arrow or Page key asks for, when something is selected and the
camera is not walking. Shift makes it fine. Ctrl, Cmd and Alt chords are left to
the browser.
-}
nudgeFor : Context -> KeyEvent -> Maybe Nudge
nudgeFor context event =
    if
        ViewportShortcuts.isTextEntryTarget event
            || ViewportShortcuts.isWidgetKey event
            || event.ctrl
            || event.meta
            || event.alt
            || not context.hasSelection
            || context.walking
            || context.helpVisible
    then
        Nothing

    else
        let
            nudge d =
                Just (Nudge { direction = d, fine = event.shift })
        in
        case event.key of
            "ArrowLeft" ->
                nudge Left

            "ArrowRight" ->
                nudge Right

            "ArrowUp" ->
                nudge Away

            "ArrowDown" ->
                nudge Toward

            "PageUp" ->
                nudge Up

            "PageDown" ->
                nudge Down

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
        , ( "Arrows", "Nudge the selection by the move step" )
        , ( "Shift Arrows", "Nudge by 1 LDU" )
        , ( "PageUp PageDown", "Raise / lower the selection" )
        , ( "X Y Z", "Turn the selection (Shift: the other way)" )
        , ( "Delete", "Remove the selection" )
        , ( "Ctrl D", "Duplicate" )
        , ( "Ctrl A", "Select all" )
        , ( "Esc", "Clear selection / exit submodel" )
        ]
      )
    , ( "Submodels"
      , [ ( "Ctrl G", "Group selection into submodel" )
        , ( "Ctrl Shift G", "Explode submodel" )
        , ( "Enter / Double-click", "Enter submodel" )
        ]
      )
    , ( "History"
      , [ ( "Ctrl Z", "Undo" )
        , ( "Ctrl Shift Z / Ctrl Y", "Redo" )
        ]
      )
    , ( "Grid"
      , [ ( "[ ]", "Finer / coarser move step: 1, 4, 8, 20 LDU" )
        , ( ", .", "Turn step: 90° or 15°" )
        ]
      )
    , ( "Document"
      , [ ( "/", "Search parts" )
        , ( "Ctrl O", "Open a file" )
        , ( "Ctrl S", "Save" )
        , ( "B", "Parts list" )
        , ( "I", "Building instructions" )
        , ( "P", "Simulate gears" )
        ]
      )
    ]
