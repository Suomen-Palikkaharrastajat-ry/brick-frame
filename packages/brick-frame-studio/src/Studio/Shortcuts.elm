module Studio.Shortcuts exposing
    ( Action(..)
    , Context
    , EditKey(..)
    , Nudge(..)
    , NudgeDirection(..)
    , Tool(..)
    , TurnDirection(..)
    , actionFor
    , editKeyFor
    , helpRows
    , turnAxis
    , turnRotation
    )

{-| The studio's keyboard shortcuts, on top of the viewport's.

Pure, like `Viewport.Shortcuts`, and guarded the same way: nothing fires while
the user is typing in a form field, and chords with Ctrl, Cmd or Alt only count
where they are listed here (undo, redo, duplicate, select all), so the browser's
own shortcuts keep working.

The viewport owns F, G, `?`, `+ -`, the digits and the held camera keys. While
something is selected, `editKeyFor` claims W A S D Q E, Page Up and Page Down
(move the selection) and the arrows (turn it). The host must then keep those
presses away from the camera, which `Studio.Core` does by consuming them.

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


{-| A keyboard move of the selection, relative to the view: `Right` is towards
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


{-| Which way a keyboard turn of the selection goes.

`TurnLeft` and `TurnRight` turn about the vertical axis, so the front of the
selection swings to the left or right of the screen. `TipForward` and
`TipBackward` tip the top of the selection away from the viewer or back towards
it, about the ground axis that lies along the screen's horizontal.

-}
type TurnDirection
    = TurnLeft
    | TurnRight
    | TipForward
    | TipBackward


{-| A keyboard edit of the selection: a move, or a turn. `fortyFive` on a turn
means 45° instead of the rotate step; on a move, `fine` means 1 LDU instead of
the grid step.
-}
type EditKey
    = MoveBy Nudge
    | RotateBy { direction : TurnDirection, fortyFive : Bool }


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

    else if context.helpVisible then
        -- The help dialog owns the keyboard: the viewport closes it on Escape,
        -- and nothing else may edit the document behind it.
        case ViewportShortcuts.normalizeKey event.key of
            "Escape" ->
                if context.menuOpen then
                    Just CloseMenu

                else
                    Nothing

            _ ->
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

            "t" ->
                Just (UseTool MoveTool)

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


{-| The keyboard edit of the selection for a key press, if any.

Only while something is selected, the camera is not walking and no help
dialog is open; otherwise the key is left to the viewport (held camera keys).
Ctrl, Cmd and Alt chords are left to the browser and to `actionFor`.

  - `W` / `S` move the selection away from / toward the viewer, `A` / `D` left
    / right on the screen.
  - `Q` / `E` and `Page Down` / `Page Up` lower / raise it.
  - `←` / `→` turn it about the vertical axis.
  - `↑` / `↓` tip it forward / backward about the ground axis most aligned with
    the screen's horizontal.
  - `Shift` makes a move 1 LDU and a turn 45°.

-}
editKeyFor : Context -> KeyEvent -> Maybe EditKey
editKeyFor context event =
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
            move d =
                Just (MoveBy (Nudge { direction = d, fine = event.shift }))

            turn d =
                Just (RotateBy { direction = d, fortyFive = event.shift })
        in
        case ViewportShortcuts.normalizeKey event.key of
            "w" ->
                move Away

            "a" ->
                move Left

            "s" ->
                move Toward

            "d" ->
                move Right

            "e" ->
                move Up

            "q" ->
                move Down

            "PageUp" ->
                move Up

            "PageDown" ->
                move Down

            "ArrowLeft" ->
                turn TurnLeft

            "ArrowRight" ->
                turn TurnRight

            "ArrowUp" ->
                turn TipForward

            "ArrowDown" ->
                turn TipBackward

            _ ->
                Nothing


direction : KeyEvent -> Int
direction event =
    if event.shift then
        -1

    else
        1


{-| The world axis and signed direction of a keyboard turn, for a camera azimuth.

Left and right turn about the LDraw Y axis. A positive turn swings the front of
the selection to the right of the screen, as seen from the default view.

Forward and backward tip about the ground axis (X or Z) that lies most nearly
along the screen's horizontal, so the top of the selection tips away from or
back towards the viewer. The screen's right and away directions match the move
keys of `editKeyFor`.

The sign is +1 or -1; multiply by the angle to turn through.

-}
turnAxis : Float -> TurnDirection -> ( Axis, Float )
turnAxis azimuth turnDirection =
    let
        rightX =
            cos azimuth

        rightZ =
            -(sin azimuth)

        awayX =
            -(sin azimuth)

        awayZ =
            -(cos azimuth)

        signOf v =
            if v < 0 then
                -1

            else
                1

        -- Rotating by +θ about X moves the top of a part (LDraw -Y) towards -Z;
        -- about Z it moves the top towards +X.
        tipForward =
            if abs rightX >= abs rightZ then
                ( AxisX, -(signOf awayZ) )

            else
                ( AxisZ, signOf awayX )
    in
    case turnDirection of
        TurnRight ->
            ( AxisY, 1 )

        TurnLeft ->
            ( AxisY, -1 )

        TipForward ->
            tipForward

        TipBackward ->
            let
                ( axis, sign ) =
                    tipForward
            in
            ( axis, -sign )


{-| The axis and the signed rotation in radians of a keyboard turn: the turn
step in degrees (or 45° when `fortyFive` is set), about the axis `turnAxis`
picks for the camera azimuth.

Pass the camera azimuth and the studio's current turn step in degrees.

-}
turnRotation : { azimuth : Float, rotateStepDegrees : Int } -> { direction : TurnDirection, fortyFive : Bool } -> ( Axis, Float )
turnRotation view turn =
    let
        stepDegrees =
            if turn.fortyFive then
                45

            else
                view.rotateStepDegrees

        ( axis, sign ) =
            turnAxis view.azimuth turn.direction
    in
    ( axis, sign * degrees (toFloat stepDegrees) )


{-| The studio's rows for the `?` dialog, after the viewport's.
-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Edit"
      , [ ( "Click", "Select (Shift or Ctrl: add or remove)" )
        , ( "T or M", "Move tool (V select, R rotate)" )
        , ( "Drag a handle", "Move along that axis" )
        , ( "Drag a part (V or T)", "Pick it up and move it with the cursor; it snaps to studs" )
        , ( "Wheel while dragging", "Lift / lower the part" )
        , ( "Click a part", "The view slides to centre it and orbits around it; Fit view (F) recentres" )
        , ( "W A S D", "Move away / left / toward / right (Shift: 1 LDU)" )
        , ( "Q / PgDn  ·  E / PgUp", "Lower / raise the selection (Shift: 1 LDU)" )
        , ( "← →", "Turn about the vertical axis (Shift: 45°)" )
        , ( "↑ ↓", "Tip forward / back (Shift: 45°)" )
        , ( "X Y Z", "Turn the selection (Shift: the other way)" )
        , ( "Delete", "Remove the selection" )
        , ( "Ctrl D", "Duplicate" )
        , ( "Ctrl A", "Select all" )
        , ( "Esc", "Cancel a drag, or clear selection" )
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
