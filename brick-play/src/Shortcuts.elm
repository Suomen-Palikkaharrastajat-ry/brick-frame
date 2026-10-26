module Shortcuts exposing
    ( Action(..)
    , actionFor
    )

{-| Brick Play's own keys, on top of the studio's and the viewport's (see
`Viewport.Shortcuts` and `Studio.Shortcuts`).

The only key here is `L`, which glues or unglues the selection. Glue exists in
free building only: a puzzle's glued parts are locked, so the key does nothing
there. Like every shortcut table it is guarded: nothing fires while the user is
typing in a form field (the dock's part search included), on a widget key, or
behind an open dialog or menu.

-}

import Viewport.Shortcuts as ViewportShortcuts exposing (KeyEvent)


{-| What a key press means in Brick Play.
-}
type Action
    = ToggleGlue


{-| The Brick Play action for a key press, if any. `glueEditable` is true in free
building, `helpVisible` and `menuOpen` say whether a dialog or the menu has the
keyboard.
-}
actionFor : { glueEditable : Bool, helpVisible : Bool, menuOpen : Bool } -> KeyEvent -> Maybe Action
actionFor context event =
    if ViewportShortcuts.isTextEntryTarget event || ViewportShortcuts.isWidgetKey event then
        Nothing

    else if not context.glueEditable || context.helpVisible || context.menuOpen then
        Nothing

    else if event.ctrl || event.meta || event.alt then
        Nothing

    else
        case ViewportShortcuts.normalizeKey event.key of
            "l" ->
                Just ToggleGlue

            _ ->
                Nothing
