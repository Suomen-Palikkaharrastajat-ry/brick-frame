module UI.Shortcuts exposing
    ( Action(..)
    , KeyEvent
    , Look
    , Movement
    , actionFor
    , hasContinuousInput
    , helpRows
    , isTextEntryTarget
    , lookFor
    , movementFor
    , normalizeKey
    , speedMultiplier
    )

{-| Keyboard shortcut mapping.

Pure, so it can be unit-tested — `Main.update` is not exposed, so key dispatch
would otherwise have no coverage at all. This module names its own `Action` type
rather than `Main.Msg`, both to avoid a circular import and to follow the house
rule that every `Msg` constructor lives in `Main`.


## Two kinds of key

Discrete keys fire once on keydown and map through `actionFor`. Movement and look
keys are _held_: `Main` tracks the pressed set and asks `movementFor` / `lookFor`
for a per-frame delta, so several can be active at once (forward and strafe
together) and so speed is a function of frame time rather than of the operating
system's key-repeat rate.


## Guarding against hijacked typing

The keydown subscription is document-global, so without a guard every shortcut
would also fire while the user is typing in a host page's form field. `actionFor`
returns `Nothing` for text-entry targets and for anything held with
Ctrl/Cmd/Alt, so browser and host shortcuts still work.

-}

import Set exposing (Set)


{-| The parts of a `keydown`/`keyup` event this module needs.
-}
type alias KeyEvent =
    { key : String
    , shift : Bool
    , ctrl : Bool
    , meta : Bool
    , alt : Bool
    , repeat : Bool
    , targetTag : String
    , targetEditable : Bool
    }


{-| A discrete, one-shot shortcut.

Held movement and look keys are deliberately absent — they go through
`movementFor` and `lookFor` instead.

-}
type Action
    = TogglePlayback
    | FitView
    | ToggleWalk
    | LeaveWalk
    | ToggleHelp
    | CloseHelp
    | ZoomIn
    | ZoomOut
    | PresetFront
    | PresetBack
    | PresetLeft
    | PresetRight
    | PresetTop
    | PresetIsometric


{-| Per-frame translation request, in multiples of the walking speed.
-}
type alias Movement =
    { forward : Float
    , right : Float
    , up : Float
    }


{-| Per-frame rotation request, in multiples of the turn rate.
-}
type alias Look =
    { azimuth : Float
    , elevation : Float
    }


{-| Fold a key name to its canonical form.

Browsers report letters in the case they were typed, so `Shift + W` arrives as
`"W"` while `w` arrives as `"w"`. Named keys keep their capitalisation so that
`"ArrowUp"` and `"Escape"` stay distinguishable from single characters.

-}
normalizeKey : String -> String
normalizeKey key =
    if String.length key == 1 then
        String.toLower key

    else
        key


{-| Whether a key event originated in something the user is typing into.

`BUTTON` is included on purpose: the browser already activates a focused button
on Space, so letting the document handler see it too made every Space press
toggle playback twice.

-}
isTextEntryTarget : KeyEvent -> Bool
isTextEntryTarget event =
    event.targetEditable
        || List.member (String.toUpper event.targetTag)
            [ "INPUT", "TEXTAREA", "SELECT", "BUTTON", "OPTION" ]


{-| The one-shot action for a key press, if any.

`helpVisible` and `walking` disambiguate `Escape`, which backs out of one layer
at a time.

-}
actionFor : { helpVisible : Bool, walking : Bool } -> KeyEvent -> Maybe Action
actionFor context event =
    if isTextEntryTarget event || event.ctrl || event.meta || event.alt then
        Nothing

    else
        case normalizeKey event.key of
            " " ->
                Just TogglePlayback

            -- Legacy IE/Edge spelling, kept from the original binding.
            "Spacebar" ->
                Just TogglePlayback

            "f" ->
                Just FitView

            "Home" ->
                Just FitView

            "g" ->
                Just ToggleWalk

            "?" ->
                Just ToggleHelp

            "/" ->
                Just ToggleHelp

            "Escape" ->
                if context.helpVisible then
                    Just CloseHelp

                else if context.walking then
                    Just LeaveWalk

                else
                    Nothing

            "+" ->
                Just ZoomIn

            "=" ->
                Just ZoomIn

            "-" ->
                Just ZoomOut

            "_" ->
                Just ZoomOut

            "1" ->
                Just PresetFront

            "2" ->
                Just PresetBack

            "3" ->
                Just PresetLeft

            "4" ->
                Just PresetRight

            "5" ->
                Just PresetTop

            "6" ->
                Just PresetIsometric

            _ ->
                Nothing


{-| Translation requested by the currently held keys.

Opposite keys cancel, so holding W and S stands still rather than jittering.

-}
movementFor : Set String -> Movement
movementFor held =
    { forward = axis held "w" "s"
    , right = axis held "d" "a"
    , up = axis held "e" "q"
    }


{-| Rotation requested by the currently held arrow keys.

Signs match the on-screen chevrons: `ArrowLeft` turns the view left.

-}
lookFor : Set String -> Look
lookFor held =
    { azimuth = axis held "ArrowLeft" "ArrowRight"
    , elevation = axis held "ArrowUp" "ArrowDown"
    }


{-| +1, -1 or 0 from a pair of opposed keys.
-}
axis : Set String -> String -> String -> Float
axis held positive negative =
    (if Set.member positive held then
        1

     else
        0
    )
        - (if Set.member negative held then
            1

           else
            0
          )


{-| How much faster movement runs while a speed modifier is held.

Crossing a city-scale model at walking pace takes long enough to be tedious, and
a modifier is cheaper than a speed control.

-}
speedMultiplier : Set String -> Float
speedMultiplier held =
    if Set.member "Shift" held then
        4.0

    else
        1.0


{-| Whether any held key needs a new frame drawn.

Deliberately not `Set.isEmpty`: holding only the speed modifier changes nothing,
and spinning up the animation loop for it would keep the app awake for no reason.

-}
hasContinuousInput : Set String -> Bool
hasContinuousInput held =
    let
        movement =
            movementFor held

        look =
            lookFor held
    in
    movement.forward
        /= 0
        || movement.right
        /= 0
        || movement.up
        /= 0
        || look.azimuth
        /= 0
        || look.elevation
        /= 0


{-| The shortcut reference, as key/description pairs grouped by heading.

Lives here next to the bindings so the two cannot drift apart.

-}
helpRows : List ( String, List ( String, String ) )
helpRows =
    [ ( "Move"
      , [ ( "W A S D", "Walk / pan" )
        , ( "Q E", "Down / up" )
        , ( "Shift", "Move faster" )
        , ( "Wheel", "Zoom, or walk forward" )
        ]
      )
    , ( "Look"
      , [ ( "Drag", "Orbit, or turn your head" )
        , ( "Shift + drag", "Pan" )
        , ( "Arrows", "Orbit / look" )
        , ( "+ -", "Zoom" )
        ]
      )
    , ( "View"
      , [ ( "G", "Street level on / off" )
        , ( "F", "Fit whole model" )
        , ( "1 2 3 4", "Front / back / left / right" )
        , ( "5 6", "Top / isometric" )
        ]
      )
    , ( "Other"
      , [ ( "Space", "Play / pause" )
        , ( "?", "This list" )
        , ( "Esc", "Close, then leave street level" )
        ]
      )
    ]
