module Ring exposing
    ( Config
    , panelHeight
    , panelPosition
    , panelWidth
    , screenPoint
    , view
    )

{-| The context ring: the controls that float beside a selected part in Brick
Play. It opens on a double-click or double-tap of a part, and holds the part's
Turn left, Turn right, Tip, Raise, Lower and Delete buttons, Glue (free building
only), a W A S D Q E move pad, and the move-step control (`Step.StepSize`: Stud,
½ stud, Bit).

The ring is a cluster of 44 px round buttons. `panelPosition` places it beside the
part's screen point, flipped to the other side near the canvas edge and kept
inside the canvas. `screenPoint` projects a render-space point with the camera,
the same matrices the studio's outlines use. Both are pure.

Every button is a real `<button>` with an `aria-label`, and its tooltip gives the
shortcut. Buttons do not take focus on a mouse press (`UI.Focus.keepFocusOnCanvas`),
so the keyboard shortcuts keep working after a click on the ring.

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Step
import Studio.Shortcuts exposing (NudgeDirection(..), TurnDirection(..))
import UI.Focus as Focus
import UI.Icon as Icon


{-| What the ring shows and what its buttons send.

`anchor` is the part's screen point, `Nothing` while it is off screen (the ring
then stays hidden). `canvas` is the canvas size in pixels. `movable` is False when
the selection is glued, and `glued` says whether it is.

-}
type alias Config msg =
    { anchor : Maybe ( Float, Float )
    , canvas : ( Float, Float )
    , free : Bool
    , glued : Bool
    , movable : Bool
    , step : Step.StepSize
    , onTurn : TurnDirection -> msg
    , onRaise : msg
    , onLower : msg
    , onDelete : msg
    , onGlue : msg
    , onMove : NudgeDirection -> msg
    , onStep : Step.StepSize -> msg
    , noOp : msg
    }


{-| The ring's width in pixels.
-}
panelWidth : Float
panelWidth =
    220


{-| The ring's height in pixels, for keeping it inside the canvas.
-}
panelHeight : Float
panelHeight =
    270


{-| The top-left corner of the ring for a part at `anchor` on a canvas of
`canvas` pixels. It sits to the right of the part, or to its left when the right
side has no room, and is kept inside the canvas with an 8 px margin.
-}
panelPosition : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
panelPosition ( anchorX, anchorY ) ( canvasWidth, canvasHeight ) =
    let
        inset =
            8

        gap =
            48

        beside =
            anchorX + gap

        left =
            if beside + panelWidth <= canvasWidth - inset then
                beside

            else
                anchorX - gap - panelWidth

        top =
            anchorY - panelHeight / 2
    in
    ( clampTo inset (canvasWidth - panelWidth - inset) left
    , clampTo inset (canvasHeight - panelHeight - inset) top
    )


clampTo : Float -> Float -> Float -> Float
clampTo low high value =
    max low (min high value)


{-| The pixel a render-space point appears at on a canvas of `width` by `height`,
or `Nothing` when it is behind the eye or the canvas is empty.
-}
screenPoint : Camera -> Int -> Int -> Vec3 -> Maybe ( Float, Float )
screenPoint camera width height point =
    if width <= 0 || height <= 0 then
        Nothing

    else
        let
            viewMatrix =
                Camera.viewMatrix camera

            projection =
                Camera.projectionMatrix
                    (toFloat width / toFloat height)
                    (Camera.nearPlane camera)
                    (Camera.farPlane camera)

            -- The eye looks down -Z, so a point in front has a negative depth.
            depth =
                Vec3.getZ (Mat4.transform viewMatrix point)

            ndc =
                Mat4.transform (Mat4.mul projection viewMatrix) point

            x =
                (Vec3.getX ndc + 1) / 2 * toFloat width

            y =
                (1 - Vec3.getY ndc) / 2 * toFloat height
        in
        if depth >= 0 || isNaN x || isNaN y || isInfinite x || isInfinite y then
            Nothing

        else
            Just ( x, y )


{-| The ring over the canvas. Empty when `anchor` is `Nothing`.
-}
view : Config msg -> Html msg
view config =
    case config.anchor of
        Nothing ->
            text ""

        Just anchor ->
            let
                ( left, top ) =
                    panelPosition anchor config.canvas
            in
            div
                [ Attr.attribute "role" "group"
                , Attr.attribute "aria-label" "Part controls"
                , Attr.attribute "data-context-ring" ""
                , Attr.style "position" "absolute"
                , Attr.style "left" (px left)
                , Attr.style "top" (px top)
                , Attr.style "width" (px panelWidth)
                , Attr.style "box-sizing" "border-box"
                , Attr.style "padding" "0.5rem"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "0.5rem"
                , Attr.style "background" "rgba(5, 19, 29, 0.92)"
                , Attr.style "backdrop-filter" "blur(8px)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.15)"
                , Attr.style "border-radius" "1rem"
                , Attr.style "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.5)"
                , Attr.style "pointer-events" "auto"
                , Attr.style "z-index" "12"
                ]
                [ actionRow config
                , padRow config
                , stepRow config
                ]


px : Float -> String
px value =
    String.fromInt (round value) ++ "px"


{-| The turn, tip, raise, lower, delete and glue buttons.
-}
actionRow : Config msg -> Html msg
actionRow config =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "0.375rem"
        ]
        ([ actionButton config (config.onTurn TurnLeft) "rotate-ccw" "Turn left" "←"
         , actionButton config (config.onTurn TurnRight) "rotate-cw" "Turn right" "→"
         , actionButton config (config.onTurn TipForward) "corner-right-down" "Tip" "↑"
         , actionButton config config.onRaise "arrow-up" "Raise" "E"
         , actionButton config config.onLower "arrow-down" "Lower" "Q"
         , actionButton config config.onDelete "trash-2" "Delete" "Delete"
         ]
            ++ (if config.free then
                    [ glueButton config ]

                else
                    []
               )
        )


actionButton : Config msg -> msg -> String -> String -> String -> Html msg
actionButton config msg icon label shortcut =
    roundButton
        { noOp = config.noOp
        , enabled = config.movable
        , onPress = msg
        , label = label
        , tooltip = label ++ " (" ++ shortcut ++ ")"
        , pressed = Nothing
        }
        (Icon.featherSized 18 icon)


{-| The Glue toggle: `aria-pressed` is on while the selection is glued, and a
press glues the unglued parts or unglues the glued ones.
-}
glueButton : Config msg -> Html msg
glueButton config =
    roundButton
        { noOp = config.noOp
        , enabled = True
        , onPress = config.onGlue
        , label =
            if config.glued then
                "Glued"

            else
                "Glue"
        , tooltip = "Glue the selection to the board, locked (L)"
        , pressed = Just config.glued
        }
        (Icon.featherSized 18
            (if config.glued then
                "lock"

             else
                "unlock"
            )
        )


{-| The WASD and QE pad: top row Q W E, bottom row A S D.
-}
padRow : Config msg -> Html msg
padRow config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Move the part"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(3, 2.75rem)"
        , Attr.style "gap" "0.375rem"
        ]
        [ padKey config Down "Move down" "Q"
        , padKey config Away "Move away" "W"
        , padKey config Up "Move up" "E"
        , padKey config Left "Move left" "A"
        , padKey config Toward "Move toward" "S"
        , padKey config Right "Move right" "D"
        ]


padKey : Config msg -> NudgeDirection -> String -> String -> Html msg
padKey config direction name key =
    roundButton
        { noOp = config.noOp
        , enabled = config.movable
        , onPress = config.onMove direction
        , label = name ++ " (" ++ key ++ ")"
        , tooltip = name ++ " (" ++ key ++ ")"
        , pressed = Nothing
        }
        (span [ Attr.attribute "aria-hidden" "true", Attr.style "font-weight" "700" ] [ text key ])


{-| The move-step control: three segments, one pressed.
-}
stepRow : Config msg -> Html msg
stepRow config =
    div
        [ Attr.attribute "role" "group"
        , Attr.attribute "aria-label" "Move step"
        , Attr.style "display" "flex"
        , Attr.style "gap" "2px"
        , Attr.style "padding" "2px"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" "rgba(255, 255, 255, 0.08)"
        ]
        (List.map (stepSegment config) Step.all)


stepSegment : Config msg -> Step.StepSize -> Html msg
stepSegment config size =
    let
        pressed =
            config.step == size
    in
    button
        [ Attr.type_ "button"
        , Attr.attribute "aria-label" (stepName size)
        , Attr.attribute "aria-pressed" (boolText pressed)
        , Attr.title (Step.title size)
        , onClick (config.onStep size)
        , Focus.keepFocusOnCanvas config.noOp
        , Attr.style "min-width" "4.25rem"
        , Attr.style "min-height" "2.75rem"
        , Attr.style "padding" "0 0.5rem"
        , Attr.style "border" "none"
        , Attr.style "border-radius" "9999px"
        , Attr.style "background" (fill pressed)
        , Attr.style "color" (ink pressed)
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "700"
        , Attr.style "cursor" "pointer"
        ]
        [ text (Step.label size) ]


stepName : Step.StepSize -> String
stepName size =
    case size of
        Step.Stud ->
            "Stud step"

        Step.HalfStud ->
            "Half-stud step"

        Step.Bit ->
            "Bit step"


{-| A 44 px round button. Disabled buttons are dimmed and send nothing.
-}
roundButton :
    { noOp : msg
    , enabled : Bool
    , onPress : msg
    , label : String
    , tooltip : String
    , pressed : Maybe Bool
    }
    -> Html msg
    -> Html msg
roundButton spec content =
    button
        ([ Attr.type_ "button"
         , Attr.attribute "aria-label" spec.label
         , Attr.title spec.tooltip
         , Attr.disabled (not spec.enabled)
         , Focus.keepFocusOnCanvas spec.noOp
         , Attr.style "width" "2.75rem"
         , Attr.style "height" "2.75rem"
         , Attr.style "min-width" "2.75rem"
         , Attr.style "min-height" "2.75rem"
         , Attr.style "padding" "0"
         , Attr.style "display" "flex"
         , Attr.style "align-items" "center"
         , Attr.style "justify-content" "center"
         , Attr.style "border" "none"
         , Attr.style "border-radius" "9999px"
         , Attr.style "background" (fill (spec.pressed == Just True))
         , Attr.style "color" (ink (spec.pressed == Just True))
         , Attr.style "font-size" "0.875rem"
         , Attr.style "opacity" (opacity spec.enabled)
         , Attr.style "cursor" (cursor spec.enabled)
         ]
            ++ (case spec.pressed of
                    Just pressed ->
                        [ Attr.attribute "aria-pressed" (boolText pressed) ]

                    Nothing ->
                        []
               )
            ++ (if spec.enabled then
                    [ onClick spec.onPress ]

                else
                    []
               )
        )
        [ content ]


fill : Bool -> String
fill on =
    if on then
        "var(--color-brand-yellow)"

    else
        "rgba(255, 255, 255, 0.1)"


ink : Bool -> String
ink on =
    if on then
        "var(--color-brand)"

    else
        "var(--color-text-on-dark)"


opacity : Bool -> String
opacity enabled =
    if enabled then
        "1"

    else
        "0.4"


cursor : Bool -> String
cursor enabled =
    if enabled then
        "pointer"

    else
        "not-allowed"


boolText : Bool -> String
boolText on =
    if on then
        "true"

    else
        "false"
