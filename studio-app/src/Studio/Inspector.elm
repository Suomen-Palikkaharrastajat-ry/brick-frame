module Studio.Inspector exposing (Config, Fields, fields, fieldsWithCollisions, view, viewWithCollisions)

{-| The inspector: the selection's position, rotation and colour as editable
fields.

Typing a value and leaving the field (or pressing Enter) sends one message, so
an edit is one undo step however many digits it took to type.

Several placements share one position — their centre — and moving it moves them
all. Rotation is shown for a single placement only, as three angles in degrees;
a placement with scale or mirroring has no angles to show.

-}

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document, Placement)
import Math.Vector3 as Vec3
import Set exposing (Set)
import Studio.Colours as Colours
import Studio.Edit as Edit exposing (Axis(..))
import Studio.Transform as Transform exposing (Euler)
import UI.Theme as Theme


{-| What the inspector can send. Positions and angles are the new value of one
field.
-}
type alias Config msg =
    { describe : String -> String
    , onPosition : Axis -> Float -> msg
    , onRotation : Axis -> Float -> msg
    , onDuplicate : msg
    , onDelete : msg
    }


{-| What is shown for a selection.
-}
type alias Fields =
    { count : Int
    , position : { x : Float, y : Float, z : Float }
    , rotation : Maybe Euler
    , colours : List Int
    , files : List String
    , colliding : Bool
    }


{-| The fields for the selected placements, or `Nothing` when none of the
selected ids is in the document.
-}
fields : Set Int -> Document -> Maybe Fields
fields selection document =
    fieldsWithCollisions Set.empty selection document


{-| The fields for the selected placements including collision status.
-}
fieldsWithCollisions : Set Int -> Set Int -> Document -> Maybe Fields
fieldsWithCollisions collidingIds selection document =
    let
        selected =
            Set.toList selection
                |> List.filterMap (\id -> Document.getPlacement id document)

        hasCollision =
            List.any (\p -> Set.member p.id collidingIds) selected
    in
    case ( selected, Edit.selectionCenter (List.map .id selected) document ) of
        ( first :: rest, Just center ) ->
            Just
                { count = List.length selected
                , position = Vec3.toRecord center
                , rotation =
                    if List.isEmpty rest && Transform.isRigid first.matrix then
                        Just (Transform.eulerDegrees first.matrix)

                    else
                        Nothing
                , colours = selected |> List.map .color |> unique
                , files = selected |> List.map .fileText |> unique
                , colliding = hasCollision
                }

        _ ->
            Nothing


unique : List comparable -> List comparable
unique items =
    Set.toList (Set.fromList items)


view : Config msg -> Document -> Set Int -> Html msg
view config document selection =
    viewWithCollisions Set.empty config document selection


viewWithCollisions : Set Int -> Config msg -> Document -> Set Int -> Html msg
viewWithCollisions collidingIds config document selection =
    case fieldsWithCollisions collidingIds selection document of
        Nothing ->
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "13px", Attr.style "line-height" "1.5" ]
                [ text "Nothing selected. Click a part, or pick one in the structure list." ]

        Just f ->
            div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "10px"
                , Attr.style "font-size" "13px"
                ]
                [ summary config f
                , if f.colliding then
                    div
                        [ Attr.attribute "role" "alert"
                        , Attr.style "padding" "6px 10px"
                        , Attr.style "border-radius" "6px"
                        , Attr.style "border-left" ("3px solid " ++ Theme.brandRed)
                        , Attr.style "background" "color-mix(in srgb, var(--color-brand-red) 8%, transparent)"
                        , Attr.style "color" Theme.textPrimary
                        , Attr.style "font-size" "12px"
                        ]
                        [ text "Overlaps another part" ]

                  else
                    text ""
                , colourLine f
                , group "Position"
                    "LDU"
                    [ numberField "Position" "X" f.position.x (config.onPosition AxisX)
                    , numberField "Position" "Y" f.position.y (config.onPosition AxisY)
                    , numberField "Position" "Z" f.position.z (config.onPosition AxisZ)
                    ]
                , case f.rotation of
                    Just e ->
                        group "Rotation"
                            "degrees"
                            [ numberField "Rotation" "X" e.x (config.onRotation AxisX)
                            , numberField "Rotation" "Y" e.y (config.onRotation AxisY)
                            , numberField "Rotation" "Z" e.z (config.onRotation AxisZ)
                            ]

                    Nothing ->
                        div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ]
                            [ text
                                (if f.count > 1 then
                                    "Select one part to edit its rotation."

                                 else
                                    "This part is scaled or mirrored; it has no simple angles."
                                )
                            ]
                , div [ Attr.style "display" "flex", Attr.style "gap" "6px" ]
                    [ actionButton config.onDuplicate "Duplicate"
                    , actionButton config.onDelete "Delete"
                    ]
                ]


{-| What is selected: the part's name, with its file underneath.
-}
summary : Config msg -> Fields -> Html msg
summary config f =
    let
        fileId file =
            if String.endsWith ".dat" (String.toLower file) then
                String.dropRight 4 file

            else
                file

        name file =
            case config.describe file of
                "" ->
                    fileId file

                description ->
                    description

        line primary secondary =
            div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "2px" ]
                [ div [ Attr.style "color" Theme.textPrimary, Attr.style "font-weight" "600", Attr.style "font-size" "14px", Attr.style "line-height" "1.3" ] [ text primary ]
                , div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ] [ text secondary ]
                ]
    in
    case f.files of
        [ file ] ->
            if f.count == 1 then
                line (name file) ("Part " ++ fileId file)

            else
                line (String.fromInt f.count ++ " × " ++ name file) ("Part " ++ fileId file)

        _ ->
            line (String.fromInt f.count ++ " parts") (String.fromInt (List.length f.files) ++ " different kinds")


colourLine : Fields -> Html msg
colourLine f =
    case f.colours of
        [ code ] ->
            div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "8px" ]
                [ span
                    [ Attr.attribute "aria-hidden" "true"
                    , Attr.style "display" "inline-block"
                    , Attr.style "width" "16px"
                    , Attr.style "height" "16px"
                    , Attr.style "border-radius" "4px"
                    , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
                    , Attr.style "background"
                        (Colours.all
                            |> List.filter (\s -> s.code == code)
                            |> List.head
                            |> Maybe.map .css
                            |> Maybe.withDefault "transparent"
                        )
                    ]
                    []
                , span [ Attr.style "color" Theme.textPrimary ] [ text (Colours.nameOf code) ]
                , span [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ] [ text ("· " ++ String.fromInt code) ]
                ]

        _ ->
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ] [ text "Several colours. Pick one to set them all." ]


{-| Three axis fields under a legend, side by side.
-}
group : String -> String -> List (Html msg) -> Html msg
group title unit children =
    Html.fieldset
        [ Attr.style "border" "none"
        , Attr.style "margin" "0"
        , Attr.style "padding" "0"
        , Attr.style "min-width" "0"
        ]
        [ Html.legend
            [ Attr.style "padding" "0"
            , Attr.style "margin-bottom" "4px"
            , Attr.style "color" Theme.textMuted
            , Attr.style "font-size" "11px"
            , Attr.style "font-weight" "600"
            , Attr.style "text-transform" "uppercase"
            , Attr.style "letter-spacing" "0.06em"
            ]
            [ text title
            , span [ Attr.style "text-transform" "none", Attr.style "font-weight" "400", Attr.style "letter-spacing" "0" ] [ text (" (" ++ unit ++ ")") ]
            ]
        , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(3, minmax(0, 1fr))", Attr.style "gap" "6px" ] children
        ]


{-| One axis value. Typing and leaving the field, or pressing Enter, sends it.
-}
numberField : String -> String -> Float -> (Float -> msg) -> Html msg
numberField quantity axis value toMsg =
    Html.label
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "4px"
        , Attr.style "padding" "0 0 0 8px"
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "background" Theme.panelSurface
        ]
        [ span [ Attr.attribute "aria-hidden" "true", Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px", Attr.style "font-weight" "600" ] [ text axis ]
        , input
            [ Attr.type_ "number"
            , Attr.step "any"
            , Attr.attribute "aria-label" (quantity ++ " " ++ axis)
            , Attr.value (formatNumber value)
            , Html.Events.on "change" (changeDecoder toMsg)
            , Attr.style "flex" "1"
            , Attr.style "width" "100%"
            , Attr.style "min-width" "0"
            , Attr.style "padding" "6px 6px 6px 0"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "border" "none"
            , Attr.style "border-radius" "6px"
            , Attr.style "background" "transparent"
            , Attr.style "color" Theme.textPrimary
            , Attr.style "font" "inherit"
            , Attr.style "font-size" "13px"
            , Attr.style "font-variant-numeric" "tabular-nums"
            ]
            []
        ]


{-| A `change` event whose field holds a number; anything else is ignored.
-}
changeDecoder : (Float -> msg) -> Decode.Decoder msg
changeDecoder toMsg =
    Decode.at [ "target", "value" ] Decode.string
        |> Decode.andThen
            (\raw ->
                case String.toFloat (String.trim raw) of
                    Just value ->
                        Decode.succeed (toMsg value)

                    Nothing ->
                        Decode.fail "not a number"
            )


{-| Two decimals at most, without trailing zeros.
-}
formatNumber : Float -> String
formatNumber value =
    String.fromFloat (toFloat (round (value * 100)) / 100)


actionButton : msg -> String -> Html msg
actionButton msg label =
    button
        [ Attr.type_ "button"
        , Attr.class "ui-button"
        , Html.Events.onClick msg
        , Attr.style "padding" "7px 12px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "font-family" "inherit"
        , Attr.style "font-size" "13px"
        , Attr.style "cursor" "pointer"
        ]
        [ text label ]
