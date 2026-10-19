module Studio.Inspector exposing (Config, Fields, fields, view)

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
    { onPosition : Axis -> Float -> msg
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
    }


{-| The fields for the selected placements, or `Nothing` when none of the
selected ids is in the document.
-}
fields : Set Int -> Document -> Maybe Fields
fields selection document =
    let
        selected =
            Set.toList selection
                |> List.filterMap (\id -> Document.getPlacement id document)
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
                }

        _ ->
            Nothing


unique : List comparable -> List comparable
unique items =
    Set.toList (Set.fromList items)


view : Config msg -> Document -> Set Int -> Html msg
view config document selection =
    case fields selection document of
        Nothing ->
            div [ Attr.style "color" Theme.textMuted, Attr.style "font-size" "12px" ]
                [ text "Nothing selected. Click a part, or pick one in the tree." ]

        Just f ->
            div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "10px"
                , Attr.style "font-family" "monospace"
                , Attr.style "font-size" "12px"
                ]
                [ div [ Attr.style "color" Theme.textPrimary, Attr.style "font-weight" "600" ] [ text (summary f) ]
                , colourLine f
                , group "Position (LDU)"
                    [ numberField "X" f.position.x (config.onPosition AxisX)
                    , numberField "Y" f.position.y (config.onPosition AxisY)
                    , numberField "Z" f.position.z (config.onPosition AxisZ)
                    ]
                , case f.rotation of
                    Just e ->
                        group "Rotation (°)"
                            [ numberField "X" e.x (config.onRotation AxisX)
                            , numberField "Y" e.y (config.onRotation AxisY)
                            , numberField "Z" e.z (config.onRotation AxisZ)
                            ]

                    Nothing ->
                        div [ Attr.style "color" Theme.textMuted ]
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


summary : Fields -> String
summary f =
    case f.files of
        [ file ] ->
            if f.count == 1 then
                file

            else
                String.fromInt f.count ++ " × " ++ file

        _ ->
            String.fromInt f.count ++ " parts"


colourLine : Fields -> Html msg
colourLine f =
    case f.colours of
        [ code ] ->
            div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "8px", Attr.style "color" Theme.textMuted ]
                [ span
                    [ Attr.style "display" "inline-block"
                    , Attr.style "width" "14px"
                    , Attr.style "height" "14px"
                    , Attr.style "border-radius" "3px"
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
                , text ("Colour " ++ String.fromInt code ++ " — pick another in the colour panel")
                ]

        _ ->
            div [ Attr.style "color" Theme.textMuted ] [ text "Several colours — pick one in the colour panel to set them all." ]


group : String -> List (Html msg) -> Html msg
group title children =
    div []
        (div
            [ Attr.style "color" Theme.textMuted
            , Attr.style "text-transform" "uppercase"
            , Attr.style "font-size" "10px"
            , Attr.style "letter-spacing" "0.06em"
            , Attr.style "margin-bottom" "4px"
            ]
            [ text title ]
            :: children
        )


numberField : String -> Float -> (Float -> msg) -> Html msg
numberField label value toMsg =
    div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "8px", Attr.style "margin-bottom" "4px" ]
        [ span [ Attr.style "width" "14px", Attr.style "color" Theme.textMuted ] [ text label ]
        , input
            [ Attr.type_ "number"
            , Attr.step "any"
            , Attr.attribute "aria-label" label
            , Attr.value (formatNumber value)
            , Html.Events.on "change" (changeDecoder toMsg)
            , Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "padding" "4px 6px"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
            , Attr.style "border-radius" "4px"
            , Attr.style "background" Theme.panelSurface
            , Attr.style "color" Theme.textPrimary
            , Attr.style "font-family" "monospace"
            , Attr.style "font-size" "12px"
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
        [ Html.Events.onClick msg
        , Attr.style "padding" "6px 10px"
        , Attr.style "background" Theme.panelSubtleBackground
        , Attr.style "color" Theme.textPrimary
        , Attr.style "border" ("1px solid " ++ Theme.borderDefault)
        , Attr.style "border-radius" "6px"
        , Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "cursor" "pointer"
        ]
        [ text label ]
