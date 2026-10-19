module Studio.Tree exposing (Row(..), rows, view)

{-| The document's structure: its submodels, and the placements of the main one
grouped by `0 STEP`.

Selecting a row selects that placement. Only the main submodel lists its parts;
the others show their names and sizes, since the studio edits the main model.

-}

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import LDraw.Document as Document exposing (Document, Entry(..))
import Set exposing (Set)
import UI.Theme as Theme


{-| A line of the tree.
-}
type Row
    = SubmodelRow { name : String, parts : Int, isMain : Bool }
    | StepRow Int
    | PartRow { id : Int, file : String, color : Int }
    | MoreRow Int


{-| How many part rows to list before saying how many more there are, so a
city-scale model does not become tens of thousands of DOM nodes.
-}
maxRows : Int
maxRows =
    300


{-| The tree for a document. The first step has no marker of its own: a step
row appears only after the first `0 STEP`.
-}
rows : Document -> List Row
rows document =
    document.submodels
        |> List.indexedMap Tuple.pair
        |> List.concatMap
            (\( index, submodel ) ->
                let
                    parts =
                        submodel.entries
                            |> List.filter
                                (\entry ->
                                    case entry of
                                        PlacementEntry _ ->
                                            True

                                        Raw _ ->
                                            False
                                )
                            |> List.length

                    header =
                        SubmodelRow
                            { name =
                                if submodel.name == "" then
                                    "Model"

                                else
                                    submodel.name
                            , parts = parts
                            , isMain = index == 0
                            }
                in
                if index == 0 then
                    header :: mainRows submodel.entries

                else
                    [ header ]
            )


mainRows : List Entry -> List Row
mainRows entries =
    let
        step entry ( step_, acc, count ) =
            case entry of
                PlacementEntry p ->
                    if count >= maxRows then
                        ( step_, acc, count + 1 )

                    else
                        ( step_, PartRow { id = p.id, file = p.part, color = p.color } :: acc, count + 1 )

                Raw text ->
                    if isStep text then
                        ( step_ + 1, StepRow (step_ + 2) :: acc, count )

                    else
                        ( step_, acc, count )

        ( _, reversed, total ) =
            List.foldl step ( 0, [], 0 ) entries
    in
    if total > maxRows then
        List.reverse (MoreRow (total - maxRows) :: reversed)

    else
        List.reverse reversed


isStep : String -> Bool
isStep text =
    String.words text == [ "0", "STEP" ]


{-| The tree as a list, with the selected placements highlighted.
-}
view : { selection : Set Int, onSelect : Int -> Bool -> msg } -> Document -> Html msg
view config document =
    div
        [ Attr.style "font-family" "monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "color" Theme.textPrimary
        ]
        (List.map (viewRow config) (rows document))


viewRow : { selection : Set Int, onSelect : Int -> Bool -> msg } -> Row -> Html msg
viewRow config row =
    case row of
        SubmodelRow { name, parts, isMain } ->
            div
                [ Attr.style "margin-top" "6px"
                , Attr.style "font-weight" "600"
                , Attr.style "color" Theme.textPrimary
                ]
                [ text
                    (name
                        ++ " · "
                        ++ String.fromInt parts
                        ++ (if parts == 1 then
                                " part"

                            else
                                " parts"
                           )
                        ++ (if isMain then
                                ""

                            else
                                " (not editable yet)"
                           )
                    )
                ]

        StepRow n ->
            div
                [ Attr.style "margin" "4px 0 2px 8px"
                , Attr.style "color" Theme.textMuted
                , Attr.style "text-transform" "uppercase"
                , Attr.style "font-size" "10px"
                , Attr.style "letter-spacing" "0.06em"
                ]
                [ text ("Step " ++ String.fromInt n) ]

        PartRow { id, file, color } ->
            div
                [ Html.Events.on "click" (shiftDecoder id config.onSelect)
                , Attr.style "padding" "2px 8px 2px 16px"
                , Attr.style "cursor" "pointer"
                , Attr.style "border-radius" "4px"
                , Attr.style "background"
                    (if Set.member id config.selection then
                        "color-mix(in srgb, var(--color-brand-yellow) 25%, transparent)"

                     else
                        "transparent"
                    )
                ]
                [ text (file ++ "  ·  colour " ++ String.fromInt color) ]

        MoreRow n ->
            div
                [ Attr.style "padding" "2px 8px 2px 16px"
                , Attr.style "color" Theme.textMuted
                ]
                [ text ("… " ++ String.fromInt n ++ " more") ]


shiftDecoder : Int -> (Int -> Bool -> msg) -> Decode.Decoder msg
shiftDecoder id onSelect =
    Decode.map (onSelect id) (Decode.oneOf [ Decode.field "shiftKey" Decode.bool, Decode.succeed False ])
