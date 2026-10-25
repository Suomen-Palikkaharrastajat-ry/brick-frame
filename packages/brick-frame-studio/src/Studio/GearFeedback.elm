module Studio.GearFeedback exposing
    ( GearFeedback
    , GearRelation(..)
    , feedbackText
    , gearFeedback
    , pill
    )

{-| Positioning feedback for a single selected gear: how it sits against its
nearest other gear, judged with the same rules `Gear.Detect.buildGearGraph`
uses (see `Detect.meshFacts`).

`gearFeedback` is pure and works on any `LDraw.Document`, so a host can show it
without a `Studio.Core.Model`. `feedbackText` gives the status line and `pill`
the status pill that Brick Contraptions shows under the canvas.

    gearFeedback [ selectedId ] document
        |> Maybe.map GearFeedback.pill

-}

import Data
import Gear.Detect as Detect
import Gear.Types exposing (GearInstance, GearSpec)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import LDraw.Document as Document exposing (Document, Placement)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3


{-| How the selected gear relates to its nearest other gear.

  - `Meshes` — the two gears mesh.
  - `TooFar` / `TooClose` — the axes are right but the centres are `distance`
    LDU apart where `required` (the sum of the pitch radii) is needed.
  - `NotParallel` — a spur pair whose axes point different ways.
  - `NotPerpendicular` — a crown, bevel or worm pair whose axes are not at
    right angles.
  - `NotSamePlane` — parallel axes, but the gears sit `offset` LDU apart
    along the axle, so their teeth cannot touch.
  - `NoOtherGear` — the selected gear is the only gear.

-}
type GearRelation
    = Meshes
    | TooFar { distance : Float, required : Float }
    | TooClose { distance : Float, required : Float }
    | NotParallel
    | NotPerpendicular
    | NotSamePlane { offset : Float }
    | NoOtherGear


{-| Feedback for the selected gear: its short label (`"24T"`, `"24T crown"`,
`"Worm"`, see `gearLabel`), the nearest other gear's label (when there is one)
and their relation.
-}
type alias GearFeedback =
    { gear : String
    , other : Maybe String
    , relation : GearRelation
    }


{-| Positioning feedback for a selection of placement ids. `Nothing` unless the
selection is exactly one placement and that placement is a known gear.

The partner is the nearest (by centre distance) gear the selected gear meshes
with, if any; otherwise simply the nearest other gear in the main model.

    gearFeedback [ selectedId ] document

-}
gearFeedback : List Int -> Document -> Maybe GearFeedback
gearFeedback selectedIds document =
    case selectedIds of
        [ selectedId ] ->
            let
                gears =
                    List.filterMap
                        (\p -> Maybe.map (\g -> ( p.id, g )) (placementGear p))
                        (Document.mainPlacements document)
            in
            case List.filter (\( id, _ ) -> id == selectedId) gears of
                ( _, selected ) :: _ ->
                    let
                        others =
                            List.filter (\( id, _ ) -> id /= selectedId) gears
                                |> List.map Tuple.second
                                |> List.sortBy (\g -> Vec3.distance g.worldPosition selected.worldPosition)

                        partner =
                            case List.filter (Detect.meshing selected) others of
                                meshed :: _ ->
                                    Just meshed

                                [] ->
                                    List.head others
                    in
                    Just
                        { gear = gearLabel selected.spec
                        , other = Maybe.map (.spec >> gearLabel) partner
                        , relation =
                            partner
                                |> Maybe.map (classify selected)
                                |> Maybe.withDefault NoOtherGear
                        }

                [] ->
                    Nothing

        _ ->
            Nothing


{-| Classify one pair; the order of checks mirrors `Detect.meshing`.
-}
classify : GearInstance -> GearInstance -> GearRelation
classify selected other =
    let
        facts =
            Detect.meshFacts selected other
    in
    if Detect.meshing selected other then
        Meshes

    else if facts.perpendicular && not facts.axisOk then
        NotPerpendicular

    else if not facts.axisOk && facts.absDot < 0.9 then
        NotParallel

    else if not facts.axisOk then
        NotSamePlane { offset = facts.axialOffset }

    else if facts.distance > facts.expected then
        TooFar { distance = facts.distance, required = facts.expected }

    else
        TooClose { distance = facts.distance, required = facts.expected }


{-| A top-level placement as a gear instance, built exactly as
`Detect.extractGears` builds one for a part at the top of the model.
-}
placementGear : Placement -> Maybe GearInstance
placementGear placement =
    gearSpecFor placement.part
        |> Maybe.map
            (\spec ->
                { id = placement.id
                , spec = spec
                , color = placement.color
                , worldPosition = Mat4.transform placement.matrix (Vec3.vec3 0 0 0)
                , worldMatrix = placement.matrix
                }
            )


{-| Short label for a gear: tooth count, plus the kind when it is not a plain
spur gear.
-}
gearLabel : GearSpec -> String
gearLabel spec =
    if spec.teeth == 1 then
        "Worm"

    else if spec.partFile == "3650b.dat" then
        String.fromInt spec.teeth ++ "T crown"

    else if List.member spec.partFile [ "32198.dat", "32269.dat" ] then
        String.fromInt spec.teeth ++ "T bevel"

    else
        String.fromInt spec.teeth ++ "T"


gearSpecFor : String -> Maybe GearSpec
gearSpecFor file =
    List.head (List.filter (\spec -> spec.partFile == file) Data.gearParts)


{-| A short status line for the feedback pill, e.g.
`"24T ↔ 24T: 40 LDU apart — needs 60"` or `"Meshes with 24T ✓"`.
-}
feedbackText : GearFeedback -> String
feedbackText feedback =
    let
        other =
            Maybe.withDefault "?" feedback.other

        pair =
            feedback.gear ++ " ↔ " ++ other ++ ": "

        ldu x =
            String.fromInt (round x)
    in
    case feedback.relation of
        Meshes ->
            "Meshes with " ++ other ++ " ✓"

        TooFar { distance, required } ->
            pair ++ ldu distance ++ " LDU apart — needs " ++ ldu required

        TooClose { distance, required } ->
            pair ++ ldu distance ++ " LDU apart — needs " ++ ldu required

        NotParallel ->
            pair ++ "axles not parallel"

        NotPerpendicular ->
            pair ++ "axles must be at right angles"

        NotSamePlane { offset } ->
            pair ++ "not in the same plane (offset " ++ ldu offset ++ ")"

        NoOtherGear ->
            feedback.gear ++ ": add another gear to mesh with"


{-| The status pill for the feedback: a rounded line of text that turns to the
brand yellow border and text once the gear meshes. The host positions it.
-}
pill : GearFeedback -> Html msg
pill feedback =
    let
        meshes =
            feedback.relation == Meshes
    in
    div
        [ Attr.attribute "role" "status"
        , Attr.attribute "aria-live" "polite"
        , Attr.attribute "data-gear-feedback" ""
        , Attr.style "align-self" "center"
        , Attr.style "max-width" "100%"
        , Attr.style "padding" "0.3125rem 0.875rem"
        , Attr.style "background" "rgba(5, 19, 29, 0.85)"
        , Attr.style "backdrop-filter" "blur(8px)"
        , Attr.style "border-radius" "9999px"
        , Attr.style "border"
            (if meshes then
                "1px solid var(--color-brand-yellow)"

             else
                "1px solid rgba(255, 255, 255, 0.15)"
            )
        , Attr.style "color"
            (if meshes then
                "var(--color-brand-yellow)"

             else
                "var(--color-text-on-dark)"
            )
        , Attr.style "font-size" "0.8125rem"
        , Attr.style "font-weight" "600"
        , Attr.style "white-space" "nowrap"
        , Attr.style "overflow" "hidden"
        , Attr.style "text-overflow" "ellipsis"
        , Attr.style "font-variant-numeric" "tabular-nums"
        ]
        [ text (feedbackText feedback) ]
