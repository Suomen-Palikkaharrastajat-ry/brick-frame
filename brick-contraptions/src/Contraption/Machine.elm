module Contraption.Machine exposing
    ( GearFeedback
    , GearRelation(..)
    , MachineState
    , analyseMachine
    , feedbackText
    , gearFeedback
    , isAccomplished
    )

{-| Technic machine analysis and accomplishment evaluation.

Determines whether all placed gears in the document form a single unified
machine where every gear is meshed and rotating together in real time.

-}

import Data
import Dict exposing (Dict)
import Gear.Detect as Detect
import Gear.Physics as Physics
import Gear.Types exposing (GearGraph, GearId, GearInstance, GearSpec)
import LDraw.Document as Document exposing (Document, Placement)
import LDraw.Resolve exposing (PartCache)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3


{-| Analyzed state of the Technic contraption.
-}
type alias MachineState =
    { gears : List GearInstance
    , gearGraph : GearGraph
    , rotatingCount : Int
    , isSingleMachine : Bool
    , motorGearId : Maybe GearId
    }


{-| Analyze the document for Technic gears and mesh connections.
-}
analyseMachine : Document -> PartCache -> MachineState
analyseMachine document partCache =
    let
        lines =
            Document.toLines document

        gears =
            Detect.extractGears Data.gearParts lines partCache

        graph =
            Detect.buildGearGraph gears
    in
    case gears of
        [] ->
            { gears = []
            , gearGraph = graph
            , rotatingCount = 0
            , isSingleMachine = False
            , motorGearId = Nothing
            }

        firstGear :: _ ->
            let
                motorId =
                    firstGear.id

                -- Propagate with arbitrary angle to identify all reachable gears
                reachedAngles =
                    Physics.propagate graph motorId 1.0

                connectedCount =
                    Dict.size reachedAngles

                singleMachine =
                    List.length gears >= 2 && connectedCount == List.length gears
            in
            { gears = gears
            , gearGraph = graph
            , rotatingCount = connectedCount
            , isSingleMachine = singleMachine
            , motorGearId = Just motorId
            }


{-| Evaluates whether the contraption goal is accomplished:
"brick-contraption play is accomplished when all gears are rotating and part of the same machine".
-}
isAccomplished : MachineState -> Bool
isAccomplished state =
    state.isSingleMachine && state.rotatingCount >= 2 && state.rotatingCount == List.length state.gears



-- ── Gear positioning feedback ─────────────────────────────────────────────────


{-| How the selected gear relates to its nearest other gear, judged with the
same rules `Gear.Detect.buildGearGraph` uses (see `Detect.meshFacts`).

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
