module Studio.Hints exposing (Hints, analyse, empty, summary)

{-| Live gear hints while editing: which gears cannot turn. Which gears mesh
is no longer tracked separately.

Only gear parts placed directly in the main model are considered — gears inside a
submodel are found by the full analysis the preview runs. The check is a pass over
the placements plus `Gear.Detect.buildGearGraph`, whose cost grows with the number
of gears rather than the size of the model, but the workspace still waits for
editing to pause before running it.

-}

import Dict
import Gear.Detect as Detect
import Gear.Types exposing (GearSpec)
import LDraw.Document as Document exposing (Document)
import LDraw.Types exposing (LDrawLine(..))
import Set exposing (Set)


{-| Placements (by document id) that are gears, tracking isolated gears that
cannot turn. Which gears mesh is no longer tracked separately.
-}
type alias Hints =
    { gears : Int
    , isolated : Set Int
    }


empty : Hints
empty =
    { gears = 0, isolated = Set.empty }


{-| How many gears are looked at, at most. A model with more is not a puzzle
about whether two gears touch.
-}
maxGears : Int
maxGears =
    300


{-| Find the gears in a document and which of them cannot turn. Which gears mesh
is no longer tracked separately.
-}
analyse : List GearSpec -> Document -> Hints
analyse specs document =
    let
        isGear placement =
            List.any (\spec -> spec.partFile == placement.part) specs

        gearPlacements =
            Document.mainPlacements document
                |> List.filter isGear
                |> List.take maxGears

        -- One instance per placement, remembering which placement it came from.
        sourced =
            gearPlacements
                |> List.filterMap
                    (\placement ->
                        Detect.extractGears specs
                            [ SubFileRef { color = placement.color, transform = placement.matrix, file = placement.part } ]
                            Dict.empty
                            |> List.head
                            |> Maybe.map (\instance -> ( placement.id, instance ))
                    )

        instances =
            List.indexedMap (\index ( _, instance ) -> { instance | id = index }) sourced

        graph =
            Detect.buildGearGraph instances

        connected index =
            Dict.get index graph.connections
                |> Maybe.map (not << List.isEmpty)
                |> Maybe.withDefault False

        isolated =
            sourced
                |> List.indexedMap (\index ( source, _ ) -> ( source, connected index ))
                |> List.filter (not << Tuple.second)
                |> List.map Tuple.first
                |> Set.fromList
    in
    { gears = List.length sourced
    , isolated = isolated
    }


{-| A line for the top bar, or nothing to say.
-}
summary : Hints -> String
summary hints =
    if hints.gears == 0 then
        ""

    else if Set.isEmpty hints.isolated then
        "⚙ " ++ plural hints.gears "gear" ++ ", all meshed"

    else
        "⚙ "
            ++ plural hints.gears "gear"
            ++ ", "
            ++ String.fromInt (Set.size hints.isolated)
            ++ " can't turn"


plural : Int -> String -> String
plural count noun =
    String.fromInt count
        ++ " "
        ++ noun
        ++ (if count == 1 then
                ""

            else
                "s"
           )
