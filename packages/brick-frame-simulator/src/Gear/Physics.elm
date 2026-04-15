module Gear.Physics exposing (angleAt, propagate)

{-| Pure gear physics: rotation propagation through a gear graph.

## Model

Gear simulation is a deterministic function of time, not an incremental
integrator. At any time `t` the motor gear has rotated:

    motorAngle = motorSpeedRadPerSec * t

All connected gears rotate at angles determined by the gear ratio from the
motor to that gear via BFS traversal of the `GearGraph`.

## Gear ratios

For each meshing pair (g1 → g2):

    ratio2 = ratio1 * -(teeth1 / teeth2)

The sign flips at each external mesh (gears rotate in opposite directions).

Worm gear simplification:
if either side has `teeth == 1`, treat it as a worm mesh and do not invert
sign. The ratio magnitude still follows `teeth1 / teeth2`.

-}

import Array
import Dict exposing (Dict)
import Gear.Types exposing (GearGraph, GearId)
import Set exposing (Set)


-- ── Public API ────────────────────────────────────────────────────────────────


{-| Propagate rotation from a motor gear through the graph via BFS.

    propagate graph motorId motorAngle

Returns a `Dict GearId Float` mapping each reachable gear to its current
angle in radians. The motor gear itself maps to `motorAngle`.

Gears not reachable from `motorId` are absent from the result.

-}
propagate : GearGraph -> GearId -> Float -> Dict GearId Float
propagate graph motorId motorAngle =
    bfsStep graph
        [ ( motorId, motorAngle ) ]
        (Set.singleton motorId)
        (Dict.singleton motorId motorAngle)


{-| Angle for a single gear given the propagation result.

Returns `0.0` if the gear is not reachable (not connected to the motor).

-}
angleAt : Dict GearId Float -> GearId -> Float
angleAt angles gearId =
    Dict.get gearId angles |> Maybe.withDefault 0.0


-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Iterative BFS over the gear graph using an explicit queue.
-}
bfsStep :
    GearGraph
    -> List ( GearId, Float )
    -> Set GearId
    -> Dict GearId Float
    -> Dict GearId Float
bfsStep graph queue visited angles =
    case queue of
        [] ->
            angles

        ( currentId, currentAngle ) :: rest ->
            let
                neighbours =
                    Dict.get currentId graph.connections |> Maybe.withDefault []

                rigidNeighbours =
                    Dict.get currentId graph.rigidAxles |> Maybe.withDefault []

                unvisited =
                    List.filter (\n -> not (Set.member n visited)) neighbours

                unvisitedRigid =
                    List.filter (\n -> not (Set.member n visited)) rigidNeighbours

                ( newVisited, newAngles, newEntries ) =
                    List.foldl
                        (\neighbourId ( vis, ang, entries ) ->
                            case ( gearTeeth graph currentId, gearTeeth graph neighbourId ) of
                                ( Just t1, Just t2 ) ->
                                    let
                                        neighbourAngle =
                                            currentAngle * meshRatio t1 t2
                                    in
                                    ( Set.insert neighbourId vis
                                    , Dict.insert neighbourId neighbourAngle ang
                                    , ( neighbourId, neighbourAngle ) :: entries
                                    )

                                _ ->
                                    ( vis, ang, entries )
                        )
                        ( visited, angles, [] )
                        unvisited

                ( newVisited2, newAngles2, newEntries2 ) =
                    List.foldl
                        (\neighbourId ( vis, ang, entries ) ->
                            ( Set.insert neighbourId vis
                            , Dict.insert neighbourId currentAngle ang
                            , ( neighbourId, currentAngle ) :: entries
                            )
                        )
                        ( newVisited, newAngles, [] )
                        unvisitedRigid
            in
            bfsStep graph
                (rest ++ List.reverse newEntries ++ List.reverse newEntries2)
                newVisited2
                newAngles2


gearTeeth : GearGraph -> GearId -> Maybe Int
gearTeeth graph gearId =
    Array.get gearId graph.instances
        |> Maybe.map (.spec >> .teeth)


meshRatio : Int -> Int -> Float
meshRatio t1 t2 =
    let
        baseRatio =
            toFloat t1 / toFloat t2
    in
    if t1 == 1 || t2 == 1 then
        baseRatio

    else
        -baseRatio
