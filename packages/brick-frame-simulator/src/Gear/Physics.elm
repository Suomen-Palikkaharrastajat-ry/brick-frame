module Gear.Physics exposing
    ( KinematicLinkage
    , LinkageState
    , angleAt
    , crankSliderPosition
    , crankSliderRodAngle
    , fourBarAngles
    , propagate
    , propagateWithLinkages
    )

{-| Pure gear physics and analytical mechanical linkage simulation.


## Model

Gear simulation is a deterministic function of time, not an incremental
integrator. At any time `t` the motor gear has rotated:

    motorAngle =
        motorSpeedRadPerSec * t

All connected gears rotate at angles determined by the gear ratio from the
motor to that gear via BFS traversal of the `GearGraph`.

Connected mechanical linkages (crank-sliders, four-bar linkages) evaluate analytical
kinematic equations synchronized with driving gears.


## Gear ratios

For each meshing pair (g1 → g2):

    ratio2 =
        ratio1 * -(teeth1 / teeth2)

The sign flips at each external mesh (gears rotate in opposite directions).

Worm gear simplification:
if either side has `teeth == 1`, treat it as a worm mesh and do not invert
sign. The ratio magnitude still follows `teeth1 / teeth2`.

-}

import Array
import Dict exposing (Dict)
import Gear.Types exposing (GearGraph, GearId)
import Set exposing (Set)


{-| Definition of a 1-DOF kinematic crank-slider linkage driven by a gear.
-}
type alias KinematicLinkage =
    { id : Int
    , drivingGearId : GearId
    , crankRadius : Float
    , rodLength : Float
    , crankOffset : Float
    }


{-| Evaluated state of a kinematic linkage at a point in time.
-}
type alias LinkageState =
    { sliderX : Float
    , rodAngle : Float
    }



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


{-| Analytical crank-slider slider displacement x(theta):
x(theta) = r \* cos(theta) + sqrt(l^2 - r^2 \* sin^2(theta))
-}
crankSliderPosition : Float -> Float -> Float -> Float
crankSliderPosition crankRadius rodLength theta =
    let
        sinTheta =
            sin theta

        rSin =
            crankRadius * sinTheta

        underRoot =
            rodLength * rodLength - rSin * rSin
    in
    crankRadius * cos theta + sqrt (max 0.0 underRoot)


{-| Analytical crank-slider connecting rod tilt angle phi(theta):
phi(theta) = asin(-r/l \* sin(theta))
-}
crankSliderRodAngle : Float -> Float -> Float -> Float
crankSliderRodAngle crankRadius rodLength theta =
    if rodLength < 0.0001 then
        0.0

    else
        let
            ratio =
                clamp -1.0 1.0 (-(crankRadius / rodLength) * sin theta)
        in
        asin ratio


{-| Analytical planar four-bar linkage angles (couplerAngle, rockerAngle).
Given link lengths `{ ground, crank, coupler, rocker }` and input crank angle `theta`.
-}
fourBarAngles :
    { ground : Float, crank : Float, coupler : Float, rocker : Float }
    -> Float
    -> Maybe ( Float, Float )
fourBarAngles { ground, crank, coupler, rocker } theta =
    let
        bx =
            crank * cos theta

        by =
            crank * sin theta

        -- Vector from rocker pivot (ground, 0) to crank pin (bx, by)
        sx =
            bx - ground

        sy =
            by

        distSquared =
            sx * sx + sy * sy

        dist =
            sqrt distSquared
    in
    if dist < 0.0001 || rocker < 0.0001 || coupler < 0.0001 then
        Nothing

    else
        let
            cosDelta =
                (rocker * rocker + distSquared - coupler * coupler) / (2 * rocker * dist)
        in
        if cosDelta < -1.0 || cosDelta > 1.0 then
            Nothing

        else
            let
                delta =
                    acos (clamp -1.0 1.0 cosDelta)

                baseAngle =
                    atan2 sy sx

                rockerAngle =
                    baseAngle + delta

                cx =
                    ground + rocker * cos rockerAngle

                cy =
                    rocker * sin rockerAngle

                couplerAngle =
                    atan2 (cy - by) (cx - bx)
            in
            Just ( couplerAngle, rockerAngle )


{-| Propagate rotation through the gear graph and evaluate all driven kinematic linkages.
-}
propagateWithLinkages :
    GearGraph
    -> List KinematicLinkage
    -> GearId
    -> Float
    -> { gearAngles : Dict GearId Float, linkagePositions : Dict Int LinkageState }
propagateWithLinkages graph linkages motorId motorAngle =
    let
        angles =
            propagate graph motorId motorAngle

        evalLinkage linkage =
            let
                theta =
                    angleAt angles linkage.drivingGearId + linkage.crankOffset

                pos =
                    crankSliderPosition linkage.crankRadius linkage.rodLength theta

                rod =
                    crankSliderRodAngle linkage.crankRadius linkage.rodLength theta
            in
            ( linkage.id, { sliderX = pos, rodAngle = rod } )

        linkageDict =
            Dict.fromList (List.map evalLinkage linkages)
    in
    { gearAngles = angles
    , linkagePositions = linkageDict
    }



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
