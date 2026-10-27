module Studio.SideSnap exposing (approachMatrix, targetsNearRay)

{-| Side-stud magnetic snapping for SNOT building techniques: detects side connection
points near the cursor ray and computes approach poses for the snap solver.
-}

import LDraw.Connectivity as Connectivity
    exposing
        ( ConnectionPoint
        , ConnectionType(..)
        , WorldConnectionPoint
        , isCompatible
        )
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


{-| Side connection points (|normal.y| < 0.3) of other parts that the cursor
ray passes within `reach` LDU of, nearest to the eye first.
-}
targetsNearRay : { origin : Vec3, direction : Vec3 } -> Float -> List WorldConnectionPoint -> List WorldConnectionPoint
targetsNearRay ray reach targets =
    let
        dirLen =
            Vec3.length ray.direction
    in
    if dirLen < 1.0e-6 then
        []

    else
        let
            unitDir =
                Vec3.scale (1 / dirLen) ray.direction

            sideTargets =
                List.filter (\tp -> abs (Vec3.getY tp.worldNormal) < 0.3) targets

            distAndTarget tp =
                let
                    v =
                        Vec3.sub tp.worldPosition ray.origin

                    t =
                        Vec3.dot v unitDir
                in
                if t <= 0 then
                    Nothing

                else
                    let
                        closestOnRay =
                            Vec3.add ray.origin (Vec3.scale t unitDir)

                        perpDist =
                            Vec3.distance tp.worldPosition closestOnRay
                    in
                    if perpDist <= reach then
                        Just ( t, perpDist, tp )

                    else
                        Nothing
        in
        sideTargets
            |> List.filterMap distAndTarget
            |> List.sortBy (\( t, _, _ ) -> t)
            |> List.map (\( _, _, tp ) -> tp)


{-| The lead matrix moved so its nearest compatible point sits on the target:
the pose that solveSnapCandidates then refines and re-orients.
-}
approachMatrix : Mat4 -> List ConnectionPoint -> WorldConnectionPoint -> Maybe Mat4
approachMatrix leadMatrix draggedPoints target =
    let
        compatiblePoints =
            List.filter (\dp -> isCompatible dp.connType target.connType) draggedPoints

        withDistance dp =
            let
                worldPos =
                    Mat4.transform leadMatrix dp.localPosition

                d =
                    Vec3.distance worldPos target.worldPosition
            in
            ( d, dp, worldPos )
    in
    case List.sortBy (\( d, _, _ ) -> d) (List.map withDistance compatiblePoints) of
        ( _, _, nearestWorldPos ) :: _ ->
            let
                delta =
                    Vec3.sub target.worldPosition nearestWorldPos
            in
            Just (Mat4.mul (Mat4.makeTranslate delta) leadMatrix)

        [] ->
            Nothing
