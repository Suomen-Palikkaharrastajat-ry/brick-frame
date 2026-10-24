module Render.Pick exposing
    ( isSphereVisible
    , nearestTriangleHit
    , nearestTriangleHitBy
    , nearestTriangleHitByFrom
    , rayAabbHit
    , rayAabbHitFrom
    , raySphereHit
    , rayTriangleHit
    , screenRay
    )

{-| Camera-space tests shared by the viewport and the simulation: is a bounding
sphere on screen, and which world-space ray does a screen point look along.

Picking a placement narrows by bounding sphere and box, then settles on the
nearest actual triangle. What a click _does_ stays with the host application.

-}

import Math.Vector3 as Vec3
import Render.Camera as Camera exposing (Camera)


{-| Whether a bounding sphere intersects the camera frustum (conservatively).
`aspect` is canvas width over height.
-}
isSphereVisible : Camera -> Float -> Vec3.Vec3 -> Float -> Bool
isSphereVisible camera aspect center radius =
    let
        eye =
            Camera.position camera

        forwardRaw =
            Vec3.sub camera.target eye

        forwardLen =
            Vec3.length forwardRaw
    in
    if forwardLen < 1.0e-6 then
        True

    else
        let
            forward =
                Vec3.scale (1 / forwardLen) forwardRaw

            worldUp =
                Vec3.vec3 0 1 0

            rightRaw =
                Vec3.cross forward worldUp

            rightLen =
                Vec3.length rightRaw
        in
        if rightLen < 1.0e-6 then
            True

        else
            let
                right =
                    Vec3.scale (1 / rightLen) rightRaw

                upRaw =
                    Vec3.cross right forward

                upLen =
                    Vec3.length upRaw

                up =
                    if upLen < 1.0e-6 then
                        worldUp

                    else
                        Vec3.scale (1 / upLen) upRaw

                toCenter =
                    Vec3.sub center eye

                z =
                    Vec3.dot toCenter forward

                x =
                    Vec3.dot toCenter right

                y =
                    Vec3.dot toCenter up

                nearPlane =
                    Camera.nearPlane camera

                farPlane =
                    Camera.farPlane camera

                tanHalfFov =
                    tan (degrees Camera.fovYDegrees / 2)

                maxX =
                    z * tanHalfFov * aspect + radius

                maxY =
                    z * tanHalfFov + radius
            in
            z
                >= nearPlane
                - radius
                && z
                <= farPlane
                + radius
                && abs x
                <= maxX
                && abs y
                <= maxY


{-| The world-space ray (origin, unit direction) through a screen point, for a
canvas of `width` x `height` pixels. `Nothing` for a degenerate canvas or camera.
-}
screenRay : Camera -> Int -> Int -> Float -> Float -> Maybe ( Vec3.Vec3, Vec3.Vec3 )
screenRay camera width height mouseX mouseY =
    if width <= 0 || height <= 0 then
        Nothing

    else
        let
            widthF =
                toFloat width

            heightF =
                toFloat height

            ndcX =
                (2 * mouseX / widthF) - 1

            ndcY =
                1 - (2 * mouseY / heightF)

            eye =
                Camera.position camera

            forwardRaw =
                Vec3.sub camera.target eye

            forwardLen =
                Vec3.length forwardRaw
        in
        if forwardLen < 1.0e-6 then
            Nothing

        else
            let
                forward =
                    Vec3.scale (1 / forwardLen) forwardRaw

                worldUp =
                    Vec3.vec3 0 1 0

                rightRaw =
                    Vec3.cross forward worldUp

                rightLen =
                    Vec3.length rightRaw
            in
            if rightLen < 1.0e-6 then
                Nothing

            else
                let
                    right =
                        Vec3.scale (1 / rightLen) rightRaw

                    upRaw =
                        Vec3.cross right forward

                    upLen =
                        Vec3.length upRaw

                    up =
                        if upLen < 1.0e-6 then
                            worldUp

                        else
                            Vec3.scale (1 / upLen) upRaw

                    aspect =
                        widthF / heightF

                    tanHalfFov =
                        tan (degrees Camera.fovYDegrees / 2)

                    dirRaw =
                        Vec3.add
                            (Vec3.add forward (Vec3.scale (ndcX * aspect * tanHalfFov) right))
                            (Vec3.scale (ndcY * tanHalfFov) up)

                    dirLen =
                        Vec3.length dirRaw
                in
                if dirLen < 1.0e-6 then
                    Nothing

                else
                    Just ( eye, Vec3.scale (1 / dirLen) dirRaw )


raySphereHit : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Float -> Maybe Float
raySphereHit rayOrigin rayDir center radius =
    let
        oc =
            Vec3.sub rayOrigin center

        b =
            2 * Vec3.dot oc rayDir

        c =
            Vec3.dot oc oc - (radius * radius)

        disc =
            b * b - 4 * c
    in
    if disc < 0 then
        Nothing

    else
        let
            sqrtDisc =
                sqrt disc

            t1 =
                (-b - sqrtDisc) / 2

            t2 =
                (-b + sqrtDisc) / 2
        in
        if t1 > 0 then
            Just t1

        else if t2 > 0 then
            Just t2

        else
            Nothing


{-| Intersection of a ray (origin, direction) with an axis-aligned bounding box (min, max).
Returns the distance `t` along the ray to the entry point (at least 0), or `Nothing` if no hit.
-}
rayAabbHit : Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Maybe Float
rayAabbHit =
    rayAabbHitFrom 0


{-| `rayAabbHit` starting from a minimum distance `tMin` along the ray (e.g. Near plane).
Returns the earliest entry `t >= tMin` or `Nothing`.
-}
rayAabbHitFrom : Float -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Vec3.Vec3 -> Maybe Float
rayAabbHitFrom tMin rayOrigin rayDir boxMin boxMax =
    let
        ox =
            Vec3.getX rayOrigin

        oy =
            Vec3.getY rayOrigin

        oz =
            Vec3.getZ rayOrigin

        dx =
            Vec3.getX rayDir

        dy =
            Vec3.getY rayDir

        dz =
            Vec3.getZ rayDir

        minX =
            min (Vec3.getX boxMin) (Vec3.getX boxMax)

        minY =
            min (Vec3.getY boxMin) (Vec3.getY boxMax)

        minZ =
            min (Vec3.getZ boxMin) (Vec3.getZ boxMax)

        maxX =
            max (Vec3.getX boxMin) (Vec3.getX boxMax)

        maxY =
            max (Vec3.getY boxMin) (Vec3.getY boxMax)

        maxZ =
            max (Vec3.getZ boxMin) (Vec3.getZ boxMax)

        axisInterval orig dir lo hi =
            if abs dir < 1.0e-7 then
                if orig < lo || orig > hi then
                    Nothing

                else
                    Just ( -1.0e30, 1.0e30 )

            else
                let
                    invDir =
                        1 / dir

                    t0 =
                        (lo - orig) * invDir

                    t1 =
                        (hi - orig) * invDir
                in
                Just ( min t0 t1, max t0 t1 )
    in
    case ( axisInterval ox dx minX maxX, axisInterval oy dy minY maxY, axisInterval oz dz minZ maxZ ) of
        ( Just ( tNearX, tFarX ), Just ( tNearY, tFarY ), Just ( tNearZ, tFarZ ) ) ->
            let
                tEnter =
                    max (max tNearX tNearY) tNearZ

                tExit =
                    min (min tFarX tFarY) tFarZ
            in
            if tEnter <= tExit && tExit >= tMin then
                if tEnter >= tMin then
                    Just tEnter

                else
                    Just tMin

            else
                Nothing

        _ ->
            Nothing


{-| Distance `t` along a ray (origin, direction) to a triangle, Möller–Trumbore.
Both faces count: picking must not depend on winding or BFC certification.
-}
rayTriangleHit : Vec3.Vec3 -> Vec3.Vec3 -> ( Vec3.Vec3, Vec3.Vec3, Vec3.Vec3 ) -> Maybe Float
rayTriangleHit rayOrigin rayDir ( a, b, c ) =
    let
        edge1 =
            Vec3.sub b a

        edge2 =
            Vec3.sub c a

        pVec =
            Vec3.cross rayDir edge2

        det =
            Vec3.dot edge1 pVec
    in
    if abs det < 1.0e-12 then
        Nothing

    else
        let
            invDet =
                1 / det

            tVec =
                Vec3.sub rayOrigin a

            u =
                Vec3.dot tVec pVec * invDet
        in
        if u < 0 || u > 1 then
            Nothing

        else
            let
                qVec =
                    Vec3.cross tVec edge1

                v =
                    Vec3.dot rayDir qVec * invDet
            in
            if v < 0 || u + v > 1 then
                Nothing

            else
                let
                    t =
                        Vec3.dot edge2 qVec * invDet
                in
                if t > 0 then
                    Just t

                else
                    Nothing


{-| The nearest of `rayTriangleHit` over a list of triangles.
-}
nearestTriangleHit : Vec3.Vec3 -> Vec3.Vec3 -> List ( Vec3.Vec3, Vec3.Vec3, Vec3.Vec3 ) -> Maybe Float
nearestTriangleHit =
    nearestTriangleHitBy identity


{-| `nearestTriangleHit` over triangles of any vertex type, given its position.
Lets a caller test a mesh's own vertex list without copying out positions.
-}
nearestTriangleHitBy : (v -> Vec3.Vec3) -> Vec3.Vec3 -> Vec3.Vec3 -> List ( v, v, v ) -> Maybe Float
nearestTriangleHitBy =
    nearestTriangleHitByFrom 0


{-| `nearestTriangleHitBy` starting from a minimum distance `tMin` along the ray (e.g. Near plane).
Avoids vector allocations in the inner loop and cuts off testing when a hit is farther than
the best hit so far.
-}
nearestTriangleHitByFrom : Float -> (v -> Vec3.Vec3) -> Vec3.Vec3 -> Vec3.Vec3 -> List ( v, v, v ) -> Maybe Float
nearestTriangleHitByFrom tMin position rayOrigin rayDir triangles =
    let
        minT =
            max 1.0e-6 tMin

        ox =
            Vec3.getX rayOrigin

        oy =
            Vec3.getY rayOrigin

        oz =
            Vec3.getZ rayOrigin

        dx =
            Vec3.getX rayDir

        dy =
            Vec3.getY rayDir

        dz =
            Vec3.getZ rayDir
    in
    List.foldl
        (\( va, vb, vc ) best ->
            let
                maxT =
                    case best of
                        Just nearest ->
                            nearest

                        Nothing ->
                            1.0e30

                a =
                    position va

                b =
                    position vb

                c =
                    position vc

                ax =
                    Vec3.getX a

                ay =
                    Vec3.getY a

                az =
                    Vec3.getZ a

                bx =
                    Vec3.getX b

                by =
                    Vec3.getY b

                bz =
                    Vec3.getZ b

                cx =
                    Vec3.getX c

                cy =
                    Vec3.getY c

                cz =
                    Vec3.getZ c

                e1x =
                    bx - ax

                e1y =
                    by - ay

                e1z =
                    bz - az

                e2x =
                    cx - ax

                e2y =
                    cy - ay

                e2z =
                    cz - az

                pvx =
                    dy * e2z - dz * e2y

                pvy =
                    dz * e2x - dx * e2z

                pvz =
                    dx * e2y - dy * e2x

                det =
                    e1x * pvx + e1y * pvy + e1z * pvz
            in
            if abs det < 1.0e-12 then
                best

            else
                let
                    invDet =
                        1 / det

                    tvx =
                        ox - ax

                    tvy =
                        oy - ay

                    tvz =
                        oz - az

                    u =
                        (tvx * pvx + tvy * pvy + tvz * pvz) * invDet
                in
                if u < 0 || u > 1 then
                    best

                else
                    let
                        qvx =
                            tvy * e1z - tvz * e1y

                        qvy =
                            tvz * e1x - tvx * e1z

                        qvz =
                            tvx * e1y - tvy * e1x

                        v =
                            (dx * qvx + dy * qvy + dz * qvz) * invDet
                    in
                    if v < 0 || u + v > 1 then
                        best

                    else
                        let
                            t =
                                (e2x * qvx + e2y * qvy + e2z * qvz) * invDet
                        in
                        if t >= minT && t < maxT then
                            Just t

                        else
                            best
        )
        Nothing
        triangles
