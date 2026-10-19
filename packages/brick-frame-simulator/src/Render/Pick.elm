module Render.Pick exposing (isSphereVisible, raySphereHit, screenRay)

{-| Camera-space tests shared by the viewport and the simulation: is a bounding
sphere on screen, and which world-space ray does a screen point look along.

Picking a placement is a ray test against its bounding sphere. What a click
_does_ stays with the host application.

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
