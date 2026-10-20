module Studio.Gizmo exposing
    ( Handle
    , dragAmount
    , handles
    , handlesWithFrame
    , hit
    , project
    , toRender
    )

{-| The move gizmo: three axis handles at the selection's centre.

Handles are laid out in the document's coordinates (LDraw, Y down) but hit-tested
and dragged on screen. Their length is a fraction of the distance to the eye, so
they stay the same size on screen however far the camera is from the model.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera)
import Studio.Edit as Edit exposing (Axis(..))


{-| One axis handle: where it points in the document, how long that is, and
where its two ends fall on screen (canvas pixels).
-}
type alias Handle =
    { axis : Axis
    , origin : Vec3
    , tip : Vec3
    , length : Float
    , screenStart : ( Float, Float )
    , screenEnd : ( Float, Float )
    }


{-| LDraw (Y down) to render space (Y up), the conversion every mesh gets.
-}
toRender : Vec3 -> Vec3
toRender p =
    vec3 (Vec3.getX p) -(Vec3.getY p) -(Vec3.getZ p)


{-| Where a render-space point lands on a canvas of `width` × `height` pixels, or
`Nothing` when it is behind the eye.
-}
project : Camera -> Int -> Int -> Vec3 -> Maybe ( Float, Float )
project camera width height point =
    let
        aspect =
            toFloat width / max 1 (toFloat height)

        viewProjection =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera))
                (Camera.viewMatrix camera)

        -- Perspective division happens inside `Mat4.transform`, so this is
        -- normalised device coordinates; check the point is in front of the eye
        -- first, because behind it the division flips the sign of everything.
        eyeSpace =
            Mat4.transform (Camera.viewMatrix camera) point

        ndc =
            Mat4.transform viewProjection point
    in
    if Vec3.getZ eyeSpace >= 0 then
        Nothing

    else
        Just
            ( (Vec3.getX ndc + 1) / 2 * toFloat width
            , (1 - Vec3.getY ndc) / 2 * toFloat height
            )


{-| The three handles for a selection centred at `center` (document coordinates).
-}
handles : Camera -> Int -> Int -> Vec3 -> List Handle
handles camera width height center =
    handlesWithFrame Mat4.identity camera width height center


{-| The three handles for a selection centred at `localCenter` inside a
coordinate frame transformed by `frameTransform` (document coordinates).
-}
handlesWithFrame : Mat4 -> Camera -> Int -> Int -> Vec3 -> List Handle
handlesWithFrame frameTransform camera width height localCenter =
    let
        worldOrigin =
            Mat4.transform frameTransform localCenter

        eye =
            Camera.position camera

        length =
            0.14 * max 1 (Vec3.distance eye (toRender worldOrigin))

        make axis =
            let
                localTip =
                    Vec3.add localCenter (Vec3.scale length (Edit.axisVector axis))

                worldTip =
                    Mat4.transform frameTransform localTip
            in
            Maybe.map2
                (\start end ->
                    { axis = axis
                    , origin = worldOrigin
                    , tip = worldTip
                    , length = length
                    , screenStart = start
                    , screenEnd = end
                    }
                )
                (project camera width height (toRender worldOrigin))
                (project camera width height (toRender worldTip))
    in
    List.filterMap make [ AxisX, AxisY, AxisZ ]


{-| The handle under a screen point, within `tolerance` pixels of its line, the
nearest first.
-}
hit : Float -> ( Float, Float ) -> List Handle -> Maybe Handle
hit tolerance point candidates =
    candidates
        |> List.filterMap
            (\h ->
                let
                    d =
                        distanceToSegment point h.screenStart h.screenEnd
                in
                if d <= tolerance then
                    Just ( d, h )

                else
                    Nothing
            )
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.map Tuple.second


{-| How far along a handle's axis a drag from `start` to `now` has gone, in
document units, snapped to `step`.
-}
dragAmount : Float -> Handle -> ( Float, Float ) -> ( Float, Float ) -> Float
dragAmount step handle ( sx, sy ) ( x, y ) =
    let
        ( ax, ay ) =
            handle.screenStart

        ( bx, by ) =
            handle.screenEnd

        ( dx, dy ) =
            ( bx - ax, by - ay )

        lengthSquared =
            dx * dx + dy * dy
    in
    if lengthSquared < 1 then
        0

    else
        -- Mouse motion projected onto the handle's on-screen direction, scaled
        -- by how many document units that many pixels stand for.
        Edit.snap step (((x - sx) * dx + (y - sy) * dy) / lengthSquared * handle.length)


distanceToSegment : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
distanceToSegment ( px, py ) ( ax, ay ) ( bx, by ) =
    let
        ( dx, dy ) =
            ( bx - ax, by - ay )

        lengthSquared =
            dx * dx + dy * dy

        t =
            if lengthSquared == 0 then
                0

            else
                clamp 0 1 (((px - ax) * dx + (py - ay) * dy) / lengthSquared)

        ( cx, cy ) =
            ( ax + t * dx, ay + t * dy )
    in
    sqrt ((px - cx) * (px - cx) + (py - cy) * (py - cy))
