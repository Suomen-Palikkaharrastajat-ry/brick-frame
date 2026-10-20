module Studio.Gizmo exposing
    ( ArcHandle
    , DragHandle(..)
    , Handle
    , arcHandles
    , dragAmount
    , dragArcAngle
    , handles
    , handlesWithFrame
    , hit
    , hitArc
    , jointArcHandle
    , orthogonalBasis
    , project
    , toRender
    )

{-| Move and rotation gizmos: linear axis handles and circular arc rotation handles.

Handles are laid out in the document's coordinates (LDraw, Y down) but hit-tested
and dragged on screen. Their size scales with distance to the eye, remaining consistent
on screen regardless of camera zoom.

-}

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera)
import Studio.Edit as Edit exposing (Axis(..))


{-| One linear axis handle: where it points in the document, how long that is, and
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


{-| A circular arc handle for rotational manipulation around an axis.
-}
type alias ArcHandle =
    { axis : Vec3
    , axisKind : Maybe Axis
    , center : Vec3
    , radius : Float
    , screenPoints : List ( Float, Float )
    , screenCenter : ( Float, Float )
    }


{-| Union of drag handle types supported by the gizmo system.
-}
type DragHandle
    = LinearDrag Handle
    | ArcDrag ArcHandle


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


{-| The three linear translation handles for a selection centred at `center`.
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


{-| Find two orthogonal unit vectors perpendicular to a given normal vector.
-}
orthogonalBasis : Vec3 -> ( Vec3, Vec3 )
orthogonalBasis n =
    let
        len =
            Vec3.length n

        unitN =
            if len < 0.0001 then
                vec3 0 1 0

            else
                Vec3.scale (1 / len) n

        up =
            if abs (Vec3.getY unitN) < 0.9 then
                vec3 0 1 0

            else
                vec3 1 0 0

        cross =
            Vec3.cross unitN up

        cLen =
            Vec3.length cross

        u =
            if cLen < 0.0001 then
                vec3 1 0 0

            else
                Vec3.scale (1 / cLen) cross

        v =
            Vec3.cross unitN u
    in
    ( u, v )


{-| Create a rotational arc handle around an arbitrary axis vector at `center`.
-}
jointArcHandle : Camera -> Int -> Int -> Vec3 -> Vec3 -> Maybe ArcHandle
jointArcHandle camera width height center axis =
    let
        eye =
            Camera.position camera

        radius =
            0.14 * max 1 (Vec3.distance eye (toRender center))

        ( u, v ) =
            orthogonalBasis axis

        segments =
            32

        step =
            2 * pi / toFloat segments

        circlePoint i =
            let
                angle =
                    toFloat i * step

                offset =
                    Vec3.add (Vec3.scale (radius * cos angle) u) (Vec3.scale (radius * sin angle) v)
            in
            Vec3.add center offset

        projected =
            List.range 0 segments
                |> List.filterMap (\i -> project camera width height (toRender (circlePoint i)))
    in
    case ( project camera width height (toRender center), projected ) of
        ( Just sc, pts ) ->
            if List.length pts >= segments // 2 then
                Just
                    { axis = axis
                    , axisKind = Nothing
                    , center = center
                    , radius = radius
                    , screenPoints = pts
                    , screenCenter = sc
                    }

            else
                Nothing

        _ ->
            Nothing


{-| The three principal rotational arc handles (X, Y, Z) around `center`.
-}
arcHandles : Camera -> Int -> Int -> Vec3 -> List ArcHandle
arcHandles camera width height center =
    let
        make ( axis, kind ) =
            case jointArcHandle camera width height center axis of
                Just h ->
                    Just { h | axisKind = Just kind }

                Nothing ->
                    Nothing
    in
    List.filterMap make
        [ ( Edit.axisVector AxisX, AxisX )
        , ( Edit.axisVector AxisY, AxisY )
        , ( Edit.axisVector AxisZ, AxisZ )
        ]


{-| The linear handle under a screen point, within `tolerance` pixels.
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


{-| The rotational arc handle under a screen point, within `tolerance` pixels.
-}
hitArc : Float -> ( Float, Float ) -> List ArcHandle -> Maybe ArcHandle
hitArc tolerance point candidates =
    let
        distanceToArc pt arc =
            case arc.screenPoints of
                [] ->
                    1000000

                first :: rest ->
                    let
                        step p2 ( p1, minD ) =
                            ( p2, min minD (distanceToSegment pt p1 p2) )

                        ( _, d ) =
                            List.foldl step ( first, 1000000 ) rest
                    in
                    d
    in
    candidates
        |> List.filterMap
            (\arc ->
                let
                    d =
                        distanceToArc point arc
                in
                if d <= tolerance then
                    Just ( d, arc )

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
        Edit.snap step (((x - sx) * dx + (y - sy) * dy) / lengthSquared * handle.length)


{-| How far around an arc handle's axis a drag from `start` to `now` has turned, in
radians, snapped to `step`.
-}
dragArcAngle : Float -> ArcHandle -> ( Float, Float ) -> ( Float, Float ) -> Float
dragArcAngle step handle ( sx, sy ) ( x, y ) =
    let
        ( cx, cy ) =
            handle.screenCenter

        startDistSq =
            (sx - cx) * (sx - cx) + (sy - cy) * (sy - cy)

        nowDistSq =
            (x - cx) * (x - cx) + (y - cy) * (y - cy)
    in
    if startDistSq < 1.0 || nowDistSq < 1.0 then
        0

    else
        let
            angleAt px py =
                atan2 -(py - cy) (px - cx)

            startA =
                angleAt sx sy

            nowA =
                angleAt x y

            wrap a =
                if a > pi then
                    wrap (a - 2 * pi)

                else if a < -pi then
                    wrap (a + 2 * pi)

                else
                    a

            deltaA =
                wrap (nowA - startA)
        in
        Edit.snap step deltaA


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
