module GizmoTest exposing (suite)

{-| Tests for the move gizmo's screen geometry.
-}

import Expect
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Render.Camera as Camera
import Studio.Edit exposing (Axis(..))
import Studio.Gizmo as Gizmo
import Test exposing (Test, describe, test)


{-| A camera looking at the origin along -Z, a quarter turn from the default
three-quarter view so axes land on screen in obvious directions.
-}
camera : Camera.Camera
camera =
    let
        base =
            Camera.init
    in
    { base | azimuth = 0, elevation = 0, distance = 200, target = vec3 0 0 0 }


width : Int
width =
    800


height : Int
height =
    600


suite : Test
suite =
    describe "Studio.Gizmo"
        [ describe "project"
            [ test "the camera target is the middle of the canvas" <|
                \_ ->
                    case Gizmo.project camera width height (vec3 0 0 0) of
                        Just ( x, y ) ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.5) 400 x
                                , \_ -> Expect.within (Expect.Absolute 0.5) 300 y
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected a point on screen"
            , test "a point behind the eye is not on screen" <|
                \_ ->
                    Gizmo.project camera width height (vec3 0 0 5000)
                        |> Expect.equal Nothing
            , test "up in the world is up on screen" <|
                \_ ->
                    case ( Gizmo.project camera width height (vec3 0 0 0), Gizmo.project camera width height (vec3 0 30 0) ) of
                        ( Just ( _, y0 ), Just ( _, y1 ) ) ->
                            Expect.lessThan y0 y1

                        _ ->
                            Expect.fail "expected both points on screen"
            ]
        , describe "handles"
            [ test "there is one per axis, starting at the centre" <|
                \_ ->
                    Gizmo.handles camera width height (vec3 0 0 0)
                        |> List.map .axis
                        |> Expect.equal [ AxisX, AxisY, AxisZ ]
            , test "they keep their size on screen as the camera backs off" <|
                \_ ->
                    let
                        screenLength c =
                            Gizmo.handles c width height (vec3 0 0 0)
                                |> List.filter (\h -> h.axis == AxisX)
                                |> List.head
                                |> Maybe.map
                                    (\h ->
                                        let
                                            ( ax, ay ) =
                                                h.screenStart

                                            ( bx, by ) =
                                                h.screenEnd
                                        in
                                        sqrt ((bx - ax) ^ 2 + (by - ay) ^ 2)
                                    )
                                |> Maybe.withDefault 0
                    in
                    Expect.within (Expect.Relative 0.01)
                        (screenLength camera)
                        (screenLength { camera | distance = 800 })
            , test "handlesWithFrame rotates handle directions by the frame's orientation" <|
                \_ ->
                    let
                        rotZ90 =
                            Mat4.makeRotate (degrees 90) (vec3 0 0 1)

                        xHandle =
                            Gizmo.handlesWithFrame rotZ90 camera width height (vec3 0 0 0)
                                |> List.filter (\h -> h.axis == AxisX)
                                |> List.head
                    in
                    case xHandle of
                        Just h ->
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 0.01) 0 (Vec3.getX h.tip)
                                , \_ -> Expect.greaterThan 1 (Vec3.getY h.tip)
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected rotated X handle"
            ]
        , describe "hit"
            [ test "a point on a handle's line picks it" <|
                \_ ->
                    let
                        all =
                            Gizmo.handles camera width height (vec3 0 0 0)

                        onX =
                            all |> List.filter (\h -> h.axis == AxisX) |> List.head
                    in
                    case onX of
                        Just h ->
                            let
                                ( ax, ay ) =
                                    h.screenStart

                                ( bx, by ) =
                                    h.screenEnd
                            in
                            Gizmo.hit 12 ( (ax + bx) / 2, (ay + by) / 2 + 3 ) all
                                |> Maybe.map .axis
                                |> Expect.equal (Just AxisX)

                        Nothing ->
                            Expect.fail "no X handle"
            , test "empty space picks nothing" <|
                \_ ->
                    Gizmo.hit 12 ( 5, 5 ) (Gizmo.handles camera width height (vec3 0 0 0))
                        |> Expect.equal Nothing
            ]
        , describe "dragAmount"
            [ test "dragging along a handle by its on-screen length moves one handle length" <|
                \_ ->
                    case List.head (Gizmo.handles camera width height (vec3 0 0 0)) of
                        Just h ->
                            let
                                ( ax, ay ) =
                                    h.screenStart

                                ( bx, by ) =
                                    h.screenEnd
                            in
                            Gizmo.dragAmount 0 h ( ax, ay ) ( bx, by )
                                |> Expect.within (Expect.Absolute 0.001) h.length

                        Nothing ->
                            Expect.fail "no handle"
            , test "the amount is snapped to the grid" <|
                \_ ->
                    case List.head (Gizmo.handles camera width height (vec3 0 0 0)) of
                        Just h ->
                            let
                                ( ax, ay ) =
                                    h.screenStart

                                ( bx, by ) =
                                    h.screenEnd

                                amount =
                                    Gizmo.dragAmount 8 h ( ax, ay ) ( ax + (bx - ax) * 0.4, ay + (by - ay) * 0.4 )
                            in
                            Expect.within (Expect.Absolute 0.0001) 0 (amount - 8 * toFloat (round (amount / 8)))

                        Nothing ->
                            Expect.fail "no handle"
            , test "sideways mouse motion across a handle does not move it" <|
                \_ ->
                    case List.head (Gizmo.handles camera width height (vec3 0 0 0)) of
                        Just h ->
                            let
                                ( ax, ay ) =
                                    h.screenStart

                                ( bx, by ) =
                                    h.screenEnd

                                -- perpendicular to the handle's screen direction
                                ( px, py ) =
                                    ( -(by - ay), bx - ax )
                            in
                            Gizmo.dragAmount 0 h ( ax, ay ) ( ax + px, ay + py )
                                |> Expect.within (Expect.Absolute 0.001) 0

                        Nothing ->
                            Expect.fail "no handle"
            ]
        ]
