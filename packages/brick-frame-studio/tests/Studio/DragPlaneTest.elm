module Studio.DragPlaneTest exposing (suite)

{-| Tests for the view-relative body-drag maths in `Studio.DragPlane`.
-}

import Expect
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Render.Camera as Camera exposing (Camera)
import Render.Pick as Pick
import Studio.DragPlane as DragPlane exposing (Mode(..))
import Test exposing (Test, describe, test)


camera : Float -> Float -> Camera
camera azimuth elevation =
    { azimuth = azimuth, elevation = elevation, distance = 200, target = vec3 0 0 0, dragging = False, lastMousePos = Nothing, minDistance = 0.5, maxDistance = 2000, viewportHeight = 600, navigation = Camera.Orbit, sceneRadius = 0 }


{-| Where the screen ray at pixel (x, y) of an 800×600 canvas meets the plane.
-}
hitAt : Camera -> DragPlane.WorkingPlane -> Float -> Float -> Maybe Vec3
hitAt cam plane x y =
    Pick.screenRay cam 800 600 x y |> Maybe.andThen (DragPlane.intersect plane)


cameraRight : Camera -> Vec3
cameraRight cam =
    vec3 (cos cam.azimuth) 0 -(sin cam.azimuth)


{-| A 1×1 brick: odd on both axes, so its stud offset is 10 on X and Z.
-}
brick : DragPlane.Box
brick =
    { min = vec3 -10 -4 -10, max = vec3 10 24 10 }


{-| The stud offset of `brick`.
-}
one : { x : Float, z : Float }
one =
    { x = 10, z = 10 }


{-| No stud offset: the plain grid (a 2×4, or a Technic part).
-}
zero : { x : Float, z : Float }
zero =
    { x = 0, z = 0 }


{-| A 2×4 brick: even on both axes.
-}
wide : DragPlane.Box
wide =
    { min = vec3 -40 -4 -20, max = vec3 40 24 20 }


at : Float -> Float -> Float -> DragPlane.Box -> DragPlane.Box
at x y z b =
    { min = Vec3.add b.min (vec3 x y z), max = Vec3.add b.max (vec3 x y z) }


suite : Test
suite =
    describe "Studio.DragPlane"
        [ describe "choose"
            [ test "a steep view drags on the ground plane" <|
                \_ -> (DragPlane.choose (camera 0.7 (degrees 20)) (vec3 0 0 0)).mode |> Expect.equal Horizontal
            , test "looking up steeply also drags on the ground plane" <|
                \_ -> (DragPlane.choose (camera 0.7 (degrees -35)) (vec3 0 0 0)).mode |> Expect.equal Horizontal
            , test "a view near the horizon drags on a camera-facing plane" <|
                \_ ->
                    let
                        plane =
                            DragPlane.choose (camera 0.7 (degrees 19)) (vec3 5 6 7)
                    in
                    Expect.all
                        [ \p -> Expect.equal Facing p.mode
                        , \p -> Expect.within (Expect.Absolute 1.0e-9) 0 (Vec3.getY p.normal)
                        , \p -> Expect.equal (vec3 5 6 7) p.point
                        ]
                        plane
            ]
        , describe "intersect"
            [ test "a ray parallel to the plane misses" <|
                \_ ->
                    DragPlane.intersect { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal } ( vec3 0 10 0, vec3 1 0 0 )
                        |> Expect.equal Nothing
            , test "a plane behind the eye misses" <|
                \_ ->
                    DragPlane.intersect { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal } ( vec3 0 10 0, vec3 0 1 0 )
                        |> Expect.equal Nothing
            , test "a ray down meets the ground below" <|
                \_ ->
                    DragPlane.intersect { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal } ( vec3 3 10 4, vec3 0 -2 0 )
                        |> Expect.equal (Just (vec3 3 0 4))
            , test "the centre pixel ray meets the plane at the grab point" <|
                \_ ->
                    let
                        cam =
                            camera 0.7 0.6
                    in
                    case hitAt cam (DragPlane.choose cam (vec3 0 0 0)) 400 300 of
                        Just p ->
                            Vec3.length p |> Expect.within (Expect.Absolute 1.0e-6) 0

                        Nothing ->
                            Expect.fail "expected a hit"
            ]
        , describe "delta direction"
            (List.concatMap
                (\( name, cam ) ->
                    let
                        plane =
                            DragPlane.choose cam (vec3 0 0 0)

                        deltaAt x y =
                            hitAt cam plane x y |> Maybe.withDefault (vec3 0 0 0)
                    in
                    [ test (name ++ ": screen left moves camera-left") <|
                        \_ -> Vec3.dot (deltaAt 300 300) (cameraRight cam) |> Expect.lessThan 0
                    , test (name ++ ": screen right moves camera-right") <|
                        \_ -> Vec3.dot (deltaAt 500 300) (cameraRight cam) |> Expect.greaterThan 0
                    , test (name ++ ": screen up moves away or up, never sideways") <|
                        \_ ->
                            let
                                d =
                                    deltaAt 400 200

                                away =
                                    vec3 -(sin cam.azimuth) 0 -(cos cam.azimuth)
                            in
                            Expect.all
                                [ \_ -> Expect.within (Expect.Absolute 1.0e-6) 0 (Vec3.dot d (cameraRight cam))
                                , \_ ->
                                    (if plane.mode == Horizontal then
                                        Vec3.dot d away

                                     else
                                        Vec3.getY d
                                    )
                                        |> Expect.greaterThan 0
                                ]
                                ()
                    ]
                )
                [ ( "steep", camera 0.7 0.6 ), ( "rotated steep", camera -2.3 0.9 ), ( "horizon", camera 1.9 0.1 ) ]
            )
        , describe "boxesOverlap"
            [ test "overlapping boxes clash" <|
                \_ -> DragPlane.boxesOverlap brick (at 5 0 5 brick) |> Expect.equal True
            , test "side-by-side boxes do not" <|
                \_ -> DragPlane.boxesOverlap brick (at 20 0 0 brick) |> Expect.equal False
            , test "a box stacked on top does not" <|
                \_ -> DragPlane.boxesOverlap brick (at 0 -28 0 brick) |> Expect.equal False
            ]
        , describe "snapTo"
            [ test "an odd stud count (offset 10) snaps to ±10 on the 20 LDU pitch" <|
                \_ ->
                    [ DragPlane.snapTo 10 20 1, DragPlane.snapTo 10 20 -9, DragPlane.snapTo 10 20 25 ]
                        |> Expect.equal [ 10, -10, 30 ]
            , test "an even stud count (offset 0) keeps the plain 20 LDU grid" <|
                \_ ->
                    [ DragPlane.snapTo 0 20 33, DragPlane.snapTo 0 20 -9 ]
                        |> Expect.equal [ 40, 0 ]
            , test "a 1 LDU grid reaches every integer on the lattice, odd ones included" <|
                \_ ->
                    [ DragPlane.snapTo 10 1 37, DragPlane.snapTo 10 1 -7, DragPlane.snapTo 0 1 -4.4 ]
                        |> Expect.equal [ 37, -7, -4 ]
            ]
        , describe "freeSpot"
            [ test "an empty scene snaps the start to the stud lattice" <|
                \_ ->
                    DragPlane.freeSpot { box = brick, offset = one, start = ( 33, -9 ), step = 20, others = [] }
                        |> Expect.equal ( 30, -10 )
            , test "an occupied start moves one step to the nearest free neighbour on the lattice" <|
                \_ ->
                    DragPlane.freeSpot { box = brick, offset = one, start = ( 0, 0 ), step = 20, others = [ brick ] }
                        |> Expect.equal ( 30, 10 )
            , test "searches outward ring by ring past occupied neighbours" <|
                \_ ->
                    let
                        others =
                            List.concatMap (\gx -> List.map (\gz -> at gx 0 gz brick) [ -20, 0, 20 ]) [ -20, 0, 20 ]
                    in
                    DragPlane.freeSpot { box = brick, offset = one, start = ( 0, 0 ), step = 20, others = others }
                        |> Expect.equal ( 50, 10 )
            , test "a part at another height does not block the spot" <|
                \_ ->
                    DragPlane.freeSpot { box = brick, offset = one, start = ( 0, 0 ), step = 20, others = [ at 0 -100 0 brick ] }
                        |> Expect.equal ( 10, 10 )
            , test "an even footprint (2x4, offset 0) keeps the plain grid" <|
                \_ ->
                    DragPlane.freeSpot { box = wide, offset = zero, start = ( 33, -9 ), step = 20, others = [] }
                        |> Expect.equal ( 40, 0 )
            ]
        , describe "freeSpotPlacement"
            [ test "keeps the placement's height, moving only X and Z" <|
                \_ ->
                    let
                        placed =
                            DragPlane.freeSpotPlacement
                                { box = brick
                                , matrix = Mat4.makeTranslate3 0 -60 0
                                , offset = one
                                , start = ( 0, 0 )
                                , gridStep = 20
                                , others = []
                                }

                        origin =
                            Mat4.transform placed (vec3 0 0 0)
                    in
                    Expect.equal ( 10, -60, 10 ) ( Vec3.getX origin, Vec3.getY origin, Vec3.getZ origin )
            , test "finds the same free lattice spot as freeSpot" <|
                \_ ->
                    let
                        placed =
                            DragPlane.freeSpotPlacement
                                { box = brick
                                , matrix = Mat4.makeTranslate3 0 -24 0
                                , offset = one
                                , start = ( 0, 0 )
                                , gridStep = 20
                                , others = [ brick ]
                                }

                        origin =
                            Mat4.transform placed (vec3 0 0 0)
                    in
                    Expect.equal ( 30, 10 ) ( Vec3.getX origin, Vec3.getZ origin )
            ]
        , describe "supportTop"
            [ test "nothing below rests on the ground" <|
                \_ -> DragPlane.supportTop (at 0 -24 0 brick) [] |> Expect.equal 0
            , test "an overlapping brick supports at its top (LDraw Y is down)" <|
                \_ -> DragPlane.supportTop (at 0 -60 0 brick) [ at 0 -24 0 brick ] |> Expect.equal -28
            , test "the highest overlapping top wins" <|
                \_ -> DragPlane.supportTop (at 0 -90 0 brick) [ at 5 -24 0 brick, at -5 -52 5 brick ] |> Expect.equal -56
            , test "a neighbour that only touches the side does not support" <|
                \_ -> DragPlane.supportTop (at 0 -24 0 brick) [ at 20 -52 0 brick ] |> Expect.equal 0
            ]
        , describe "targetPosition"
            [ test "horizontal: the grabbed point follows the cursor and the part rests on support" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 42 0 -17
                        , original = vec3 0 -24 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = one
                        , elevationOffset = 0
                        , footprint = at 0 -24 0 brick
                        , others = [ at 40 -24 20 brick ]
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 50 -52 10)
            , test "a 1×1 lands on the stud lattice, not the plain grid" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 3 0 3
                        , original = vec3 0 -24 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = one
                        , elevationOffset = 0
                        , footprint = at 0 -24 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 10 -24 -10)
            , test "a 2×4 (offset 0) keeps the plain 20 LDU grid" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 33 0 -7
                        , original = vec3 0 -24 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at 0 -24 0 wide
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 40 -24 0)
            , test "an X offset of 10 (a 1×2 turned 90°) puts X on ±10 while Z stays on the 0 grid" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 3 0 3
                        , original = vec3 0 -24 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = { x = 10, z = 0 }
                        , elevationOffset = 0
                        , footprint = at 0 -24 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 10 -24 0)
            , test "a 1 LDU grid reaches odd integers on the lattice" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 33 0 -7
                        , original = vec3 0 -24 0
                        , gridStep = 1
                        , verticalStep = 1
                        , offset = one
                        , elevationOffset = 0
                        , footprint = at 0 -24 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 33 -24 7)
            , test "horizontal: the wheel lift applies on top of the support" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 0 0 0
                        , original = vec3 0 -24 0
                        , gridStep = 1
                        , verticalStep = 1
                        , offset = zero
                        , elevationOffset = -8
                        , footprint = at 0 -24 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 0 -32 0)
            , test "facing: height follows the plane but never goes below ground" <|
                \_ ->
                    let
                        target cursorY =
                            DragPlane.targetPosition
                                { plane = { point = vec3 0 0 0, normal = vec3 0 0 1, mode = Facing }
                                , cursor = vec3 0 cursorY 0
                                , original = vec3 0 -24 0
                                , gridStep = 4
                                , verticalStep = 4
                                , offset = zero
                                , elevationOffset = 0
                                , footprint = at 0 -24 0 brick
                                , others = []
                                , keepHeight = False
                                }
                    in
                    Expect.all
                        [ \_ -> Expect.equal (vec3 0 -64 0) (target 40)
                        , \_ -> Expect.equal (vec3 0 -24 0) (target -40)
                        ]
                        ()
            , test "horizontal: a part sliding off a brick drops to the ground" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 60 0 0
                        , original = vec3 0 -52 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at 0 -52 0 brick
                        , others = [ at 0 -24 0 brick ]
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 60 -24 0)
            , test "horizontal: a part sunk into the ground rests on it after a move" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 0 0 0
                        , original = vec3 -60 0 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at -60 0 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 -60 -24 0)
            , test "horizontal: a part floating above its support settles onto it (no remembered gap)" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 0 0 0
                        , original = vec3 0 -90 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at 0 -90 0 brick
                        , others = [ at 0 -24 0 brick ]
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 0 -52 0)
            , test "horizontal: a floating part slid off its support drops to the ground" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 60 0 0
                        , original = vec3 0 -90 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at 0 -90 0 brick
                        , others = [ at 0 -24 0 brick ]
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 60 -24 0)
            , test "horizontal: wheel lowering never sinks the part below the ground" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 0 0 0
                        , original = vec3 0 -24 0
                        , gridStep = 20
                        , verticalStep = 20
                        , offset = zero
                        , elevationOffset = 16
                        , footprint = at 0 -24 0 brick
                        , others = []
                        , keepHeight = False
                        }
                        |> Expect.equal (vec3 0 -24 0)
            , test "keepHeight holds the original height" <|
                \_ ->
                    DragPlane.targetPosition
                        { plane = { point = vec3 0 0 0, normal = vec3 0 1 0, mode = Horizontal }
                        , cursor = vec3 20 0 0
                        , original = vec3 0 -100 0
                        , gridStep = 1
                        , verticalStep = 1
                        , offset = zero
                        , elevationOffset = 0
                        , footprint = at 0 -100 0 brick
                        , others = []
                        , keepHeight = True
                        }
                        |> Expect.equal (vec3 20 -100 0)
            ]
        , test "worldBox bounds a rotated box" <|
            \_ ->
                DragPlane.worldBox (Mat4.mul (Mat4.makeTranslate3 100 0 0) (Mat4.makeRotate (pi / 2) (vec3 0 1 0))) { min = vec3 -20 0 -10, max = vec3 20 8 10 }
                    |> (\b -> [ Vec3.getX b.min, Vec3.getX b.max, Vec3.getZ b.min, Vec3.getZ b.max ])
                    |> List.map (\v -> toFloat (round v))
                    |> Expect.equal [ 90, 110, -20, 20 ]
        ]
