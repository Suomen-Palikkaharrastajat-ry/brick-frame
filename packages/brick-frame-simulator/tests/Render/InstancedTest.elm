module Render.InstancedTest exposing (suite)

{-| Tests for the instanced renderer.

The load-bearing claims are that placement extraction stops at the right level
of the tree (one entry per physical brick, not per primitive), that mirrored
placements are detected so their cull face can be reversed, and that the
frustum and screen-size tests actually reject what they should — an over-eager
cull silently deletes parts of the model, which is much harder to notice than a
slow one.

-}

import Dict
import Expect
import LDraw.Resolve exposing (PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (vec3)
import Math.Vector4 as Vec4
import Render.Camera as Camera
import Render.Instanced as Instanced exposing (Detail(..))
import Render.Style as Style
import Set
import Test exposing (Test, describe, test)



-- ── Fixtures ──────────────────────────────────────────────────────────────────


triangle : LDrawLine
triangle =
    Triangle { color = 16, p1 = vec3 0 0 0, p2 = vec3 10 0 0, p3 = vec3 0 0 10 }


ref : Int -> Mat4.Mat4 -> String -> LDrawLine
ref color transform file =
    SubFileRef { color = color, transform = transform, file = file }


{-| A model with one sub-model holding two bricks, plus a brick at the top level.
-}
cache : LDraw.Resolve.PartCache
cache =
    Dict.fromList
        [ ( "sub.ldr"
          , Loaded
                [ ref 16 Mat4.identity "3001.dat"
                , ref 4 (Mat4.makeTranslate3 20 0 0) "3001.dat"
                ]
          )
        , ( "3001.dat", Loaded [ triangle ] )
        ]


submodels : Set.Set String
submodels =
    Set.fromList [ "sub.ldr" ]


topLevel : List LDrawLine
topLevel =
    [ ref 1 Mat4.identity "sub.ldr"
    , ref 2 (Mat4.makeTranslate3 0 0 40) "3001.dat"
    ]


placements : List Instanced.RawPlacement
placements =
    Instanced.extractPlacements submodels topLevel cache 15


{-| A camera looking at the origin from +Z, framing roughly 200 LDU.
-}
camera : Camera.Camera
camera =
    -- Built from `Camera.init` rather than spelled out, so adding a camera field
    -- does not break this fixture.
    let
        base =
            Camera.init
    in
    { base
        | azimuth = 0
        , elevation = 0
        , distance = 200
        , target = vec3 0 0 0
    }


viewProjection : Mat4.Mat4
viewProjection =
    Mat4.mul
        (Camera.projectionMatrix 1.0 (Camera.nearPlane camera) (Camera.farPlane camera))
        (Camera.viewMatrix camera)


rawBrick : Int -> Mat4.Mat4 -> Instanced.RawPlacement
rawBrick color matrix =
    { part = "3001.dat", color = color, matrix = matrix }


{-| A small copy of the part: its projected radius is about 1/1000 of a full one,
so it is always `Far`.
-}
tiny : Mat4.Mat4 -> Instanced.RawPlacement
tiny matrix =
    rawBrick 2 (Mat4.mul matrix (Mat4.makeScale3 0.001 0.001 0.001))


{-| A camera at the origin looking down -Z from `distance` away, with a fixed
800 x 1000 viewport.
-}
cameraAt : Float -> Vec3.Vec3 -> Camera.Camera
cameraAt distance target =
    let
        base =
            Camera.init
    in
    { base | azimuth = 0, elevation = 0, distance = distance, target = target }


{-| The detail band of every visible placement, and how many entities
`Instanced.render` emits for the same camera.
-}
renderSummary : Camera.Camera -> Instanced.InstancedScene -> ( List Detail, Int )
renderSummary cam scene =
    let
        aspect =
            800 / 1000

        viewProjectionNow =
            Mat4.mul
                (Camera.projectionMatrix aspect (Camera.nearPlane cam) (Camera.farPlane cam))
                (Camera.viewMatrix cam)

        bands =
            Instanced.visiblePlacements
                { viewProjection = viewProjectionNow
                , eye = Camera.position cam
                , viewportHeight = 1000
                , coarsen = False
                }
                scene
                |> List.map Tuple.second

        entities =
            Instanced.render
                { camera = cam
                , style = Style.defaultStyle
                , width = 800
                , height = 1000
                , coarsen = False
                }
                scene
    in
    ( bands, List.length entities )


{-| The spatial cell a placement falls into.
-}
cellOf : Instanced.InstancedScene -> Instanced.Placement -> ( Int, Int, Int )
cellOf scene placement =
    Instanced.chunkKeyFor scene.chunkCell placement.center



-- ── Suite ─────────────────────────────────────────────────────────────────────


suite : Test
suite =
    describe "Render.Instanced"
        [ describe "extractPlacements"
            [ test "recurses into sub-models and stops at library parts" <|
                \_ ->
                    -- Two bricks inside sub.ldr plus one at the top level.
                    Expect.equal 3 (List.length placements)
            , test "does not descend into a part's own primitives" <|
                \_ ->
                    placements
                        |> List.map .part
                        |> Expect.equal [ "3001.dat", "3001.dat", "3001.dat" ]
            , test "colour code 16 inherits from the enclosing reference" <|
                \_ ->
                    -- sub.ldr is referenced in colour 1; its first brick inherits.
                    placements
                        |> List.map .color
                        |> Expect.equal [ 1, 4, 2 ]
            , test "the same part in different colours resolves to one part key" <|
                \_ ->
                    placements
                        |> List.map .part
                        |> Set.fromList
                        |> Set.size
                        |> Expect.equal 1
            , test "sub-model transforms accumulate into the placement matrix" <|
                \_ ->
                    placements
                        |> List.drop 1
                        |> List.head
                        |> Maybe.map (\p -> Vec3.getX (Mat4.transform p.matrix (vec3 0 0 0)))
                        |> Maybe.withDefault -1
                        |> Expect.within (Expect.Absolute 1.0e-6) 20
            , test "a self-referencing sub-model terminates instead of hanging" <|
                \_ ->
                    let
                        cyclic =
                            Dict.fromList
                                [ ( "loop.ldr", Loaded [ ref 16 Mat4.identity "loop.ldr" ] ) ]
                    in
                    Instanced.extractPlacements
                        (Set.fromList [ "loop.ldr" ])
                        [ ref 16 Mat4.identity "loop.ldr" ]
                        cyclic
                        15
                        |> List.length
                        |> Expect.equal 1
            ]
        , describe "build"
            [ test "one mesh per distinct part, shared across its colours" <|
                \_ ->
                    -- Three placements of 3001.dat in three different colours
                    -- must share a single baked mesh; colour is a uniform.
                    Instanced.build cache placements
                        |> .parts
                        |> Dict.size
                        |> Expect.equal 1
            , test "each placement still carries its own resolved colour" <|
                \_ ->
                    Instanced.build cache placements
                        |> .placements
                        |> List.map (\p -> ( Vec4.getX p.color, Vec4.getY p.color, Vec4.getZ p.color ))
                        |> Set.fromList
                        |> Set.size
                        |> Expect.equal 3
            , test "every placement survives when its part is cached" <|
                \_ ->
                    Instanced.build cache placements
                        |> .placements
                        |> List.length
                        |> Expect.equal 3
            , test "a placement whose part is missing is dropped" <|
                \_ ->
                    Instanced.build Dict.empty placements
                        |> .placements
                        |> Expect.equal []
            , test "a mirrored placement is flagged so its cull face can flip" <|
                \_ ->
                    let
                        mirrored =
                            Instanced.extractPlacements
                                Set.empty
                                [ ref 4 (Mat4.makeScale3 -1 1 1) "3001.dat" ]
                                cache
                                15
                    in
                    Instanced.build cache mirrored
                        |> .placements
                        |> List.map .flipped
                        |> Expect.equal [ True ]
            , test "an ordinary placement is not flagged as mirrored" <|
                \_ ->
                    Instanced.build cache placements
                        |> .placements
                        |> List.map .flipped
                        |> Expect.equal [ False, False, False ]
            , test "the Y-up flip alone does not count as a mirror" <|
                \_ ->
                    -- makeScale3 1 -1 -1 has determinant +1: a rotation.
                    Instanced.build cache (Instanced.extractPlacements Set.empty [ ref 4 Mat4.identity "3001.dat" ] cache 15)
                        |> .placements
                        |> List.map .flipped
                        |> Expect.equal [ False ]
            , test "proxy chunks are built for the far band" <|
                \_ ->
                    Instanced.build cache placements
                        |> .proxyChunks
                        |> List.isEmpty
                        |> Expect.equal False
            ]
        , describe "incremental edits"
            [ test "replaceSource adds the placements of a new source" <|
                \_ ->
                    let
                        base =
                            Instanced.build cache placements

                        added =
                            Instanced.replaceSource cache 7 [ rawBrick 4 (Mat4.makeTranslate3 0 0 100) ] base
                    in
                    Expect.all
                        [ \scene -> Expect.equal 4 (List.length scene.placements)
                        , \scene -> Expect.equal [ 7 ] (List.filter (\id -> id == 7) (List.map .source scene.placements))
                        , \scene -> Expect.equal 3 (List.length base.placements)
                        ]
                        added
            , test "replacing a source moves its placement and leaves the others alone" <|
                \_ ->
                    let
                        base =
                            Instanced.build cache [] |> Instanced.replaceSource cache 1 [ rawBrick 4 Mat4.identity ] |> Instanced.replaceSource cache 2 [ rawBrick 4 (Mat4.makeTranslate3 500 0 0) ]

                        moved =
                            Instanced.replaceSource cache 1 [ rawBrick 4 (Mat4.makeTranslate3 0 0 30) ] base

                        centreOf source scene =
                            scene.placements
                                |> List.filter (\p -> p.source == source)
                                |> List.map (.center >> Vec3.toRecord)
                    in
                    Expect.all
                        [ \scene -> Expect.equal (centreOf 2 base) (centreOf 2 scene)
                        , \scene -> Expect.notEqual (centreOf 1 base) (centreOf 1 scene)
                        , \scene -> Expect.equal 2 (List.length scene.placements)
                        ]
                        moved
            , test "removeSource drops the placements and their proxy cell" <|
                \_ ->
                    let
                        one =
                            Instanced.build cache [] |> Instanced.replaceSource cache 1 [ rawBrick 4 Mat4.identity ]
                    in
                    Instanced.removeSource 1 one
                        |> Expect.all
                            [ \scene -> Expect.equal [] scene.placements
                            , \scene -> Expect.equal [] scene.proxyChunks
                            ]
            , test "a source can expand to several placements" <|
                \_ ->
                    Instanced.build cache []
                        |> Instanced.replaceSource cache 3 [ rawBrick 4 Mat4.identity, rawBrick 1 (Mat4.makeTranslate3 20 0 0) ]
                        |> .placements
                        |> List.map .source
                        |> Expect.equal [ 3, 3 ]
            , test "a part missing from the cache is skipped" <|
                \_ ->
                    Instanced.build cache []
                        |> Instanced.replaceSource cache 1 [ { part = "unknown.dat", color = 4, matrix = Mat4.identity } ]
                        |> .placements
                        |> Expect.equal []
            , test "an incremental scene matches a scene built in one go" <|
                \_ ->
                    let
                        raws =
                            [ rawBrick 4 Mat4.identity, rawBrick 1 (Mat4.makeTranslate3 20 0 0), rawBrick 2 (Mat4.makeTranslate3 40 0 0) ]

                        whole =
                            Instanced.build cache raws

                        stepwise =
                            List.indexedMap Tuple.pair raws
                                |> List.foldl (\( i, raw ) scene -> Instanced.replaceSource cache i [ raw ] scene) (Instanced.build cache [])

                        summary scene =
                            scene.placements |> List.map (\p -> ( p.part, Vec3.toRecord p.center, Vec4.toRecord p.color ))
                    in
                    Expect.equal (summary whole) (summary stepwise)
            ]
        , describe "frustum culling"
            [ test "a sphere at the camera target is visible" <|
                \_ ->
                    Instanced.sphereInFrustum
                        (Instanced.frustumFromMatrix viewProjection)
                        (vec3 0 0 0)
                        10
                        |> Expect.equal True
            , test "a sphere behind the camera is rejected" <|
                \_ ->
                    Instanced.sphereInFrustum
                        (Instanced.frustumFromMatrix viewProjection)
                        (vec3 0 0 600)
                        10
                        |> Expect.equal False
            , test "a sphere far off to the side is rejected" <|
                \_ ->
                    Instanced.sphereInFrustum
                        (Instanced.frustumFromMatrix viewProjection)
                        (vec3 5000 0 0)
                        10
                        |> Expect.equal False
            , test "a sphere large enough to straddle the edge is kept" <|
                \_ ->
                    -- Culling must test the whole sphere, not just its centre.
                    Instanced.sphereInFrustum
                        (Instanced.frustumFromMatrix viewProjection)
                        (vec3 5000 0 0)
                        6000
                        |> Expect.equal True
            ]
        , describe "screen-size bands"
            [ test "a part filling much of the viewport gets full detail" <|
                \_ ->
                    Instanced.projectedRadiusPx 1000 100 20
                        |> (\r -> Instanced.detailFor (2 * r))
                        |> Expect.equal Near
            , test "a part a few pixels across drops to the proxy band" <|
                \_ ->
                    Instanced.projectedRadiusPx 1000 100000 20
                        |> (\r -> Instanced.detailFor (2 * r))
                        |> Expect.equal Far
            , test "bands are ordered Near, then Mid, then Far as size shrinks" <|
                \_ ->
                    [ Instanced.detailFor 100, Instanced.detailFor 40, Instanced.detailFor 1 ]
                        |> Expect.equal [ Near, Mid, Far ]
            , test "a camera inside the bounding sphere never culls it as tiny" <|
                \_ ->
                    Instanced.projectedRadiusPx 1000 5 20
                        |> Expect.greaterThan 1000
            , test "projected size shrinks with distance" <|
                \_ ->
                    Expect.greaterThan
                        (Instanced.projectedRadiusPx 1000 400 20)
                        (Instanced.projectedRadiusPx 1000 200 20)
            ]
        , describe "threshold"
            [ test "a small model stays on the baked path" <|
                \_ ->
                    Instanced.triangleBudgetExceeded 500 |> Expect.equal False
            , test "a city-scale model takes the instanced path" <|
                \_ ->
                    Instanced.triangleBudgetExceeded 22750 |> Expect.equal True
            ]
        , describe "chunkKeyFor"
            [ test "nearby positions share a cell" <|
                \_ ->
                    Expect.equal
                        (Instanced.chunkKeyFor 100 (vec3 10 10 10))
                        (Instanced.chunkKeyFor 100 (vec3 90 90 90))
            , test "positions a cell apart do not" <|
                \_ ->
                    Expect.notEqual
                        (Instanced.chunkKeyFor 100 (vec3 10 10 10))
                        (Instanced.chunkKeyFor 100 (vec3 110 10 10))
            , test "negative coordinates floor away from zero" <|
                \_ ->
                    Instanced.chunkKeyFor 100 (vec3 -10 0 0)
                        |> Expect.equal ( -1, 0, 0 )
            ]
        , describe "resolved cells versus proxy chunks"
            [ test "a cell of only far placements draws one proxy box and no per-placement entities" <|
                \_ ->
                    -- Camera a long way off: every placement is Far, all sit in one cell.
                    let
                        scene =
                            Instanced.build cache
                                [ tiny Mat4.identity
                                , tiny (Mat4.makeTranslate3 40 0 0)
                                , tiny (Mat4.makeTranslate3 0 0 40)
                                ]

                        ( bands, count ) =
                            renderSummary (cameraAt 100000 (vec3 0 0 0)) scene
                    in
                    Expect.equal ( [ Far, Far, Far ], Dict.size scene.chunkIndex ) ( bands, count )
            , test "a resolved placement suppresses the proxy box of its own cell" <|
                \_ ->
                    -- REGRESSION. Before the fix the cell's merged box was drawn over
                    -- the Near brick: a non-box part sits inside its own bounding box, so
                    -- the grey box covered it and its edge lines showed through.
                    --
                    -- Two tiny Far members share the brick's cell, so the counts tell
                    -- the two behaviours apart: the brick's 3 entities (triangles,
                    -- edges, conditional lines) plus one triangle entity per Far
                    -- member is 5; the old proxy path drew 3 + one merged box = 4.
                    let
                        scene =
                            Instanced.build cache
                                [ rawBrick 4 Mat4.identity
                                , tiny Mat4.identity
                                , tiny (Mat4.makeTranslate3 1 0 0)
                                ]

                        ( bands, count ) =
                            renderSummary (cameraAt 30 (vec3 0 0 0)) scene

                        cells =
                            List.map (cellOf scene) scene.placements
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 (Set.size (Set.fromList cells))
                        , \_ -> Expect.equal 1 (Dict.size scene.chunkIndex)
                        , \_ -> Expect.equal ( [ Near, Far, Far ], 5 ) ( bands, count )
                        ]
                        ()
            , test "when every visible placement is Near no proxy box is drawn" <|
                \_ ->
                    -- Three Near bricks in one cell: 3 entities each and nothing else.
                    -- Before the fix the cell's box added a fourth entity on top.
                    let
                        scene =
                            Instanced.build cache
                                [ rawBrick 4 Mat4.identity
                                , rawBrick 1 (Mat4.makeTranslate3 2 0 0)
                                , rawBrick 2 (Mat4.makeTranslate3 0 0 2)
                                ]

                        ( bands, count ) =
                            renderSummary (cameraAt 30 (vec3 0 0 0)) scene
                    in
                    Expect.equal ( [ Near, Near, Near ], 9 ) ( bands, count )
            , test "a resolved cell does not hide the proxy box of a neighbouring far cell" <|
                \_ ->
                    -- The Near brick is in cell 0; the tiny brick is at x = -4, which is
                    -- cell -1. Only cell 0 is resolved, so cell -1 keeps its proxy box:
                    -- 3 entities for the brick plus 1 for the box.
                    let
                        scene =
                            Instanced.build cache
                                [ rawBrick 4 Mat4.identity
                                , tiny (Mat4.makeTranslate3 -4 0 0)
                                ]

                        ( bands, count ) =
                            renderSummary (cameraAt 30 (vec3 0 0 0)) scene

                        cells =
                            List.map (cellOf scene) scene.placements
                    in
                    Expect.all
                        [ \_ -> Expect.equal 2 (Set.size (Set.fromList cells))
                        , \_ -> Expect.equal ( [ Near, Far ], 4 ) ( bands, count )
                        ]
                        ()
            ]
        ]
