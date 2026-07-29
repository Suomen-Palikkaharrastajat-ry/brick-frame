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
    { azimuth = 0
    , elevation = 0
    , distance = 200
    , target = vec3 0 0 0
    , dragging = False
    , lastMousePos = Nothing
    }


viewProjection : Mat4.Mat4
viewProjection =
    Mat4.mul
        (Camera.projectionMatrix 1.0 (Camera.nearPlane camera) (Camera.farPlane camera))
        (Camera.viewMatrix camera)



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
        ]
