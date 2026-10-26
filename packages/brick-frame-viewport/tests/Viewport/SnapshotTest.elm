module Viewport.SnapshotTest exposing (suite)

{-| Tests for `Viewport.Snapshot`: which triangles go to the path tracer (the
baked scene or the instanced placements, minus hidden sources, ghosted when
context is shown) and the JSON request around them.
-}

import Dict
import Expect
import Json.Decode as Decode
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3
import Math.Vector4 as Vec4
import Render.Camera as Camera
import Render.Instanced exposing (InstancedScene, PartMesh, Placement)
import Render.Mesh exposing (Vertex)
import Render.Scene exposing (Scene)
import Set
import Test exposing (Test, describe, test)
import Viewport.Snapshot as Snapshot
import WebGL


corner : Float -> Float -> Vertex
corner x y =
    { position = Vec3.vec3 x y 0
    , normal = Vec3.vec3 0 0 1
    , color = Vec4.vec4 0.1 0.2 0.3 1
    , inherit = 0
    }


{-| A triangle in part-local space, used by every placement.
-}
partSurface : List ( Vertex, Vertex, Vertex )
partSurface =
    [ ( corner 0 0, corner 10 0, corner 0 10 ) ]


partMesh : PartMesh
partMesh =
    { triangles = WebGL.triangles []
    , lines = WebGL.triangles []
    , conditionals = WebGL.triangles []
    , boundsMin = Vec3.vec3 0 0 0
    , boundsMax = Vec3.vec3 10 10 0
    , localCenter = Vec3.vec3 5 5 0
    , localRadius = 7
    , bfcCertified = False
    , surface = partSurface
    }


placement : Int -> Float -> Placement
placement source x =
    { source = source
    , part = "3001.dat"
    , matrix = Mat4.makeTranslate3 x 0 0
    , color = Vec4.vec4 0.1 0.2 0.3 1
    , center = Vec3.vec3 (x + 5) 5 0
    , radius = 7
    , flipped = False
    }


instanced : InstancedScene
instanced =
    { parts = Dict.fromList [ ( "3001.dat", partMesh ) ]
    , placements = [ placement 1 0, placement 2 100 ]
    , proxyChunks = []
    , chunkCell = 200
    , chunkIndex = Dict.empty
    , boundsMin = Vec3.vec3 0 0 0
    , boundsMax = Vec3.vec3 110 10 0
    }


bakedScene : Scene
bakedScene =
    { triangleMesh = WebGL.triangles []
    , lineMesh = WebGL.triangles []
    , conditionalMesh = WebGL.triangles []
    , bfcCertified = False
    , surface = [ ( corner 50 50, corner 60 50, corner 50 60 ) ]
    }


{-| Floats for the first placement's triangle, drawn at the origin.
-}
firstTriangle : List Float
firstTriangle =
    [ 0, 0, 0, 10, 0, 0, 0, 10, 0, 0.1, 0.2, 0.3, 1, 0 ]


{-| Floats for the second placement's triangle, drawn 100 units along X.
-}
secondTriangle : List Float
secondTriangle =
    [ 100, 0, 0, 110, 0, 0, 100, 10, 0, 0.1, 0.2, 0.3, 1, 0 ]


suite : Test
suite =
    describe "Viewport.Snapshot"
        [ describe "sceneTriangles"
            [ test "an instanced scene draws every placement, in order" <|
                \_ ->
                    Snapshot.sceneTriangles
                        { scene = Nothing
                        , instanced = Just instanced
                        , hidden = Set.empty
                        , activeSources = Nothing
                        }
                        |> Expect.equal (firstTriangle ++ secondTriangle)
            , test "hidden sources are left out" <|
                \_ ->
                    Snapshot.sceneTriangles
                        { scene = Nothing
                        , instanced = Just instanced
                        , hidden = Set.fromList [ 1 ]
                        , activeSources = Nothing
                        }
                        |> Expect.equal secondTriangle
            , test "placements outside the active sources are drawn as ghosts" <|
                \_ ->
                    Snapshot.sceneTriangles
                        { scene = Nothing
                        , instanced = Just instanced
                        , hidden = Set.empty
                        , activeSources = Just (Set.fromList [ 1 ])
                        }
                        |> Expect.equal
                            (firstTriangle ++ [ 100, 0, 0, 110, 0, 0, 100, 10, 0, 0.1, 0.2, 0.3, 0.25, 1 ])
            , test "a baked scene is drawn instead of the instanced placements" <|
                \_ ->
                    Snapshot.sceneTriangles
                        { scene = Just bakedScene
                        , instanced = Just instanced
                        , hidden = Set.empty
                        , activeSources = Nothing
                        }
                        |> Expect.equal [ 50, 50, 0, 60, 50, 0, 50, 60, 0, 0.1, 0.2, 0.3, 1, 0 ]
            , test "with neither scene there is nothing to draw" <|
                \_ ->
                    Snapshot.sceneTriangles
                        { scene = Nothing
                        , instanced = Nothing
                        , hidden = Set.empty
                        , activeSources = Nothing
                        }
                        |> Expect.equal []
            ]
        , describe "encodeRequest"
            [ test "carries the output size, time budget, camera and triangles" <|
                \_ ->
                    let
                        camera =
                            Camera.init

                        request =
                            Snapshot.encodeRequest
                                { filename = "render.png"
                                , camera = camera
                                , triangles = firstTriangle
                                }

                        decodes : Decode.Decoder a -> a -> Bool
                        decodes decoder expected =
                            Decode.decodeValue decoder request == Ok expected
                    in
                    Expect.equal [ True, True, True, True, True, True, True ]
                        [ decodes (Decode.field "width" Decode.int) 1920
                        , decodes (Decode.field "height" Decode.int) 1080
                        , decodes (Decode.field "transparent" Decode.bool) False
                        , decodes (Decode.field "filename" Decode.string) "render.png"
                        , decodes (Decode.field "timeBudgetMs" Decode.int) 30000
                        , decodes (Decode.at [ "camera", "fovYDegrees" ] Decode.float) Camera.fovYDegrees
                        , decodes (Decode.field "triangles" (Decode.list Decode.float)) firstTriangle
                        ]
            , test "the camera is Y-up with its eye and target in render space" <|
                \_ ->
                    let
                        request =
                            Snapshot.encodeRequest
                                { filename = "render.png"
                                , camera = Camera.init
                                , triangles = []
                                }
                    in
                    Expect.all
                        [ \_ ->
                            Decode.decodeValue (Decode.at [ "camera", "up" ] (Decode.list Decode.float)) request
                                |> Expect.equal (Ok [ 0, 1, 0 ])
                        , \_ ->
                            Decode.decodeValue (Decode.at [ "camera", "position" ] (Decode.list Decode.float)) request
                                |> Expect.equal (Ok (vecList (Camera.position Camera.init)))
                        , \_ ->
                            Decode.decodeValue (Decode.at [ "camera", "target" ] (Decode.list Decode.float)) request
                                |> Expect.equal (Ok [ 0, 0, 0 ])
                        ]
                        ()
            ]
        ]


vecList : Vec3.Vec3 -> List Float
vecList v =
    [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]
