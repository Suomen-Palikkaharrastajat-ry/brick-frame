module Viewport.Snapshot exposing (encodeRequest, imageHeight, imageWidth, sceneTriangles, timeBudgetMs)

{-| The request a host sends to the offline path tracer (`captureRender`).

The request carries the camera, the output size, a time budget and every
visible triangle of the scene in render space. `sceneTriangles` picks the same
geometry the viewport's `view` draws, so the snapshot matches the screen:

  - a baked scene (`Scene`) is drawn whole;
  - otherwise the instanced scene is drawn placement by placement, leaving out
    the sources in `hiddenSources` and drawing ghost context at ghost alpha;
  - placements are drawn at full detail, because the proxy boxes that stand in
    for far-away placements are a screen-size optimisation, not geometry.

-}

import Dict
import Json.Encode as Encode
import LDraw.ColorData exposing (Finish(..))
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Render.Instanced as Instanced exposing (InstancedScene)
import Render.Scene exposing (Scene)
import Render.Shader as Shader
import Render.Snapshot as Snapshot
import Set exposing (Set)


{-| Output width in pixels.
-}
imageWidth : Int
imageWidth =
    1920


{-| Output height in pixels.
-}
imageHeight : Int
imageHeight =
    1080


{-| How long the path tracer may run, in milliseconds.
-}
timeBudgetMs : Int
timeBudgetMs =
    30000


{-| Every visible triangle of the viewport's scene, flattened for the path tracer.

`scene` is the baked scene when the model took the baked path; otherwise
`instanced` is drawn. Placements whose source is in `hidden` are left out, and
those outside `activeSources` (when set) are drawn as ghosts.

-}
sceneTriangles :
    { scene : Maybe Scene
    , instanced : Maybe InstancedScene
    , hidden : Set Int
    , activeSources : Maybe (Set Int)
    }
    -> List Float
sceneTriangles config =
    case ( config.scene, config.instanced ) of
        ( Just scene, _ ) ->
            Snapshot.placedTriangles
                { matrix = Mat4.identity
                , instanceColor = Shader.noInstanceColor
                , finish = Solid
                , ghost = False
                }
                scene.surface

        ( Nothing, Just instanced ) ->
            instanced.placements
                |> List.concatMap (placementTriangles instanced config.hidden config.activeSources)

        ( Nothing, Nothing ) ->
            []


{-| The triangles of one visible placement, or none when it is hidden or has no mesh.
-}
placementTriangles : InstancedScene -> Set Int -> Maybe (Set Int) -> Instanced.Placement -> List Float
placementTriangles instanced hidden activeSources placement =
    if Set.member placement.source hidden then
        []

    else
        case Dict.get placement.part instanced.parts of
            Nothing ->
                []

            Just mesh ->
                Snapshot.placedTriangles
                    { matrix = placement.matrix
                    , instanceColor = placement.color
                    , finish = placement.finish
                    , ghost = isGhost activeSources placement.source
                    }
                    mesh.surface


isGhost : Maybe (Set Int) -> Int -> Bool
isGhost activeSources source =
    case activeSources of
        Just sources ->
            not (Set.member source sources)

        Nothing ->
            False


{-| The JSON object `captureRender` takes: output size, the camera, and the triangles.
-}
encodeRequest : { filename : String, camera : Camera, triangles : List Float } -> Encode.Value
encodeRequest config =
    Encode.object
        [ ( "width", Encode.int imageWidth )
        , ( "height", Encode.int imageHeight )
        , ( "transparent", Encode.bool False )
        , ( "filename", Encode.string config.filename )
        , ( "timeBudgetMs", Encode.int timeBudgetMs )
        , ( "camera"
          , Encode.object
                [ ( "position", encodeVec3 (Camera.position config.camera) )
                , ( "target", encodeVec3 (Camera.lookTarget config.camera) )

                -- Matches the view matrix in Render.Camera.
                , ( "up", encodeVec3 (Vec3.vec3 0 1 0) )
                , ( "fovYDegrees", Encode.float Camera.fovYDegrees )
                ]
          )
        , ( "triangles", Encode.list Encode.float config.triangles )
        ]


encodeVec3 : Vec3 -> Encode.Value
encodeVec3 v =
    Encode.list Encode.float
        [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]
