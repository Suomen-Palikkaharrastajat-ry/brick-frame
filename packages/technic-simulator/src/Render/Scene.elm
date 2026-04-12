module Render.Scene exposing (Scene, buildScene, renderScene, renderSceneWithStyle)

{-| Scene assembly: convert flat geometry into WebGL meshes and render them.


## Render order

1.  Triangle mesh (opaque geometry, depth test enabled, optional back-face cull)
2.  Edge line mesh (flat dark lines, depth test enabled, no culling)

Rendering triangles before lines avoids depth-fighting artefacts where edges
would flicker through filled faces.

-}

import LDraw.Geometry exposing (ConditionalEdge)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Render.EdgeShader as EdgeShader exposing (EdgeVertex)
import Render.Lighting exposing (LightUniforms)
import Render.Mesh exposing (Vertex)
import Render.Shader as Shader
import Render.Style as Style
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest as DepthTest



-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Compiled WebGL meshes for a LDraw model.

Built once after all parts are loaded; held in the app Model.

-}
type alias Scene =
    { triangleMesh : WebGL.Mesh Vertex
    , lineMesh : WebGL.Mesh EdgeVertex
    , conditionalLines : List ConditionalEdge
    , bfcCertified : Bool
    }



-- ── Public API ────────────────────────────────────────────────────────────────


{-| Build a `Scene` from flat geometry lists.

    buildScene { triangles, lines } bfcCertified

Call this once after `LDraw.Geometry.flatten` completes and all sub-parts
have been loaded. Uploads the geometry to the GPU.

-}
buildScene :
    { triangles : List ( Vertex, Vertex, Vertex )
    , lines : List ( Vec3, Vec3 )
    , conditionalLines : List ConditionalEdge
    }
    -> Bool
    -> Scene
buildScene geom bfc =
    { triangleMesh = WebGL.triangles geom.triangles
    , lineMesh = WebGL.lines (List.map lineToVertices geom.lines)
    , conditionalLines = geom.conditionalLines
    , bfcCertified = bfc
    }


{-| Produce a list of `WebGL.Entity` values for the current frame.

    renderScene scene camera light aspect

  - `aspect` — canvas width / height (for the projection matrix).
  - Returns two entities: triangles then lines.

-}
renderScene : Scene -> Camera -> LightUniforms -> Float -> List WebGL.Entity
renderScene scene camera light aspect =
    renderSceneWithStyle scene camera (Style.fromLight light) aspect


{-| Render the scene with the provided material style configuration.
-}
renderSceneWithStyle : Scene -> Camera -> Style.Style -> Float -> List WebGL.Entity
renderSceneWithStyle scene camera styleInput aspect =
    let
        style =
            Style.clampStyle styleInput

        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect 0.1 2000.0

        modelMat =
            Mat4.identity

        triangleUniforms =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , viewPosition = cameraPosition camera
            , lightDirection = style.lightDirection
            , ambientStrength = style.ambientStrength
            , specularStrength = style.specularStrength
            , specularPower = style.specularPower
            , rimStrength = style.rimStrength
            , rimPower = style.rimPower
            , vibrance = style.vibrance
            }

        edgeUniforms =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , edgeColor = style.edgeColor
            }

        bfcSettings =
            if scene.bfcCertified then
                [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.back ]

            else
                [ DepthTest.default ]

        triangleEntity =
            WebGL.entityWith
                bfcSettings
                Shader.vertexShader
                Shader.fragmentShader
                scene.triangleMesh
                triangleUniforms

        lineEntity =
            WebGL.entityWith
                [ DepthTest.default ]
                EdgeShader.vertexShader
                EdgeShader.fragmentShader
                scene.lineMesh
                edgeUniforms

        conditionalVisibleLines =
            scene.conditionalLines
                |> List.filter (conditionalLineVisible (cameraPosition camera))
                |> List.map conditionalToVertices

        conditionalEntity =
            if List.isEmpty conditionalVisibleLines then
                Nothing

            else
                Just
                    (WebGL.entityWith
                        [ DepthTest.default ]
                        EdgeShader.vertexShader
                        EdgeShader.fragmentShader
                        (WebGL.lines conditionalVisibleLines)
                        edgeUniforms
                    )
    in
    triangleEntity
        :: lineEntity
        :: (case conditionalEntity of
                Just entity ->
                    [ entity ]

                Nothing ->
                    []
           )



-- ── Internal ──────────────────────────────────────────────────────────────────


lineToVertices : ( Vec3, Vec3 ) -> ( EdgeVertex, EdgeVertex )
lineToVertices ( p1, p2 ) =
    ( { position = p1 }, { position = p2 } )


conditionalToVertices : ConditionalEdge -> ( EdgeVertex, EdgeVertex )
conditionalToVertices cond =
    ( { position = cond.p1 }, { position = cond.p2 } )


conditionalLineVisible : Vec3 -> ConditionalEdge -> Bool
conditionalLineVisible eye cond =
    let
        edge =
            Vec3.sub cond.p2 cond.p1

        toEye =
            Vec3.sub eye cond.p1

        side1 =
            Vec3.dot (Vec3.cross edge (Vec3.sub cond.c1 cond.p1)) toEye

        side2 =
            Vec3.dot (Vec3.cross edge (Vec3.sub cond.c2 cond.p1)) toEye
    in
    side1 * side2 > 0


cameraPosition : Camera -> Vec3
cameraPosition cam =
    let
        x =
            cam.distance * sin cam.azimuth * cos cam.elevation

        y =
            cam.distance * sin cam.elevation

        z =
            cam.distance * cos cam.azimuth * cos cam.elevation
    in
    Vec3.add cam.target (Vec3.vec3 x y z)
