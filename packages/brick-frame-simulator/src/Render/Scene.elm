module Render.Scene exposing
    ( Scene
    , buildScene
    , conditionalLineVisible
    , conditionalToQuad
    , renderScene
    , renderSceneWithStyle
    )

{-| Scene assembly: convert flat geometry into WebGL meshes and render them.


## Render order

1.  Triangle mesh (opaque geometry, depth-write on, optional back-face cull)
2.  Edge line mesh + conditional lines (antialiased, depth-write off, alpha blend)

Triangles are rendered first with full depth writing. Edges then use
`lessOrEqual` depth test with `write = False` so they composite correctly
over the opaque pass without z-fighting or blocking each other.

-}

import LDraw.Geometry exposing (ConditionalEdge)
import Math.Matrix4 as Mat4
import Math.Vector3 as Vec3 exposing (Vec3)
import Render.Camera as Camera exposing (Camera)
import Render.EdgeShader as EdgeShader exposing (ConditionalVertex, EdgeVertex)
import Render.Lighting exposing (LightUniforms)
import Render.Mesh exposing (Vertex)
import Render.Shader as Shader
import Render.Style as Style
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest



-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Compiled WebGL meshes for a LDraw model.

Built once after all parts are loaded; held in the app Model.

-}
type alias Scene =
    { triangleMesh : WebGL.Mesh Vertex
    , lineMesh : WebGL.Mesh EdgeVertex
    , conditionalMesh : WebGL.Mesh ConditionalVertex
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
    , lineMesh = WebGL.triangles (List.concatMap lineToQuad geom.lines)
    , conditionalMesh = WebGL.triangles (List.concatMap conditionalToQuad geom.conditionalLines)
    , bfcCertified = bfc
    }


{-| Produce a list of `WebGL.Entity` values for the current frame.

    renderScene scene camera light aspect

  - `aspect` — canvas width / height (for the projection matrix).
  - Returns two entities: triangles then lines.

-}
renderScene : Scene -> Camera -> LightUniforms -> Int -> Int -> List WebGL.Entity
renderScene scene camera light width height =
    renderSceneWithStyle scene camera (Style.fromLight light) width height


{-| Render the scene with the provided material style configuration.
-}
renderSceneWithStyle : Scene -> Camera -> Style.Style -> Int -> Int -> List WebGL.Entity
renderSceneWithStyle scene camera styleInput width height =
    let
        style =
            Style.clampStyle styleInput

        aspect =
            toFloat width / toFloat height

        viewMat =
            Camera.viewMatrix camera

        projMat =
            Camera.projectionMatrix aspect (Camera.nearPlane camera) (Camera.farPlane camera)

        modelMat =
            Mat4.identity

        triangleUniforms =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , viewPosition = Camera.position camera
            , lightDirection = style.lightDirection

            -- Baked geometry resolves every colour up front, so no vertex here
            -- is flagged for substitution and this value is never read.
            , instanceColor = Shader.noInstanceColor
            , ambientStrength = style.ambientStrength
            , lightStrength = style.lightStrength
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
            , viewportWidth = toFloat width
            , viewportHeight = toFloat height
            , lineWidth = style.edgeWidth
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

        -- Edges use lessOrEqual (not strict less) so they pass the depth test
        -- when sitting at the same depth as a triangle surface, and write=False
        -- prevents them from blocking later geometry or each other.
        -- Alpha blending lets the smooth falloff in the fragment shader composite
        -- correctly over the opaque triangle pass.
        edgeSettings =
            [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
            , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
            ]

        lineEntity =
            WebGL.entityWith
                edgeSettings
                EdgeShader.vertexShader
                EdgeShader.fragmentShader
                scene.lineMesh
                edgeUniforms

        -- Conditional edges are uploaded once in buildScene; the visibility
        -- test runs per-vertex on the GPU (see EdgeShader.conditionalVertexShader),
        -- so there is no per-frame CPU filtering or re-upload. Invisible edges
        -- collapse to zero-area quads and rasterize to nothing.
        conditionalUniforms =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , edgeColor = style.edgeColor
            , viewportWidth = toFloat width
            , viewportHeight = toFloat height
            , lineWidth = style.edgeWidth
            , eyePosition = Camera.position camera
            }

        conditionalEntity =
            WebGL.entityWith
                edgeSettings
                EdgeShader.conditionalVertexShader
                EdgeShader.conditionalFragmentShader
                scene.conditionalMesh
                conditionalUniforms
    in
    [ triangleEntity, lineEntity, conditionalEntity ]



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Expand a line segment into two screen-aligned triangles (a quad).

Each call produces 6 vertices: two triangles that together form a rectangle
centred on the segment. The vertex shader expands them perpendicular to the
segment direction by `lineWidth` pixels.

-}
lineToQuad : ( Vec3, Vec3 ) -> List ( EdgeVertex, EdgeVertex, EdgeVertex )
lineToQuad ( p1, p2 ) =
    let
        v side pos other =
            { position = pos, other = other, side = side }
    in
    [ ( v -1 p1 p2, v 1 p1 p2, v 1 p2 p1 )
    , ( v -1 p1 p2, v 1 p2 p1, v -1 p2 p1 )
    ]


{-| Expand a conditional edge into a screen-aligned quad, carrying the control
points on every vertex for the GPU visibility test.

The geometry (`position` / `other` / `side`) matches `lineToQuad`, but the
predicate fields `cp1` / `cp2` / `cc1` / `cc2` are stamped **identically** onto
all six vertices — anchored on `cond.p1` — so the shader evaluates one predicate
per edge rather than a per-vertex one that would tear the quad.

-}
conditionalToQuad : ConditionalEdge -> List ( ConditionalVertex, ConditionalVertex, ConditionalVertex )
conditionalToQuad cond =
    let
        v side pos other =
            { position = pos
            , other = other
            , side = side
            , cp1 = cond.p1
            , cp2 = cond.p2
            , cc1 = cond.c1
            , cc2 = cond.c2
            }
    in
    [ ( v -1 cond.p1 cond.p2, v 1 cond.p1 cond.p2, v 1 cond.p2 cond.p1 )
    , ( v -1 cond.p1 cond.p2, v 1 cond.p2 cond.p1, v -1 cond.p2 cond.p1 )
    ]


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
