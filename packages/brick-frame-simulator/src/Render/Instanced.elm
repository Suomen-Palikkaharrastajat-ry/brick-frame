module Render.Instanced exposing
    ( InstancedScene, PartMesh, Placement, ProxyChunk, RawPlacement, PartKey
    , Detail(..)
    , build, extractPlacements, chunkKeyFor
    , render, visiblePlacements, detailFor
    , Frustum, frustumFromMatrix, sphereInFrustum, projectedRadiusPx
    , triangleBudgetExceeded
    )

{-| Instanced rendering of large LDraw models.

`Render.Scene` bakes every world transform into the vertex buffers, which
produces one flat mesh per model. That is fine for a few hundred parts and
impossible for a few thousand: a city-scale model expands to millions of
triangles, all of them near-duplicates of a couple of hundred distinct parts.

This module inverts that. Each distinct **part** is flattened **once**, in its
own local space (`LDraw.Geometry.localOptions`), and every placement of it
becomes a `WebGL.Entity` carrying its own `modelMatrix` and `instanceColor`.

For a 22,750-part model this is a ~47× reduction in resident geometry:

    baked world-space   10,911,354 triangles, 3 meshes
    instanced              232,242 triangles, 202 meshes

Colour is deliberately kept off the geometry. Keying meshes by part _and_
colour is the obvious first move and costs dearly: a model uses a part in
roughly five colours on average, so it multiplies the largest buffer in the
scene fivefold — on this model, 1,092,318 baked triangles instead of 232,242.
Instead `Render.Mesh.Vertex` carries an `inherit` flag and the vertex shader
blends in the placement's colour only where LDraw colour 16 was inherited all
the way down, which leaves printed and multi-colour parts correct.

The cost moves from memory to draw calls, so the renderer culls hard:

1.  **Frustum culling** against each placement's world bounding sphere.
2.  **Screen-size bands** (`Detail`), because the edge and conditional-line
    passes are far more expensive per line than the triangle pass and are
    invisible on a part only a few pixels across.
3.  **Merged proxy chunks** for placements too small to resolve. Culling alone
    cannot rescue a fully zoomed-out view: at that scale a 1×1 plate is still
    around six pixels, so nothing is small enough to simply drop, and every
    placement is inside the frustum. Instead each such placement contributes a
    twelve-triangle box of its own colour, pre-merged by spatial cell, turning
    tens of thousands of draw calls into a few dozen.


## Coordinate handling

Part meshes stay in LDraw's Y-down space. The Y-up conversion is folded into
every placement matrix as `Mat4.makeScale3 1 -1 -1`. That matrix has
determinant +1 — a rotation, not a reflection — so it does not disturb triangle
winding.

Placements whose own transform has a **negative** determinant (mirrored
sub-models, which LDraw models use heavily) do invert winding, so `flipped` is
recorded per placement and the cull face is switched accordingly.

@docs InstancedScene, PartMesh, Placement, ProxyChunk, RawPlacement, PartKey
@docs Detail
@docs build, extractPlacements, chunkKeyFor
@docs render, visiblePlacements, detailFor
@docs Frustum, frustumFromMatrix, sphereInFrustum, projectedRadiusPx
@docs triangleBudgetExceeded

-}

import Dict exposing (Dict)
import LDraw.Colors as Colors
import LDraw.Geometry as Geometry
import LDraw.Resolve exposing (PartCache, PartStatus(..))
import LDraw.Types exposing (LDrawLine(..))
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4)
import Render.Camera as Camera exposing (Camera)
import Render.EdgeShader as EdgeShader exposing (ConditionalVertex, EdgeVertex)
import Render.Mesh exposing (Vertex)
import Render.Scene as Scene
import Render.Shader as Shader
import Render.Style as Style
import Set exposing (Set)
import WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest



-- ── Types ─────────────────────────────────────────────────────────────────────


{-| Identifies one distinct part file. Everything is keyed by this alone —
colour lives on the placement, not in the geometry.

A part that mixes inherited and hard-coded colours (a printed tile, say) still
bakes once: `LDraw.Geometry` flags the inherited surfaces per vertex and the
shader substitutes `instanceColor` only into those. On a typical model a part
appears in around five colours, so keying geometry by colour would multiply the
scene's largest buffer by five for no visible difference.

-}
type alias PartKey =
    String


{-| All geometry for one distinct part, in local LDraw space.

The edge buffers are colourless by construction — `LDraw.Geometry` keeps only
positions for type-2 and type-5 lines, and the stroke colour is the `edgeColor`
uniform — and the triangle buffer is re-coloured per placement by the shader, so
the whole record is shared across every colour the part is placed in.

-}
type alias PartMesh =
    { triangles : WebGL.Mesh Vertex
    , lines : WebGL.Mesh EdgeVertex
    , conditionals : WebGL.Mesh ConditionalVertex
    , boundsMin : Vec3
    , boundsMax : Vec3
    , localCenter : Vec3
    , localRadius : Float
    , bfcCertified : Bool
    }


{-| A part reference located in the model tree, before its geometry is known.
-}
type alias RawPlacement =
    { part : PartKey
    , color : Int
    , matrix : Mat4
    }


{-| A placement ready to draw: its model matrix and the culling data derived
from the part's local bounds.
-}
type alias Placement =
    { part : PartKey
    , matrix : Mat4
    , color : Vec4
    , center : Vec3
    , radius : Float
    , flipped : Bool
    }


{-| Every placement in one spatial cell, merged into a single box mesh.

Drawn in place of the individual placements once they are too small to resolve.

-}
type alias ProxyChunk =
    { mesh : WebGL.Mesh Vertex
    , center : Vec3
    , radius : Float
    }


{-| A whole model prepared for instanced drawing.
-}
type alias InstancedScene =
    { parts : Dict PartKey PartMesh
    , placements : List Placement
    , proxyChunks : List ProxyChunk
    , boundsMin : Vec3
    , boundsMax : Vec3
    }


{-| How much of a placement to draw, chosen from its size on screen.

  - `Near` — full geometry: triangles, edge lines and conditional lines.
  - `Mid` — triangles and edge lines; conditional lines dropped.
  - `Far` — too small to resolve; drawn by the proxy pass, not per placement.

Conditional lines are dropped first on purpose: `EdgeShader.ConditionalVertex`
carries seven `Vec3` attributes per vertex against six for a triangle vertex's
worth of data, making it the most expensive buffer in the pipeline.

-}
type Detail
    = Near
    | Mid
    | Far


{-| Six clip-space planes, each `(a, b, c, d)` with inside meaning
`a*x + b*y + c*z + d >= 0`.
-}
type alias Frustum =
    List ( Vec3, Float )



-- ── Constants ─────────────────────────────────────────────────────────────────


{-| Screen diameter, in pixels, above which a placement keeps its conditional
lines. Below this the smoothing they provide is not resolvable.
-}
nearPixels : Float
nearPixels =
    64.0


{-| Screen diameter, in pixels, above which a placement is drawn with its real
mesh. Below it the placement is `Far` and belongs to the proxy pass.

Measured against a bounding-sphere diameter, which is the box's space diagonal
and so overstates the apparent size by roughly half. A 2×4 brick reaches this
threshold at about the point its studs stop being distinguishable, which is
where a plain coloured box becomes an honest substitute.

-}
midPixels : Float
midPixels =
    24.0


{-| Vertical field of view of `Render.Camera.projectionMatrix`, in radians.
Used to convert a world radius into a pixel radius.
-}
fovY : Float
fovY =
    degrees Camera.fovYDegrees


{-| Converts LDraw Y-down space to the Y-up convention used for rendering.
Determinant is +1, so triangle winding is preserved.
-}
yUpFlip : Mat4
yUpFlip =
    Mat4.makeScale3 1 -1 -1



-- ── Building ──────────────────────────────────────────────────────────────────


{-| Walk a model tree and collect every leaf part reference with its world
transform.

    extractPlacements submodels lines cache rootColor

`submodels` is the set of file names that came from the enclosing MPD archive.
Those are recursed into; every other reference is a library part and becomes a
placement. This is what keeps the result at one entry per physical brick rather
than one per primitive.

Colour code 16 (and Studio's -1) inherits from the enclosing reference, matching
`LDraw.Geometry`.

-}
extractPlacements : Set String -> List LDrawLine -> PartCache -> Int -> List RawPlacement
extractPlacements submodels lines cache rootColor =
    collectPlacements submodels cache rootColor Mat4.identity 0 lines []
        |> List.reverse


collectPlacements :
    Set String
    -> PartCache
    -> Int
    -> Mat4
    -> Int
    -> List LDrawLine
    -> List RawPlacement
    -> List RawPlacement
collectPlacements submodels cache parentColor transform depth lines acc =
    List.foldl (collectPlacement submodels cache parentColor transform depth) acc lines


collectPlacement :
    Set String
    -> PartCache
    -> Int
    -> Mat4
    -> Int
    -> LDrawLine
    -> List RawPlacement
    -> List RawPlacement
collectPlacement submodels cache parentColor transform depth line acc =
    case line of
        SubFileRef ref ->
            let
                childColor =
                    if ref.color == 16 || ref.color == -1 then
                        parentColor

                    else
                        ref.color

                childTransform =
                    Mat4.mul transform ref.transform
            in
            if Set.member ref.file submodels && depth < maxSubmodelDepth then
                case Dict.get ref.file cache of
                    Just (Loaded subLines) ->
                        collectPlacements submodels cache childColor childTransform (depth + 1) subLines acc

                    _ ->
                        acc

            else
                { part = ref.file
                , color = childColor
                , matrix = childTransform
                }
                    :: acc

        _ ->
            acc


{-| Guards against an MPD that references itself, directly or in a cycle.
-}
maxSubmodelDepth : Int
maxSubmodelDepth =
    32


{-| Build the drawable scene from raw placements and a populated part cache.

Placements whose part is missing from the cache are dropped, matching
`LDraw.Geometry.flatten`'s silent-skip behaviour.

-}
build : PartCache -> List RawPlacement -> InstancedScene
build cache raws =
    let
        parts =
            List.foldl (addPartMesh cache) Dict.empty raws

        placements =
            List.filterMap (toPlacement parts) raws

        boundsMin =
            List.foldl (\p acc -> minVec acc (Vec3.sub p.center (uniformVec p.radius))) hugeVec placements

        boundsMax =
            List.foldl (\p acc -> maxVec acc (Vec3.add p.center (uniformVec p.radius))) (Vec3.negate hugeVec) placements
    in
    { parts = parts
    , placements = placements
    , proxyChunks = buildProxyChunks parts boundsMin boundsMax placements
    , boundsMin = boundsMin
    , boundsMax = boundsMax
    }


addPartMesh : PartCache -> RawPlacement -> Dict PartKey PartMesh -> Dict PartKey PartMesh
addPartMesh cache raw parts =
    if Dict.member raw.part parts then
        parts

    else
        case Dict.get raw.part cache of
            Just (Loaded partLines) ->
                Dict.insert raw.part (buildPartMesh cache partLines) parts

            _ ->
                parts


{-| Flatten one part in local space, once.

The colour passed to `flattenWith` is code 16, so any surface that inherits ends
up flagged for `instanceColor` substitution; surfaces with a hard-coded colour
keep it. Nothing here depends on the colours the part is actually placed in.

-}
buildPartMesh : PartCache -> List LDrawLine -> PartMesh
buildPartMesh cache partLines =
    let
        geom =
            Geometry.flattenWith Geometry.localOptions partLines cache 16 Mat4.identity

        bounds =
            List.foldl
                (\( a, b, c ) acc -> growBounds a.position (growBounds b.position (growBounds c.position acc)))
                (List.foldl (\( a, b ) acc -> growBounds a (growBounds b acc)) emptyBounds geom.lines)
                geom.triangles

        center =
            Vec3.scale 0.5 (Vec3.add bounds.min bounds.max)
    in
    { triangles = WebGL.triangles geom.triangles
    , lines = WebGL.triangles (List.concatMap lineToQuad geom.lines)
    , conditionals = WebGL.triangles (List.concatMap Scene.conditionalToQuad geom.conditionalLines)
    , boundsMin = bounds.min
    , boundsMax = bounds.max
    , localCenter = center
    , localRadius = 0.5 * Vec3.length (Vec3.sub bounds.max bounds.min)
    , bfcCertified = geom.bfcCertified
    }


{-| Attach the world bounding sphere and winding flag a raw placement needs
before it can be culled and drawn.
-}
toPlacement : Dict PartKey PartMesh -> RawPlacement -> Maybe Placement
toPlacement parts raw =
    Dict.get raw.part parts
        |> Maybe.map
            (\mesh ->
                let
                    matrix =
                        Mat4.mul yUpFlip raw.matrix
                in
                { part = raw.part
                , matrix = matrix

                -- Extraction already resolved inheritance, so the code is
                -- concrete here and the parent argument goes unused; this is a
                -- plain lookup that still normalises Studio's colour offsets.
                , color = Colors.toVec4 (Colors.resolveColor raw.color raw.color)
                , center = Mat4.transform matrix mesh.localCenter
                , radius = mesh.localRadius * maxScale matrix
                , flipped = determinant3 matrix < 0
                }
            )



-- ── Proxy chunks ──────────────────────────────────────────────────────────────


{-| Merge every placement's bounding box into one mesh per spatial cell.

Built eagerly with the scene. A `Far` placement contributes 12 triangles here
instead of a draw call of its own, so a 22,750-part model costs roughly 273,000
proxy triangles across a few dozen meshes — the same order as the part meshes it
stands in for, and a draw-call count the GPU does not notice.

-}
buildProxyChunks : Dict PartKey PartMesh -> Vec3 -> Vec3 -> List Placement -> List ProxyChunk
buildProxyChunks parts boundsMin boundsMax placements =
    let
        cell =
            chunkCellSize boundsMin boundsMax
    in
    placements
        |> List.foldl (groupByChunk parts cell) Dict.empty
        |> Dict.values
        |> List.filterMap toProxyChunk


{-| Edge length of one spatial cell, chosen so the model's longest axis is cut
into `chunkDivisions` slices. Flat models such as a city layout leave the
vertical axis as a single slice, which is what we want.
-}
chunkCellSize : Vec3 -> Vec3 -> Float
chunkCellSize boundsMin boundsMax =
    let
        extent =
            Vec3.sub boundsMax boundsMin

        longest =
            List.foldl max 0 [ Vec3.getX extent, Vec3.getY extent, Vec3.getZ extent ]
    in
    max 1 (longest / toFloat chunkDivisions)


{-| Slices per axis for proxy chunking. Six gives a few dozen occupied cells on
a typical flat layout: coarse enough to keep draw calls low, fine enough that
frustum-culling whole chunks still pays off when zoomed part way in.
-}
chunkDivisions : Int
chunkDivisions =
    6


{-| Integer cell coordinates a world position falls into.
-}
chunkKeyFor : Float -> Vec3 -> ( Int, Int, Int )
chunkKeyFor cell p =
    ( floor (Vec3.getX p / cell)
    , floor (Vec3.getY p / cell)
    , floor (Vec3.getZ p / cell)
    )


groupByChunk :
    Dict PartKey PartMesh
    -> Float
    -> Placement
    -> Dict ( Int, Int, Int ) (List ( Vertex, Vertex, Vertex ))
    -> Dict ( Int, Int, Int ) (List ( Vertex, Vertex, Vertex ))
groupByChunk parts cell placement chunks =
    case Dict.get placement.part parts of
        Nothing ->
            chunks

        Just mesh ->
            Dict.update (chunkKeyFor cell placement.center)
                (\existing ->
                    Just (boxTriangles placement mesh ++ Maybe.withDefault [] existing)
                )
                chunks


toProxyChunk : List ( Vertex, Vertex, Vertex ) -> Maybe ProxyChunk
toProxyChunk triangles =
    if List.isEmpty triangles then
        Nothing

    else
        let
            bounds =
                List.foldl
                    (\( a, b, c ) acc -> growBounds a.position (growBounds b.position (growBounds c.position acc)))
                    emptyBounds
                    triangles
        in
        Just
            { mesh = WebGL.triangles triangles
            , center = Vec3.scale 0.5 (Vec3.add bounds.min bounds.max)
            , radius = 0.5 * Vec3.length (Vec3.sub bounds.max bounds.min)
            }


{-| The 12 triangles of a placement's bounding box, in world space and in the
placement's colour, with outward per-face normals.
-}
boxTriangles : Placement -> PartMesh -> List ( Vertex, Vertex, Vertex )
boxTriangles placement mesh =
    let
        corner xs ys zs =
            Mat4.transform placement.matrix
                (vec3
                    (pick xs (Vec3.getX mesh.boundsMin) (Vec3.getX mesh.boundsMax))
                    (pick ys (Vec3.getY mesh.boundsMin) (Vec3.getY mesh.boundsMax))
                    (pick zs (Vec3.getZ mesh.boundsMin) (Vec3.getZ mesh.boundsMax))
                )

        pick high lo hi =
            if high then
                hi

            else
                lo

        -- Corner naming: c<x><y><z>, True = the max side of that axis.
        c000 =
            corner False False False

        c100 =
            corner True False False

        c110 =
            corner True True False

        c010 =
            corner False True False

        c001 =
            corner False False True

        c101 =
            corner True False True

        c111 =
            corner True True True

        c011 =
            corner False True True

        -- Winding is chosen so each face's normal points away from the box; a
        -- mirrored placement reverses that, so the quad order flips with it.
        quad a b c d =
            if placement.flipped then
                [ faceTriangle placement.color a c b, faceTriangle placement.color a d c ]

            else
                [ faceTriangle placement.color a b c, faceTriangle placement.color a c d ]
    in
    List.concat
        [ quad c001 c101 c111 c011 -- +Z
        , quad c100 c000 c010 c110 -- -Z
        , quad c101 c100 c110 c111 -- +X
        , quad c000 c001 c011 c010 -- -X
        , quad c010 c011 c111 c110 -- +Y
        , quad c000 c100 c101 c001 -- -Y
        ]


faceTriangle : Vec4 -> Vec3 -> Vec3 -> Vec3 -> ( Vertex, Vertex, Vertex )
faceTriangle color a b c =
    let
        normal =
            normalizeSafe (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))
    in
    -- inherit = 0: the placement's colour is already baked into the box.
    ( { position = a, normal = normal, color = color, inherit = 0 }
    , { position = b, normal = normal, color = color, inherit = 0 }
    , { position = c, normal = normal, color = color, inherit = 0 }
    )


normalizeSafe : Vec3 -> Vec3
normalizeSafe v =
    if Vec3.length v < 1.0e-8 then
        vec3 0 1 0

    else
        Vec3.normalize v



-- ── Transform helpers ─────────────────────────────────────────────────────────


{-| Largest column length of the upper-left 3×3 block: how much the transform
can stretch a radius.
-}
maxScale : Mat4 -> Float
maxScale mat =
    let
        m =
            Mat4.toRecord mat
    in
    sqrt
        (List.foldl max
            0
            [ (m.m11 * m.m11) + (m.m21 * m.m21) + (m.m31 * m.m31)
            , (m.m12 * m.m12) + (m.m22 * m.m22) + (m.m32 * m.m32)
            , (m.m13 * m.m13) + (m.m23 * m.m23) + (m.m33 * m.m33)
            ]
        )


determinant3 : Mat4 -> Float
determinant3 mat =
    let
        m =
            Mat4.toRecord mat
    in
    (m.m11 * ((m.m22 * m.m33) - (m.m23 * m.m32)))
        - (m.m12 * ((m.m21 * m.m33) - (m.m23 * m.m31)))
        + (m.m13 * ((m.m21 * m.m32) - (m.m22 * m.m31)))



-- ── Culling ───────────────────────────────────────────────────────────────────


{-| Extract the six frustum planes from a combined `projection * view` matrix
(Gribb–Hartmann). Normals point inward, so a point is inside every plane when
each `dot normal p + d >= 0`.
-}
frustumFromMatrix : Mat4 -> Frustum
frustumFromMatrix mat =
    let
        m =
            Mat4.toRecord mat

        plane a b c d =
            let
                normal =
                    vec3 a b c

                len =
                    Vec3.length normal
            in
            if len < 1.0e-8 then
                ( vec3 0 0 1, 0 )

            else
                ( Vec3.scale (1 / len) normal, d / len )
    in
    [ plane (m.m41 + m.m11) (m.m42 + m.m12) (m.m43 + m.m13) (m.m44 + m.m14)
    , plane (m.m41 - m.m11) (m.m42 - m.m12) (m.m43 - m.m13) (m.m44 - m.m14)
    , plane (m.m41 + m.m21) (m.m42 + m.m22) (m.m43 + m.m23) (m.m44 + m.m24)
    , plane (m.m41 - m.m21) (m.m42 - m.m22) (m.m43 - m.m23) (m.m44 - m.m24)
    , plane (m.m41 + m.m31) (m.m42 + m.m32) (m.m43 + m.m33) (m.m44 + m.m34)
    , plane (m.m41 - m.m31) (m.m42 - m.m32) (m.m43 - m.m33) (m.m44 - m.m34)
    ]


{-| Whether a world-space bounding sphere touches the frustum at all.
-}
sphereInFrustum : Frustum -> Vec3 -> Float -> Bool
sphereInFrustum planes center radius =
    List.all (\( normal, d ) -> Vec3.dot normal center + d >= -radius) planes


{-| Radius of a world-space sphere once projected, in pixels.

Uses the vertical field of view fixed by `Render.Camera.projectionMatrix`.
Distances at or inside the sphere return a huge value so a placement the camera
sits inside is never culled as too small.

-}
projectedRadiusPx : Float -> Float -> Float -> Float
projectedRadiusPx viewportHeight eyeDistance radius =
    if eyeDistance <= radius then
        1.0e9

    else
        radius / eyeDistance * (viewportHeight / (2 * tan (fovY / 2)))


{-| Pick the detail band for a placement from its projected size.

There is deliberately no hysteresis here, unlike `Gear.Lod`. A gear is a handful
of objects whose LOD is re-evaluated as it rotates; placements number in the
tens of thousands and their band is a pure function of a static camera, so a
settled view yields a settled answer. Keeping per-placement previous state would
cost a dictionary update per placement per frame to prevent flicker that only
occurs while the camera is already moving — which is when
`Render.Instanced` is being asked for coarser output anyway.

-}
detailFor : Float -> Detail
detailFor pixelDiameter =
    if pixelDiameter >= nearPixels then
        Near

    else if pixelDiameter >= midPixels then
        Mid

    else
        Far


{-| Placements that survive frustum culling, paired with their detail band.

    visiblePlacements { viewProjection, eye, viewportHeight, coarsen } scene

`coarsen` drops everything one band, for use while the camera is moving.

-}
visiblePlacements :
    { viewProjection : Mat4
    , eye : Vec3
    , viewportHeight : Float
    , coarsen : Bool
    }
    -> InstancedScene
    -> List ( Placement, Detail )
visiblePlacements config scene =
    let
        frustum =
            frustumFromMatrix config.viewProjection
    in
    List.filterMap
        (\placement ->
            if sphereInFrustum frustum placement.center placement.radius then
                let
                    diameter =
                        2
                            * projectedRadiusPx config.viewportHeight
                                (Vec3.distance config.eye placement.center)
                                placement.radius
                in
                Just ( placement, coarsenBy config.coarsen (detailFor diameter) )

            else
                Nothing
        )
        scene.placements


coarsenBy : Bool -> Detail -> Detail
coarsenBy active detail =
    if not active then
        detail

    else
        case detail of
            Near ->
                Mid

            Mid ->
                Far

            Far ->
                Far



-- ── Rendering ─────────────────────────────────────────────────────────────────


{-| Draw an instanced scene.

    render { camera, style, width, height, coarsen } scene

Returns one entity per visible placement per active pass. `Far` placements
produce nothing here — they belong to the merged proxy pass.

-}
render :
    { camera : Camera
    , style : Style.Style
    , width : Int
    , height : Int
    , coarsen : Bool
    }
    -> InstancedScene
    -> List WebGL.Entity
render config scene =
    let
        style =
            Style.clampStyle config.style

        aspect =
            toFloat config.width / toFloat config.height

        viewMat =
            Camera.viewMatrix config.camera

        projMat =
            Camera.projectionMatrix aspect
                (Camera.nearPlane config.camera)
                (Camera.farPlane config.camera)

        eye =
            Camera.position config.camera

        viewProjection =
            Mat4.mul projMat viewMat

        frustum =
            frustumFromMatrix viewProjection

        visible =
            visiblePlacements
                { viewProjection = viewProjection
                , eye = eye
                , viewportHeight = toFloat config.height
                , coarsen = config.coarsen
                }
                scene

        triangleUniforms modelMat instanceColor =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , viewPosition = eye
            , lightDirection = style.lightDirection
            , instanceColor = instanceColor
            , ambientStrength = style.ambientStrength
            , lightStrength = style.lightStrength
            , specularStrength = style.specularStrength
            , specularPower = style.specularPower
            , rimStrength = style.rimStrength
            , rimPower = style.rimPower
            , vibrance = style.vibrance
            }

        edgeUniforms modelMat =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , edgeColor = style.edgeColor
            , viewportWidth = toFloat config.width
            , viewportHeight = toFloat config.height
            , lineWidth = style.edgeWidth
            }

        conditionalUniforms modelMat =
            { modelMatrix = modelMat
            , viewMatrix = viewMat
            , projectionMatrix = projMat
            , edgeColor = style.edgeColor
            , viewportWidth = toFloat config.width
            , viewportHeight = toFloat config.height
            , lineWidth = style.edgeWidth
            , eyePosition = eye
            }
    in
    proxyEntities frustum triangleUniforms scene
        ++ List.concatMap (placementEntities scene triangleUniforms edgeUniforms conditionalUniforms) visible


{-| Draw the merged proxy chunks that stand in for `Far` placements.

Chunks are drawn whenever they are in view rather than being matched to the
placements that were actually demoted: a chunk that overlaps the close-up
region is hidden behind the real geometry drawn over it, and the depth buffer
sorts that out for free. Boxes are always wound outward by `boxTriangles`, so
back-face culling is unconditional here.

-}
proxyEntities : Frustum -> (Mat4 -> Vec4 -> Shader.Uniforms) -> InstancedScene -> List WebGL.Entity
proxyEntities frustum triangleUniforms scene =
    scene.proxyChunks
        |> List.filter (\chunk -> sphereInFrustum frustum chunk.center chunk.radius)
        |> List.map
            (\chunk ->
                WebGL.entityWith
                    [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.back ]
                    Shader.vertexShader
                    Shader.fragmentShader
                    chunk.mesh
                    -- Proxy boxes carry their placement's colour on the vertex,
                    -- already resolved, so nothing is substituted.
                    (triangleUniforms Mat4.identity Shader.noInstanceColor)
            )


placementEntities :
    InstancedScene
    -> (Mat4 -> Vec4 -> Shader.Uniforms)
    -> (Mat4 -> EdgeShader.Uniforms)
    -> (Mat4 -> EdgeShader.ConditionalUniforms)
    -> ( Placement, Detail )
    -> List WebGL.Entity
placementEntities scene triangleUniforms edgeUniforms conditionalUniforms ( placement, detail ) =
    case ( detail, Dict.get placement.part scene.parts ) of
        ( Far, _ ) ->
            []

        ( _, Nothing ) ->
            []

        ( _, Just mesh ) ->
            let
                triangleEntity =
                    WebGL.entityWith
                        (triangleSettings mesh.bfcCertified placement.flipped)
                        Shader.vertexShader
                        Shader.fragmentShader
                        mesh.triangles
                        (triangleUniforms placement.matrix placement.color)

                lineEntity =
                    WebGL.entityWith
                        edgeSettings
                        EdgeShader.vertexShader
                        EdgeShader.fragmentShader
                        mesh.lines
                        (edgeUniforms placement.matrix)
            in
            case detail of
                Near ->
                    [ triangleEntity
                    , lineEntity
                    , WebGL.entityWith
                        edgeSettings
                        EdgeShader.conditionalVertexShader
                        EdgeShader.conditionalFragmentShader
                        mesh.conditionals
                        (conditionalUniforms placement.matrix)
                    ]

                _ ->
                    [ triangleEntity, lineEntity ]


{-| Cull the back faces of a BFC-certified part, switching to front faces when
the placement's own transform mirrors it and therefore reverses winding.
-}
triangleSettings : Bool -> Bool -> List WebGL.Settings.Setting
triangleSettings bfcCertified flipped =
    if not bfcCertified then
        [ DepthTest.default ]

    else if flipped then
        [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.front ]

    else
        [ DepthTest.default, WebGL.Settings.cullFace WebGL.Settings.back ]


{-| Matches `Render.Scene`: edges pass at equal depth without writing, and
alpha-blend over the opaque pass.
-}
edgeSettings : List WebGL.Settings.Setting
edgeSettings =
    [ DepthTest.lessOrEqual { write = False, near = 0, far = 1 }
    , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    ]



-- ── Sizing helpers ────────────────────────────────────────────────────────────


{-| Whether a model is large enough that baking world-space geometry would be
unreasonable, and the instanced path should be used instead.
-}
triangleBudgetExceeded : Int -> Bool
triangleBudgetExceeded count =
    count > instancedThreshold


{-| Placement count above which the instanced renderer takes over. Below it the
baked path in `Render.Scene` is cheaper and is already well tested.
-}
instancedThreshold : Int
instancedThreshold =
    2000



-- ── Internal ──────────────────────────────────────────────────────────────────


{-| Expand a line segment into a screen-aligned quad, mirroring
`Render.Scene`'s private `lineToQuad`.
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


type alias Bounds =
    { min : Vec3, max : Vec3 }


emptyBounds : Bounds
emptyBounds =
    { min = hugeVec, max = Vec3.negate hugeVec }


hugeVec : Vec3
hugeVec =
    vec3 1.0e12 1.0e12 1.0e12


uniformVec : Float -> Vec3
uniformVec n =
    vec3 n n n


growBounds : Vec3 -> Bounds -> Bounds
growBounds p bounds =
    { min = minVec bounds.min p, max = maxVec bounds.max p }


minVec : Vec3 -> Vec3 -> Vec3
minVec a b =
    vec3
        (min (Vec3.getX a) (Vec3.getX b))
        (min (Vec3.getY a) (Vec3.getY b))
        (min (Vec3.getZ a) (Vec3.getZ b))


maxVec : Vec3 -> Vec3 -> Vec3
maxVec a b =
    vec3
        (max (Vec3.getX a) (Vec3.getX b))
        (max (Vec3.getY a) (Vec3.getY b))
        (max (Vec3.getZ a) (Vec3.getZ b))
