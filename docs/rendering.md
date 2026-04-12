# Rendering

The simulator renders 3D LDraw models using **WebGL** via
[elm-explorations/webgl 1.1.3](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/).
All rendering logic lives in `packages/technic-simulator/src/Render/`.

## Architecture overview

Each animation frame produces a list of `WebGL.Entity` values that are passed
to a `WebGL.toHtml` canvas element. There are two categories of entity:

- **Static scene** — the non-gear geometry of the loaded model, built once
  after all parts resolve and held in `Model.scene : Maybe Scene`.
- **Gear entities** — one entity per detected gear, built from pre-computed
  meshes (`Model.gearMeshes`) with a per-frame rotation matrix applied.

## Coordinate system

LDraw uses a right-handed coordinate system where **−Y is up** (Y increases
downward). The simulator converts to the standard WebGL Y-up convention by
negating every Y coordinate during geometry flattening. This negation is
applied in two places:

- `LDraw.Geometry.flatten` (Elm, [packages/technic-simulator/src/LDraw/Geometry.elm](../packages/technic-simulator/src/LDraw/Geometry.elm)) — for the Elm-side path.
- `geometry-worker.js` ([elm-app/geometry-worker.js](../elm-app/geometry-worker.js)) — for the Web Worker path.

All world positions stored in `GearInstance.worldPosition` and
`GearInstance.worldMatrix` are already in Y-up space.

## Geometry flattening pipeline

LDraw files are hierarchical: a top-level file references sub-files (parts,
sub-parts, primitives) via **type-1 lines**, each carrying a 4×4 transform.
Flattening resolves this tree into a flat list of triangles and edges ready
for the GPU.

Steps (in order):

1. **Parse** — `LDraw.Parser.parseFile` splits text into `List LDrawLine`.
2. **Resolve** — `LDraw.Resolve` fetches missing sub-files over HTTP and
   caches them in `PartCache`. Common gear parts are pre-loaded from
   `Data.embeddedParts` at init (no HTTP needed for those).
3. **Flatten** — `LDraw.Geometry.flatten lines cache parentColor worldTransform`
   accumulates transforms (`Mat4.mul parentMat localMat`) as it recurses,
   collecting:
   - **Triangles** (type 3): one triangle, per-face normal via cross product.
   - **Quads** (type 4): split into two triangles along the p1–p3 diagonal.
   - **Edge lines** (type 2): passed through as `(Vec3, Vec3)` pairs.
   - **Conditional lines** (type 5): stored as `ConditionalEdge` records for
     per-frame visibility testing (see below).
   - Color inheritance: color code 16 inherits the parent's color; code 24
     resolves to black (edge color).
4. **Web Worker** — the flatten step is offloaded to `geometry-worker.js` via
   the `requestGeometryFlatten` port to avoid blocking the main thread. The
   worker sends back a JSON payload decoded in `GeometryFlattened`.

Gear geometry is **not** included in the static scene. Gears are detected,
flattened separately per-gear in `buildGearMeshes`, and rendered with a live
rotation transform each frame.

## Scene building and rendering

After flattening, `Render.Scene.buildScene` uploads the geometry to the GPU:

```elm
buildScene :
    { triangles : List ( Vertex, Vertex, Vertex )
    , lines : List ( Vec3, Vec3 )
    , conditionalLines : List ConditionalEdge
    }
    -> Bool   -- bfcCertified
    -> Scene
```

`Scene` holds three compiled `WebGL.Mesh` values plus the `bfcCertified` flag.

Each frame, `Render.Scene.renderScene scene camera light aspect` returns
entities in this order:

1. **Triangle mesh** — depth test enabled; back-face culling enabled only when
   `bfcCertified = True` (see BFC section below).
2. **Edge line mesh** — flat dark lines, depth test enabled.
3. **Conditional lines** — a dynamic mesh rebuilt each frame from the edges
   that pass the visibility test (see below). Omitted when the list is empty.

MVP matrices are built fresh each frame from the current `Camera` state.

## Shaders

### Lambert shaders (`Render.Shader`)

Used for triangle geometry of both the static scene and gears.

**Vertex shader** transforms position through MVP and passes the world-space
normal and RGBA colour to the fragment stage:

```glsl
vNormalWorld = mat3(modelMatrix) * normal;
gl_Position  = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
```

**Fragment shader** computes Lambert diffuse with an ambient floor:

```glsl
float diffuse = max(0.0, dot(normalize(vNormalWorld), normalize(lightDirection)));
float light   = ambientStrength + (1.0 - ambientStrength) * diffuse;
gl_FragColor  = vec4(vColor.rgb * light, vColor.a);
```

### Edge shader (`Render.EdgeShader`)

Used for type-2 edge lines and conditional lines. Vertices carry only
`position`; the fragment shader outputs a fixed dark colour
(`vec4(0.1, 0.1, 0.1, 1.0)`).

### Guide shader (`Render.GuideShader`)

Used for component (axle/beam) visualisation overlays.

## Lighting

A single directional light is defined in `Render.Lighting`:

```elm
defaultLight : LightUniforms
defaultLight =
    { lightDirection  = vec3 0.3 1.0 0.5   -- world space, normalised in shader
    , ambientStrength = 0.3
    }
```

There are no point lights, spotlights, shadows, or reflections.

## Camera

`Render.Camera` implements a spherical orbit camera:

| Field | Meaning |
|-------|---------|
| `azimuth` | Horizontal angle around Y axis (radians) |
| `elevation` | Vertical angle from XZ plane (radians, clamped ±π/2) |
| `distance` | Orbit radius in LDU (clamped 0.5–500) |
| `target` | World-space orbit centre (default origin) |

Mouse drag updates `azimuth` and `elevation` at 0.005 rad/px. Scroll wheel
scales `distance` by `1 + delta × 0.001`.

Projection: `Mat4.makePerspective 45 aspect 0.1 2000.0` (FOV 45°, clip 0.1–2000 LDU).

## Gear rendering

Gear meshes are pre-built once (`buildGearMeshes` in `Main.elm`) by flattening
each gear's part file with `LDraw.Geometry.flatten`. At render time, each gear
gets its own `WebGL.Entity` with a rotation model matrix:

```
modelMatrix = translate(pivot) · rotate(angle, axis) · translate(-pivot)
```

where `pivot` is the gear's `worldPosition`, `angle` comes from
`Gear.Animate.gearAngles`, and `axis` is extracted from the gear's world matrix
(local Z column after Y-up conversion).

Frustum culling: each gear's bounding sphere is tested against the view frustum
before creating an entity — gears outside the frustum are skipped.

## BFC (back-face culling)

LDraw parts may declare `0 BFC CERTIFY CCW` to guarantee consistent CCW winding.
When `bfcCertified = True`, `renderScene` passes
`WebGL.Settings.cullFace WebGL.Settings.back` to the triangle entity, discarding
back-facing triangles. For uncertified parts this setting is omitted, which is
why some parts may show inside-out faces (see Limitations).

## Conditional lines (type 5)

A conditional line is rendered only when both of its control points (`c1`, `c2`)
are on the same side of the edge as seen from the camera. The test in
`Render.Scene.conditionalLineVisible` uses the sign of the dot products of the
cross product with the eye direction:

```elm
side1 * side2 > 0   -- same side ⟹ render the edge
```

The visible set is recomputed each frame and uploaded as a fresh `WebGL.lines`
mesh.

## LOD meshes

The Haskell generator emits `Data.lodParts : Dict String String` containing
simplified versions of the embedded gear parts (every other geometry line
kept). These are available for distance-based LOD switching, though the main
app currently uses full-detail meshes for all gear rendering.

## Limitations

- **Edge line width is always 1 px.** WebGL 1.0 does not support `lineWidth > 1`
  on most hardware. Thick edges would require rendering lines as geometry quads.
- **No BFC for uncertified parts.** Parts without `BFC CERTIFY CCW` render
  double-sided, so inside-out faces may be visible.
- **No shadows.** The scene uses a single directional light with no shadow map.
- **No transparency sorting.** Transparent LEGO colors (alpha < 1) are rendered
  in draw order; overlapping transparent faces may composite incorrectly.
- **No smooth shading for static geometry.** Normals are per-face (computed by
  cross product per triangle). Gear meshes share the same limitation, though
  adjacent triangles from the same quad already have consistent normals.
- **No normal maps or PBR.** Shading is Lambert diffuse only.
- **Co-axial rotation is top-level only.** Top-level `SubFileRef` nodes that
  share an axle with a detected gear are rendered with rotation. Parts nested
  inside sub-assemblies are still baked into the static scene and do not rotate.
