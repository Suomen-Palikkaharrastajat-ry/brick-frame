# Using Brick Frame simulator in a custom Elm application

`packages/brick-frame-simulator/` is a self-contained Elm source package that
provides LDraw parsing, 3D rendering, gear detection, and gear physics as
**pure functions**. It has no ports of its own — those live in the host
application. The `elm-app/` demo wires everything together and serves as a
reference implementation.

## Web Components distribution

The project also ships a browser-native Web Components bundle with two custom
elements:

- `<bricks-viewer>`: render model only (no simulation controls)
- `<bricks-simulator>`: render model + simulation UI

Minimal HTML usage:

```html
<script src="https://kehys.palikkaharrastajat.fi/brick-frame.iife.js"></script>

<bricks-viewer src="https://kehys.palikkaharrastajat.fi/examples/gears.ldr"></bricks-viewer>
<bricks-simulator src="https://kehys.palikkaharrastajat.fi/examples/gears.ldr"></bricks-simulator>
```

Both elements accept:

- `src` (`.ldr`, `.mpd`, `.dat`, `.io`)
- `ldraw-base`
- `ldraw-fallback-base`
- `max-rpm`

Runtime methods are also available:

- `loadFromText(text, filename?)`
- `loadFromFile(file)`
- `loadFromUrl(url)`

Events emitted by the wrapper:

- `model-loaded`
- `model-error`
- `play-state-changed`
- `simulation-ready`
- `simulation-unavailable`

## Setup

### 1. Add the package to your source directories

In your application's `elm.json`, add the path to the package's `src/`
directory:

```json
{
  "source-directories": [
    "src",
    "../packages/brick-frame-simulator/src"
  ]
}
```

Adjust the relative path as needed.

### 2. Add required dependencies

Your `elm.json` `"dependencies"` must include:

```json
"elm-explorations/webgl": "1.1.3 <= v < 2.0.0",
"elm-explorations/linear-algebra": "1.0.3 <= v < 2.0.0",
"elm/http": "2.0.0 <= v < 3.0.0",
"elm/json": "1.1.3 <= v < 2.0.0",
"elm/time": "1.0.0 <= v < 2.0.0",
"elm/browser": "1.0.2 <= v < 2.0.0"
```

See `elm-app/elm.json` for the full list used by the demo app.

### 3. Generate Data.elm

The host app must have an `elm-app/src/Data.elm` (or equivalent path) produced
by the Haskell generator. Run `make generate` once before first build. This
file exports pre-loaded part data and the color table — it is **never edited
by hand**.

### 4. Add JavaScript ports

The package delegates two operations to JavaScript via ports:

| Port | Direction | Purpose |
|------|-----------|---------|
| `requestGeometryFlatten` | Elm → JS | Send part tree JSON to a Web Worker |
| `geometryFlattened` | JS → Elm | Receive flattened geometry JSON |
| `geometryFlattenFailed` | JS → Elm | Worker error string |
| `requestFileUpload` | Elm → JS | Open native file picker |
| `fileContentReceived` | JS → Elm | LDraw file text |
| `fileLoadError` | JS → Elm | File read error string |
| `setUrlHash` | Elm → JS | Update `window.location.hash` |

Copy `elm-app/src/Ports.elm` and `elm-app/geometry-worker.js` into your app and
wire them up in your JS entry point as in `elm-app/main.js`.

`elm-app/main.js` also includes an in-session cache for geometry worker results
keyed by flatten payload, so repeated loads of the same model can skip repeat
flatten work.

---

## Module reference

| Module | Responsibility | Key functions |
|--------|---------------|---------------|
| `LDraw.Parser` | Parse LDraw text into an AST | `parseFile`, `parseLine`, `splitMpd` |
| `LDraw.Types` | LDraw AST types | `LDrawLine`, `SubFileRef`, `Triangle`, … |
| `LDraw.Resolve` | HTTP part cache | `PartCache`, `pendingParts`, `fetchPart` |
| `LDraw.Colors` | Color code → RGBA | `resolveColor`, `toVec4` |
| `LDraw.Geometry` | Flatten part tree → vertex buffers | `flatten`, `FlatGeometry` |
| `Render.Camera` | Orbit camera | `init`, `viewMatrix`, `projectionMatrix`, `onMouseMove`, `onWheel` |
| `Render.Mesh` | Vertex type aliases | `Vertex`, `EdgeVertex` |
| `Render.Shader` | Lightweight plastic GLSL shaders | `vertexShader`, `fragmentShader` |
| `Render.EdgeShader` | Edge line GLSL shaders | `vertexShader`, `fragmentShader` |
| `Render.Lighting` | Light uniforms | `LightUniforms`, `defaultLight` |
| `Render.Style` | Scene/material style config | `Style`, `defaultStyle`, `clampStyle`, `fromLight` |
| `Render.Scene` | Build and render a scene | `buildScene`, `renderScene`, `renderSceneWithStyle`, `Scene` |
| `Gear.Types` | Core gear types | `GearSpec`, `GearInstance`, `GearGraph`, `GearId` |
| `Gear.Detect` | Gear detection from part tree | `extractGears`, `buildGearGraph` |
| `Gear.Physics` | Rotation propagation | `propagate`, `angleAt` |
| `Gear.Animate` | Motor + animation state | `MotorState`, `defaultMotor`, `gearAngles` |
| `Gear.Components` | Axle/beam detection | `extractComponents`, `ComponentInstance` |

---

## Minimal integration walkthrough

The steps below sketch the loading and rendering lifecycle. The demo app
`elm-app/src/Main.elm` is the complete reference.

### Step 1 — Initialise the part cache

Pre-seed the cache with parts embedded at build time to avoid HTTP round-trips
for the four core gear types:

```elm
import Data
import LDraw.Parser as Parser
import LDraw.Resolve as Resolve

embeddedPartCache : Resolve.PartCache
embeddedPartCache =
    Dict.foldl
        (\name text acc ->
            Dict.insert name (Resolve.Loaded (Parser.parseFile text)) acc
        )
        Resolve.initCache
        Data.embeddedParts
```

### Step 2 — Load the top-level model

Fetch a `.ldr` / `.mpd` file via HTTP or a file upload port, then parse it:

```elm
lines : List LDraw.Types.LDrawLine
lines = Parser.parseFile rawText
```

For `.mpd` files, split first:

```elm
files : Dict String String
files = Parser.splitMpd rawText
-- Insert all files into the part cache, then use the root file as `lines`.
```

For BrickLink Studio `.io` files, unzip in JS and load `modelv2.ldr` first
(fallback `model.ldr`, then `model2.ldr`), stripping UTF-8 BOM before passing
text into Elm.

### Step 3 — Resolve sub-parts

Call `Resolve.pendingParts lines cache` to get the list of sub-files that are
referenced but not yet cached. Fetch each with an HTTP command and add the
response to the cache. Repeat until `pendingParts == []`.

The demo app handles this with the `PartLoaded` message — each response may
introduce new sub-file references that require further fetches.

### Step 4 — Flatten geometry

Once all parts are loaded, offload the flatten step to the Web Worker via the
`requestGeometryFlatten` port (see `elm-app/geometry-worker.js` for the worker
implementation). When `geometryFlattened` fires back, decode the JSON payload
and build the scene:

```elm
import LDraw.Geometry as Geometry
import Render.Scene as Scene

flatGeom : Geometry.FlatGeometry
flatGeom = -- decoded from worker JSON response

scene : Scene.Scene
scene = Scene.buildScene
    { triangles        = flatGeom.triangles
    , lines            = flatGeom.lines
    , conditionalLines = flatGeom.conditionalLines
    }
    flatGeom.bfcCertified
```

Alternatively, call `Geometry.flatten` directly on the main thread for small
models (no worker needed):

```elm
flatGeom = Geometry.flatten lines cache 15 Math.Matrix4.identity
```

### Step 5 — Detect gears

```elm
import Gear.Detect as Detect
import Data

gearInstances : List Gear.Types.GearInstance
gearInstances = Detect.extractGears Data.gearParts lines cache

gearGraph : Gear.Types.GearGraph
gearGraph = Detect.buildGearGraph gearInstances
```

### Step 6 — Build gear meshes

Pre-compute a `WebGL.Mesh Vertex` for each gear so rotation is cheap at render
time:

```elm
-- For each GearInstance:
let flatGeom = Geometry.flatten (getPartLines gear.spec.partFile cache) cache 15 Mat4.identity
    mesh     = WebGL.triangles flatGeom.triangles
```

Store the mesh, centre (`gear.worldPosition`), and axis (local Z of
`gear.worldMatrix`) in your model.

### Step 7 — Animate

On each `Browser.Events.onAnimationFrame`, accumulate `totalTime` and compute
angles:

```elm
import Gear.Animate as Animate

gearAngles : Dict Gear.Types.GearId Float
gearAngles = Animate.gearAngles motor gearGraph totalTime
```

### Step 8 — Render

```elm
import Render.Scene as Scene
import Render.Camera as Camera
import Render.Style as Style

-- Static scene entity list
entities : List WebGL.Entity
entities = Scene.renderSceneWithStyle scene camera Style.defaultStyle aspect

-- Per-gear rotation entities (one per gear)
gearEntities : List WebGL.Entity
gearEntities =
    List.filterMap
        (\gear ->
            let angle  = Dict.get gear.id gearAngles |> Maybe.withDefault 0.0
                rotMat = rotationAround angle gearAxis gearPivot
            in  -- build WebGL.entityWith using Render.Shader.vertexShader/fragmentShader
        )
        (Array.toList gearGraph.instances)

-- Combine and pass to WebGL.toHtml
allEntities = entities ++ gearEntities
```

See `Main.elm` around `renderGearEntities` for the complete rotation matrix
construction and frustum culling pattern.

---

## Data.elm exports

`Data.elm` is auto-generated by `make generate`. Do not edit it manually.

| Export | Type | Description |
|--------|------|-------------|
| `ldrawColors` | `Dict Int { r, g, b, alpha }` | Known LDraw/LEGO color codes used by the simulator |
| `embeddedParts` | `Dict String String` | Pre-loaded LDraw part text for core gears and their dependencies |
| `lodParts` | `Dict String String` | Simplified (LOD) versions of embedded parts |
| `gearParts` | `List { partFile, teeth, pitchRadius }` | Known gear specs |
| `exampleModels` | `List { label, url }` | Sample model URLs (not exposed in demo UI) |

---

## Running tests for the package

The package has its own test suite in `packages/brick-frame-simulator/tests/`:

```sh
make test-lib
# or manually:
cd packages/brick-frame-simulator && elm-test
```

Tests cover `LDraw.Parser`, `LDraw.Geometry`, `Gear.Detect`, `Gear.Physics`,
and `LDraw.Colors`.
