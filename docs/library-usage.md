# Using the Brick Frame packages in a custom Elm application

The code is split into source packages that applications pull in through
`source-directories` (Elm packages cannot declare ports, so they are plain
directories, not published packages):

| Package | What it is | Stateful? |
|---------|-----------|-----------|
| `packages/brick-frame-simulator/` | LDraw parsing and writing, `LDraw.Document`, geometry, WebGL rendering, gear detection and physics | no — pure functions |
| `packages/brick-frame-viewport/` | `Viewport`: camera, mouse/touch/keyboard input, model loading, picking, editable-document scene | yes — own `Model`/`Msg`/`update` |
| `packages/brick-frame-simulation/` | `Simulation`: gear analysis, playback, motor, meshes of the moving parts | yes — own `Model`/`Msg`/`update` |
| `packages/brick-frame-ui/` | Theme tokens, shared view widgets (`UI.ViewPad`, `UI.Status`, …) and `theme.css` | no |
| `packages/brick-frame-data/` | Generated `Data.elm` (do not edit) and `EmbeddedParts` | no |

None of them has ports. A host application owns the ports and the JavaScript,
and talks to `Viewport` and `Simulation` through their messages and **events**.
`viewer-app/` (the viewer and the Web Components) and `studio-app/` (the editor)
are two hosts built this way; `viewer-app/src/Main.elm` is the shortest complete
reference.

The two stateful packages are components with their own `Msg` type. A host wraps
them in constructors of its own (`ViewportMsg Viewport.Msg`,
`SimulationMsg Simulation.Msg`) and maps them with `Html.map`, `Cmd.map` and
`Sub.map`. See [Msg architecture](#msg-architecture).

## Web Components distribution

The project also ships a browser-native Web Components bundle with a custom
element:

- `<bricks-viewer>`: render model with optional controller overlay and gear animation support when available

Minimal HTML usage:

```html
<script src="https://kehys.palikkaharrastajat.fi/bricks-viewer.iife.js"></script>

<bricks-viewer src="https://kehys.palikkaharrastajat.fi/examples/gears.ldr"></bricks-viewer>
```

The deployed build is configured with `VITE_LDRAW_BASE=https://kehys.palikkaharrastajat.fi/ldraw/`, so part fetches resolve over HTTPS instead of a filesystem-relative `/ldraw` path.

The viewer element accepts:

- `src` (`.ldr`, `.mpd`, `.dat`, `.io`)
- `controls` (presence boolean)
- `ldraw-base`
- `ldraw-fallback-base`
- `max-rpm`
- `motor-index` (advanced)
- `rpm` (advanced)
- `worker-mode` (`auto`, `inline`, `external`, `off`)
- `worker-url` (used when `worker-mode="external"`)
- `camera-azimuth` (degrees)
- `camera-elevation` (degrees)
- `camera-distance` (float)
- `camera-target-x`
- `camera-target-y`
- `camera-target-z`
- `camera-navigation` (`orbit` — the default — or `walk` for a street-level view)

Runtime methods are also available:

- `loadFromText(text, filename?)`
- `loadFromFile(file)`
- `loadFromUrl(url)`

Interactive configuration playground:

- `/docs/index.html` contains a live embed playground for both Viewer and
  Simulator (form editor + raw HTML + iframe preview).

### Embedding on another domain

Web Components are designed to work when your site origin differs from the
bundle origin:

```html
<script src="https://kehys.palikkaharrastajat.fi/bricks-viewer.iife.js"></script>
<bricks-viewer src="https://kehys.palikkaharrastajat.fi/examples/gears.ldr"></bricks-viewer>
```

By default (`worker-mode="auto"`), geometry flattening uses an inline worker
so the component does not depend on same-origin worker chunk URLs. If worker
startup fails, runtime falls back to main-thread flattening automatically.

Advanced worker controls:

```html
<bricks-viewer
  src="https://kehys.palikkaharrastajat.fi/examples/gears.ldr"
  worker-mode="external"
  worker-url="https://cdn.example.org/geometry-worker.js"
></bricks-viewer>
```

- `worker-mode="off"` disables workers entirely.
- `worker-mode="inline"` forces blob/inline worker usage.
- `worker-mode="external"` uses `worker-url` (or the bundle default worker
  chunk when `worker-url` is omitted).

`file://` pages are best-effort only; for reliable local use, serve your HTML
over `http://localhost` (for example `python3 -m http.server`).

Camera attributes are optional. `camera-azimuth` and `camera-elevation` are in
degrees and are converted internally to the runtime camera hash values.

Events emitted by the wrapper:

- `model-loaded`
- `model-error`
- `play-state-changed`
- `simulation-ready`
- `simulation-unavailable`
- `camera-changed` — payload carries `azimuth`, `elevation`, `azimuthDeg`,
  `elevationDeg`, `distance`, `targetX/Y/Z` and `navigation` (`"orbit"`/`"walk"`)

## Setup

### 1. Add the packages to your source directories

In your application's `elm.json`:

```json
{
  "source-directories": [
    "src",
    "../packages/brick-frame-simulator/src",
    "../packages/brick-frame-viewport/src",
    "../packages/brick-frame-simulation/src",
    "../packages/brick-frame-data/src",
    "../packages/brick-frame-ui/src"
  ]
}
```

Adjust the relative paths as needed. A viewer without simulation can leave out
`brick-frame-simulation`; `brick-frame-ui` is only for the widgets.

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

`viewer-app/elm.json` and `studio-app/elm.json` list the full set.

### 3. Generate the data modules

`packages/brick-frame-data/src/Data.elm` and
`packages/brick-frame-simulator/src/LDraw/ColorData.elm` are produced by the
Haskell generator. Run `make generate` once before the first build. They are
**never edited by hand**: colours come from `LDConfig.ldr` in the synced LDraw
library (`make sync-ldraw`).

### 4. Ports and the JavaScript runtime

The packages delegate effects to the host. The runtime in
`packages/brick-frame-viewport/runtime/initBricksRuntime.js` implements the
JavaScript half — file picking and reading (including BrickLink Studio `.io`
archives), the geometry worker (`geometry-worker.js` next to it), URL-hash
syncing, drag and drop and resize observation — and expects the host's compiled
Elm module:

```js
import { initBricksRuntime } from '../packages/brick-frame-viewport/runtime/initBricksRuntime.js'
import { Elm } from './src/Main.elm'

initBricksRuntime({ elm: Elm.Main, node: document.getElementById('app'), /* options */ })
```

The host declares these ports (copy `viewer-app/src/Ports.elm` or
`studio-app/src/Ports.elm`):

| Port | Direction | Purpose |
|------|-----------|---------|
| `requestGeometryFlatten` | Elm → JS | JSON for the geometry worker (from `Viewport`'s `FlattenRequested` event) |
| `geometryFlattened` | JS → Elm | Flattened geometry JSON → `Viewport.GeometryFlattened` |
| `geometryFlattenFailed` | JS → Elm | Worker error string → `Viewport.GeometryFlattenFailed` |
| `requestFileUpload` | Elm → JS | Open the native file picker |
| `fileContentReceived` | JS → Elm | LDraw text of a file or URL |
| `fileLoadError` | JS → Elm | File read error string |
| `setUrlHash` | Elm → JS | Update `window.location.hash` |
| `viewportResized` | JS → Elm | Optional: container size from a `ResizeObserver` |
| `runtimeEvent` | Elm → JS | Optional: lifecycle events for wrappers (viewer only) |

The runtime also keeps an in-session cache of geometry worker results keyed by
flatten payload, so reloading a model can skip the flatten.

`initBricksRuntime` options include `elm`, `node`, `ldrawBase`,
`ldrawFallbackBase`, `defaultModel`, `initialHash`, `syncUrlHash`,
`useWindowResize`, `workerMode`/`workerUrl`, `studioUrl` (shows the viewer's
"Edit in studio" link), `loadModelInJs` (read the default model in JavaScript
and deliver it through `fileContentReceived`, as the studio does) and
`extraFlags` (merged into the Elm flags).

### 5. Flags

The flags the runtime passes are one record, shared by the viewer and the studio:
`ldrawBase`, `ldrawFallbackBase`, `defaultModelUrl`, `initialHash`, `maxRpm`,
`uiMode`, `controlsEnabled`, `initialMotorIndex`, `initialRpm`, `useWindowResize`,
`ambientStrength`, `lightStrength`, `vibrance`, `edgeWidth`,
`supportsPointerEvents` and `studioUrl`. A host decodes the ones it uses and
ignores the rest; the studio adds `draft` (via `extraFlags`).

---

## The Viewport component

`Viewport` shows an LDraw model and lets the user move around it.

```elm
import Viewport

type alias Model = { viewport : Viewport.Model, ... }
type Msg = ViewportMsg Viewport.Msg | ...

( viewport, cmd ) =
    Viewport.init
        { ldrawBase = flags.ldrawBase
        , ldrawFallbackBase = flags.ldrawFallbackBase
        , defaultModelUrl = flags.defaultModelUrl   -- "" to load nothing
        , initialHash = flags.initialHash           -- camera: #az=..&el=..&d=..
        , supportsPointerEvents = flags.supportsPointerEvents
        , useWindowResize = flags.useWindowResize
        , renderStyle = Style.defaultStyle
        , partCache = EmbeddedParts.cache           -- parts compiled into the bundle
        }
```

It owns the camera (orbit, pan, zoom, walk, view presets, fit view), mouse, touch
and pointer input (exactly one of the Touch or Pointer listener families is
bound, chosen by `supportsPointerEvents`), held keys, and the load pipeline:
fetch → resolve parts → geometry worker → scene. Frame the camera by writing
nothing: the viewport auto-fits until the user moves it.

### Messages the host forwards

Browser events inside the canvas are handled by `Viewport.view`; document-level
events (mouse move/up, keys, visibility, resize and animation frames) by
`Viewport.subscriptions`. The host forwards its own port subscriptions as
`Viewport.FileContentReceived`, `FileLoadError`, `GeometryFlattened`,
`GeometryFlattenFailed` and `WindowResize` (when it sizes the canvas itself).

### Events the host handles

`Viewport.update` returns `( Model, Cmd Msg, List Event )`. The effects the
viewport cannot perform come back as events, in order:

| Event | Meaning | Host does |
|-------|---------|-----------|
| `CameraSettled` | The camera came to rest | Write the URL hash |
| `Clicked x y shift` | Mouse press-release without dragging | Decide what is under it (`Simulation.pickGear`, `Viewport.pickSource`) |
| `Tapped x y` | One-finger tap | Same |
| `PressStarted x y` | A mouse button or finger went down | Optionally `Viewport.captureDrag` to take the gesture |
| `DragMoved x y` / `DragEnded x y` | While a gesture is captured | Drive a tool; the camera does not move |
| `LoadStarted` | A new model replaces the old one | Reset anything derived from it |
| `PartsReady` | All parts loaded; model small enough to bake | Analyse, then `Viewport.beginFlatten { lines, fallbackLines }` |
| `LargeModel n` | Instanced path; nothing to analyse | e.g. `Simulation.markUnavailable` |
| `FlattenRequested json` | Geometry worker request | Send through `requestGeometryFlatten` |
| `ModelReady` | A scene is showing | Report it |
| `LoadFailed message` | Fetching or reading failed | Report it |
| `KeyAccepted keyEvent` | A key for the host (not typing, not auto-repeat) | `Viewport.Shortcuts.actionFor`, then map to messages |

A click on the canvas that did not start on the canvas (a toolbar button) is not
a click: the viewport only reports releases that follow a canvas press.

### Drawing

```elm
Viewport.view
    { extraEntities = \camera style aspect -> Simulation.entities camera style aspect sim
    , coarsenWhileMoving = not sim.simulationAvailable
    }
    model.viewport
```

`extraEntities` adds host-owned WebGL entities (moving gears, a selection
outline) over either kind of scene. The viewport draws the canvas only; status
boxes, controls and panels are the host's (the `brick-frame-ui` widgets help).

### Editable documents

`Viewport.loadDocument document viewport` shows an `LDraw.Document` through the
instanced renderer, and `Viewport.applyDocument` keeps the scene in step with
later edits, rebuilding only what changed — see `docs/rendering.md`. The document
records which placements an edit touched (`Document.touched`); call
`Document.clearTouched` after applying. `Viewport.pickSource x y viewport` returns
the id of the document placement under a point: the nearest surface the ray
actually hits (part triangles, not bounding boxes), or `Nothing` over empty space.
`Viewport.pickPoint` also returns where the ray met it (render space), for
dragging a part by the spot that was grabbed.

Event coordinates are canvas pixels only if the viewport knows where the canvas
sits. A host that places it anywhere but the window's top-left corner (beside a
panel, under a header) must call `Viewport.setCanvasOrigin left top` whenever its
layout changes, or every pick lands offset from the pointer. The studio does this
from `Studio.Layout`.

---

## The Simulation component

`Simulation` finds the gears in a loaded model and plays them.

```elm
simulation =
    Simulation.init
        { gearSpecs = Data.gearParts
        , lodCache = EmbeddedParts.lodCache
        , maxRpm = flags.maxRpm
        , initialRpm = flags.initialRpm
        , requestedMotorIndex = Nothing
        }
```

It never touches the viewport. When the viewport reports `PartsReady`, the host
runs the analysis and tells the viewport what to bake:

```elm
Viewport.PartsReady ->
    let
        ( simulation, analysis ) =
            Simulation.analyse
                { lines = viewport.topLevelLines, cache = viewport.partCache }
                model.simulation

        ( viewport2, events ) =
            Viewport.beginFlatten
                { lines = analysis.staticLines, fallbackLines = analysis.fallbackLines }
                viewport
    in
    ...
```

`analysis.staticLines` are everything that is not a gear or riding on one;
`fallbackLines` are what the viewport flattens locally if the worker fails.
Playback messages are `Play`, `Pause`, `Stop`, `ToggleMotor`, `SetMotorGear`,
`SetMotorSpeed` and `Tick` (animation frame); `Simulation.update` returns events
(`PlayStateChanged`). `Simulation.reset` on `LoadStarted`,
`Simulation.refreshLods camera` after anything that may move the camera, and
`Simulation.entities` for drawing complete the contract. Panels that show gears,
the motor and the clock are the host's: read the fields of `Simulation.Model`.

---

## Msg architecture

`Viewport` and `Simulation` are the exception to "all `Msg` constructors live in
the app's `Main`": they are stateful components with their own `Model`, `Msg` and
`update`, so their input handling can be shared instead of copied. Everything
else in the packages is pure functions. The viewer keeps one flat `Msg` for its
own UI, with `ViewportMsg` and `SimulationMsg` wrappers; the studio gives each
page its own nested `Msg` (`WorkspaceMsg`, `PreviewMsg`). Keyboard handling still
goes through `Viewport.Shortcuts.actionFor`, which holds the guard that stops
document-wide keydowns hijacking a host page's form fields.

---

## Module reference

| Module | Responsibility | Key functions |
|--------|---------------|---------------|
| `LDraw.Parser` | Parse LDraw text into an AST | `parseFile`, `parseLine`, `splitMpd` |
| `LDraw.Types` | LDraw AST types | `LDrawLine`, `SubFileRef`, `Triangle`, … |
| `LDraw.Document` | Editable, round-trippable file: submodels, placements with stable ids, verbatim META/comment lines | `parse`, `toLines`, `placements`, `mainPlacements`, `addPlacement`, `updatePlacement`, `removePlacement`, `insertPlacement`, `touched` |
| `LDraw.Writer` | The reverse of the parser | `writeDocument`, `writeLines` |
| `LDraw.Resolve` | HTTP part cache | `PartCache`, `pendingParts`, `fetchPart` |
| `LDraw.Colors` | Color code → RGBA | `resolveColor`, `toVec4` |
| `LDraw.ColorData` | Generated LDraw colour table (do not edit) | `colorTable` |
| `LDraw.Geometry` | Flatten part tree → vertex buffers | `flatten`, `FlatGeometry` |
| `Render.Camera` | Orbit and walk camera | `init`, `viewMatrix`, `projectionMatrix`, `onMouseMove`, `onPan`, `onWheel`, `zoomByRatio`, `orbitBy`, `lookAround`, `moveBy`, `dollyBy`, `enterWalk`, `walkSpeed`, `setNavigation`, `setSceneRadius`, `setZoomRange`, `setViewportHeight`, `fovYDegrees` |
| `Render.Pick` | Screen rays, ray–sphere/box/triangle tests, frustum test | `screenRay`, `raySphereHit`, `rayAabbHit`, `rayTriangleHit`, `nearestTriangleHit`, `nearestTriangleHitBy`, `isSphereVisible` |
| `Render.Mesh` | Vertex type aliases | `Vertex`, `EdgeVertex` |
| `Render.Shader` | Lightweight plastic GLSL shaders | `vertexShader`, `fragmentShader` |
| `Render.EdgeShader` | Edge line GLSL shaders | `vertexShader`, `fragmentShader` |
| `Render.Lighting` | Light uniforms | `LightUniforms`, `defaultLight` |
| `Render.Style` | Scene/material style config | `Style`, `defaultStyle`, `clampStyle`, `fromLight` |
| `Render.Scene` | Build and render a baked scene | `buildScene`, `renderScene`, `renderSceneWithStyle`, `Scene` |
| `Render.Instanced` | Instanced scene for large or editable models | `build`, `buildFrom`, `replaceSource`, `removeSource`, `render` |
| `Gear.Types` | Core gear types | `GearSpec`, `GearInstance`, `GearGraph`, `GearId` |
| `Gear.Detect` | Gear detection from part tree | `extractGears`, `buildGearGraph` |
| `Gear.Physics` | Rotation propagation | `propagate`, `angleAt` |
| `Gear.Animate` | Motor + animation state | `MotorState`, `defaultMotor`, `gearAngles` |
| `Gear.Components` | Axle/beam detection | `extractComponents`, `ComponentInstance` |
| `Viewport` | The viewport component | `init`, `update`, `view`, `subscriptions`, `loadDocument`, `applyDocument`, `pickSource`, `pickPoint`, `setCanvasOrigin`, `captureDrag`, `beginFlatten` |
| `Viewport.Shortcuts` | Pure keyboard mapping and its typing guard | `actionFor`, `movementFor`, `lookFor`, `helpRows` |
| `Viewport.Hash` | Camera ↔ URL hash | `decode`, `encodeCamera` |
| `Viewport.Framing` | Model bounds, auto-fit, zoom range, view presets | `cameraForBounds`, `applyZoomRange`, `ViewPreset` |
| `Viewport.Sync` | Plan scene updates from document edits | `plan`, `buildAll` |
| `Simulation` | The simulation component | `init`, `analyse`, `update`, `entities`, `pickGear`, `refreshLods`, `reset` |
| `Simulation.Meshes` | Meshes of gears, axles and parts that ride on them | `buildGearMeshes`, `buildComponentMeshRenders` |

---

## Using the pure modules directly

If the components do not fit, the pure modules compose into the same pipeline.
`viewer-app` before the extraction followed these steps.

### Step 1 — Initialise the part cache

Pre-seed the cache with parts embedded at build time to avoid HTTP round-trips
for the core gear types (`EmbeddedParts.cache` does exactly this):

```elm
embeddedPartCache : Resolve.PartCache
embeddedPartCache =
    Dict.foldl
        (\name text acc -> Dict.insert name (Resolve.Loaded (Parser.parseFile text)) acc)
        Resolve.initCache
        Data.embeddedParts
```

### Step 2 — Load the top-level model

Fetch a `.ldr` / `.mpd` file via HTTP or a file upload port, then parse it with
`Parser.parseFile`. For `.mpd` files, `Resolve.seedFromMpd` inserts the embedded
files into the part cache and returns the main file's name. For BrickLink Studio
`.io` files, unzip in JS and load `modelv2.ldr` first (fallback `model.ldr`, then
`model2.ldr`), stripping a UTF-8 BOM before passing text into Elm.

### Step 3 — Resolve sub-parts

Call `Resolve.pendingParts lines cache` to get the sub-files that are referenced
but not yet cached, fetch each with `Resolve.fetchPart`, and add the response to
the cache. Repeat until nothing is pending — each response may introduce new
references.

### Step 4 — Flatten geometry

Call `Geometry.flatten lines cache 15 Math.Matrix4.identity` for small models, or
offload to the Web Worker (`Viewport.Wire.flattenRequest` builds the payload and
`Viewport.Wire.flattenResultDecoder` reads the answer), then

```elm
scene = Scene.buildScene { triangles = flat.triangles, lines = flat.lines, conditionalLines = flat.conditionalLines } flat.bfcCertified
```

### Step 5 — Gears, animation and rendering

`Detect.extractGears Data.gearParts lines cache` and `Detect.buildGearGraph`
find the train; `Animate.gearAngles motor graph time` gives every gear's angle at
a time (angle = ratio × speed × t, so scrubbing needs no integration state);
`Scene.renderSceneWithStyle` draws the static scene and per-gear
`WebGL.entityWith` calls with a rotation matrix about each gear's axis draw the
moving parts. `Simulation.Meshes` and `Simulation.entities` are the reference
for the rotation matrix and frustum-culling pattern.

### Camera setup

`Render.Camera.Camera` carries three fields beyond the orbit parameters, and a
host application is responsible for keeping them current (`Viewport` does this
for you through `Viewport.Framing`):

| Field | Set via | Why |
|-------|---------|-----|
| `viewportHeight` | `Camera.setViewportHeight` on every resize | Orbit and pan sensitivity are relative to canvas height |
| `minDistance` / `maxDistance` | `Camera.setZoomRange` once the model bounds are known | The usable zoom range depends on model size; leaving the defaults caps a large model at 2000 LDU |
| `sceneRadius` | `Camera.setSceneRadius` once the model bounds are known | **Leaving this at 0 clips geometry further than ten orbit distances away.** It also sets the walking speed |

### Walk mode

`Camera.enterWalk groundY` switches to first-person navigation at street level;
`Camera.setNavigation Camera.Orbit` returns. In walk mode, route drags through
`Camera.lookAround` instead of `Camera.orbitBy`, and wheel/pinch through
`Camera.dollyBy` instead of `Camera.zoomByRatio` — `Camera.onMouseMove` already
dispatches on `navigation` for you (`Viewport.Navigation` has the dispatching
helpers). Movement is `Camera.moveBy`, scaled by `Camera.walkSpeed` and the frame
time. There is no collision detection.

Wheel deltas must be normalised for `deltaMode` before reaching
`Camera.onWheel`; see `Viewport.Decode.wheelDeltaDecoder`. Pinch gestures should
use `Camera.zoomByRatio` with the ratio of successive finger separations rather
than converting pixels into a synthetic wheel delta.

---

## Data.elm exports

`packages/brick-frame-data/src/Data.elm` is auto-generated by `make generate`. Do
not edit it manually. `EmbeddedParts` (hand-written, same directory) turns its
part text into ready-to-use part caches.

| Export | Type | Description |
|--------|------|-------------|
| `ldrawColors` | `Dict Int { r, g, b, alpha }` | Every official LDraw colour, parsed from `viewer-app/public/ldraw/LDConfig.ldr` |
| `embeddedParts` | `Dict String String` | Pre-loaded LDraw part text for core gears and their dependencies |
| `lodParts` | `Dict String String` | Simplified (LOD) versions of embedded parts |
| `gearParts` | `List { partFile, teeth, pitchRadius }` | Known gear specs |
| `exampleModels` | `List { label, url }` | Sample model URLs (not exposed in demo UI) |

---

## Running tests for the packages

Each package has its own test suite:

```sh
make test-lib
# or manually:
cd packages/brick-frame-simulator && elm-test
cd packages/brick-frame-viewport && elm-test
cd packages/brick-frame-simulation && elm-test
cd packages/brick-frame-ui && elm-test
```

They cover parsing and writing (including the `parse → write → parse` identity on
the example files, an MPD and a Studio-style file), geometry, gear detection and
physics, the viewport's input and loading state machine and its document mode,
the simulation's analysis and playback, and the shared UI tokens.
