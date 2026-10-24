# AGENTS.md — Palikkakehys

Browser-based Brick Frame gear simulator built with Elm + WebGL. Loads LDraw `.ldr`/`.mpd` files, renders 3D models, detects gear trains, and simulates gear rotation physics with playback controls.

## Project Structure

```
packages/brick-frame-simulator/src/
  LDraw/
    Types.elm           # LDrawLine union type and supporting types
    Parser.elm          # parseLine, parseFile, splitMpd
    Resolve.elm         # PartCache, HTTP part fetching
    Colors.elm          # LDraw color code → RGBA
    Geometry.elm        # Transform tree → flat WebGL vertex buffers
  Render/
    Camera.elm          # Orbit camera (azimuth/elevation/distance)
    Mesh.elm            # Vertex and EdgeVertex types
    Shader.elm          # Lambert GLSL shaders
    EdgeShader.elm      # Edge line GLSL shaders
    GuideShader.elm     # Component visualisation shaders
    Lighting.elm        # Light uniforms
    Scene.elm           # buildScene, renderScene
  Gear/
    Types.elm           # GearSpec, GearInstance, GearGraph
    Detect.elm          # extractGears, buildGearGraph
    Physics.elm         # propagate (BFS gear ratio), angleAt
    Animate.elm         # MotorState, gearAngles
    Components.elm      # Axle/beam detection

packages/brick-frame-viewport/   # Viewport component (camera, input, loading, document scenes) + runtime/ JS + geometry-worker.js
packages/brick-frame-simulation/ # Simulation component (gear analysis, playback, moving-part meshes)
packages/brick-frame-ui/         # Theme tokens, shared view widgets, theme.css
packages/brick-frame-data/src/   # Data.elm (generated, never edit) + EmbeddedParts.elm

viewer-app/src/                  # Viewer + Web Components host
  Main.elm              # Model, Msg, update, view; wraps ViewportMsg / SimulationMsg
  Ports.elm             # JS interop (file reading, geometry worker, URL hash)
  UI/                   # Viewer-only panels

studio-app/src/                  # LDraw studio (editor) host — see docs/studio.md
  Main.elm              # Pages: Workspace and Preview (nested WorkspaceMsg / PreviewMsg)
  Studio/               # Workspace, Edit (commands + history), Gizmo, Palette, Inspector, Preview, ...

src/
  Lib.hs                # LDrawColor, GearSpec, Elm module generation
generator/Main.hs       # Writes packages/brick-frame-data/src/Data.elm
tests/Main.hs           # Haskell test suite (hspec)
```

## Documentation

In-depth documentation for agents and contributors:

- [docs/rendering.md](docs/rendering.md) — WebGL pipeline, shaders, coordinate system, camera, BFC, conditional lines, and rendering limitations.
- [docs/physics.md](docs/physics.md) — Gear detection algorithm, adjacency graph, BFS rotation propagation, gear types, and physics limitations.
- [docs/library-usage.md](docs/library-usage.md) — How to integrate `packages/brick-frame-simulator` into a custom Elm application, including setup, port contracts, module reference, and a step-by-step walkthrough.
- [docs/studio.md](docs/studio.md) — The LDraw studio: architecture, document model, editing commands and history, gizmo, palette and part index, layout, draft/save, and the simulation preview.

## Key Technical Facts

- **LDraw format:** Text lines, types 0–5. Type 1 = sub-file ref with 4×4 transform.
- **Studio v2 support:** Type 11 lines are parsed as type-1-compatible refs (extra Studio tokens ignored).
- **Coordinate system:** LDraw uses right-handed Y-down; negate Y during geometry flatten.
- **Part resolution:** Primary base is local synced assets (`/ldraw`, configurable via `VITE_LDRAW_BASE`) with optional remote fallback (`VITE_LDRAW_FALLBACK_BASE`).
- **Key gear parts:** `3647.dat` (8T), `4019.dat` (16T), `3648.dat` (24T), `3649.dat` (40T)
- **Color inheritance:** Color `16` and Studio `-1` inherit from parent; Color `24` is edge color (render black).
- **Gear physics:** Pure function of time — angle = ratio × motorSpeed × t (enables free scrubbing).
- **Msg architecture:** Each app's `Main.elm` owns its `Msg`; the stateful `Viewport` and `Simulation` components are the exception (own `Model`/`Msg`/`update`, no ports, effects returned as events). The studio nests page Msgs. Everything else is pure functions.

## Current Runtime Notes

- **Controls panel:** Gear/motor controls are collapsible via a `Minimize` / `Maximize` toggle in the top-right panel.
- **Desktop camera controls:** Drag = orbit, `Shift + drag` = pan, wheel = zoom.
- **Touch controls:** 1-finger orbit, 2-finger pan + pinch (zoom engages past a 12 px dead-zone). Exactly one of the Touch or Pointer listener families is bound, chosen by the `supportsPointerEvents` flag.
- **Fit view:** the ⛶ button in the viewer control pad re-frames the model; it is the only way back to auto-fit after a manual camera move.
- **Two navigation modes:** `Camera.navigation` is `Orbit` or `Walk`. Walk is first-person at street level (`G`, or the person button) — drags pivot about the eye, WASD walks, wheel dollies. Free-fly, no collision. Both modes keep `position = target + eyeOffset`, so culling/picking/LOD are mode-agnostic; keep it that way.
- **Camera sensitivity is viewport-relative**, not fixed constants — see docs/rendering.md § Camera. Zoom limits come from the model's bounding sphere (`Viewport.Framing.applyZoomRange`), so never reintroduce a hard-coded distance clamp. The same goes for the far plane: it reads `sceneRadius`, because orbit distance alone gives 400 LDU of view when the eye is inside a city.
- **Keyboard shortcuts** live in `packages/brick-frame-viewport/src/Viewport/Shortcuts.elm` (studio adds `Studio/Shortcuts.elm`) as pure functions returning an `Action` that the host's `msgForAction` maps. `?` shows a reference built from `Shortcuts.helpRows`. Never bind a key without going through `Shortcuts.actionFor` — it holds the guard that stops document-global keydowns hijacking host page form fields.
- **Key guard:** `isTextEntryTarget` covers form fields only; `isWidgetKey` leaves Space/Enter to focused buttons and arrows to ARIA composite widgets. Never add `BUTTON` back to the text-entry list — it silenced every shortcut after a toolbar click.
- **Studio layout** listens to `Browser.Events.onResize` directly; the runtime's `viewportResized` port observes a node Elm has replaced and never fires.
- **Known mobile issue (unverified on device):** the iOS animation-freeze report has three plausible causes fixed (replaceState flood, Touch/Pointer double-tracking, unreachable touch-state reset). Still listed in docs/rendering.md limitations until confirmed on hardware.

## Development Environment

Uses **devenv** (Nix). Always run Elm/Haskell commands inside the devenv shell:

```sh
make shell     # Enter the devenv shell interactively
make dev       # Start the viewer's Vite dev server (hot reload)
make studio-dev # Start the studio's Vite dev server
make generate  # Run Haskell generator (writes packages/brick-frame-data/src/Data.elm)
```

## Build Commands

| Command | Description |
|---------|-------------|
| `make dev` | Start Vite dev server with hot reload |
| `make studio-dev` | Start the studio dev server (serves `/ldraw` from the viewer's public dir) |
| `make build` | Production build (viewer + studio at `/studio/`) → `build/` |
| `make studio-build` | Build the studio only |
| `make ldraw-index` | Rebuild `parts-index.json` (part palette search index) |
| `make generate` | Run Haskell generator only |
| `make test` | hlint + cabal test + elm-test |
| `make test-lib` | Run tests in the four `packages/` only |
| `make check` | Validate formatting + hlint + elm-review (no changes) |
| `make format` | Auto-format Haskell (fourmolu) and Elm (elm-format) |
| `make elm-review` | Run elm-review LLM-agent rules only |
| `make elm-review-fix` | Run elm-review with auto-fix |
| `make clean` | Remove build artifacts |

## Rules for Agent Sessions

1. **Never edit `packages/brick-frame-data/src/Data.elm` or `packages/brick-frame-simulator/src/LDraw/ColorData.elm`** — both are auto-generated by the Haskell generator (`make generate`). Colours come from `viewer-app/public/ldraw/LDConfig.ldr`; add colours by syncing that file, not by hand-editing Elm.
2. **Msg constructors live in an app's `Main.elm`** (studio pages have their own nested Msg) — sub-modules expose only pure functions, except the stateful `Viewport` and `Simulation` components.
3. **Run `make check` before committing** — elm-format + hlint + elm-review must all pass.
4. **No new hex colour values** — use existing `@theme` CSS variables from `main.css`.
5. **When adding Elm packages**, also verify they are available in devenv (run `elm-json install` inside the shell).
6. **LDraw CDN** is CORS-friendly at `raw.githubusercontent.com` — no proxy needed.

## elm-review: LLM-Agent Rules

Four custom rules in `viewer-app/review/src/ReviewConfig.elm` and `studio-app/review/src/ReviewConfig.elm` (backed by `vendor/master-builder/review/src/LlmAgent/`) enforce LLM-friendly code:

| Rule | Failure mode it prevents |
|------|--------------------------|
| `NoTailwindRawStrings` | `Tw.raw "..."` strings that agents cannot statically verify |
| `RequireModuleDoc` | Modules without a `{-\| ... -}` comment that agents must read fully to understand |
| `RequireTypeAnnotation` | Top-level functions without type annotations that force agents to infer types manually |
| `NoExposingEverything` | `exposing (..)` wildcards that hide the public/private boundary |

Auto-generated files exempt from these rules: `.elm-tailwind/` (directory) and `src/Data.elm` (file).

## Commit Convention

Follow Conventional Commits with phase prefix:

```
feat(phase-1): scaffold WebGL canvas with orbit camera

Co-Authored-By: Claude <noreply@anthropic.com>
```

Types: `feat`, `fix`, `refactor`, `style`, `docs`, `test`, `chore`, `build`, `ci`

## Sandbox Network Policy

Configuration for `agent-sandbox` (proxied sessions). Only hosts needed to build, test and fetch LDraw assets are allowed.

```toml agent-sandbox
[ports]
dev = 8180

[network]
allowed_hosts = [
  "*.cachix.org:443",              # git over HTTPS, GitHub API, submodules, codeload
  "*.github.com:443",              # git over HTTPS, GitHub API, submodules, codeload
  "*.githubusercontent.com:443",   # raw.githubusercontent.com (LDraw CDN), release assets
  "package.elm-lang.org:443",      # Elm package registry
  "hackage.haskell.org:443",       # Cabal packages
  "registry.npmjs.org:443",        # npm packages (Vite, elm-tailwind-classes, ...)
  "cache.nixos.org:443",           # Nix binary cache (devenv/nix)
  "library.ldraw.org:443",         # `make sync-ldraw` source archive
]
```
