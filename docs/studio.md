# LDraw studio

`studio-app/` is a browser editor for LDraw models, with the gear simulator as its
preview. It is a second host of the shared packages (see
[library-usage.md](library-usage.md)); the viewer in `viewer-app/` stays a
read-only viewer and links here with "Edit in studio" when `studioUrl` is set.

## Decisions

- **Apps and shared packages.** `viewer-app/`, `studio-app/`, `brick-play/`, and
  `brick-contraptions/` compile the shared packages in `packages/brick-frame-{simulator,viewport,simulation,studio,ui,data}`.
- **Reusable studio package.** `packages/brick-frame-studio` provides the core 3D editing
  engine (`Studio.Core`, `Studio.Edit`, `Studio.Gizmo`, `Studio.Overlay`, `Studio.Transform`,
  `Studio.Layout`, `Studio.Shortcuts`, `Studio.PartIndex`, `Studio.BuildGroups`, `Studio.Colours`, `Studio.Hints`).
  This allows specialized applications like `brick-play` (minimal brick building) and
  `brick-contraptions` (Technic playground) to reuse 3D editing, snapping, and gizmos
  without duplicating editor code.
- **Generated data is shared.** `Data.elm` is generated into
  `packages/brick-frame-data/src` (gitignored, never edited); `EmbeddedParts.elm`
  is hand-written in the same directory.
- **One LDraw library.** The synced library stays at `viewer-app/public/ldraw`.
  In production the studio fetches `/ldraw` from the site root (the studio is
  served under `/studio/`); in dev a Vite middleware serves it from the viewer's
  public directory.
- **Msg rule.** `Viewport`, `Simulation`, and `Studio.Core` are stateful components with their
  own `Msg`; the studio nests page messages (`WorkspaceMsg`, `PreviewMsg`).
- **Snapping.** Supports stud, hole, and axle snapping with collision avoidance,
  in addition to grid snapping.

## Architecture

`Main` holds a `Workspace` page and an optional `Preview` page. Neither page has
ports: they return events and `Main` maps them to `Ports`. `Main` also intercepts
`Workspace.SimulateRequested` (opens the preview) and `Preview.Leave`.

The studio modules are split between the reusable package (`packages/brick-frame-studio`)
and studio-specific application UI (`studio-app/`):

| Module | Location | Role |
|--------|----------|------|
| `Studio.Core` | `packages/brick-frame-studio` | Reusable 3D studio TEA component: viewport, selection, gizmo, snapping, history |
| `Studio.Edit` | `packages/brick-frame-studio` | Pure commands with inverses, and undo/redo history |
| `Studio.Gizmo` | `packages/brick-frame-studio` | Move and rotate gizmo: hit testing, drag, grid/stud snapping |
| `Studio.Overlay` / `Studio.Shader` | `packages/brick-frame-studio` | Selection outlines, snap guides, and gizmo drawing entities |
| `Studio.Transform` | `packages/brick-frame-studio` | Matrix ⇄ position/Euler angles for the inspector |
| `Studio.PartIndex` / `Studio.BuildGroups` | `packages/brick-frame-studio` | Part search, curated classic build groups and metadata |
| `Studio.Shortcuts` | `packages/brick-frame-studio` | Pure keyboard shortcuts layered on `Viewport.Shortcuts` |
| `Studio.Layout` | `packages/brick-frame-studio` | Canvas-size and pane dimension arithmetic |
| `Studio.Hints` | `packages/brick-frame-studio` | Live gear analysis hints for the document |
| `Studio.Colours` | `packages/brick-frame-studio` | Curated color swatches and categorization |
| `Studio.Workspace` | `studio-app` | Studio page wiring together document workspace, tools, and side panels |
| `Studio.Palette` | `studio-app` | Categorized part drawer and search UI |
| `Studio.Inspector` | `studio-app` | Part placement, transform fields, and submodel inspection |
| `Studio.Tree` | `studio-app` | ARIA submodel / placement hierarchy tree |
| `Studio.BOM` | `studio-app` | Bill of materials / parts list modal |
| `Studio.Instructions` | `studio-app` | Step-by-step building instructions page |
| `Studio.Preview` | `studio-app` | Simulation preview page |

## Document, commands and history

`LDraw.Document` parses a file into submodels of placements with stable ids and
keeps META and comment lines verbatim, so `parse → write → parse` is the
identity (`LDraw.Writer`). Every edit is an `Edit.Command` — `Translate`,
`Rotate`, `Recolor`, `SetMatrix`, `Add`, `Remove`, `Restore`, `Group` — that
names placements by id. `Edit.apply` returns the edited document and the inverse
command; a history is two stacks of inverses, so undo and redo are both `apply`.
The document records the ids an edit touched (`Document.touched`) and the next
free id.

## Keeping the scene in step

The viewport shows the document through the instanced renderer
(`Viewport.loadDocument` / `applyDocument`). `Viewport.Sync.plan` reads only
`Document.touched` and rebuilds the affected proxy cells with
`Instanced.replaceSource` / `removeSource`; see "Editing: updating the scene in
place" in [rendering.md](rendering.md). The workspace clears `touched` after each
apply.

## Gizmo and selection

Clicking picks a placement (`Viewport.pickSource`); shift-click extends the
selection. The gizmo captures the drag (`Viewport.captureDrag`), so the camera
holds still. With the move tool, a part can also be dragged by its body
(`Viewport.pickPoint`): it slides over the horizontal plane through the grabbed
point, the whole selection if the part is in it, snapping to the move step and
to studs like a handle drag, one undo step per drag. Empty space and Shift-drag
still move the camera. Translation snaps to a grid of 20/8/4/1 LDU; rotation is in 90° or
15° steps; the inspector accepts exact position and Euler angles. Selected parts
get outlines from `Studio.Overlay`, and at rest nothing else is outlined: gear
hints (meshed / cannot turn) appear only in the status summary, and red
collision boxes only while a drag is under way.

Picking narrows by bounding sphere and box, then tests the part's triangles
(`Instanced.PartMesh.surface`), so the visually nearest surface wins and empty
space picks nothing. Pointer events arrive in window coordinates; the studio
tells the viewport where its canvas sits (`Viewport.setCanvasOrigin`, from
`Studio.Layout`), and the viewport converts to canvas pixels before picking.

## Palette and part index

`scripts/build-part-index.mjs` (run by `scripts/sync-ldraw.sh`, or
`make ldraw-index`) writes `parts-index.json` next to the synced library. The
studio loads it once and searches in memory. A typed part number works even when
the index is missing.

Browsing is by **build group** (`Studio.BuildGroups`), LEGO Creator style:
Bricks, Plates, Tiles, Slopes, Round, Arches, Windows & doors, Walls & fences,
Brackets & hinges, Wedges & wings, Wheels, Vehicles, Accessories, Nature,
Baseplates, then Technic. The palette opens on Bricks. Each group maps a few
LDraw categories and lists a curated set of classic part numbers first (1 x 1,
1 x 2 … 2 x 4), then the rest by size. Prints, stickers, Duplo and other
systems are never shelved; a search still finds them, last. "Everything" drops
the group and shows LDraw's own category picker. A search starts across every
part: exact part number, part-number prefix, description prefix, description
match, then keyword-only matches; within each, curated classics first, then
everyday building parts before Technic, ties in group order. Two-part footprint
queries ("1 x 2", "2 x 4") rank exact 2D footprint matches ahead of 3D extended
sizes ("1 x 2 x 3"). Sizes are matched whole, never inside a larger number
("2 x 4" does not match "12 x 4"), and "2x4", "2 x4" and "2x 4" are read as
"2 x 4". Picking a group during a search retains the search query and narrows
results to that group; leaving personal shelves (Favourites or Recent) resets to
All. Vehicles (Train, Car, Boat, Plane, Tipper, Trailer) and Accessories
(Container, Flag, Roadsign) are shelved in their respective build groups with
curated classics. Group chips use `role="radiogroup"` and `role="radio"` with
`aria-checked` and arrow-key navigation guarded by `isWidgetKey`; in narrow
viewport mode they render in a single horizontally scrolling row. Filtered
search and group results are cached in state during updates, avoiding duplicate
work during rendering and thumbnail fetching. When curating, check part numbers
against `parts-index.json`: many classics carry a suffix there (`3023b`,
`3040b`, `3665b`), which is checked by `scripts/check-curated-parts.test.mjs`.

## Layout

A header row (56 px wide, 48 px narrow) holds the brand, a document summary and
the actions: New, Open, Save, Export (a popover), Realistic, Parts list,
Instructions and Simulate. At ≥ 960 px the studio shows 300 px side panes (parts
and colour on the left, selection and structure on the right). Narrower, the
header keeps only Simulate and a "More" menu, and a 46 px tab bar opens the
panels as sheets. The canvas size is computed in `Studio.Layout` from the window
size, not measured; `Workspace`, `Preview` and `Instructions` listen to
`Browser.Events.onResize` themselves, because Elm replaces the runtime's mount
node and its `ResizeObserver` never fires.

On the canvas: the tool rail (tools, undo/redo, edit and submodel actions) at
the top left, the snapping buttons along the bottom (top on a phone) and the
camera pad at the bottom right. Everything floating over the canvas is opaque.

## Keyboard

Keys are pure functions in `Studio.Shortcuts` on top of `Viewport.Shortcuts`;
`?` lists them all. Besides the editing keys (V M R, X Y Z, Delete, Ctrl D/A/G,
Ctrl Z/Y, `[` `]` `,` `.`):

- **Arrows** nudge the selection by the move step relative to the screen (the
  camera's right and forward snapped to a ground axis), Shift by 1 LDU, and
  **PageUp/PageDown** raise and lower it. Without a selection the arrows orbit.
  `Workspace` intercepts these key presses before the viewport sees them, so the
  camera does not orbit at the same time.
- `/` part search, Ctrl O open, Ctrl S save, B parts list, I instructions,
  P simulate, Esc closes the menu, clears the selection, leaves a submodel, or
  leaves Simulate/Instructions.

The guard (`Viewport.Shortcuts.isTextEntryTarget` and `isWidgetKey`) leaves
typing in form fields alone, and leaves Space/Enter to a focused button and the
arrows to composite widgets (by ARIA role: `treeitem`, `option`, `radio`, …).
Buttons are deliberately *not* text-entry targets: clicking a toolbar button
focuses it, and that used to silence every shortcut.

Page changes (Simulate, Instructions, back) are decided in `Main`, which only
sees messages that come from outside a page, so key-driven page changes are
re-sent as commands (`Request` in `Workspace`).

## Accessibility

- Landmarks: `header`, two labelled `aside` panes, `main` for the canvas; pane
  titles are `h2`s.
- The canvas wrapper is focusable (`role="application"`, described by a hidden
  help text) and is where the keyboard lands; clicking the canvas focuses it.
- The structure list is an ARIA tree and the colour swatches a listbox; both
  move focus with the arrow keys (`FocusElement`) and select with Enter/Space.
  Picking a colour recolours the selection, so the arrows never pick.
- A polite live region (`Workspace.announce`) says what an action did: the
  selection, undo/redo, deletions, nudges, tool and step changes.
- Toggles carry `aria-pressed`; icon buttons carry a name, a tooltip with the
  key and `aria-keyshortcuts` (`UI.Button.icon`). Focus is always visible
  (`theme.css`).

## Draft and save

The document is saved to `localStorage` as a draft (an empty string is a draft)
and restored on load. `Ctrl+S` is intercepted in `main.js` and downloads the file
(`Ports.downloadFile`).

## Export and Send to App

The **Export** menu in the header provides both file downloads and instant cross-app deep links:

1. **File exports**: Download the active document as an LDraw model (`.ldr`), multi-part archive (`.mpd`), WebGL/glTF 3D scene (`.gltf`), AR model (`.usdz` for iOS Quick Look), or take a 1080p picture (`.png`) of the current view.
2. **Send to App**: Transfer the project directly into companion Palikkakehys applications without server roundtrips:
   - **Brick Play**: Exports the document as a limited building kit inventory. Placements are grouped by part file and colour into a piece kit (`Studio.UrlPayload.extractKit`). In Brick Play, users build freely within the constraints of this kit, spawning bricks from inventory cards in the bottom dock until the available counts are exhausted.
   - **Brick Contraptions**: Exports the complete assembled model alongside its part kit. In Brick Contraptions, the model loads directly onto the canvas for mechanism simulation, motor control, and accomplishment evaluation, while its parts appear in a dedicated "Project" tab in the Technic dock.

Each companion app entry opens the target app in a new browser tab and includes a **Copy Link** button that copies the shareable URL to the clipboard, displaying an ephemeral toast confirmation.

### URL Payload Format

Cross-app deep-linking operates through URL hash fragments (`#...`), keeping sharing fully static and client-side:

- **Brick Play URL**: `<playBaseUrl>#parts=<encoded-parts>`
- **Brick Contraptions URL**: `<contraptionsBaseUrl>#model=<encoded-model>&parts=<encoded-parts>`

Parameters:
- `parts`: A comma-separated list of `<part-file>:<ldraw-color>:<count>` items (for example `3001.dat:14:5,3002.dat:4:2`), URL percent-encoded.
- `model`: The full LDraw document text generated by `LDraw.Writer.writeDocument`, URL percent-encoded.

Parsing and generation logic is implemented in `Studio.UrlPayload` (`packages/brick-frame-studio`).

## Simulation preview

Preview snapshots the document with `Writer.writeDocument` into a fresh
`Viewport` + `Simulation`, seeded with the workspace's camera and already-loaded
parts, so nothing is refetched. Leaving restores the camera and the selection.
`Studio.Hints` runs the same analysis, debounced by 500 ms with a generation
token, to show gear hints while editing.

## Testing

`make test-lib` runs the package suites; `studio-app` has its own `elm-test`
(`make test` covers both), plus `make ldraw-index-test` for the index builder.
