# LDraw studio

`studio-app/` is a browser editor for LDraw models, with the gear simulator as its
preview. It is a second host of the shared packages (see
[library-usage.md](library-usage.md)); the viewer in `viewer-app/` stays a
read-only viewer and links here with "Edit in studio" when `studioUrl` is set.

## Decisions

- **Two apps, shared packages.** `viewer-app/` (renamed from `elm-app/`) and
  `studio-app/` both compile `packages/brick-frame-{simulator,viewport,simulation,ui,data}`.
- **Generated data is shared.** `Data.elm` is generated into
  `packages/brick-frame-data/src` (gitignored, never edited); `EmbeddedParts.elm`
  is hand-written in the same directory.
- **One LDraw library.** The synced library stays at `viewer-app/public/ldraw`.
  In production the studio fetches `/ldraw` from the site root (the studio is
  served under `/studio/`); in dev a Vite middleware serves it from the viewer's
  public directory.
- **Msg rule.** `Viewport` and `Simulation` are stateful components with their
  own `Msg`; the studio nests page messages (`WorkspaceMsg`, `PreviewMsg`).
- **Grid snapping only** in the first release; there is no connection-aware
  snapping.

## Architecture

`Main` holds a `Workspace` page and an optional `Preview` page. Neither page has
ports: they return events and `Main` maps them to `Ports`. `Main` also intercepts
`Workspace.SimulateRequested` (opens the preview) and `Preview.Leave`.

| Module | Role |
|--------|------|
| `Studio.Workspace` | Document, selection, history, tools, panels; owns a `Viewport` |
| `Studio.Edit` | Pure commands with inverses, and the undo/redo history |
| `Studio.Gizmo` | Move gizmo: screen-space hit testing, drag, grid snap |
| `Studio.Overlay` / `Studio.Shader` | Selection outlines and gizmo drawing as extra WebGL entities |
| `Studio.Transform` | Matrix ⇄ position/Euler angles for the inspector |
| `Studio.PartIndex` / `Studio.Palette` | Part search and the add-part palette |
| `Studio.Colours` / `Studio.Inspector` / `Studio.Tree` | Colour picker, selection fields, submodel/placement tree |
| `Studio.Shortcuts` | Editing keys, layered on `Viewport.Shortcuts` and its typing guard |
| `Studio.Layout` | Wide/narrow layout and canvas-size arithmetic |
| `Studio.Hints` | Live gear hints for the document |
| `Studio.Preview` | Simulation preview page |

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
holds still. Translation snaps to a grid of 20/8/4/1 LDU; rotation is in 90° or
15° steps; the inspector accepts exact position and Euler angles. Selected parts
get outlines from `Studio.Overlay`.

## Palette and part index

`scripts/build-part-index.mjs` (run by `scripts/sync-ldraw.sh`, or
`make ldraw-index`) writes `parts-index.json` next to the synced library. The
studio loads it once and searches in memory. A typed part number works even when
the index is missing.

## Layout

At ≥ 960 px the studio shows 300 px side panes. Narrower, it shows a 46 px tab
bar with sheets. The canvas size is computed in `Studio.Layout`, not measured.

## Draft and save

The document is saved to `localStorage` as a draft (an empty string is a draft)
and restored on load. `Ctrl+S` is intercepted in `main.js` and downloads the file
(`Ports.downloadFile`).

## Simulation preview

Preview snapshots the document with `Writer.writeDocument` into a fresh
`Viewport` + `Simulation`, seeded with the workspace's camera and already-loaded
parts, so nothing is refetched. Leaving restores the camera and the selection.
`Studio.Hints` runs the same analysis, debounced by 500 ms with a generation
token, to show gear hints while editing.

## Testing

`make test-lib` runs the package suites; `studio-app` has its own `elm-test`
(`make test` covers both), plus `make ldraw-index-test` for the index builder.
