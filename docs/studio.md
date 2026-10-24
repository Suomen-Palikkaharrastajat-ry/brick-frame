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
| `Studio.Layout` | Wide/narrow layout, header height and canvas-size arithmetic |
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

## Simulation preview

Preview snapshots the document with `Writer.writeDocument` into a fresh
`Viewport` + `Simulation`, seeded with the workspace's camera and already-loaded
parts, so nothing is refetched. Leaving restores the camera and the selection.
`Studio.Hints` runs the same analysis, debounced by 500 ms with a generation
token, to show gear hints while editing.

## Testing

`make test-lib` runs the package suites; `studio-app` has its own `elm-test`
(`make test` covers both), plus `make ldraw-index-test` for the index builder.
