# LDraw studio

`studio-app/` is a browser editor for LDraw models, with the gear simulator as its
preview. It is a second host of the shared packages (see
[library-usage.md](library-usage.md)); the viewer in `viewer-app/` stays a
read-only viewer and links here with "Edit in studio" when `studioUrl` is set.

## Decisions

- **Apps and shared packages.** `viewer-app/`, `studio-app/` and `brick-play/`
  compile the shared packages in `packages/brick-frame-{simulator,viewport,simulation,studio,ui,data}`.
- **Reusable studio package.** `packages/brick-frame-studio` provides the core 3D editing
  engine (`Studio.Core`, `Studio.Edit`, `Studio.Gizmo`, `Studio.Overlay`, `Studio.Transform`,
  `Studio.Layout`, `Studio.Shortcuts`, `Studio.PartIndex`, `Studio.BuildGroups`, `Studio.Colours`, `Studio.Hints`).
  This allows the specialized application `brick-play` (minimal brick building, with
  a Technic mode) to reuse 3D editing, snapping, and gizmos without duplicating
  editor code.
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

### Glued parts

A glued part stays on the board. It is written as a META line directly before
its part line:

    0 !PALIKKA GLUED
    1 4 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat

A glued placement is one whose previous line is that exact meta line (surrounding
whitespace ignored). Files without it parse as before, and it round-trips through
`LDraw.Writer`. The meta line is only meaningful directly before a placement, so
every document edit keeps the two together: `Document.removePlacement` takes the
line along, `groupIntoSubmodel` keeps a glued part glued inside its group (and
glues the group reference), `explodeSubmodel` drops the glue of the placement it
inlines, and `Document.autoStepDocument` puts step markers before the line.
Duplicated or added placements are never glued.

`LDraw.Document` provides `isGlued`, `gluedIds`, `setGlued` (idempotent) and
`isGlueLine`. Glue is an edit: `Edit.SetGlued` with its inverse, one undo step.

In `Studio.Core`:

- `setGlued : Bool -> Model -> (Model, Cmd Msg, List Event)` glues or unglues the
  selection as one undo step. `isGlued`, `selectionGlued` query it. Glued parts can
  be selected.
- A glued part is never changed by a move. `movableSelection` (the selection
  without glued parts) is what turn, nudge, raise, lower, delete, body drag,
  gizmo handles and the gear alignment use. A press on a glued part does not start
  a drag (it selects, or does nothing in `DragPartsOnly`). A mixed selection moves
  only its unglued parts. Glued parts stay obstacles for snapping and free spots.
  A glued part's colour does not change either (`SetColorSelection` uses the same
  selection).
- `setGlueEditable False` (puzzle mode) makes `setGlued` a no-op and refuses
  undo or redo that would unglue a part. Default `True`.
- `selectionMovable` says whether the selection holds an unglued part.
- `clearUnglued` resets the document to its glued parts. `resetDocument` (Clear)
  is unchanged.

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
holds still. With the select or move tool (as in BrickLink Studio), a part can
also be dragged by its body (`Viewport.pickBodyPoint`, which adds the axle-hole fallback), the whole selection if the
part is in it, snapping to the move step and to studs like a handle drag, one
undo step per drag (see "Dragging a part by its body" below). A select-tool
press that never moves the part is a click and selects that part alone. The gizmo arrows
and rings stay the move and rotate tools' own. An armed palette part is placed
by the click rather than grabbing the part under it. Empty space pans the camera
with a left drag (right drag orbits it), and Shift + drag on a part moves it
finely. Translation snaps to a grid of 20/8/4/1 LDU; rotation is in 90° or
15° steps; the inspector accepts exact position and Euler angles. Selected parts
get outlines from `Studio.Overlay`, and at rest nothing else is outlined: gear
hints (meshed / cannot turn) appear only in the status summary, and red
collision boxes only while a drag is under way.

Picking narrows by bounding sphere and box, then tests the part's triangles
(`Instanced.PartMesh.surface`), so the visually nearest surface wins and empty
space picks nothing. Pointer events arrive in window coordinates; the studio
tells the viewport where its canvas sits (`Viewport.setCanvasOrigin`, from
`Studio.Layout`), and the viewport converts to canvas pixels before picking.

### Dragging a part by its body

The body drag is view-relative (`Studio.DragPlane`, shared with `Studio.Core`):
dragging left moves the part to the camera's left, whichever way the camera is
turned. On press a **working plane** is fixed through the grabbed point (render
space), chosen once from the camera:

- **Horizontal** when the camera looks down or up steeply (`abs elevation >= 20°`):
  screen left/right is camera-left/right on the ground, screen up/down is away
  from / toward the camera. The dragged parts rest on whatever their footprint
  (the union of their bounding boxes) overlaps at the new place, or the ground,
  keeping the height they had above their support at press time.
- **Camera-facing** (vertical) near the horizon, where a ground plane would run
  off to infinity: screen up/down is up/down, in move steps, never below the
  ground.

Every move casts the screen ray (`Render.Pick.screenRay`) onto that plane and
moves the lead part by the distance from the grab point, so the grabbed point
stays under the cursor with no jump on the first move. A ray parallel to the
plane, or one meeting it behind the eye, leaves the part where it is. Then come
the move-step grid (X and Z), shifted by the lead part's stud offset so a studded
part's origin lands on the stud lattice (studs at 10 mod 20 LDU: `DragPlane.snapTo`
with `Connectivity.studOffset`; Technic parts keep the plain grid), the wheel lift (plate, Shift brick, Alt 1 LDU),
stud snapping (`Connectivity.solveSnapWithHysteresis`, off with Alt), collision
marking, and the rest of the selection follows rigidly. In the workspace, holding
Shift moves on a 1 LDU grid instead of the move step (`dragGridStep`); the Y axis
lock holds X and Z and lifts or lowers the parts with the cursor's vertical
movement, without any support search; the X and Z locks hold the height and the
other ground axis. Inside a submodel the plane and supports
are in world coordinates and the result goes back through the submodel's
transform. A part dragged in from the palette has no grab point and slides over
the ground, resting on what is under it.

A press on a part's body grabs it even where the ray touches no triangle: `Viewport.pickBodyPoint`
falls back to the placement whose centre is nearest the ray within 35% of its bounding radius
(`axleHoleFraction`). This covers a gear's empty axle hole, so pressing on it drags the
gear instead of panning. Click selection (`pickSource`) keeps the strict triangle test.

### Follow the clicked part

An explicit click or tap that selects a part (also a part pressed and released
without moving it, with the select or move tool) slides the view onto the
selection — the middle of the selected placements' bounding spheres —
with `Viewport.pivotOn`: a short translation glide that keeps azimuth, elevation
and distance (no rotation, no zoom; see [rendering.md](rendering.md) § Camera).
The next orbit and zoom turn around the clicked part, and W/A/S/D keep moving
along the same screen directions. Nothing else moves the pivot: not adding a
part, keyboard or action-bar moves and turns, a drag drop, undo/redo, Ctrl+A or
the inspector. It never happens during a drag, in walk navigation, or on the
Preview and Instructions pages. Deselecting keeps the last pivot; Fit View
(`F`, the camera globe's reset button) recentres on the whole model.

### Host placement (Brick Play)

`Studio.Core.addPart` rests a new part on the ground (origin Y = −bottom);
`Core.addPartAt part colour matrix` adds it at exactly the given placement, for a
host with its own heights (Brick Play's Technic axle line). `Core.freeSpotFor part`
finds where to add a ground-resting part without overlapping anything
(`Studio.DragPlane.freeSpot`: stud-lattice positions ring by ring outward from the
selection, or from the camera target when nothing is selected, until the part's
box clashes with no placement's box); `Core.freeSpotForPlacement part matrix`
does the same for a matrix, keeping its height and orientation. `Core.addPart`
snaps X and Z to the stud lattice too; `Core.addPartAt` keeps the matrix exactly.
A turn (`rotateSelectionBy`) re-snaps the first studded part of the selection onto
the lattice for its new orientation, moving the whole selection rigidly, unless fine
mode is on; a nudge is relative, so it keeps the lattice it had. Brick Play adds
every dock piece at `freeSpotFor`, so the first lands in the middle of the view
and the next ones beside the selected piece. `Core.setDragSettles False` makes
horizontal-plane body drags keep the press-time height instead of settling on
the support (camera-facing drags, the wheel and raise/lower are unchanged).

In Technic mode Brick Play (`Technic.Placement`) adds every catalog Technic part —
gears, axles, beams, pins, bushes — with its origin on one **axle line** at
Y = −60 LDU (60 above the ground, so a 40T's teeth clear it): each of these parts
has its origin on its own axis or hole row, so gears of any size share one axis
height and mesh by sliding along X/Z only. A new gear goes into the selected
part's plane, on the first free spot along X; other Technic parts use
`freeSpotForPlacement`, and a project's non-Technic parts rest on the ground.
Technic mode turns drag settling off. `Core.partBox` gives a host the studio's
local box for a part.

While the camera is still auto-fitting (`Viewport.CameraAutoFit`: not moved
since the start or the last Fit View), each part added with `addPart`/`addPartAt`
re-frames the whole model once its geometry is built, so a model built from the
dock stays in view; after any camera move (or a click that glides onto a part)
adds leave the camera alone.

## Brick Play

Brick Play embeds `Studio.Core` with the gizmo off, so it edits like the studio
while showing its own chrome. Its `Main.elm` owns the Msgs and forwards the action
bar to the component. It keeps the chrome minimal: no title badge (a visually
hidden `h1` keeps the heading), one top-right menu, and a compact dock. Technic
mode (below) adds the gear chrome to the same view.

- **Modes.** The URL hash picks free building (no hash, or `free=1`) or a puzzle
  (any other hash). Free building's dock is the curated catalog (`Free.Catalog`:
  Bricks, Plates, Slopes & tiles, Technic), unlimited, with a colour row for the
  parts added next and a search over the whole part library. The search reads
  `parts-index.json` once, on first use, through `Studio.PartIndex` and the shared
  matcher `Studio.PartSearch` (the palette uses the same ranking). Glue is free
  building only: the Glue toggle in the action bar and `L` (`Shortcuts`) glue or
  unglue the selection, and a glued selection shows "(glued)" with its move buttons
  off. A puzzle's dock is its counted kit, its glued parts are locked, and Clear
  keeps them (`Core.clearUnglued`). The hash formats are in
  [examples.md](examples.md).
- **No gizmo.** `Core.setGizmoStyle NoGizmo` draws and hit-tests no arrows or
  rings. A body drag still moves a part on the view-relative plane
  (`Studio.DragPlane`), and turning, raising and lowering go through the keyboard
  or the action bar. The yellow selection box is still drawn.
- **Drag mode.** Brick Play uses `DragPartsOrPan OneFingerPans`
  and a camera globe (see
  [rendering.md](rendering.md) § Interaction controls).
- **Shift fine moves.** In `DragPartsOrPan` a Shift + left press on a part grabs
  it (Shift + press on empty canvas pans), and holding Shift during a body drag
  moves on a 1 LDU grid instead of the move step. Shift + click still toggles the
  part in the selection when released without moving. `Core.setDragMode` turns
  `Viewport.setShiftPressGrabs` on for this mode only; the studio workspace keeps
  Shift + drag as a pan.
- **Action bar.** While something is selected, a floating bar above the dock shows
  Turn left, Turn right, Tip, Raise, Lower and Delete. Each calls the same command
  as its key (`Core.turnSelected`, `Core.raiseSelected`, `Core.lowerSelected`,
  `Core.deleteSelection`).
- **Menu.** A visible `?` button (Keyboard shortcuts, the same as the `?` key) sits
  beside one hamburger button at the top right (`UI.HamburgerMenu`), which opens
  Undo, Redo, Delete and Clear, each with its shortcut shown. Items send the same
  Msgs as the old toolbar buttons, wrapped in `MenuPicked` so the menu closes first.
  A click outside (a backdrop) or Escape closes it; Escape closes the menu before it
  deselects anything. While the menu is open the studio's editing keys are paused,
  like the help dialog.
- **Accomplishment banner (Technic mode).** One green line, "All N gears are
  rotating together!" (`role="status"`), in the top row between the title-less
  left edge and the buttons, the buttons' height. It shrinks and ellipsizes on narrow
  windows. The green is LDraw's Green from the colour table; the theme has no green
  token.
- **Part cards.** Dock items are `UI.PartCard.compactView`s: the picture only, about
  56 px. Name, part number and colour are in the tooltip (`title`) and the accessible
  name (e.g. "Add 24T Gear (3648)"). Brick Play puts the count left on the image
  corner ("3/4", muted at 0). Images come from the shared thumbnail pipeline
  (`runtime/thumbnail-pipeline.js`, also used by the studio palette) over the
  `requestThumbnails` / `thumbnailLoaded` ports, and are cached in IndexedDB. A
  neutral placeholder keeps the card size until an image arrives; a part with none
  left is disabled, not hidden.
- **Dock height.** The dock is `PartCard.compactPanelHeight` (130 px) tall, the same
  as the camera globe panel, and its bottom edge sits on the globe's
  (0.75 rem from the bottom). The free dock has the colour row on its left, then a row
  of tabs (`role="tablist"`) with the search box, above the card row, which scrolls
  sideways when it overflows. A puzzle's counted kit is one row of cards.
- **Mesh status (Technic mode).** With one gear selected, a status pill says how
  it sits against its nearest other gear (`Machine.gearFeedback`, worded by
  `Machine.feedbackText`), e.g. `24T ↔ 24T: 40 LDU apart — needs 60` or
  `Meshes with 24T ✓`. It turns yellow once the two mesh.
- **Playing (Technic mode).** Space plays and pauses the machine in the same
  view as editing. Gears and their driven parts animate, and their static copies are
  hidden (`Technic.Animation`). Any edit stops the machine and rewinds it to 0.
  Without gears Space is unbound, and the play button is hidden.
- **Placement.** Technic parts go on the axle line (`Technic.Placement`); other
  parts use `Core.freeSpotFor`. See "Host placement" above.

## Gear alignment

With `Core.setGearAlignment True`, gears that a committed edit meshes are turned
so their teeth interlock (`Studio.GearAlign`; the tooth geometry is in
`Gear.Phase`, see `docs/physics.md` § Tooth phase).

- **When it runs.** After each committed edit that moves or adds parts: a drop
  (including magnetic snap), a nudge, a turn from the keyboard or action bar, and
  an added part. Only meshes that did not exist before the edit and involve a
  moved gear are closed. A drag that moves no gear or axle, and any edit while
  alignment is off, does no alignment work at all. Worms are never turned, and a
  turn under half a degree is dropped.
- **Groups.** A gear turns with its rigid group: the gears and axles on its axle
  line that are connected to it along the line (their extents along the axis
  overlap, within 2 LDU, chained outward from the moved gear). Parts on the same
  line with a gap between them stay put. The group turns about the shared line,
  because an axle re-seats in 90° steps only, which never changes tooth phase.
  If a group has several meshes, the turn minimises the worst misalignment over
  its moving gears; fixed partners cannot move, so their offsets are only
  reported.
- **Undo.** The turns are recorded in the same `Group` as the edit, so one undo
  restores both the position and the rotation, and redo applies both again. Drags
  record the turns in the drop's history entry as well.

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
camera globe at the bottom right (`UI.CameraGlobe`: drag the globe to fly, tap to
jump, reset to fit, log-scale zoom slider, with walk and help beside it). The
snap bar holds four step chips (move step, turn step, snap distance, collision
mode) and a **Fine** toggle (`aria-pressed`): while on, drags and nudges move
on a 1 LDU grid, the touch alternative to Shift + drag. On a phone the chips wrap
onto two rows rather than scrolling, so Fine is always reachable. On a phone
held upright (under 600 px wide) the bottom area shows either the palette or the
globe, switched by one toggle at the bottom edge; the palette is the default.
Everything floating over the canvas is opaque.

## Keyboard and mouse

Keys are pure functions in `Studio.Shortcuts` on top of `Viewport.Shortcuts`;
`?` lists them all. The editing keys:

- **Tools**: `T` (or `M`) move, `V` select, `R` rotate. X Y Z turn the selection
  about that axis (Shift the other way); `[` `]` set the move step, `,` `.` the turn step.
- **W A S D**, **Q E** and **Page Up / Page Down** move the selection relative to
  the screen (W and S away from and toward the viewer, A and D left and right, Q
  and Page Down lower, E and Page Up raise), by the move step; Shift makes it 1 LDU.
- **Arrows** turn the selection: ← → about the vertical axis, ↑ ↓ tip it about the
  ground axis most aligned with the screen's horizontal. A turn is the turn step,
  Shift makes it 45°.
- Each key press is one undo step. The workspace consumes these keys, so the camera
  does not move at the same time. With nothing selected they move or orbit the
  camera as in the viewport. During a drag the movement keys and Delete are
  ignored (nothing is removed from a half-dragged document) and the arrows change
  the drag's step.
- While the `?` dialog is open no key edits the document (`Studio.Shortcuts`
  returns nothing for the context's `helpVisible`); `?` or Esc close it.
- `Delete`, Ctrl D/A/G, Ctrl Z/Y and the other chords are checked against the text-entry
  guard too, so typing in a form field never edits the document.
- `/` focuses part search only; `?` lists the keys.
- Ctrl O open, Ctrl S save, B parts list, I instructions, P simulate.
- **Esc** cancels a drag in progress: the document goes back to how it was when the
  drag began, with no undo step. Otherwise it closes the menu, clears the selection,
  leaves a submodel, or leaves Simulate/Instructions.

The mouse:

- Left click selects; Shift or Ctrl/Cmd click adds or removes. With an armed
  palette part, a click places it on the surface under the point.
- Left drag on a part moves or turns it (through the gizmo, or the body drag of
  the select and move tools, which follows the cursor relative to the view);
  Shift + left drag on a part moves it finely, on a 1 LDU grid, and a Shift +
  press on a part that is released without moving toggles it in the selection.
- Left drag on empty space pans the camera (`Viewport.panCurrentGesture`); one
  finger does the same on touch, and the camera globe does the orbiting. A click
  or tap on empty space still clears the selection.
- Right drag orbits, around the selection once something has been selected (F
  recentres on the model). Middle drag, Shift + drag on empty space and Space +
  drag pan. Wheel zooms, and while dragging a part it raises or lowers it (a plate;
  Shift three plates; Alt 1 LDU). Right click during a drag cancels it.
  Space is pan-while-held in the workspace, not play/pause.
- Gear tooth alignment: a committed move, turn or add that meshes gears turns the
  gears on that axle line so the teeth interlock (`Studio.GearAlign`). The turns
  are recorded in the same history entry, so one undo restores both.

The guard (`Viewport.Shortcuts.isTextEntryTarget` and `isWidgetKey`) leaves
typing in form fields alone, and leaves Space/Enter to a focused button and the
arrows to composite widgets (by ARIA role: `treeitem`, `option`, `radio`, …).
Buttons are deliberately *not* text-entry targets: clicking a toolbar button
focuses it, and that used to silence every shortcut. Nor do mouse clicks leave
buttons focused: the header, tool rail, snap bar, breadcrumbs, camera globe, tab bar,
export menu and inspector prevent the default of `mousedown`
(`Workspace.keepFocusOnCanvas`), so Space + drag after clicking Undo pans instead
of undoing again. Presses on inputs, selects and text areas are let through so
they still take focus; Tab focus is unchanged. The palette, colour swatches and
structure tree are left out, since their roving focus follows the click.

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
   - **Brick Play with model**: Exports the complete assembled model alongside its part kit. The model loads directly onto the canvas as a puzzle, where Technic mode (mechanism simulation, motor control, and accomplishment evaluation) switches on by itself, and the kit is always counted in the dock.

Each companion app entry opens the target app in a new browser tab and includes a **Copy Link** button that copies the shareable URL to the clipboard, displaying an ephemeral toast confirmation.

### URL Payload Format

Cross-app deep-linking operates through URL hash fragments (`#...`), keeping sharing fully static and client-side:

- **Brick Play URL**: `<playBaseUrl>#parts=<encoded-parts>`
- **Brick Play with model URL**: `<playBaseUrl>#model=<encoded-model>&parts=<encoded-parts>`

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
