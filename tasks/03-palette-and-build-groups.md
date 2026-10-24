# Task 3: Part Palette, Build Groups, Accessibility, Performance and CI Guards

Address all palette and build group issues identified in `TODO.md` under `## Part palette (Studio.Palette, Studio.BuildGroups)`.

## Scope and Requirements

1. **Size matching in search**:
   - In `studio-app/src/Studio/Palette.elm`:
     Refine `termIn` / ranking so that when a two-part size (e.g. "1 x 2" or "2 x 4") is queried, exact-footprint matches rank ahead of 3D extended sizes (e.g. "Brick 1 x 2 x 3" or "1 x 2 x 0.667"), or reject matches followed by " x <digit>" when searching for 2D footprints.

2. **Group chips narrow layout**:
   - In `studio-app/src/Studio/Palette.elm`:
     In narrow layout (phone/compact viewport), render group chips in a horizontally scrolling single row (`flex-wrap: nowrap; overflow-x: auto` with scrollbar hiding/styling) instead of wrapping into 4 rows that push part results below the fold.

3. **Chip semantics for assistive technology**:
   - In `studio-app/src/Studio/Palette.elm`:
     Update group chips container to `role="radiogroup"` and each chip to `role="radio"` with `aria-checked` ("true"/"false").
     Support arrow-key navigation between chips using the `isWidgetKey` guard pattern so arrow keys orbit the camera elsewhere.

4. **Clicking a group during search retains query**:
   - In `studio-app/src/Studio/Workspace.elm` (`PaletteGroup` message handler):
     Clicking an explicit category/group chip during a search should narrow the search to that group rather than throwing the search query away.
     Update "Searching every part" note logic and `docs/studio.md`.

5. **Memoize / avoid duplicate `filterParts`**:
   - In `studio-app/src/Studio/Workspace.elm`:
     Do not run `filterParts` twice per update (in view and in `requestVisibleThumbnails`).
     Compute/cache the filtered parts list once in `update` (or memoize based on query, group, category, colour) and reuse it.

6. **Shelve uncategorized parts in `BuildGroups.all`**:
   - In `studio-app/src/Studio/BuildGroups.elm`:
     Add curated groups for previously unshelved parts:
     - "Vehicles" (Train, Vehicle, Car, Boat, Plane, Tipper, Trailer)
     - "Accessories" or "Signs & Flags" (Roadsign, Flag, etc.) with classic curated parts.

7. **CI guard for curated part numbers**:
   - Add a node test in `scripts/` (e.g. `scripts/check-curated-parts.test.mjs`, registered in `package.json` or Makefile `make test`) that extracts curated essential part numbers from `BuildGroups.elm` and verifies they exist in `viewer-app/public/ldraw/parts-index.json`.

8. **Palette unit tests**:
   - In `studio-app/tests/WorkspaceTest.elm` and/or `studio-app/tests/PaletteTest.elm`:
     - Test that selecting a group during a search retains the query and narrows results.
     - Test "Searching every part" note visibility rules.
     - Test exact-id ranking when an id has variants ("3001" before "3001b.dat").

## Quality Standards
- `devenv shell -- make check` must pass (elm-format, hlint, elm-review).
- `devenv shell -- make elm-test` must pass.
- Curated parts check script passes.
