# Task 1: Picking Improvements and Tests

Address all picking issues identified in `TODO.md` under `## Picking (Viewport.pickSource / pickPoint)`.

## Scope and Requirements

1. **Ignore hits in front of the near plane**:
   - In `packages/brick-frame-viewport/src/Viewport.elm` (`pickPoint`, the `closer` fold):
     Accept a triangle hit only when `t >= Camera.nearPlane camera`.
   - In `Pick.rayAabbHit` (or when calling it from `boxCandidate`):
     Account for near plane so box hits behind the eye or clipped before near plane are properly handled/clipped.
   - Add a unit test with the eye inside a part's bounding box where geometry is in front of the near plane or behind the camera.

2. **Picking matches LOD proxy boxes**:
   - In `pickPoint` against `Instanced` proxy chunks (`Far` level of detail):
     Placements at `Far` level of detail are drawn as proxy boxes. If `mesh.surface` has no hit or placement is at `Far` LOD, accept the box hit (`c.boxT`) as fallback.
   - Ensure clicking the visible box of a far placement picks it.

3. **Embedded viewer / web-components canvas offset**:
   - In `packages/brick-frame-viewport/src/Viewport.elm`:
     In canvas `mousedown`/`pointerdown` decoder (and touchstart if target offset is available), read `clientX - offsetX` and `clientY - offsetY` when target is canvas, updating `canvasOrigin` dynamically.
   - Ensure `setCanvasOrigin` remains available as an explicit override/optimisation.
   - Document this behavior in `docs/library-usage.md`.

4. **Decide on parts that can no longer be picked**:
   - For placements with edge lines but no triangles (or unresolved parts / singular matrix where invert is impossible), fall back to the box entry `t` (`boxCandidate` / `surfaceHit`) so they remain pickable.

5. **Triangle test cost on very large parts**:
   - Review `nearestTriangleHitBy` in `Render.Pick.elm` / `pickPoint`. Ensure early exit / efficient bounding checks avoid unnecessary allocations.

6. **Missing tests**:
   - Add unit tests in `packages/brick-frame-viewport/tests/ViewportTest.elm` and/or `packages/brick-frame-simulator/tests/Render/PickTest.elm` covering:
     - A ray inside a part's box that misses its triangles returns `Nothing`.
     - A part seen through the empty corner of a nearer part's box is picked.
     - Two coplanar surfaces: the smaller part wins (`pickDepthTolerance`).
     - A mirrored or scaled placement matrix picks at the right depth.
     - Multi-touch `TouchStart` / `TouchMove` / `TouchEnd` and `PointerTouchStart` / `PointerTouchMove` handling with canvas offset.

## Quality Standards
- `devenv shell -- make check` must pass (elm-format, hlint, elm-review).
- `devenv shell -- make test-lib` and `devenv shell -- make elm-test` must pass.
