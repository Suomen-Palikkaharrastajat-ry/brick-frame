# Task 2: Selection Highlight and Hints Cleanup

Address selection highlight and hints issues identified in `TODO.md` under `## Selection highlight (Studio.Overlay)`.

## Scope and Requirements

1. **Collision outline only marks the dragged parts**:
   - In `studio-app/src/Studio/Workspace.elm`:
     In `applyDragDelta` (and wherever `collidingIds` is computed for the drag outline), outline only the dragged ids rather than both sides of the colliding pair (`List.map Tuple.first collidingPairs`).
   - Non-selected stationary parts should not be outlined in red during a drag.

2. **Remove the unused `Hints.meshed`**:
   - In `studio-app/src/Studio/Hints.elm`:
     Drop the `meshed` field and any helper code that computes it.
     `isolated` is still used (for its count).
   - Update the module documentation and type annotations (clarifying "which gears mesh" is no longer tracked separately).
   - Update `studio-app/tests/HintsTest.elm` to test `isolated` count without asserting on `meshed`.

## Quality Standards
- `devenv shell -- make check` must pass (elm-format, hlint, elm-review).
- `devenv shell -- make elm-test` must pass.
