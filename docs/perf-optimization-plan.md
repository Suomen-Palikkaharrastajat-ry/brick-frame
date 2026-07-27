# Renderer & simulation performance plan

Four optimizations to the Elm viewer, in Red-Green TDD order. Each step writes a
failing test first, then the implementation.

Test commands:

```sh
make test-lib   # cd packages/brick-frame-simulator && elm-test
make elm-test   # cd elm-app && elm-test
```

---

## P2 — Gate `onAnimationFrame` so idle models stop redrawing

**Priority: first.** Smallest diff, largest battery/CPU win, no rendering risk.

### Current behaviour

`elm-app/src/Main.elm:3170` subscribes unconditionally:

```elm
, Browser.Events.onAnimationFrame AnimationFrame
```

The handler at `Main.elm:757` does real work in exactly three cases:

1. `model.loadPhase == FlatteningGeometry` — drives the fake loading-progress bar
   (`Main.elm:760`).
2. `applyHeldControl` — a held rotate button is down, i.e.
   `model.heldControl /= NoHeldControl` (`Main.elm:783`, set by `StartHoldRotate`
   at `Main.elm:957`, cleared by `EndHoldRotate` at `Main.elm:965`).
3. `model.playback.running` — the gear animation is playing (`Main.elm:786`).

Otherwise the frame resolves to `( { heldModel | lastFrameTime = Nothing }, … )`
(`Main.elm:822`) — a no-op that still forces a full Elm `update` + `view` +
virtual-DOM diff at 60 fps on a completely static model.

### Change

Add a predicate and gate the subscription:

```elm
needsAnimationFrame : Model -> Bool
needsAnimationFrame model =
    model.playback.running
        || (model.loadPhase == FlatteningGeometry)
        || (case model.heldControl of
                NoHeldControl ->
                    False

                HoldRotate _ _ ->
                    True
           )
```

```elm
, if needsAnimationFrame model then
    Browser.Events.onAnimationFrame AnimationFrame

  else
    Sub.none
```

Pattern-match `heldControl` rather than using `/=` — explicit, and robust if a
non-comparable field is ever added to the type.

### Tests (`elm-app/tests/`)

`needsAnimationFrame` must be exposed from `Main` for testing.

- idle + stopped + no held control → `False`
- `playback.running = True` → `True`
- `loadPhase = FlatteningGeometry` → `True`
- `heldControl = HoldRotate 1 0` → `True`

### Watch out

`lastFrameTime` is reset to `Nothing` on `Play` (`Main.elm:857`) and in the idle
branch (`Main.elm:822`), so the first frame after resubscribing computes
`dtSeconds = 0.0` rather than a huge delta. Gating is therefore safe — but the
"stop" path must keep clearing `lastFrameTime`, otherwise a pause/resume cycle
would integrate all the wall-clock time spent paused into one frame. Add a
regression test for pause → resume producing no time jump.

---

## P1 — Move conditional-line visibility to the GPU

**Priority: second.** The single largest per-frame cost.

### Current behaviour

`packages/brick-frame-simulator/src/Render/Scene.elm:167`:

```elm
conditionalVisibleQuads =
    scene.conditionalLines
        |> List.filter (conditionalLineVisible (cameraPosition camera))
        |> List.concatMap conditionalToQuad
```

then `Main.elm:182`: `WebGL.triangles conditionalVisibleQuads`.

Every frame this walks the full conditional-edge list, runs two cross products
and two dot products per edge on the CPU (`conditionalLineVisible`,
`Scene.elm:224`), allocates 6 `EdgeVertex` records per surviving edge
(`lineToQuad`, `Scene.elm:208`), and then hands a **freshly-built list** to
`WebGL.triangles` — which forces a new GPU buffer allocation and upload on every
single frame. `elm-explorations/webgl` caches meshes by reference identity; a new
list each frame defeats that cache entirely.

Note `Scene.conditionalLines : List ConditionalEdge` (`Scene.elm:44`) is the only
field of `Scene` that is *not* a pre-built `WebGL.Mesh` — the other two are
uploaded once in `buildScene` (`Scene.elm:69-70`).

### Change

Upload all conditional edges **once** in `buildScene`, carrying the control
points `c1`/`c2` as vertex attributes, and perform the visibility test in the
vertex shader — collapsing invisible quads to zero area so they rasterize to
nothing.

1. New vertex type in `Render/EdgeShader.elm`:

   ```elm
   type alias ConditionalVertex =
       { position : Vec3
       , other : Vec3
       , side : Float
       , c1 : Vec3
       , c2 : Vec3
       }
   ```

2. New uniform `eyePosition : Vec3` added to a conditional-specific `Uniforms`
   record.

3. New `conditionalVertexShader` — the existing body plus, before the offset
   computation:

   ```glsl
   vec3 edge   = other - position;      // NB: only correct for the p1 vertices
   vec3 toEye  = eyePosition - position;
   float s1 = dot(cross(edge, c1 - position), toEye);
   float s2 = dot(cross(edge, c2 - position), toEye);
   if (s1 * s2 <= 0.0) { gl_Position = vec4(2.0, 2.0, 2.0, 1.0); return; }
   ```

   **Critical:** the CPU test always uses `p1` as the reference point
   (`Vec3.sub eye cond.p1`, `Vec3.sub cond.c1 cond.p1`). The quad's vertices
   alternate which endpoint is `position` vs `other` (`lineToQuad`,
   `Scene.elm:214`). Evaluating the predicate from each vertex's own `position`
   would give a **different sign for the `p2` vertices** and tear the quad. Add a
   dedicated `p1` attribute (or always store `c1`/`c2` pre-subtracted relative to
   `p1`, plus `edge` and the `p1` world position) so every one of the 6 vertices
   evaluates the *identical* predicate. Getting this wrong produces half-drawn
   conditional lines — subtle and easy to miss visually.

4. `Scene` becomes:

   ```elm
   , conditionalMesh : WebGL.Mesh ConditionalVertex
   ```

   built once in `buildScene`. `renderSceneWithStyle` then always emits the
   entity, with no CPU filtering.

5. `eyePosition` must be in the **same space** as the stored control points. The
   CPU version compares world-space `cameraPosition camera` against world-space
   `cond.*`. Confirm what `modelMatrix` is in `edgeUniforms` — if it is not
   identity, transform the eye into model space on the CPU once per frame rather
   than transforming every vertex.

### Tests (`packages/brick-frame-simulator/tests/Render/`)

GLSL cannot be unit-tested here, so test the extracted pure parts:

- Keep `conditionalLineVisible` exposed as the **reference implementation** and
  test it directly (front-facing pair visible, straddling pair hidden,
  degenerate/zero-length edge does not crash).
- Test the new vertex-builder: one `ConditionalEdge` produces 6 vertices, and
  **all 6 carry identical `c1`, `c2` and reference-point values** — this is the
  regression guard for the tearing bug above.
- Test that `buildScene` output is referentially stable across calls with the
  same input, so the mesh cache actually holds.

### Verify visually

`make run` (or the docs playground) and orbit a gear model — conditional lines
mark cylinder silhouettes, so they should smoothly appear/disappear as the
camera moves. Any popping, tearing, or half-edges means step 3 is wrong.

---

## P3 — Spatial-hash `buildGearGraph`

**Priority: third.** Load-time only, not per-frame — but O(n²) on large models.

### Current behaviour

`packages/brick-frame-simulator/src/Gear/Detect.elm:53`. Nested `List.foldl` over
`List.range 0 (n-1)` × `List.range (i+1) (n-1)`, with **two `Array.get` calls per
pair** (`Detect.elm:67`). Both the module docstring (`Detect.elm:13`) and the
comment call this out as O(n²).

### Change

Bucket gears into a spatial hash and only compare within neighbouring cells.

**The trap — `coAxial` is not distance-bounded.** `meshing` (`Detect.elm:124`)
is bounded: it requires `abs (dist - expected) <= radialTolerance` where
`expected = r1 + r2`, so a cell size of `2 * maxPitchRadius + maxTolerance` with
a 3×3×3 neighbour sweep is sound. But `coAxial` (`Detect.elm:174`) only requires
`absDot >= 0.99 && radialOffset <= 2.0` — the *axial* separation is unbounded.
Two gears at opposite ends of a long axle are `coAxial` at any distance. A naive
Euclidean spatial hash **silently drops those rigid-axle links**, which changes
simulation results (rigid axles propagate 1:1 rotation through
`Animate.gearAngles`).

Handle the two relations with two different indexes:

1. **Meshing** — 3D spatial hash keyed on `floor (pos / cellSize)`, sweeping the
   27 neighbouring cells. `cellSize = 2 * maxPitchRadius + 2.0`.

2. **Co-axial** — hash by *axle line* rather than position. For each gear compute
   its canonical axis (flip sign so the first non-zero component is positive,
   since `absDot` makes direction irrelevant) and the closest point on its axis
   to the origin: `p - (p · a) a`. Quantize `(canonicalAxis, closestPoint)` to a
   bucket. Gears sharing an axle land in the same bucket regardless of how far
   apart they are along it. Sweep neighbouring buckets in the quantized
   dimensions so pairs straddling a boundary are not missed — the `0.99` /
   `2.0` thresholds are tight, so quantization error must stay well inside them.

Keep `meshing`, `coAxial`, `gearAxis`, and `addConnection` byte-for-byte
unchanged — only the *pair enumeration* changes. That keeps the diff auditable
and makes the equivalence test below meaningful.

Also drop the `Array.get` calls from the inner loop by folding over the instance
records directly; the array indirection is pure overhead once pairs are
enumerated from buckets.

### Tests (`packages/brick-frame-simulator/tests/Gear/DetectTest.elm`)

The decisive test is **equivalence with the current implementation**:

- Keep the existing O(n²) version as `buildGearGraphReference` (test-only, or
  private + exposed for tests) and assert both produce identical `connections`
  and `rigidAxles` across a battery of layouts.
- Explicit case: **two co-axial gears separated by a long axial distance** —
  must still appear in `rigidAxles`. This is the test that fails if the naive
  spatial hash is used.
- Explicit case: two gears straddling a cell boundary still detected as meshing.
- Worm-drive directionality preserved (`isWorm`, `Detect.elm:73-77`) — worm→wheel
  edge only, no back edge.
- Connection list **ordering**: `addConnection` prepends (`Detect.elm:244`), so
  changing iteration order changes list order. Either sort before comparing, or
  preserve the `i < j` ascending enumeration. Decide explicitly — downstream BFS
  in `Animate` may be order-sensitive for tie-breaking.

### Note

Ordering is the likeliest source of a false "equivalent" result. Compare as sets
*and* assert the chosen ordering convention separately.

---

## P4 — Wire up distance-based gear LOD

**Priority: last.** Feature work, not a pure refactor; largest behavioural risk.

### Current state

`src/Lib.hs:163` generates `Data.lodParts : Dict String String` — every embedded
gear part with every other geometry line dropped (`simplifyPartText`), exposed at
`src/Lib.hs:166,199-202`. `docs/rendering.md:211-214` confirms it is emitted but
unused: *"available for distance-based LOD switching, though the main app
currently uses full-detail meshes for all gear rendering."*

### Change

At gear-mesh build time, choose `Data.lodParts` over `Data.embeddedParts` when
the gear's distance from the camera exceeds a threshold.

Design decisions to settle before implementing:

- **Where the switch happens.** Rebuilding meshes as the camera moves would
  reintroduce exactly the per-frame upload cost P1 removes. Prefer building
  *both* LOD meshes once at load, then selecting which pre-built mesh to draw
  per frame — selection is cheap, uploading is not.
- **Hysteresis.** A single threshold makes gears flicker between LODs when the
  camera hovers near it. Use separate switch-up/switch-down distances.
- **Threshold units.** LDU, scaled by pitch radius — a 40-tooth gear should stay
  detailed further out than an 8-tooth one.

### Tests

- LOD selection is a pure function of (distance, pitchRadius) → `Full | Simplified`;
  test the threshold and the hysteresis band directly.
- Every key in `Data.embeddedParts` has a counterpart in `Data.lodParts`, so
  selection can never fail to find a mesh (guards against generator drift).
- Simplified parts still parse and flatten without error.

### Verify visually

Zoom out on a dense model and confirm no popping at the switch distance.

---

## Suggested order and commits

1. `perf: only subscribe to animation frames when the scene is animating` (P2)
2. `perf: build conditional-line mesh once and test visibility on the GPU` (P1)
3. `perf: spatial-hash gear pair detection` (P3)
4. `feat: distance-based gear LOD` (P4)

P1 and P3 touch `packages/brick-frame-simulator`, so `make test-lib` must pass;
P2 touches `elm-app`, so `make elm-test` must pass. Run `make test` before the
final push.
