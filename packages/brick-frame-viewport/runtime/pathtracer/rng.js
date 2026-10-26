/**
 * Seedable 32-bit PRNG (sfc32) for the path tracer's hot loop.
 *
 * `createRng(seed)` returns `{ next() }` where `next()` yields a float in
 * [0, 1). Two generators with the same seed produce the same sequence, so
 * renders are reproducible. No `Math.random` anywhere in the tracer.
 */

/** Deterministic generator seeded from an integer (any value). */
export function createRng(seed = 1) {
  let a = 0x9e3779b9 ^ seed
  let b = 0x243f6a88 ^ (seed * 0x85ebca6b)
  let c = 0xb7e15162 ^ (seed * 0xc2b2ae35)
  let d = 1
  // Warm up so nearby seeds decorrelate.
  const state = { a: a | 0, b: b | 0, c: c | 0, d }
  const warm = () => {
    let t = (state.a + state.b) | 0
    state.a = state.b ^ (state.b >>> 9)
    state.b = (state.c + (state.c << 3)) | 0
    state.c = (state.c << 21) | (state.c >>> 11)
    state.d = (state.d + 1) | 0
    t = (t + state.d) | 0
    state.c = (state.c + t) | 0
    return t
  }
  for (let i = 0; i < 16; i++) warm()
  return {
    next() {
      return (warm() >>> 0) / 4294967296
    },
  }
}
