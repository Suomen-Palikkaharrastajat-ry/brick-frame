import assert from 'node:assert/strict'
import test from 'node:test'
import { buildBVH, createHit, intersect, occluded } from './bvh.js'
import { createRng } from './rng.js'

/** One triangle in the contract layout (14 numbers). */
function tri(a, b, c, material = 0) {
  return [...a, ...b, ...c, 0.8, 0.8, 0.8, 1, material]
}

function flat(list) {
  return Float32Array.from(list.flat())
}

/** Independent reference: plane intersection plus a same-side test per edge. */
function bruteForceNearest(triangles, ray) {
  let best = Infinity
  let bestIndex = -1
  const count = triangles.length / 14
  for (let i = 0; i < count; i++) {
    const v = [0, 1, 2].map((k) => triangles.slice(i * 14 + k * 3, i * 14 + k * 3 + 3))
    const n = cross(sub(v[1], v[0]), sub(v[2], v[0]))
    const dir = [ray.dx, ray.dy, ray.dz]
    const origin = [ray.ox, ray.oy, ray.oz]
    const denom = dot(n, dir)
    if (Math.abs(denom) < 1e-12) continue
    const t = dot(n, sub(v[0], origin)) / denom
    if (!(t > 1e-9 && t < best)) continue
    const p = add(origin, scale(dir, t))
    const inside = [0, 1, 2].every((k) => {
      const e = sub(v[(k + 1) % 3], v[k])
      return dot(cross(e, sub(p, v[k])), n) >= -1e-6
    })
    if (inside) {
      best = t
      bestIndex = i
    }
  }
  return { t: best, index: bestIndex }
}

const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
const add = (a, b) => [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
const scale = (a, s) => [a[0] * s, a[1] * s, a[2] * s]
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]

test('ray hits a single triangle and misses when pointed away', () => {
  const bvh = buildBVH(flat([tri([-1, -1, -5], [1, -1, -5], [0, 1, -5])]))
  const hit = intersect(bvh, { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: -1, tMin: 0, tMax: Infinity }, createHit())
  assert.ok(hit)
  assert.ok(Math.abs(hit.t - 5) < 1e-5)
  assert.equal(hit.index, 0)
  assert.ok(hit.u >= 0 && hit.v >= 0 && hit.u + hit.v <= 1)
  assert.equal(intersect(bvh, { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: 1, tMin: 0, tMax: Infinity }), null)
  assert.equal(intersect(bvh, { ox: 0, oy: 0, oz: 0, dx: 1, dy: 0, dz: 0, tMin: 0, tMax: Infinity }), null)
})

test('two-sided: a back-facing hit is still reported', () => {
  const bvh = buildBVH(flat([tri([-1, -1, -5], [0, 1, -5], [1, -1, -5])]))
  const hit = intersect(bvh, { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: -1, tMin: 0, tMax: Infinity })
  assert.ok(hit)
  assert.ok(Math.abs(hit.t - 5) < 1e-5)
})

test('nearest of stacked triangles wins and tMax bounds the search', () => {
  const far = tri([-10, -10, -10], [10, -10, -10], [0, 10, -10])
  const near = tri([-10, -10, -4], [10, -10, -4], [0, 10, -4])
  const bvh = buildBVH(flat([far, near]))
  const ray = { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: -1, tMin: 0, tMax: Infinity }
  const hit = intersect(bvh, ray)
  assert.equal(hit.index, 1)
  assert.ok(Math.abs(hit.t - 4) < 1e-5)
  assert.equal(intersect(bvh, { ...ray, tMax: 3 }), null)
  assert.equal(occluded(bvh, { ...ray, tMax: 3 }), false)
  assert.equal(occluded(bvh, { ...ray, tMax: 5 }), true)
})

test('occluded ignores geometry behind the origin', () => {
  const bvh = buildBVH(flat([tri([-1, -1, 5], [1, -1, 5], [0, 1, 5])]))
  assert.equal(occluded(bvh, { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: -1, tMin: 0, tMax: Infinity }), false)
  assert.equal(occluded(bvh, { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: 1, tMin: 0, tMax: Infinity }), true)
})

test('BVH matches brute force on a random triangle soup', () => {
  const rng = createRng(42)
  const rand = (lo, hi) => lo + (hi - lo) * rng.next()
  const list = []
  for (let i = 0; i < 400; i++) {
    const c = [rand(-50, 50), rand(-50, 50), rand(-50, 50)]
    const jitter = () => [rand(-6, 6), rand(-6, 6), rand(-6, 6)]
    list.push(tri(add(c, jitter()), add(c, jitter()), add(c, jitter())))
  }
  const triangles = flat(list)
  const bvh = buildBVH(triangles)
  assert.equal(bvh.count, 400)
  const ray = { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: 0, tMin: 0, tMax: Infinity }
  const hit = createHit()
  let hits = 0
  for (let r = 0; r < 300; r++) {
    const o = [rand(-80, 80), rand(-80, 80), rand(-80, 80)]
    const d = [rand(-1, 1), rand(-1, 1), rand(-1, 1)]
    Object.assign(ray, { ox: o[0], oy: o[1], oz: o[2], dx: d[0], dy: d[1], dz: d[2] })
    const expected = bruteForceNearest(triangles, ray)
    const got = intersect(bvh, ray, hit)
    if (expected.index < 0) {
      assert.equal(got, null)
      assert.equal(occluded(bvh, ray), false)
      continue
    }
    hits++
    assert.ok(got, 'BVH missed a triangle the reference hits')
    assert.ok(Math.abs(got.t - expected.t) <= 1e-4 * Math.max(1, expected.t), `t ${got.t} vs ${expected.t}`)
    assert.equal(occluded(bvh, { ...ray, tMax: expected.t * 0.999 }), false)
    assert.equal(occluded(bvh, { ...ray, tMax: expected.t * 1.001 }), true)
  }
  assert.ok(hits > 20, `too few hits to be meaningful: ${hits}`)
})

test('leaves hold at most 4 triangles and cover every triangle once', () => {
  const list = []
  for (let i = 0; i < 97; i++) list.push(tri([i, 0, 0], [i + 1, 0, 0], [i, 1, i % 7]))
  const bvh = buildBVH(flat(list))
  const seen = new Uint8Array(97)
  for (let node = 0; node < bvh.nodeCount; node++) {
    const count = bvh.meta[node * 3 + 1]
    if (count === 0) continue
    assert.ok(count <= 4)
    for (let k = 0; k < count; k++) seen[bvh.prims[bvh.meta[node * 3] + k]]++
  }
  assert.ok(seen.every((v) => v === 1))
})
