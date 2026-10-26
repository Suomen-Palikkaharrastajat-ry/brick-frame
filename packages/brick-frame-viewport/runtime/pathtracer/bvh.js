/**
 * Bounding volume hierarchy over a flat triangle soup, for the path tracer.
 *
 * Input is the contract layout: 14 numbers per triangle
 * (`x1,y1,z1, x2,y2,z2, x3,y3,z3, r,g,b,a, material`). The builder uses
 * binned SAH (16 bins) with leaves of at most 4 triangles, and stores the
 * result in flat typed arrays:
 *
 * - `bounds`: Float32Array, 6 per node (min xyz, max xyz), padded slightly.
 * - `meta`: Uint32Array, 3 per node `[a, count, axis]`. A leaf has
 *   `count > 0` and `a` = first slot; an interior node has `count = 0`,
 *   `a` = index of its right child (the left child is always `node + 1`)
 *   and `axis` = split axis used to visit the near child first.
 * - `tris`: Float32Array, 9 per slot `[v0, edge1, edge2]`, edges precomputed
 *   for Möller–Trumbore.
 * - `prims`: Uint32Array mapping each slot back to the input triangle index.
 *
 * Pure functions with no DOM access, so it runs under `node --test`.
 */

const STRIDE = 14
const BINS = 16
const LEAF_MAX = 4
const BOUNDS_PAD = 1e-3
const STACK_SIZE = 256
// Beyond this depth the builder splits at the median, so the traversal stack stays bounded.
const MAX_SAH_DEPTH = 96
const DIR_EPS = 1e-30
const T_EPS = 1e-12

/**
 * Scratch record returned by `intersect`. Pass one in to avoid allocation.
 * `t` is the ray parameter, `slot` the BVH slot, `index` the input triangle
 * index, and `u`, `v` the barycentric coordinates (w = 1 - u - v).
 */
export function createHit() {
  return { t: Infinity, slot: 0, index: 0, u: 0, v: 0 }
}

/** Triangle count of a flat contract array. */
export function triangleCount(triangles) {
  return Math.floor(triangles.length / STRIDE)
}

/**
 * Build a BVH over `triangles` (14 numbers per triangle).
 * Returns the flat arrays described in the module header, plus `count`.
 */
export function buildBVH(triangles) {
  const count = triangleCount(triangles)
  const primMin = new Float64Array(count * 3)
  const primMax = new Float64Array(count * 3)
  const centroid = new Float64Array(count * 3)
  for (let i = 0; i < count; i++) {
    const base = i * STRIDE
    for (let a = 0; a < 3; a++) {
      const v0 = triangles[base + a]
      const v1 = triangles[base + 3 + a]
      const v2 = triangles[base + 6 + a]
      const lo = Math.min(v0, v1, v2)
      const hi = Math.max(v0, v1, v2)
      primMin[i * 3 + a] = lo
      primMax[i * 3 + a] = hi
      centroid[i * 3 + a] = (lo + hi) / 2
    }
  }

  const prims = new Uint32Array(count)
  for (let i = 0; i < count; i++) prims[i] = i

  const maxNodes = Math.max(1, 2 * count)
  const bounds = new Float32Array(maxNodes * 6)
  const meta = new Uint32Array(maxNodes * 3)
  const binCount = new Uint32Array(BINS)
  const binMin = new Float64Array(BINS * 3)
  const binMax = new Float64Array(BINS * 3)
  const leftMin = new Float64Array((BINS - 1) * 3)
  const leftMax = new Float64Array((BINS - 1) * 3)
  const leftCount = new Uint32Array(BINS - 1)
  let nodeCount = 0

  function surfaceArea(mn, mx) {
    const dx = mx[0] - mn[0]
    const dy = mx[1] - mn[1]
    const dz = mx[2] - mn[2]
    if (dx < 0) return 0
    return 2 * (dx * dy + dy * dz + dz * dx)
  }

  const mn = [0, 0, 0]
  const mx = [0, 0, 0]
  const cmn = [0, 0, 0]
  const cmx = [0, 0, 0]

  /** Builds the subtree over prims[start..end) and returns its node index. */
  function build(start, end, depth) {
    const node = nodeCount++
    mn[0] = mn[1] = mn[2] = Infinity
    mx[0] = mx[1] = mx[2] = -Infinity
    cmn[0] = cmn[1] = cmn[2] = Infinity
    cmx[0] = cmx[1] = cmx[2] = -Infinity
    for (let k = start; k < end; k++) {
      const p = prims[k]
      for (let a = 0; a < 3; a++) {
        if (primMin[p * 3 + a] < mn[a]) mn[a] = primMin[p * 3 + a]
        if (primMax[p * 3 + a] > mx[a]) mx[a] = primMax[p * 3 + a]
        const c = centroid[p * 3 + a]
        if (c < cmn[a]) cmn[a] = c
        if (c > cmx[a]) cmx[a] = c
      }
    }
    const b = node * 6
    for (let a = 0; a < 3; a++) {
      bounds[b + a] = mn[a] - BOUNDS_PAD
      bounds[b + 3 + a] = mx[a] + BOUNDS_PAD
    }

    const n = end - start
    if (n <= LEAF_MAX) {
      makeLeaf(node, start, n)
      return node
    }

    // Largest centroid extent picks the split axis; degenerate extents mean
    // every centroid coincides, so a median split by index is the only option.
    let axis = 0
    for (let a = 1; a < 3; a++) {
      if (cmx[a] - cmn[a] > cmx[axis] - cmn[axis]) axis = a
    }
    const extent = cmx[axis] - cmn[axis]
    let mid = -1

    if (extent > 0 && depth < MAX_SAH_DEPTH) {
      const best = findBestSplit(start, end)
      if (best.bin >= 0) {
        mid = partition(start, end, best.axis, best.bin)
        axis = best.axis
      }
    }
    if (mid <= start || mid >= end) {
      mid = start + (n >> 1)
      sortRangeByCentroid(start, end, axis)
    }

    meta[node * 3 + 1] = 0
    meta[node * 3 + 2] = axis
    build(start, mid, depth + 1)
    const right = build(mid, end, depth + 1)
    meta[node * 3] = right
    return node
  }

  function makeLeaf(node, start, n) {
    meta[node * 3] = start
    meta[node * 3 + 1] = n
    meta[node * 3 + 2] = 0
  }

  function binOf(c, lo, scale) {
    const bin = Math.floor((c - lo) * scale)
    return bin >= BINS ? BINS - 1 : bin < 0 ? 0 : bin
  }

  /** Binned SAH over all three axes; returns the cheapest split and whether a leaf is cheaper. */
  function findBestSplit(start, end) {
    let bestCost = Infinity
    let bestAxis = -1
    let bestBin = -1
    for (let a = 0; a < 3; a++) {
      const lo = cmn[a]
      const ext = cmx[a] - lo
      if (ext <= 0) continue
      const scale = BINS / ext
      binCount.fill(0)
      for (let i = 0; i < BINS; i++) {
        for (let k = 0; k < 3; k++) {
          binMin[i * 3 + k] = Infinity
          binMax[i * 3 + k] = -Infinity
        }
      }
      for (let k = start; k < end; k++) {
        const p = prims[k]
        const bin = binOf(centroid[p * 3 + a], lo, scale)
        binCount[bin]++
        for (let q = 0; q < 3; q++) {
          if (primMin[p * 3 + q] < binMin[bin * 3 + q]) binMin[bin * 3 + q] = primMin[p * 3 + q]
          if (primMax[p * 3 + q] > binMax[bin * 3 + q]) binMax[bin * 3 + q] = primMax[p * 3 + q]
        }
      }
      // Sweep from the left to get prefix bounds/counts for each split.
      const acc = [Infinity, Infinity, Infinity, -Infinity, -Infinity, -Infinity]
      let runCount = 0
      for (let i = 0; i < BINS - 1; i++) {
        runCount += binCount[i]
        for (let q = 0; q < 3; q++) {
          acc[q] = Math.min(acc[q], binMin[i * 3 + q])
          acc[3 + q] = Math.max(acc[3 + q], binMax[i * 3 + q])
          leftMin[i * 3 + q] = acc[q]
          leftMax[i * 3 + q] = acc[3 + q]
        }
        leftCount[i] = runCount
      }
      // Sweep from the right and combine with the prefix sums.
      const racc = [Infinity, Infinity, Infinity, -Infinity, -Infinity, -Infinity]
      let rightCount = 0
      for (let i = BINS - 1; i >= 1; i--) {
        rightCount += binCount[i]
        for (let q = 0; q < 3; q++) {
          racc[q] = Math.min(racc[q], binMin[i * 3 + q])
          racc[3 + q] = Math.max(racc[3 + q], binMax[i * 3 + q])
        }
        const s = i - 1
        const nl = leftCount[s]
        if (nl === 0 || rightCount === 0) continue
        const areaL = surfaceArea(leftMin.subarray(s * 3, s * 3 + 3), leftMax.subarray(s * 3, s * 3 + 3))
        const areaR = surfaceArea(racc.slice(0, 3), racc.slice(3))
        const cost = areaL * nl + areaR * rightCount
        if (cost < bestCost) {
          bestCost = cost
          bestAxis = a
          bestBin = s
        }
      }
    }
    return { axis: bestAxis, bin: bestBin }
  }

  /** Partition prims[start..end) so bins <= splitBin come first. Returns the split index. */
  function partition(start, end, axis, splitBin) {
    const lo = cmn[axis]
    const scale = BINS / (cmx[axis] - lo)
    let i = start
    let j = end - 1
    while (i <= j) {
      if (binOf(centroid[prims[i] * 3 + axis], lo, scale) <= splitBin) {
        i++
      } else {
        const tmp = prims[i]
        prims[i] = prims[j]
        prims[j] = tmp
        j--
      }
    }
    return i
  }

  function sortRangeByCentroid(start, end, axis) {
    const sorted = Array.from(prims.subarray(start, end))
    sorted.sort((p, q) => centroid[p * 3 + axis] - centroid[q * 3 + axis])
    for (let k = 0; k < sorted.length; k++) prims[start + k] = sorted[k]
  }

  if (count > 0) build(0, count, 0)

  const tris = new Float32Array(count * 9)
  for (let slot = 0; slot < count; slot++) {
    const base = prims[slot] * STRIDE
    const o = slot * 9
    for (let a = 0; a < 3; a++) {
      const v0 = triangles[base + a]
      const v1 = triangles[base + 3 + a]
      const v2 = triangles[base + 6 + a]
      tris[o + a] = v0
      tris[o + 3 + a] = v1 - v0
      tris[o + 6 + a] = v2 - v0
    }
  }

  return {
    count,
    nodeCount,
    bounds: bounds.slice(0, nodeCount * 6),
    meta: meta.slice(0, nodeCount * 3),
    tris,
    prims,
    stack: new Uint32Array(STACK_SIZE),
  }
}

/**
 * Slab test of node `node` against the ray. `ix,iy,iz` are reciprocal
 * direction components; the test is pruned to `tMax`.
 */
function boxHit(bounds, node, ox, oy, oz, ix, iy, iz, tMax) {
  const b = node * 6
  let t0 = (bounds[b] - ox) * ix
  let t1 = (bounds[b + 3] - ox) * ix
  let tNear = Math.min(t0, t1)
  let tFar = Math.max(t0, t1)
  t0 = (bounds[b + 1] - oy) * iy
  t1 = (bounds[b + 4] - oy) * iy
  tNear = Math.max(tNear, Math.min(t0, t1))
  tFar = Math.min(tFar, Math.max(t0, t1))
  t0 = (bounds[b + 2] - oz) * iz
  t1 = (bounds[b + 5] - oz) * iz
  tNear = Math.max(tNear, Math.min(t0, t1))
  tFar = Math.min(tFar, Math.max(t0, t1))
  return tNear <= tFar && tFar >= 0 && tNear < tMax
}

/**
 * Möller–Trumbore test of slot `slot`. Two-sided (no culling). Returns the
 * ray parameter, or -1 for a miss; barycentrics are written to `out` as
 * `out[0] = u, out[1] = v`.
 */
function triangleHit(tris, slot, ox, oy, oz, dx, dy, dz, tMin, tMax, out) {
  const o = slot * 9
  const v0x = tris[o]
  const v0y = tris[o + 1]
  const v0z = tris[o + 2]
  const e1x = tris[o + 3]
  const e1y = tris[o + 4]
  const e1z = tris[o + 5]
  const e2x = tris[o + 6]
  const e2y = tris[o + 7]
  const e2z = tris[o + 8]
  // pvec = dir × e2
  const px = dy * e2z - dz * e2y
  const py = dz * e2x - dx * e2z
  const pz = dx * e2y - dy * e2x
  const det = e1x * px + e1y * py + e1z * pz
  if (det > -T_EPS && det < T_EPS) return -1
  const inv = 1 / det
  const tx = ox - v0x
  const ty = oy - v0y
  const tz = oz - v0z
  const u = (tx * px + ty * py + tz * pz) * inv
  if (u < 0 || u > 1) return -1
  // qvec = tvec × e1
  const qx = ty * e1z - tz * e1y
  const qy = tz * e1x - tx * e1z
  const qz = tx * e1y - ty * e1x
  const v = (dx * qx + dy * qy + dz * qz) * inv
  if (v < 0 || u + v > 1) return -1
  const t = (e2x * qx + e2y * qy + e2z * qz) * inv
  if (t < tMin || t > tMax) return -1
  out[0] = u
  out[1] = v
  return t
}

const scratchUV = new Float64Array(2)

/**
 * Nearest hit of `ray` (`{ ox, oy, oz, dx, dy, dz, tMin, tMax }`, direction
 * need not be normalised, `tMax` may be Infinity). Writes into `out` (a
 * record from `createHit`) and returns it, or returns null on a miss.
 */
export function intersect(bvh, ray, out = createHit()) {
  const { bounds, meta, tris, prims, stack } = bvh
  if (bvh.count === 0) return null
  const ox = ray.ox
  const oy = ray.oy
  const oz = ray.oz
  const dx = ray.dx
  const dy = ray.dy
  const dz = ray.dz
  const tMin = ray.tMin ?? 0
  const ix = 1 / (dx === 0 ? DIR_EPS : dx)
  const iy = 1 / (dy === 0 ? DIR_EPS : dy)
  const iz = 1 / (dz === 0 ? DIR_EPS : dz)
  let tBest = ray.tMax ?? Infinity
  let found = false
  let sp = 0
  let node = 0
  for (;;) {
    if (boxHit(bounds, node, ox, oy, oz, ix, iy, iz, tBest)) {
      const count = meta[node * 3 + 1]
      if (count > 0) {
        const first = meta[node * 3]
        for (let k = first; k < first + count; k++) {
          const t = triangleHit(tris, k, ox, oy, oz, dx, dy, dz, tMin, tBest, scratchUV)
          if (t >= 0) {
            tBest = t
            found = true
            out.t = t
            out.slot = k
            out.index = prims[k]
            out.u = scratchUV[0]
            out.v = scratchUV[1]
          }
        }
      } else {
        const right = meta[node * 3]
        const left = node + 1
        const axis = meta[node * 3 + 2]
        const dirNeg = axis === 0 ? dx < 0 : axis === 1 ? dy < 0 : dz < 0
        const near = dirNeg ? right : left
        const far = dirNeg ? left : right
        stack[sp++] = far
        node = near
        continue
      }
    }
    if (sp === 0) break
    node = stack[--sp]
  }
  return found ? out : null
}

/**
 * True when any triangle lies strictly within `(tMin, tMax)` along the ray.
 * Used for shadow rays; exits on the first hit.
 */
export function occluded(bvh, ray) {
  const { bounds, meta, tris, stack } = bvh
  if (bvh.count === 0) return false
  const ox = ray.ox
  const oy = ray.oy
  const oz = ray.oz
  const dx = ray.dx
  const dy = ray.dy
  const dz = ray.dz
  const tMin = ray.tMin ?? 0
  const tMax = ray.tMax ?? Infinity
  const ix = 1 / (dx === 0 ? DIR_EPS : dx)
  const iy = 1 / (dy === 0 ? DIR_EPS : dy)
  const iz = 1 / (dz === 0 ? DIR_EPS : dz)
  let sp = 0
  let node = 0
  for (;;) {
    if (boxHit(bounds, node, ox, oy, oz, ix, iy, iz, tMax)) {
      const count = meta[node * 3 + 1]
      if (count > 0) {
        const first = meta[node * 3]
        for (let k = first; k < first + count; k++) {
          if (triangleHit(tris, k, ox, oy, oz, dx, dy, dz, tMin, tMax, scratchUV) >= 0) return true
        }
      } else {
        stack[sp++] = meta[node * 3]
        node = node + 1
        continue
      }
    }
    if (sp === 0) break
    node = stack[--sp]
  }
  return false
}
