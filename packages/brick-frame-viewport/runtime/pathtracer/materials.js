/**
 * LEGO-like BSDFs for the path tracer.
 *
 * Materials (the contract's `material` codes, plus MATTE for the ground):
 * - PLASTIC (0): Lambert base in the linear colour under a GGX clear coat
 *   (IOR 1.46, F0 0.04, roughness 0.15). The coat is chosen stochastically
 *   by its Fresnel weight.
 * - TRANSPARENT (1): smooth dielectric (IOR 1.49). Reflection or refraction
 *   is picked by Fresnel; the Beer–Lambert tint is applied by the integrator
 *   (see trace.js), which tracks the medium.
 * - CHROME (2): GGX metal, roughness 0.08, F0 = linear colour.
 * - RUBBER (3): Lambert with a very dull coat (F0 0.02, roughness 0.6).
 * - PEARL (4): plastic with a rougher (0.3) coat whose F0 leans toward the
 *   colour, so the highlight takes on the paint tint.
 * - MATTE (5): pure Lambert, used for the studio ground.
 *
 * Shading is done in a local frame where the normal is +z. Callers pass a
 * frame `{ t, b, n }` (world-space, orthonormal) with `n` facing the viewer,
 * and world-space `wo`. Output vectors are written into caller-provided
 * records, so nothing is allocated per sample.
 */

export const MATERIAL = Object.freeze({
  PLASTIC: 0,
  TRANSPARENT: 1,
  CHROME: 2,
  RUBBER: 3,
  PEARL: 4,
  MATTE: 5,
})

/** Index of refraction of the clear coat / plastic. */
const COAT_IOR = 1.46
/** Index of refraction of transparent LEGO plastic. */
export const TRANSPARENT_IOR = 1.49
const COAT_F0 = ((COAT_IOR - 1) / (COAT_IOR + 1)) ** 2
const RUBBER_F0 = 0.02
const PEARL_TINT = 0.35
const PLASTIC_ROUGHNESS = 0.15
const PEARL_ROUGHNESS = 0.3
const RUBBER_ROUGHNESS = 0.6
const CHROME_ROUGHNESS = 0.08
/** Clamp for the coat selection probability, keeps both lobes reachable. */
const COAT_PROB_MIN = 0.02
const COAT_PROB_MAX = 0.95
const INV_PI = 1 / Math.PI

/** Allocates a shading frame `{ t, b, n }`. */
export function createFrame() {
  return { t: new Float64Array(3), b: new Float64Array(3), n: new Float64Array(3) }
}

/** Allocates the scratch record that `evalSurface` fills. */
export function createEval() {
  return { f: new Float64Array(3), pdf: 0 }
}

/** Allocates the scratch record that `sampleSurface` fills. */
export function createSample() {
  return { wi: new Float64Array(3), weight: new Float64Array(3), pdf: 0 }
}

/** Builds an orthonormal (t, b) pair around `frame.n`, which must be unit length. */
export function setFrame(frame, nx, ny, nz) {
  const sign = nz >= 0 ? 1 : -1
  const a = -1 / (sign + nz)
  const b = nx * ny * a
  const { t, b: bt, n } = frame
  n[0] = nx
  n[1] = ny
  n[2] = nz
  t[0] = 1 + sign * nx * nx * a
  t[1] = sign * b
  t[2] = -sign * nx
  bt[0] = b
  bt[1] = sign + ny * ny * a
  bt[2] = -ny
  return frame
}

function dot3(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

function schlick(cosTheta, f0) {
  const m = 1 - Math.min(1, Math.max(0, cosTheta))
  const m2 = m * m
  return f0 + (1 - f0) * m2 * m2 * m
}

/** GGX normal distribution with alpha² = a2. */
function ggxD(a2, nh) {
  const d = nh * nh * (a2 - 1) + 1
  return a2 / (Math.PI * d * d)
}

/** Smith G1 for GGX. */
function ggxG1(a2, nx) {
  return (2 * nx) / (nx + Math.sqrt(a2 + (1 - a2) * nx * nx))
}

// Scratch vectors, reused by every call. The module is single-threaded per worker.
const lo = new Float64Array(3)
const li = new Float64Array(3)
const lh = new Float64Array(3)
const f0v = new Float64Array(3)
const fcol = new Float64Array(3)
const sampleLocal = new Float64Array(3)

function toLocal(frame, v, out) {
  out[0] = dot3(v, frame.t)
  out[1] = dot3(v, frame.b)
  out[2] = dot3(v, frame.n)
  return out
}

function toWorld(frame, l, out) {
  for (let i = 0; i < 3; i++) {
    out[i] = frame.t[i] * l[0] + frame.b[i] * l[1] + frame.n[i] * l[2]
  }
  return out
}

/** Probability of choosing the coat lobe for outgoing direction with cosine `nv`. */
function coatProbability(nv, f0avg) {
  return Math.min(COAT_PROB_MAX, Math.max(COAT_PROB_MIN, schlick(nv, f0avg)))
}

/**
 * Sets the coat F0 per channel in `f0v` for the material and returns its
 * specular roughness.
 */
function materialParams(material, albedo) {
  switch (material) {
    case MATERIAL.CHROME:
      f0v[0] = albedo[0]
      f0v[1] = albedo[1]
      f0v[2] = albedo[2]
      return CHROME_ROUGHNESS
    case MATERIAL.RUBBER:
      f0v[0] = f0v[1] = f0v[2] = RUBBER_F0
      return RUBBER_ROUGHNESS
    case MATERIAL.PEARL:
      for (let i = 0; i < 3; i++) {
        f0v[i] = COAT_F0 + (albedo[i] - COAT_F0) * PEARL_TINT
      }
      return PEARL_ROUGHNESS
    default:
      f0v[0] = f0v[1] = f0v[2] = COAT_F0
      return PLASTIC_ROUGHNESS
  }
}

/**
 * Evaluates the BSDF for world-space `wo` (toward the viewer) and `wi`
 * (toward the light). Writes the colour into `out.f` and the density of
 * `sampleSurface` choosing `wi` into `out.pdf`. Only valid for non-delta
 * materials (everything except TRANSPARENT).
 */
export function evalSurface(material, albedo, frame, wo, wi, out) {
  toLocal(frame, wo, lo)
  toLocal(frame, wi, li)
  const nv = lo[2]
  const nl = li[2]
  if (nv <= 0 || nl <= 0) {
    out.f[0] = out.f[1] = out.f[2] = 0
    out.pdf = 0
    return out
  }

  if (material === MATERIAL.MATTE) {
    for (let i = 0; i < 3; i++) out.f[i] = albedo[i] * INV_PI
    out.pdf = nl * INV_PI
    return out
  }

  const roughness = materialParams(material, albedo)
  const alpha = roughness * roughness
  const a2 = alpha * alpha
  // Half vector in local space.
  let hx = lo[0] + li[0]
  let hy = lo[1] + li[1]
  let hz = lo[2] + li[2]
  const hl = Math.sqrt(hx * hx + hy * hy + hz * hz)
  hx /= hl
  hy /= hl
  hz /= hl
  lh[0] = hx
  lh[1] = hy
  lh[2] = hz
  const nh = Math.max(0, hz)
  const vh = Math.max(1e-6, dot3(lo, lh))

  const d = ggxD(a2, nh)
  const g = ggxG1(a2, nv) * ggxG1(a2, nl)
  const f0avg = (f0v[0] + f0v[1] + f0v[2]) / 3
  for (let i = 0; i < 3; i++) {
    fcol[i] = schlick(vh, f0v[i])
  }
  const specScale = d * g / (4 * nv * nl)
  const specPdf = (d * nh) / (4 * vh)

  if (material === MATERIAL.CHROME) {
    for (let i = 0; i < 3; i++) out.f[i] = specScale * fcol[i]
    out.pdf = specPdf
    return out
  }

  // Clear coat over a Lambert base.
  const pCoat = coatProbability(nv, f0avg)
  const fvAvg = schlick(nv, f0avg)
  const flAvg = schlick(nl, f0avg)
  const diffuseScale = (1 - fvAvg) * (1 - flAvg) * INV_PI
  for (let i = 0; i < 3; i++) {
    out.f[i] = specScale * fcol[i] + diffuseScale * albedo[i]
  }
  out.pdf = pCoat * specPdf + (1 - pCoat) * nl * INV_PI
  return out
}

/**
 * Samples an outgoing-to-incoming direction for a non-delta material.
 * `rng` supplies three uniform numbers. Writes `out.wi` (world space),
 * `out.pdf` and `out.weight` = f·cos/pdf. Returns false if the sample is
 * below the surface (the caller should terminate the path).
 */
export function sampleSurface(material, albedo, frame, wo, rng, out) {
  toLocal(frame, wo, lo)
  const nv = lo[2]
  if (nv <= 0) return false
  const roughness = materialParams(material, albedo)
  const f0avg = (f0v[0] + f0v[1] + f0v[2]) / 3
  const pCoat = material === MATERIAL.CHROME ? 1 : material === MATERIAL.MATTE ? 0 : coatProbability(nv, f0avg)

  const u1 = rng.next()
  const u2 = rng.next()
  if (material === MATERIAL.MATTE || rng.next() >= pCoat) {
    // Cosine-weighted hemisphere.
    const r = Math.sqrt(u1)
    const phi = 2 * Math.PI * u2
    sampleLocal[0] = r * Math.cos(phi)
    sampleLocal[1] = r * Math.sin(phi)
    sampleLocal[2] = Math.sqrt(Math.max(0, 1 - u1))
  } else {
    // GGX half vector, then reflect wo about it.
    const alpha = roughness * roughness
    const a2 = alpha * alpha
    const cosT = Math.sqrt((1 - u1) / (1 + (a2 - 1) * u1))
    const sinT = Math.sqrt(Math.max(0, 1 - cosT * cosT))
    const phi = 2 * Math.PI * u2
    const hx = sinT * Math.cos(phi)
    const hy = sinT * Math.sin(phi)
    const hz = cosT
    const voh = lo[0] * hx + lo[1] * hy + lo[2] * hz
    sampleLocal[0] = 2 * voh * hx - lo[0]
    sampleLocal[1] = 2 * voh * hy - lo[1]
    sampleLocal[2] = 2 * voh * hz - lo[2]
  }
  if (sampleLocal[2] <= 0) return false
  toWorld(frame, sampleLocal, out.wi)
  evalSurface(material, albedo, frame, wo, out.wi, evalOut)
  if (!(evalOut.pdf > 0)) return false
  const nl = sampleLocal[2]
  for (let i = 0; i < 3; i++) out.weight[i] = (evalOut.f[i] * nl) / evalOut.pdf
  out.pdf = evalOut.pdf
  return true
}

const evalOut = createEval()

/**
 * Dielectric (transparent plastic) interaction. `dir` is the incoming ray
 * direction, `n` the unit normal facing the incoming ray (`dot(n, dir) < 0`),
 * and `inside` says whether the ray currently travels inside the solid.
 * Writes `out.dir` (unit) and `out.transmit`. Reflection is chosen with the
 * Fresnel probability, so both branches carry unit weight.
 */
export function sampleDielectric(dir, n, inside, rng, out) {
  const eta = inside ? TRANSPARENT_IOR : 1 / TRANSPARENT_IOR
  const cosI = -dot3(dir, n)
  const sin2T = eta * eta * (1 - cosI * cosI)
  const r0 = ((TRANSPARENT_IOR - 1) / (TRANSPARENT_IOR + 1)) ** 2
  const fresnel = sin2T > 1 ? 1 : schlick(cosI, r0)
  if (sin2T > 1 || rng.next() < fresnel) {
    const k = 2 * dot3(dir, n)
    out.dir[0] = dir[0] - k * n[0]
    out.dir[1] = dir[1] - k * n[1]
    out.dir[2] = dir[2] - k * n[2]
    out.transmit = false
    return out
  }
  const cosT = Math.sqrt(1 - sin2T)
  const c = eta * cosI - cosT
  let x = eta * dir[0] + c * n[0]
  let y = eta * dir[1] + c * n[1]
  let z = eta * dir[2] + c * n[2]
  const len = Math.sqrt(x * x + y * y + z * z)
  out.dir[0] = x / len
  out.dir[1] = y / len
  out.dir[2] = z / len
  out.transmit = true
  return out
}

/** Allocates the scratch record that `sampleDielectric` fills. */
export function createDielectricSample() {
  return { dir: new Float64Array(3), transmit: false }
}
