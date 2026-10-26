/**
 * Scene preparation for the path tracer: camera basis, studio lights, sky and
 * the infinite ground plane, plus per-triangle colour and material arrays.
 *
 * Lighting ("studio product shot"), all in linear RGB, render space Y-up:
 * - Key: a 6° cone (soft-edged studio softbox) above and to the camera's
 *   right, about 40° elevation, so shadows fall visibly beside the model. Irradiance 3.5 times the sky irradiance, so
 *   the key is clearly dominant and casts readable soft shadows.
 * - Fill: a 20° cone from the camera's left, lower and modest. Irradiance
 *   0.45 (about 0.4 times the sky irradiance).
 * - Sky: a dim vertical gradient from a grey horizon to a slightly bluer
 *   zenith. Its cosine-weighted mean radiance (about 0.33) gives the sky
 *   irradiance π·0.33 used by the ground and the shadow catcher.
 * - Softboxes: two rectangular emitters, a soft overhead box front-left and a
 *   narrow tall strip behind. They are lights like the cones (next-event
 *   estimation plus MIS), so they add a little diffuse light and pool on the
 *   ground. Their radiance is high but they subtend little solid angle, so
 *   their irradiance stays near the fill's. Metal reflects them as bright
 *   bands and highlights.
 * - Reflection environment: the sky, with a darker band at the horizon. Only
 *   bounced rays see it. A primary ray reads the plain sky gradient through
 *   `skyRadiance`, so the visible backdrop is clean and lights are never
 *   visible to the camera directly.
 * Ground: a Lambert plane of linear albedo ~0.6 (a slightly warm light grey),
 * placed at min-Y − 0.01 so the model sits on it.
 */
import { buildBVH } from './bvh.js'
import { MATERIAL } from './materials.js'

/** Key light cone half-angle (degrees): a soft-edged studio softbox. */
const KEY_CONE_DEG = 6
/** Fill light cone half-angle (degrees). */
const FILL_CONE_DEG = 20
/** Linear horizon and zenith sky radiance (dim studio backdrop). */
const SKY_HORIZON = [0.4, 0.4, 0.42]
const SKY_ZENITH = [0.27, 0.3, 0.36]
/** Cosine-weighted mean of sqrt(cos θ) over the hemisphere, the sky gradient's weight. */
const SKY_SQRT_MEAN = 0.8
/** Cosine-weighted mean sky radiance per channel, then averaged to one scalar. */
const SKY_MEAN_RADIANCE =
  SKY_HORIZON.reduce((sum, h, i) => sum + h + (SKY_ZENITH[i] - h) * SKY_SQRT_MEAN, 0) / 3
export const SKY_IRRADIANCE = Math.PI * SKY_MEAN_RADIANCE
/** Key-to-sky irradiance ratio (about 3:1 to 4:1 for a clearly dominant key). */
const KEY_TO_SKY_RATIO = 3.5
const KEY_IRRADIANCE = KEY_TO_SKY_RATIO * SKY_IRRADIANCE
/** Modest fill, about 0.4 times the sky irradiance. */
const FILL_IRRADIANCE = 0.45
/** Dark band at the horizon in the reflection environment: depth (0..1) and width in sin(elevation). */
const BAND_DEPTH = 0.9
const BAND_WIDTH = 0.3
/**
 * Softboxes, placed in camera terms: `h` toward the camera (horizontal), `r`
 * right, `u` up. `hint` is the axis of the long edge ('right' or 'up'), and
 * `ha`, `hb` are the half-sizes in tangent units at unit distance. Radiance is
 * `irradiance` over the solid angle, so a small, bright box adds little light.
 */
const SOFTBOXES = [
  { h: 0.9, r: -0.5, u: 1, hint: 'right', ha: 0.5, hb: 0.3, irradiance: 0.35 },
  { h: -1, r: 0, u: 0.36, hint: 'up', ha: 0.4, hb: 0.08, irradiance: 0.25 },
]
/** Linear albedo of the studio ground: a slightly warm light grey. */
export const GROUND_ALBEDO = [0.6, 0.58, 0.55]
/** The ground sits this far (LDU) below the model's lowest point. */
const GROUND_OFFSET = 0.01
const STRIDE = 14

/** sRGB transfer function (0..1) to linear. */
export function srgbToLinear(c) {
  const v = Math.min(1, Math.max(0, c))
  return v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4
}

function normalize3(v) {
  const len = Math.hypot(v[0], v[1], v[2])
  if (!(len > 0)) throw new Error('Degenerate camera vector')
  return [v[0] / len, v[1] / len, v[2] / len]
}

function cross3(a, b) {
  return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
}

function dot3(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

function vec3(value, name) {
  if (!Array.isArray(value) && !ArrayBuffer.isView(value)) throw new Error(`Camera ${name} must be a 3-vector`)
  if (value.length !== 3) throw new Error(`Camera ${name} must be a 3-vector`)
  return [Number(value[0]), Number(value[1]), Number(value[2])]
}

/**
 * Camera basis from the contract camera. `forward` is the unit view
 * direction, `right` and `up` are orthonormal with it. `tanHalfY` is
 * tan(fovY/2), and `aspect` is width/height of the output image.
 */
export function makeCamera(contract, width, height) {
  const origin = vec3(contract.position, 'position')
  const target = vec3(contract.target, 'target')
  const upHint = vec3(contract.up, 'up')
  const forward = normalize3([target[0] - origin[0], target[1] - origin[1], target[2] - origin[2]])
  const right = normalize3(cross3(forward, upHint))
  const up = cross3(right, forward)
  const fov = Number(contract.fovYDegrees)
  if (!(fov > 0 && fov < 180)) throw new Error('Camera fovYDegrees must be in (0, 180)')
  return {
    origin,
    forward,
    right,
    up,
    tanHalfY: Math.tan((fov * Math.PI) / 360),
    aspect: width / height,
  }
}

/**
 * A directional light with a uniform cone of `coneDeg` half-angle around `dir`.
 * `irradiance` is the white irradiance on a surface facing the light.
 */
function makeLight(dir, coneDeg, irradiance) {
  const d = normalize3(dir)
  const helper = Math.abs(d[1]) < 0.9 ? [0, 1, 0] : [1, 0, 0]
  const t = normalize3(cross3(helper, d))
  const b = cross3(d, t)
  const cosMax = Math.cos((coneDeg * Math.PI) / 180)
  const solidAngle = 2 * Math.PI * (1 - cosMax)
  return { kind: 'cone', dir: d, t, b, cosMax, solidAngle, irradiance }
}

/** Solid angle of a rectangle with half-sizes ha, hb seen from unit distance at its centre. */
function rectSolidAngle(ha, hb) {
  return 4 * Math.asin((ha * hb) / Math.sqrt((1 + ha * ha) * (1 + hb * hb)))
}

/**
 * A rectangular softbox facing `dir`, its long axis along `hint`. Points on
 * it are `dir + x·t + y·b` (at unit distance) with |x| ≤ ha and |y| ≤ hb.
 */
function makeRect(dir, hint, ha, hb, irradiance) {
  const c = normalize3(dir)
  const along = dot3(hint, c)
  const t = normalize3([hint[0] - along * c[0], hint[1] - along * c[1], hint[2] - along * c[2]])
  const b = cross3(c, t)
  return {
    kind: 'rect',
    dir: c,
    t,
    b,
    ha,
    hb,
    area: 4 * ha * hb,
    solidAngle: rectSolidAngle(ha, hb),
    irradiance,
  }
}

/**
 * Samples a direction toward `light` from uniform numbers (u1, u2). Writes
 * the unit direction to `out` and returns its density per steradian.
 * Rectangles sample the plane at unit distance uniformly, so the density
 * is |p|³ / area with |p| = √(1 + x² + y²).
 */
export function sampleLight(light, u1, u2, out) {
  if (light.kind === 'rect') {
    const x = (2 * u1 - 1) * light.ha
    const y = (2 * u2 - 1) * light.hb
    const len = Math.sqrt(1 + x * x + y * y)
    for (let i = 0; i < 3; i++) {
      out[i] = (light.dir[i] + light.t[i] * x + light.b[i] * y) / len
    }
    return (len * len * len) / light.area
  }
  const cosT = 1 - u1 * (1 - light.cosMax)
  const sinT = Math.sqrt(Math.max(0, 1 - cosT * cosT))
  const phi = 2 * Math.PI * u2
  const cx = sinT * Math.cos(phi)
  const cy = sinT * Math.sin(phi)
  for (let i = 0; i < 3; i++) {
    out[i] = light.t[i] * cx + light.b[i] * cy + light.dir[i] * cosT
  }
  return 1 / light.solidAngle
}

/**
 * Density per steradian with which `sampleLight` picks the unit direction
 * (x, y, z), or 0 when that direction is outside the light.
 */
export function lightPdf(light, x, y, z) {
  const d = light.dir
  const dc = x * d[0] + y * d[1] + z * d[2]
  if (light.kind === 'rect') {
    if (!(dc > 0)) return 0
    const t = light.t
    const b = light.b
    const u = (x * t[0] + y * t[1] + z * t[2]) / dc
    const v = (x * b[0] + y * b[1] + z * b[2]) / dc
    if (Math.abs(u) > light.ha || Math.abs(v) > light.hb) return 0
    const len = Math.sqrt(1 + u * u + v * v)
    return (len * len * len) / light.area
  }
  return dc >= light.cosMax ? 1 / light.solidAngle : 0
}

/**
 * Key and fill lights placed relative to the camera, so the model is lit
 * from the same side the user sees it.
 */
export function studioLights(camera) {
  const toCam = [-camera.forward[0], -camera.forward[1], -camera.forward[2]]
  const upDot = dot3(toCam, camera.up)
  const toCamH = normalize3([
    toCam[0] - camera.up[0] * upDot,
    toCam[1] - camera.up[1] * upDot,
    toCam[2] - camera.up[2] * upDot,
  ])
  const { right, up } = camera
  const key = makeLight(
    [0.6 * toCamH[0] + 0.9 * right[0] + 1.0 * up[0], 0.6 * toCamH[1] + 0.9 * right[1] + 1.0 * up[1], 0.6 * toCamH[2] + 0.9 * right[2] + 1.0 * up[2]],
    KEY_CONE_DEG,
    KEY_IRRADIANCE,
  )
  const fill = makeLight(
    [0.6 * toCamH[0] - 0.9 * right[0] + 0.6 * up[0], 0.6 * toCamH[1] - 0.9 * right[1] + 0.6 * up[1], 0.6 * toCamH[2] - 0.9 * right[2] + 0.6 * up[2]],
    FILL_CONE_DEG,
    FILL_IRRADIANCE,
  )
  const softboxes = SOFTBOXES.map((s) => {
    const dir = [0, 1, 2].map((i) => s.h * toCamH[i] + s.r * right[i] + s.u * up[i])
    return makeRect(dir, s.hint === 'up' ? up : right, s.ha, s.hb, s.irradiance)
  })
  return [key, fill, ...softboxes]
}

/**
 * Sky radiance for direction height `dirY` (rays going below the horizon
 * always meet the ground plane first). Writes linear RGB into `out`.
 */
export function skyRadiance(dirY, out) {
  const s = Math.sqrt(Math.min(1, Math.max(0, dirY)))
  for (let i = 0; i < 3; i++) {
    out[i] = SKY_HORIZON[i] + (SKY_ZENITH[i] - SKY_HORIZON[i]) * s
  }
  return out
}

/**
 * Radiance that rays which have already bounced see for direction height
 * `dirY`: the sky gradient with a darker band at the horizon. Primary rays
 * never use it, so the visible backdrop stays `skyRadiance`.
 */
export function environmentRadiance(dirY, out) {
  skyRadiance(dirY, out)
  const k = 1 - BAND_DEPTH * Math.exp(-((dirY / BAND_WIDTH) ** 2))
  out[0] *= k
  out[1] *= k
  out[2] *= k
  return out
}

/** Scalar irradiance of the reflection environment on an upward surface (midpoint rule over elevation). */
function environmentIrradiance() {
  const steps = 512
  const rgb = [0, 0, 0]
  let sum = 0
  for (let i = 0; i < steps; i++) {
    const theta = ((i + 0.5) / steps) * (Math.PI / 2)
    const cosT = Math.cos(theta)
    environmentRadiance(cosT, rgb)
    sum += ((rgb[0] + rgb[1] + rgb[2]) / 3) * cosT * Math.sin(theta)
  }
  return 2 * Math.PI * sum * ((Math.PI / 2) / steps)
}

/** Irradiance of the reflection environment, used by the shadow catcher's sky sample. */
export const ENV_IRRADIANCE = environmentIrradiance()

/**
 * Turns a contract request into everything the integrator reads. Throws if
 * there are no triangles. Triangle colours are converted to linear here. Per-triangle data is indexed by BVH slot, so a
 * hit's `slot` addresses `albedo` (3 floats) and `material` directly.
 */
export function prepareScene(request) {
  const triangles = request.triangles
  if (!triangles || triangles.length < STRIDE) throw new Error('No model triangles to render')
  if (triangles.length % STRIDE !== 0) throw new Error('Triangle data is not 14 numbers per triangle')
  const width = Math.round(request.width)
  const height = Math.round(request.height)
  if (!(width > 0 && height > 0)) throw new Error('Render size must be positive')

  const bvh = buildBVH(triangles)
  const albedo = new Float32Array(bvh.count * 3)
  const material = new Uint8Array(bvh.count)
  let minY = Infinity
  for (let i = 0; i < triangles.length; i += STRIDE) {
    minY = Math.min(minY, triangles[i + 1], triangles[i + 4], triangles[i + 7])
  }
  for (let slot = 0; slot < bvh.count; slot++) {
    const base = bvh.prims[slot] * STRIDE
    // Contract colours are sRGB; shading works in linear RGB.
    albedo[slot * 3] = srgbToLinear(triangles[base + 9])
    albedo[slot * 3 + 1] = srgbToLinear(triangles[base + 10])
    albedo[slot * 3 + 2] = srgbToLinear(triangles[base + 11])
    const code = Math.round(triangles[base + 13])
    material[slot] = Number.isFinite(code) && code >= 0 && code <= MATERIAL.PEARL ? code : MATERIAL.PLASTIC
  }

  const camera = makeCamera(request.camera, width, height)
  return {
    width,
    height,
    transparent: Boolean(request.transparent),
    camera,
    lights: studioLights(camera),
    bvh,
    albedo,
    material,
    groundY: minY - GROUND_OFFSET,
    skyIrradiance: ENV_IRRADIANCE,
    groundAlbedo: GROUND_ALBEDO,
  }
}
