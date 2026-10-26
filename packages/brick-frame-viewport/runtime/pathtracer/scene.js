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
  return { dir: d, t, b, cosMax, solidAngle, irradiance }
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
  return [key, fill]
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
    skyIrradiance: SKY_IRRADIANCE,
    groundAlbedo: GROUND_ALBEDO,
  }
}
