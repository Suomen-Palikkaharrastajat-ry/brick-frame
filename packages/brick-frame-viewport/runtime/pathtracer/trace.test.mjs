import assert from 'node:assert/strict'
import test from 'node:test'
import { GROUND_ALBEDO, prepareScene } from './scene.js'
import { createRay, captureGuide, primaryRay, renderRowSample, tracePath } from './trace.js'
import { createRng } from './rng.js'
import { createHit, intersect } from './bvh.js'
import { denoise } from './denoise.js'
import { toRgba8 } from './output.js'

const norm = (v) => {
  const l = Math.hypot(v[0], v[1], v[2])
  return [v[0] / l, v[1] / l, v[2] / l]
}
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2]

/** Two triangles forming a 120 × 120 LDU white square at height y. */
function squareAt(y) {
  const h = 60
  const a = [-h, y, -h]
  const b = [h, y, -h]
  const c = [h, y, h]
  const d = [-h, y, h]
  return [...a, ...b, ...c, 1, 1, 1, 1, 0, ...a, ...c, ...d, 1, 1, 1, 1, 0]
}

function sceneFor(width, height, { triangles, camera, transparent = false }) {
  return prepareScene({
    width,
    height,
    transparent,
    camera: camera ?? { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 },
    triangles: Float32Array.from(triangles ?? squareAt(40)),
  })
}

test('the centre pixel ray points at the camera target', () => {
  const scene = sceneFor(32, 18, {})
  // Sub-pixel offset of 1 lands exactly on the image centre (16, 9).
  const ray = primaryRay(scene, 15, 8, 1, 1, createRay())
  const want = norm([0, 30 - 80, -300 + 0].map((v, i) => [0, 30, 0][i] - [0, 80, 300][i]))
  const got = norm([ray.dx, ray.dy, ray.dz])
  assert.ok(Math.abs(dot(got, want) - 1) < 1e-9, `centre dir ${got} vs ${want}`)
  assert.equal(ray.ox, 0)
  assert.equal(ray.oy, 80)
  assert.equal(ray.oz, 300)
})

test('a 1920x1080 render is not stretched: horizontal tan is the aspect times the vertical tan', () => {
  const scene = sceneFor(1920, 1080, {})
  const cam = scene.camera
  const left = primaryRay(scene, 0, 540, 0, 0.5, createRay())
  const top = primaryRay(scene, 960, 0, 0.5, 0, createRay())
  const fwd = cam.forward
  const tanH = dot([left.dx, left.dy, left.dz], cam.right) / dot([left.dx, left.dy, left.dz], fwd)
  const tanV = dot([top.dx, top.dy, top.dz], cam.up) / dot([top.dx, top.dy, top.dz], fwd)
  assert.ok(Math.abs(Math.abs(tanV) - cam.tanHalfY) < 1e-3, `vertical tan ${tanV}`)
  assert.ok(Math.abs(Math.abs(tanH) / Math.abs(tanV) - 1920 / 1080) < 1e-3, `ratio ${tanH / tanV}`)
  assert.ok(Math.abs(tanH) > Math.abs(tanV), 'horizontal FOV should be the wider one')
})

test('prepareScene places the ground just below the lowest vertex', () => {
  const scene = sceneFor(8, 8, { triangles: [...squareAt(40).slice(0, 14)], })
  assert.ok(Math.abs(scene.groundY - (40 - 0.01)) < 1e-6)
})

test('colours are converted from sRGB to linear for shading', () => {
  const tri = [0, 0, 0, 1, 0, 0, 0, 0, 1, 0.5, 0.5, 0.5, 1, 0]
  const scene = sceneFor(8, 8, { triangles: tri })
  assert.ok(Math.abs(scene.albedo[0] - 0.214) < 0.01)
})

test('an end-to-end tiny render is finite, lit, and shadowed under the square', () => {
  const width = 32
  const height = 18
  const scene = sceneFor(width, height, {})
  const rng = createRng(7)
  const sums = new Float32Array(width * height * 4)
  const ray = createRay()
  const passes = 16
  for (let pass = 0; pass < passes; pass++) {
    for (let y = 0; y < height; y++) {
      renderRowSample(scene, rng, y, sums.subarray(y * width * 4, (y + 1) * width * 4))
    }
  }
  const albedo = new Float32Array(width * height * 3)
  const normal = new Float32Array(width * height * 3)
  for (let y = 0; y < height; y++) {
    captureGuide(scene, y, albedo.subarray(y * width * 3, (y + 1) * width * 3), normal.subarray(y * width * 3, (y + 1) * width * 3))
  }
  const color = new Float32Array(width * height * 3)
  const alpha = new Float32Array(width * height)
  for (let p = 0; p < width * height; p++) {
    const a = sums[p * 4 + 3] / passes
    alpha[p] = a
    for (let k = 0; k < 3; k++) color[p * 3 + k] = sums[p * 4 + k] / passes / Math.max(a, 1e-9)
  }
  for (const v of color) assert.ok(Number.isFinite(v))
  for (const v of alpha) assert.ok(Number.isFinite(v) && v >= 0 && v <= 1)

  // Ground pixels are those whose first hit is the grey ground albedo, facing up.
  let lo = Infinity
  let hi = 0
  let groundPixels = 0
  for (let p = 0; p < width * height; p++) {
    const isGround = Math.abs(albedo[p * 3] - GROUND_ALBEDO[0]) < 1e-6 && normal[p * 3 + 1] > 0.99
    if (!isGround) continue
    groundPixels++
    const lum = 0.2126 * color[p * 3] + 0.7152 * color[p * 3 + 1] + 0.0722 * color[p * 3 + 2]
    lo = Math.min(lo, lum)
    hi = Math.max(hi, lum)
  }
  assert.ok(groundPixels > 50, `need ground pixels, got ${groundPixels}`)
  assert.ok(hi > 0.05, 'lit ground should be non-black')
  assert.ok(lo < 0.9 * hi, `shadowed ground (${lo}) should be darker than lit ground (${hi})`)

  const denoised = denoise(color, alpha, albedo, normal, width, height, 2)
  const rgba = toRgba8(denoised.color, denoised.alpha, width, height)
  assert.equal(rgba.length, width * height * 4)
})

test('transparent renders: sky is alpha 0, and the lit scene keeps alpha 1 on the model', () => {
  const width = 32
  const height = 18
  const scene = sceneFor(width, height, { transparent: true })
  const rng = createRng(3)
  const out = new Float64Array(4)
  const ray = createRay()
  // Top-left pixel looks at the sky above the ground plane.
  primaryRay(scene, 0, 0, 0.5, 0.5, ray)
  tracePath(scene, rng, ray, out)
  assert.equal(out[3], 0)
  assert.equal(out[0], 0)
  // Find a pixel whose primary ray hits the square's interior.
  let target = null
  for (let y = 0; y < height && !target; y++) {
    for (let x = 0; x < width && !target; x++) {
      primaryRay(scene, x, y, 0.5, 0.5, ray)
      const hit = intersect(scene.bvh, ray, createHit())
      if (hit && hit.t < 1e6 && scene.material[hit.slot] === 0) target = [x, y]
    }
  }
  assert.ok(target, 'some pixel should see the square')
  primaryRay(scene, target[0], target[1], 0.5, 0.5, ray)
  let covered = 0
  for (let i = 0; i < 8; i++) {
    tracePath(scene, rng, ray, out)
    covered += out[3]
  }
  assert.ok(covered / 8 > 0.99, `model pixel alpha ${covered / 8}`)
})

test('an opaque render paints the sky in the top rows', () => {
  const scene = sceneFor(32, 18, {})
  const out = new Float64Array(4)
  const ray = createRay()
  primaryRay(scene, 0, 0, 0.5, 0.5, ray)
  tracePath(scene, createRng(5), ray, out)
  assert.equal(out[3], 1)
  assert.ok(out[0] > 0.2 && out[2] > 0.2, `sky should be visible, got ${out}`)
})

/**
 * Irradiance a light gives an upward-facing surface. Cones are taken at their
 * axis; a rectangle is integrated over its solid angle on a midpoint grid
 * (dω = dx·dy / |p|³ on the plane at unit distance).
 */
function upIrradiance(light) {
  if (light.kind !== 'rect') return light.irradiance * Math.max(0, light.dir[1])
  const n = 200
  const dx = (2 * light.ha) / n
  const dy = (2 * light.hb) / n
  let sum = 0
  for (let i = 0; i < n; i++) {
    const x = -light.ha + (i + 0.5) * dx
    for (let j = 0; j < n; j++) {
      const y = -light.hb + (j + 0.5) * dy
      const len = Math.sqrt(1 + x * x + y * y)
      const cosUp = (light.dir[1] + light.t[1] * x + light.b[1] * y) / len
      if (cosUp > 0) sum += (cosUp / (len * len * len)) * dx * dy
    }
  }
  return (light.irradiance / light.solidAngle) * sum
}

test('direct and sky light on the ground match the analytic Lambert estimate (MIS is unbiased)', () => {
  // A distant dummy triangle makes the scene ground-only.
  const scene = prepareScene({
    width: 8,
    height: 8,
    transparent: false,
    camera: { position: [170, 150, 190], target: [10, 20, -10], up: [0, 1, 0], fovYDegrees: 45 },
    triangles: [1e5, 0, 1e5, 1e5 + 1, 0, 1e5, 1e5, 0, 1e5 + 1, 1, 1, 1, 1, 0],
  })
  const rng = createRng(3)
  const out = new Float64Array(4)
  const ray = createRay()
  primaryRay(scene, 4, 7, 0.5, 0.5, ray)
  const n = 20000
  let sum = 0
  for (let i = 0; i < n; i++) {
    tracePath(scene, rng, ray, out)
    sum += (out[0] + out[1] + out[2]) / 3
  }
  // Lambert: albedo/pi · E·cos per light, plus albedo times the mean sky radiance (sky irradiance / pi).
  const direct = scene.lights.reduce((acc, l) => acc + upIrradiance(l) / Math.PI, 0)
  const albedoMean = (GROUND_ALBEDO[0] + GROUND_ALBEDO[1] + GROUND_ALBEDO[2]) / 3
  const expected = albedoMean * (direct + scene.skyIrradiance / Math.PI)
  const measured = sum / n
  assert.ok(Math.abs(measured - expected) / expected < 0.03, `ground ${measured} vs analytic ${expected}`)
})
