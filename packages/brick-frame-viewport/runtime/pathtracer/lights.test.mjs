import assert from 'node:assert/strict'
import test from 'node:test'
import { prepareScene, skyRadiance, lightPdf, sampleLight } from './scene.js'
import { createRay, primaryRay, tracePath } from './trace.js'
import { createRng } from './rng.js'

/** A far-away dummy triangle, so the scene holds only the ground and sky. */
const dummy = [1e5, 0, 1e5, 1e5 + 1, 0, 1e5, 1e5, 0, 1e5 + 1, 1, 1, 1, 1, 0]
const CAMERA = { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }

const norm = (v) => {
  const l = Math.hypot(v[0], v[1], v[2])
  return [v[0] / l, v[1] / l, v[2] / l]
}
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]

/**
 * A white chrome square of half-size 200 through `centre`, with unit normal
 * `n`. Two triangles, 14 numbers each (corners, rgba, material 2 = chrome).
 */
function mirrorThrough(centre, n) {
  const helper = Math.abs(n[1]) < 0.9 ? [0, 1, 0] : [1, 0, 0]
  const t = norm(cross(helper, n))
  const b = cross(n, t)
  const h = 200
  const p = (sx, sz) => [0, 1, 2].map((i) => centre[i] + sx * h * t[i] + sz * h * b[i])
  const [a, bb, c, d] = [p(-1, -1), p(1, -1), p(1, 1), p(-1, 1)]
  return [...a, ...bb, ...c, 1, 1, 1, 1, 2, ...a, ...c, ...d, 1, 1, 1, 1, 2]
}

/** Mean linear RGB seen by the centre pixel of an 8×8 render with `camera`. */
function centreRadiance(triangles, camera, samples = 4000) {
  const scene = prepareScene({ width: 8, height: 8, transparent: false, camera, triangles: Float32Array.from(triangles) })
  const rng = createRng(11)
  const ray = primaryRay(scene, 4, 4, 0, 0, createRay())
  const out = new Float64Array(4)
  const sum = [0, 0, 0]
  for (let i = 0; i < samples; i++) {
    tracePath(scene, rng, ray, out)
    sum[0] += out[0]
    sum[1] += out[1]
    sum[2] += out[2]
  }
  return sum.map((v) => v / samples)
}

function softboxesOf(scene) {
  return scene.lights.filter((light) => light.kind === 'rect')
}

test('a softbox is a rectangle: sampled densities agree with lightPdf and integrate to its solid angle', () => {
  const scene = prepareScene({ width: 8, height: 8, transparent: false, camera: CAMERA, triangles: Float32Array.from(dummy) })
  const rng = createRng(5)
  const dir = new Float64Array(3)
  for (const light of softboxesOf(scene)) {
    let invPdfSum = 0
    const n = 20000
    for (let i = 0; i < n; i++) {
      const pdf = sampleLight(light, rng.next(), rng.next(), dir)
      const again = lightPdf(light, dir[0], dir[1], dir[2])
      assert.ok(Math.abs(again - pdf) / pdf < 1e-9, `pdf ${pdf} vs lightPdf ${again}`)
      invPdfSum += 1 / pdf
    }
    // E[1 / pdf] over the light's own samples is the measure of its support: the solid angle.
    const estimate = invPdfSum / n
    assert.ok(Math.abs(estimate - light.solidAngle) / light.solidAngle < 0.01, `solid angle ${estimate} vs ${light.solidAngle}`)
    // The opposite direction is off the rectangle.
    assert.equal(lightPdf(light, -light.dir[0], -light.dir[1], -light.dir[2]), 0)
  }
})

test('primary rays see the plain backdrop, even when aimed straight at a softbox', () => {
  // Lights depend on the camera, so take the softbox directions from a camera looking along -z.
  const probe = prepareScene({
    width: 8,
    height: 8,
    transparent: false,
    camera: { position: [0, 0, 0], target: [0, 0, -1], up: [0, 1, 0], fovYDegrees: 45 },
    triangles: Float32Array.from(dummy),
  })
  // The camera sits at the origin, so the centre ray escapes (nothing is above it).
  for (const light of softboxesOf(probe)) {
    const c = light.dir
    const mean = centreRadiance(dummy, { position: [0, 0, 0], target: [c[0] * 10, c[1] * 10, c[2] * 10], up: [0, 1, 0], fovYDegrees: 45 }, 64)
    const sky = skyRadiance(c[1], [0, 0, 0])
    for (let i = 0; i < 3; i++) {
      assert.ok(Math.abs(mean[i] - sky[i]) < 1e-9, `channel ${i}: ${mean[i]} vs backdrop ${sky[i]}`)
    }
  }
})

test('the default camera top row shows the plain backdrop gradient', () => {
  const scene = prepareScene({
    width: 32,
    height: 18,
    transparent: false,
    camera: CAMERA,
    // A white square high above the model, out of the top rows' way.
    triangles: Float32Array.from(mirrorThrough([0, 40, 0], [0, 1, 0])),
  })
  const out = new Float64Array(4)
  const rng = createRng(2)
  const ray = createRay()
  let checked = 0
  for (let x = 0; x < scene.width; x++) {
    primaryRay(scene, x, 0, 0.5, 0.5, ray)
    if (ray.dy <= 0) continue
    tracePath(scene, rng, ray, out)
    const sky = skyRadiance(ray.dy, [0, 0, 0])
    for (let i = 0; i < 3; i++) assert.ok(Math.abs(out[i] - sky[i]) < 1e-9, `pixel ${x} channel ${i}`)
    checked++
  }
  assert.ok(checked > 0, 'the top row should face the sky')
})

test('chrome reflecting a softbox is much brighter than chrome reflecting the dark horizon band', () => {
  const scene = prepareScene({ width: 8, height: 8, transparent: false, camera: CAMERA, triangles: Float32Array.from(dummy) })
  const [overhead] = softboxesOf(scene)
  // The centre ray of the default camera hits the target; a mirror through it reflects that ray into `to`.
  const camForward = norm([CAMERA.target[0] - CAMERA.position[0], CAMERA.target[1] - CAMERA.position[1], CAMERA.target[2] - CAMERA.position[2]])
  const reflectInto = (to) => {
    const r = norm(to)
    const n = norm([r[0] - camForward[0], r[1] - camForward[1], r[2] - camForward[2]])
    return centreRadiance(mirrorThrough(CAMERA.target, n), CAMERA, 4000)
  }
  const toward = reflectInto(overhead.dir)
  // A low horizontal direction, away from the softboxes, where the environment is dark.
  const bandDir = norm([-overhead.dir[0], 0.05, -overhead.dir[2]])
  const dark = reflectInto(bandDir)
  const bright = (toward[0] + toward[1] + toward[2]) / 3
  const low = (dark[0] + dark[1] + dark[2]) / 3
  const expected = overhead.irradiance / overhead.solidAngle
  assert.ok(bright > 0.5 * expected, `softbox reflection ${bright} vs its radiance ${expected}`)
  assert.ok(bright > 10 * low, `softbox ${bright} vs horizon band ${low}`)
})
