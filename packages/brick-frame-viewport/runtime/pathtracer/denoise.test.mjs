import assert from 'node:assert/strict'
import test from 'node:test'
import { denoise } from './denoise.js'
import { createRng } from './rng.js'

const W = 16
const H = 8

/**
 * Left half red albedo, right half blue albedo, both under the same noisy
 * illumination around 1.0. Returns the noisy colour and the guides.
 */
function noisyTwoToneScene() {
  const rng = createRng(11)
  const albedo = new Float32Array(W * H * 3)
  const normal = new Float32Array(W * H * 3)
  const color = new Float32Array(W * H * 3)
  const alpha = new Float32Array(W * H).fill(1)
  const illumination = new Float32Array(W * H)
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      const p = y * W + x
      const left = x < W / 2
      const a = left ? [0.6, 0.2, 0.2] : [0.2, 0.2, 0.6]
      const e = 1 + 0.3 * (rng.next() * 2 - 1)
      illumination[p] = e
      for (let k = 0; k < 3; k++) {
        albedo[p * 3 + k] = a[k]
        color[p * 3 + k] = a[k] * e
      }
      normal[p * 3 + 2] = 1
    }
  }
  return { color, alpha, albedo, normal, illumination }
}

function stdDev(values) {
  const mean = values.reduce((s, v) => s + v, 0) / values.length
  return Math.sqrt(values.reduce((s, v) => s + (v - mean) ** 2, 0) / values.length)
}

test('denoising smooths the illumination of a flat albedo area and keeps the albedo edge', () => {
  const { color, alpha, albedo, normal, illumination } = noisyTwoToneScene()
  const out = denoise(color, alpha, albedo, normal, W, H)

  // Flat interior of the left (red) half: columns 1..5 keep their neighbours.
  const inputLeft = []
  const outputLeft = []
  for (let y = 0; y < H; y++) {
    for (let x = 1; x < 6; x++) {
      const p = y * W + x
      inputLeft.push(illumination[p])
      outputLeft.push(out.color[p * 3] / albedo[p * 3])
    }
  }
  assert.ok(stdDev(outputLeft) < 0.5 * stdDev(inputLeft), `flat std ${stdDev(outputLeft)} vs ${stdDev(inputLeft)}`)

  // Across the albedo edge (between x = 7 and 8) the red and blue stay distinct.
  const p7 = 3 * W + 7
  const p8 = 3 * W + 8
  assert.ok(out.color[p7 * 3] - out.color[p8 * 3] > 0.25, 'red side keeps more red than the blue side')
  assert.ok(out.color[p8 * 3 + 2] - out.color[p7 * 3 + 2] > 0.25, 'blue side keeps more blue than the red side')
})

test('denoising keeps black and bright areas both filtered', () => {
  const rng = createRng(5)
  const n = W * H
  const albedo = new Float32Array(n * 3).fill(0.02)
  const normal = new Float32Array(n * 3)
  const color = new Float32Array(n * 3)
  const alpha = new Float32Array(n).fill(1)
  const illumination = new Float32Array(n)
  for (let p = 0; p < n; p++) {
    // Left half is dark (about 0.05 after demodulation), right half bright (about 3).
    const scale = p % W < W / 2 ? 0.05 : 3
    const e = scale * (1 + 0.3 * (rng.next() * 2 - 1))
    illumination[p] = e
    for (let k = 0; k < 3; k++) color[p * 3 + k] = e * 0.02
    normal[p * 3 + 2] = 1
  }
  const out = denoise(color, alpha, albedo, normal, W, H)
  const dark = []
  const bright = []
  const darkIn = []
  const brightIn = []
  for (let p = 0; p < n; p++) {
    const x = p % W
    if (x >= 1 && x <= 5) {
      dark.push(out.color[p * 3] / 0.02)
      darkIn.push(illumination[p])
    } else if (x >= 10 && x <= 14) {
      bright.push(out.color[p * 3] / 0.02)
      brightIn.push(illumination[p])
    }
  }
  assert.ok(stdDev(dark) < 0.5 * stdDev(darkIn), 'dark area is smoothed')
  assert.ok(stdDev(bright) < 0.5 * stdDev(brightIn), 'bright area is smoothed')
})
