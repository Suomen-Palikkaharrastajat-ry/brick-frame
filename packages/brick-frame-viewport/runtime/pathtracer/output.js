/**
 * Display mapping for the finished render: exposure, ACES filmic tone curve
 * (Narkowicz fit), then linear → sRGB, packed into RGBA8 with alpha kept as
 * coverage. The output is ready for ImageData.
 */

/** Scene-linear exposure multiplier applied before the tone curve. */
export const DEFAULT_EXPOSURE = 0.7

/** ACES filmic approximation, input linear, output display-linear in 0..1. */
export function acesFilmic(x) {
  const v = x < 0 ? 0 : x
  const y = (v * (2.51 * v + 0.03)) / (v * (2.43 * v + 0.59) + 0.14)
  return y < 0 ? 0 : y > 1 ? 1 : y
}

/** Display-linear (0..1) to sRGB-encoded (0..1). */
export function linearToSrgb(x) {
  return x <= 0.0031308 ? 12.92 * x : 1.055 * x ** (1 / 2.4) - 0.055
}

/**
 * Tone-maps `color` (3 floats per pixel, linear) and `alpha` (1 per pixel)
 * into an RGBA8 `Uint8ClampedArray` of `width * height * 4` bytes.
 */
export function toRgba8(color, alpha, width, height, exposure = DEFAULT_EXPOSURE) {
  const out = new Uint8ClampedArray(width * height * 4)
  for (let p = 0; p < width * height; p++) {
    const c = p * 3
    const o = p * 4
    out[o] = Math.round(255 * linearToSrgb(acesFilmic(color[c] * exposure)))
    out[o + 1] = Math.round(255 * linearToSrgb(acesFilmic(color[c + 1] * exposure)))
    out[o + 2] = Math.round(255 * linearToSrgb(acesFilmic(color[c + 2] * exposure)))
    const a = alpha[p]
    out[o + 3] = Math.round(255 * (a < 0 ? 0 : a > 1 ? 1 : a))
  }
  return out
}
