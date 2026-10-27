import { deflateSync, inflateSync } from 'fflate'

/**
 * Base64url encode a Uint8Array (no padding, `-` and `_`).
 *
 * @param {Uint8Array} bytes
 * @returns {string}
 */
export function base64UrlEncode(bytes) {
  let binary = ''
  const len = bytes.byteLength
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i])
  }
  return btoa(binary)
    .replace(/\+/g, '-')
    .replace(/\//g, '_')
    .replace(/=+$/, '')
}

/**
 * Base64url decode a string to Uint8Array.
 *
 * @param {string} str
 * @returns {Uint8Array}
 */
export function base64UrlDecode(str) {
  let base64 = str.replace(/-/g, '+').replace(/_/g, '/')
  while (base64.length % 4 !== 0) {
    base64 += '='
  }
  const binary = atob(base64)
  const bytes = new Uint8Array(binary.length)
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i)
  }
  return bytes
}

/**
 * Convert an ArrayBuffer, DataView, or Uint8Array to a Uint8Array.
 *
 * @param {ArrayBuffer | DataView | Uint8Array | { buffer: ArrayBuffer, byteOffset?: number, byteLength?: number }} data
 * @returns {Uint8Array}
 */
export function toUint8Array(data) {
  if (data instanceof Uint8Array) {
    return data
  }
  if (Array.isArray(data)) {
    return new Uint8Array(data)
  }
  if (data instanceof ArrayBuffer) {
    return new Uint8Array(data)
  }
  if (data && data.buffer instanceof ArrayBuffer) {
    return new Uint8Array(data.buffer, data.byteOffset ?? 0, data.byteLength ?? data.buffer.byteLength)
  }
  return new Uint8Array(0)
}

/**
 * Check whether a hash string represents a high-density binary hash (`#b/...` or `b/...`).
 *
 * @param {unknown} hash
 * @returns {boolean}
 */
export function isBinaryHash(hash) {
  if (typeof hash !== 'string') return false
  const trimmed = hash.trim()
  return trimmed.startsWith('#b/') || trimmed.startsWith('b/')
}

/**
 * Encode wire-format binary data into a `#b/<base64url>` hash fragment.
 * Uses raw deflate (level 9) via fflate and base64url encoding.
 *
 * @param {ArrayBuffer | DataView | Uint8Array} data
 * @returns {string}
 */
export function encodeHash(data) {
  const bytes = toUint8Array(data)
  const compressed = deflateSync(bytes, { level: 9 })
  return `#b/${base64UrlEncode(compressed)}`
}

/**
 * Decode a `#b/<base64url>` hash fragment into wire-format binary data.
 * Returns null if the hash is invalid or decompression fails.
 *
 * @param {string} hash
 * @returns {Uint8Array | null}
 */
export function decodeHash(hash) {
  if (!hash || typeof hash !== 'string') return null
  let str = hash.trim()
  if (str.startsWith('#')) str = str.slice(1)
  if (!str.startsWith('b/')) return null
  const b64 = str.slice(2)
  try {
    const compressed = base64UrlDecode(b64)
    return inflateSync(compressed)
  } catch (_) {
    return null
  }
}
