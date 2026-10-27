import assert from 'node:assert/strict'
import test from 'node:test'
import {
  base64UrlEncode,
  base64UrlDecode,
  encodeHash,
  decodeHash,
  isBinaryHash,
  toUint8Array,
} from './url-hash-codec.js'

test('base64url round trip preserves binary content', () => {
  const bytes = new Uint8Array([0, 1, 2, 250, 255, 127, 128, 63])
  const encoded = base64UrlEncode(bytes)
  assert.equal(encoded.includes('+'), false)
  assert.equal(encoded.includes('/'), false)
  assert.equal(encoded.includes('='), false)
  const decoded = base64UrlDecode(encoded)
  assert.deepEqual(Array.from(decoded), Array.from(bytes))
})

test('isBinaryHash recognizes binary hashes', () => {
  assert.equal(isBinaryHash('#b/abc123_-'), true)
  assert.equal(isBinaryHash('b/abc123_-'), true)
  assert.equal(isBinaryHash('#parts=3001.dat:14:1'), false)
  assert.equal(isBinaryHash(''), false)
  assert.equal(isBinaryHash(null), false)
})

test('encodeHash and decodeHash round trip', () => {
  const payload = new Uint8Array([0x42, 0x46, 0x01, 0x00, 0x05, 0x10, 0x20, 0x30])
  const hash = encodeHash(payload)
  assert.equal(hash.startsWith('#b/'), true)
  const restored = decodeHash(hash)
  assert.notEqual(restored, null)
  assert.deepEqual(Array.from(restored), Array.from(payload))
})

test('decodeHash returns null on corrupted or non-binary input', () => {
  assert.equal(decodeHash(''), null)
  assert.equal(decodeHash('#parts=something'), null)
  assert.equal(decodeHash('#b/invalid-corrupted-deflate-data!@#'), null)
})
