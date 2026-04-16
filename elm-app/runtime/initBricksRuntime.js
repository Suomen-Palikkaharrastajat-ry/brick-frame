import { unzipSync, inflateSync } from 'fflate'
import { Elm } from '../src/Main.elm'
import GeometryWorker from '../geometry-worker.js?worker'
import geometryWorkerSource from '../geometry-worker.js?raw'

function pathFromBase(relativePath) {
  const rawBase = String(import.meta.env.BASE_URL ?? '/')
  const base = rawBase.endsWith('/') ? rawBase : `${rawBase}/`
  const relative = String(relativePath ?? '').replace(/^\/+/, '')
  return `${base}${relative}`
}

const defaultLdrawBase = pathFromBase('ldraw')
const defaultFallbackBase = ''
const defaultMaxRpm = 50
const defaultModelUrl = pathFromBase('examples/wheeler.ldr')
const defaultWorkerMode = 'auto'
const defaultWorkerUrl = ''
const allowedExtensions = new Set(['.ldr', '.mpd', '.dat', '.io'])
const allowedWorkerModes = new Set(['auto', 'inline', 'external', 'off'])
const GEOMETRY_FLATTEN_CACHE_LIMIT = 8
const RESIZE_POLL_INTERVAL_MS = 250

function normalizeMode(rawMode) {
  return String(rawMode ?? '').toLowerCase() === 'viewer' ? 'viewer' : 'simulator'
}

function normalizeWorkerMode(rawMode) {
  const normalized = String(rawMode ?? '').trim().toLowerCase()
  return allowedWorkerModes.has(normalized) ? normalized : defaultWorkerMode
}

function normalizeWorkerUrl(rawUrl) {
  const normalized = String(rawUrl ?? '').trim()
  return normalized || defaultWorkerUrl
}

function hasAllowedExtension(filename) {
  const lower = String(filename ?? '').toLowerCase()
  return [...allowedExtensions].some((ext) => lower.endsWith(ext))
}

function inferFileExtension(nameOrUrl) {
  const normalized = String(nameOrUrl ?? '').toLowerCase().split('?')[0].split('#')[0]
  return [...allowedExtensions].find((ext) => normalized.endsWith(ext)) ?? null
}

function decodeLdrawBytes(bytes) {
  const decoded = new TextDecoder('utf-8').decode(bytes)
  return decoded.charCodeAt(0) === 0xfeff ? decoded.slice(1) : decoded
}

// ---------------------------------------------------------------------------
// ZipCrypto decryption (traditional PKZIP encryption, password "soho0909")
// BrickLink Studio 2 uses this to encrypt .io archives.
// ---------------------------------------------------------------------------

const _CRC32_TABLE = (() => {
  const t = new Uint32Array(256)
  for (let i = 0; i < 256; i++) {
    let c = i
    for (let j = 0; j < 8; j++) c = (c & 1) ? (0xedb88320 ^ (c >>> 1)) : (c >>> 1)
    t[i] = c >>> 0
  }
  return t
})()

function _crc32Byte(crc, b) {
  return ((_CRC32_TABLE[(crc ^ b) & 0xff] ^ (crc >>> 8)) >>> 0)
}

function _zipCryptoInitKeys(password) {
  let k0 = 0x12345678
  let k1 = 0x23456789
  let k2 = 0x34567890
  for (let i = 0; i < password.length; i++) {
    const b = password.charCodeAt(i) & 0xff
    k0 = _crc32Byte(k0, b)
    k1 = (Math.imul((k1 + (k0 & 0xff)) >>> 0, 134775813) + 1) >>> 0
    k2 = _crc32Byte(k2, (k1 >>> 24) & 0xff)
  }
  return [k0, k1, k2]
}

function _zipCryptoKeystream(keys) {
  const tmp = ((keys[2] & 0xffff) | 2) >>> 0
  return ((tmp * (tmp ^ 1)) >>> 8) & 0xff
}

function _zipCryptoUpdateKeys(keys, plainByte) {
  keys[0] = _crc32Byte(keys[0], plainByte)
  keys[1] = (Math.imul((keys[1] + (keys[0] & 0xff)) >>> 0, 134775813) + 1) >>> 0
  keys[2] = _crc32Byte(keys[2], (keys[1] >>> 24) & 0xff)
}

// Decrypt `src` (Uint8Array) in-place using password, returning plaintext.
// The first 12 bytes are the ZipCrypto encryption header and are discarded.
function _zipCryptoDecrypt(src, password) {
  const keys = _zipCryptoInitKeys(password)
  const result = new Uint8Array(src.length)
  for (let i = 0; i < src.length; i++) {
    const ks = _zipCryptoKeystream(keys)
    const plain = (src[i] ^ ks) & 0xff
    result[i] = plain
    _zipCryptoUpdateKeys(keys, plain)
  }
  // Discard the 12-byte encryption header; remainder is compressed data.
  return result.subarray(12)
}

// ---------------------------------------------------------------------------
// Minimal ZIP central-directory parser (handles encrypted entries).
// ---------------------------------------------------------------------------

function _readUint16LE(u8, offset) {
  return (u8[offset] | (u8[offset + 1] << 8)) >>> 0
}

function _readUint32LE(u8, offset) {
  return (u8[offset] | (u8[offset + 1] << 8) | (u8[offset + 2] << 16) | (u8[offset + 3] << 24)) >>> 0
}

// Locate the End-of-Central-Directory record by scanning backward.
function _findEocd(u8) {
  // EOCD is at least 22 bytes from the end; signature is 0x06054b50.
  for (let i = u8.length - 22; i >= 0; i--) {
    if (u8[i] === 0x50 && u8[i + 1] === 0x4b && u8[i + 2] === 0x05 && u8[i + 3] === 0x06) {
      return i
    }
  }
  return -1
}

// Parse central directory entries, return array of entry descriptors.
function _parseCentralDir(u8) {
  const eocdOffset = _findEocd(u8)
  if (eocdOffset < 0) throw new Error('ZIP central directory not found')

  const cdOffset = _readUint32LE(u8, eocdOffset + 16)
  const cdSize = _readUint32LE(u8, eocdOffset + 12)
  const entries = []
  let pos = cdOffset

  while (pos < cdOffset + cdSize) {
    if (u8[pos] !== 0x50 || u8[pos + 1] !== 0x4b || u8[pos + 2] !== 0x01 || u8[pos + 3] !== 0x02) break
    const flags = _readUint16LE(u8, pos + 8)
    const compression = _readUint16LE(u8, pos + 10)
    const compressedSize = _readUint32LE(u8, pos + 20)
    const localHeaderOffset = _readUint32LE(u8, pos + 42)
    const nameLen = _readUint16LE(u8, pos + 28)
    const extraLen = _readUint16LE(u8, pos + 30)
    const commentLen = _readUint16LE(u8, pos + 32)
    const name = new TextDecoder('utf-8', { fatal: false }).decode(u8.subarray(pos + 46, pos + 46 + nameLen))
    entries.push({ name, flags, compression, compressedSize, localHeaderOffset })
    pos += 46 + nameLen + extraLen + commentLen
  }

  return entries
}

// Extract a single entry (decrypt + inflate if needed).
function _extractEntry(u8, entry, password) {
  const lh = entry.localHeaderOffset
  if (u8[lh] !== 0x50 || u8[lh + 1] !== 0x4b || u8[lh + 2] !== 0x03 || u8[lh + 3] !== 0x04) {
    throw new Error(`Bad local file header for "${entry.name}"`)
  }
  const nameLen = _readUint16LE(u8, lh + 26)
  const extraLen = _readUint16LE(u8, lh + 28)
  const dataOffset = lh + 30 + nameLen + extraLen
  const encrypted = !!(entry.flags & 0x1)

  let data = u8.subarray(dataOffset, dataOffset + entry.compressedSize)

  if (encrypted) {
    if (!password) throw new Error(`Entry "${entry.name}" is encrypted but no password given`)
    data = _zipCryptoDecrypt(data, password)
  }

  if (entry.compression === 0) return data          // stored (no compression)
  if (entry.compression === 8) return inflateSync(data) // deflate
  throw new Error(`Unsupported compression method ${entry.compression} in "${entry.name}"`)
}

// Try to extract a preferred .ldr file from a potentially encrypted archive.
function _extractLdrWithPassword(uint8Array, password) {
  const entries = _parseCentralDir(uint8Array)
  if (entries.length === 0) throw new Error('Archive is empty')

  const normalize = (n) => String(n).replaceAll('\\', '/').toLowerCase()
  // Prefer modelv2.ldr / model.ldr which use standard LDraw color codes.
  // model2.ldr uses Studio-internal color indices that don't match LDraw IDs.
  const preferred = ['modelv2.ldr', 'model.ldr', 'model2.ldr']

  for (const target of preferred) {
    const entry = entries.find((e) => normalize(e.name).endsWith(target))
    if (entry) return decodeLdrawBytes(_extractEntry(uint8Array, entry, password))
  }

  const anyLdr = entries.find((e) => normalize(e.name).endsWith('.ldr'))
  if (anyLdr) return decodeLdrawBytes(_extractEntry(uint8Array, anyLdr, password))

  throw new Error('No .ldr model found in archive')
}

// ---------------------------------------------------------------------------

function extractLdrawFromStudioArchive(arrayBuffer) {
  const uint8Array = new Uint8Array(arrayBuffer)

  // Fast path: unencrypted archive — use fflate for decompression.
  try {
    const files = unzipSync(uint8Array)
    const entries = Object.entries(files)
    if (entries.length === 0) throw new Error('Archive is empty')

    const normalizedEntries = entries.map(([name, bytes]) => ({
      name,
      normalized: String(name).replaceAll('\\', '/').toLowerCase(),
      bytes,
    }))

    // Prefer modelv2.ldr / model.ldr which use standard LDraw color codes.
    // model2.ldr uses Studio-internal color indices that don't match LDraw IDs.
    const preferred = ['modelv2.ldr', 'model.ldr', 'model2.ldr']

    for (const target of preferred) {
      const found = normalizedEntries.find((entry) => entry.normalized.endsWith(target))
      if (found) return decodeLdrawBytes(found.bytes)
    }

    const anyLdr = normalizedEntries.find((entry) => entry.normalized.endsWith('.ldr'))
    if (anyLdr) return decodeLdrawBytes(anyLdr.bytes)

    throw new Error('No .ldr model found in archive')
  } catch (_) {
    // Encrypted archives (BrickLink Studio uses ZipCrypto with a fixed password).
    return _extractLdrWithPassword(uint8Array, 'soho0909')
  }
}

function createFileReader({ reportLoadError, pushFileText }) {
  function readStudioIoFile(file) {
    const reader = new FileReader()
    reader.onload = (event) => {
      try {
        const arrayBuffer = event.target?.result
        if (!(arrayBuffer instanceof ArrayBuffer)) {
          throw new Error('Invalid .io file payload')
        }
        const text = extractLdrawFromStudioArchive(arrayBuffer)
        pushFileText(text)
      } catch (error) {
        const details = error instanceof Error ? error.message : String(error)
        reportLoadError(`Failed to read "${file.name}" as BrickLink Studio .io: ${details}`)
      }
    }
    reader.onerror = () => {
      reportLoadError(`Failed to read "${file.name}".`)
    }
    reader.readAsArrayBuffer(file)
  }

  function readLdrawFile(file) {
    if (!file) return

    if (!hasAllowedExtension(file.name)) {
      reportLoadError(
        `Unsupported file type "${file.name}". Please use .ldr, .mpd, .dat, or .io.`,
      )
      return
    }

    if (String(file.name).toLowerCase().endsWith('.io')) {
      readStudioIoFile(file)
      return
    }

    const reader = new FileReader()
    reader.onload = (event) => {
      pushFileText(String(event.target?.result ?? ''))
    }
    reader.onerror = () => {
      reportLoadError(`Failed to read "${file.name}".`)
    }
    reader.readAsText(file)
  }

  async function loadFromUrl(url) {
    const target = String(url ?? '').trim()
    if (!target) {
      reportLoadError('Load URL cannot be empty.')
      return false
    }

    try {
      const extension = inferFileExtension(target)
      const response = await fetch(target)
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`)
      }

      if (extension === '.io') {
        const buffer = await response.arrayBuffer()
        pushFileText(extractLdrawFromStudioArchive(buffer))
      } else {
        pushFileText(await response.text())
      }
      return true
    } catch (error) {
      const details = error instanceof Error ? error.message : String(error)
      reportLoadError(`Failed to load "${target}". ${details}`)
      return false
    }
  }

  return { readLdrawFile, loadFromUrl }
}

function identityTransform() {
  return {
    o: [0, 0, 0],
    x: [1, 0, 0],
    y: [0, 1, 0],
    z: [0, 0, 1],
  }
}

function add(a, b) {
  return [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
}

function sub(a, b) {
  return [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

function scale(v, s) {
  return [v[0] * s, v[1] * s, v[2] * s]
}

function cross(a, b) {
  return [
    a[1] * b[2] - a[2] * b[1],
    a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0],
  ]
}

function dot(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

function length(v) {
  return Math.sqrt(dot(v, v))
}

function normalizeSafe(v) {
  const len = length(v)
  if (len < 1e-8) return [0, 1, 0]
  return [v[0] / len, v[1] / len, v[2] / len]
}

function applyTransform(t, p) {
  return add(
    t.o,
    add(
      add(scale(t.x, p[0]), scale(t.y, p[1])),
      scale(t.z, p[2]),
    ),
  )
}

function applyDirection(t, v) {
  return add(
    add(scale(t.x, v[0]), scale(t.y, v[1])),
    scale(t.z, v[2]),
  )
}

function composeTransforms(parent, local) {
  return {
    o: applyTransform(parent, local.o),
    x: applyDirection(parent, local.x),
    y: applyDirection(parent, local.y),
    z: applyDirection(parent, local.z),
  }
}

function toYUp(p) {
  return [p[0], -p[1], p[2]]
}

function faceNormal(p1, p2, p3) {
  return normalizeSafe(cross(sub(p2, p1), sub(p3, p1)))
}

function resolveColor(parentColor, lineColor, colorTable) {
  if (lineColor === 24 || lineColor === -2) {
    return [0, 0, 0, 1]
  }
  const code = (lineColor === 16 || lineColor === -1) ? parentColor : lineColor
  const mapped = colorTable[String(code)]
  if (!mapped) return [1, 0, 1, 1]
  return [mapped.r, mapped.g, mapped.b, mapped.alpha]
}

function hasBfcCertify(lines) {
  return lines.some((line) => line.k === 'comment' && line.text.includes('BFC CERTIFY CCW'))
}

function flattenLines(lines, cache, parentColor, transform, colorTable, acc) {
  for (const line of lines) {
    switch (line.k) {
      case 'subfile': {
        const loaded = cache[line.file]
        if (!loaded) break
        const childColor = (line.color === 16 || line.color === -1) ? parentColor : line.color
        const childTransform = composeTransforms(transform, line.transform)
        flattenLines(loaded, cache, childColor, childTransform, colorTable, acc)
        break
      }
      case 'tri': {
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        const p3 = toYUp(applyTransform(transform, line.p3))
        const normal = faceNormal(p1, p2, p3)
        const color = resolveColor(parentColor, line.color, colorTable)
        acc.triangles.push([
          { position: p1, normal, color },
          { position: p2, normal, color },
          { position: p3, normal, color },
        ])
        break
      }
      case 'quad': {
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        const p3 = toYUp(applyTransform(transform, line.p3))
        const p4 = toYUp(applyTransform(transform, line.p4))
        const color = resolveColor(parentColor, line.color, colorTable)
        const n1 = faceNormal(p1, p2, p3)
        const n2 = faceNormal(p1, p3, p4)
        acc.triangles.push([
          { position: p1, normal: n1, color },
          { position: p2, normal: n1, color },
          { position: p3, normal: n1, color },
        ])
        acc.triangles.push([
          { position: p1, normal: n2, color },
          { position: p3, normal: n2, color },
          { position: p4, normal: n2, color },
        ])
        break
      }
      case 'line': {
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        acc.lines.push([p1, p2])
        break
      }
      case 'cond': {
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        const c1 = toYUp(applyTransform(transform, line.c1))
        const c2 = toYUp(applyTransform(transform, line.c2))
        acc.conditionalLines.push({ p1, p2, c1, c2 })
        break
      }
      default:
        break
    }
  }
}

function flattenGeometryPayload(payloadText) {
  const payload = JSON.parse(String(payloadText ?? '{}'))
  const lines = payload.lines ?? []
  const cache = payload.cache ?? {}
  const parentColor = payload.parentColor ?? 15
  const colorTable = payload.colorTable ?? {}

  const acc = { triangles: [], lines: [], conditionalLines: [] }
  // windingFlipped=true: same as the geometry worker — compensates for toYUp's Y-negation
  // which is a det=-1 reflection that inverts every triangle's winding order.
  flattenLines(lines, cache, parentColor, identityTransform(), colorTable, acc, true)

  return {
    ok: true,
    bfcCertified: hasBfcCertify(lines),
    triangles: acc.triangles,
    lines: acc.lines,
    conditionalLines: acc.conditionalLines,
  }
}

function flattenGeometryPayloadSafe(payloadText) {
  try {
    return flattenGeometryPayload(payloadText)
  } catch (error) {
    return {
      ok: false,
      error: error instanceof Error ? error.message : String(error),
    }
  }
}

function createInlineGeometryWorker() {
  if (typeof Worker === 'undefined') {
    throw new Error('Web Worker is not available in this browser')
  }
  if (typeof Blob === 'undefined') {
    throw new Error('Blob is not available in this browser')
  }
  if (typeof URL === 'undefined' || typeof URL.createObjectURL !== 'function') {
    throw new Error('URL.createObjectURL is not available in this browser')
  }

  const workerBlob = new Blob([geometryWorkerSource], { type: 'text/javascript' })
  const objectUrl = URL.createObjectURL(workerBlob)
  const worker = new Worker(objectUrl)
  return { worker, objectUrl }
}

function createExternalGeometryWorker(workerUrl) {
  if (workerUrl) {
    if (typeof Worker === 'undefined') {
      throw new Error('Web Worker is not available in this browser')
    }
    return { worker: new Worker(workerUrl), objectUrl: null }
  }

  return { worker: new GeometryWorker(), objectUrl: null }
}

export function initBricksRuntime(options) {
  const {
    node,
    mode = 'simulator',
    ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? defaultLdrawBase,
    ldrawFallbackBase = import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaultFallbackBase,
    maxRpm = import.meta.env.VITE_MAX_RPM ?? defaultMaxRpm,
    defaultModel = defaultModelUrl,
    initialHash = typeof window !== 'undefined' ? window.location.hash ?? '' : '',
    syncUrlHash = true,
    useWindowResize = true,
    controlsEnabled = false,
    initialMotorIndex = -1,
    initialRpm = 0,
    workerMode = defaultWorkerMode,
    workerUrl = defaultWorkerUrl,
    ambientStrength = undefined,
    lightStrength = undefined,
    vibrance = undefined,
    edgeWidth = undefined,
    runtimeEventHandler = null,
    dragDropTarget = typeof window !== 'undefined' ? window : null,
    suppressGestureTarget = typeof window !== 'undefined' ? window : null,
    documentRef = typeof document !== 'undefined' ? document : null,
  } = options ?? {}

  if (!node) {
    throw new Error('initBricksRuntime requires a mount node')
  }

  const maxRpmRaw = Number(maxRpm)
  const sanitizedMaxRpm = Number.isFinite(maxRpmRaw) && maxRpmRaw >= 1 ? maxRpmRaw : defaultMaxRpm
  const parsedMotorIndex = Number(initialMotorIndex)
  const sanitizedMotorIndex = Number.isFinite(parsedMotorIndex) ? Math.max(-1, Math.trunc(parsedMotorIndex)) : -1
  const parsedInitialRpm = Number(initialRpm)
  const sanitizedInitialRpm = Number.isFinite(parsedInitialRpm) ? parsedInitialRpm : 0
  const parsedAmbientStrength = Number(ambientStrength)
  const sanitizedAmbientStrength = Number.isFinite(parsedAmbientStrength) ? parsedAmbientStrength : null
  const parsedLightStrength = Number(lightStrength)
  const sanitizedLightStrength = Number.isFinite(parsedLightStrength) ? parsedLightStrength : null
  const parsedVibrance = Number(vibrance)
  const sanitizedVibrance = Number.isFinite(parsedVibrance) ? parsedVibrance : null
  const parsedEdgeWidth = Number(edgeWidth)
  const sanitizedEdgeWidth = Number.isFinite(parsedEdgeWidth) ? parsedEdgeWidth : null

  // .io files are ZIP archives (binary) — Elm's Http.expectString would corrupt them.
  // Pass an empty defaultModelUrl so Elm starts idle; we load via the JS binary path below.
  const defaultModelIsIo = inferFileExtension(String(defaultModel ?? '')) === '.io'

  const app = Elm.Main.init({
    node,
    flags: {
      ldrawBase,
      ldrawFallbackBase,
      defaultModelUrl: defaultModelIsIo ? '' : String(defaultModel ?? ''),
      initialHash,
      maxRpm: sanitizedMaxRpm,
      uiMode: normalizeMode(mode),
      controlsEnabled: Boolean(controlsEnabled),
      initialMotorIndex: sanitizedMotorIndex,
      initialRpm: sanitizedInitialRpm,
      useWindowResize: Boolean(useWindowResize),
      ambientStrength: sanitizedAmbientStrength,
      lightStrength: sanitizedLightStrength,
      vibrance: sanitizedVibrance,
      edgeWidth: sanitizedEdgeWidth,
    },
  })

  const cleanups = []
  const geometryFlattenCache = new Map()
  const requestedWorkerMode = normalizeWorkerMode(workerMode)
  const configuredWorkerUrl = normalizeWorkerUrl(workerUrl)
  let activeWorkerMode = 'off'
  let geometryWorker = null
  let geometryWorkerObjectUrl = null
  let pendingFlattenKey = null
  let pendingFlattenPayload = null
  let lastViewportWidth = null
  let lastViewportHeight = null

  function rememberFlattenResult(cacheKey, resultText) {
    geometryFlattenCache.set(cacheKey, resultText)
    if (geometryFlattenCache.size > GEOMETRY_FLATTEN_CACHE_LIMIT) {
      const oldestKey = geometryFlattenCache.keys().next().value
      if (oldestKey !== undefined) {
        geometryFlattenCache.delete(oldestKey)
      }
    }
  }

  function addEventListener(target, eventName, handler, eventOptions) {
    if (!target?.addEventListener) {
      return
    }
    target.addEventListener(eventName, handler, eventOptions)
    cleanups.push(() => {
      target.removeEventListener(eventName, handler, eventOptions)
    })
  }

  function emitViewportSize(widthRaw, heightRaw) {
    if (!app.ports.viewportResized) {
      return
    }
    const width = Math.max(1, Math.round(Number(widthRaw) || 0))
    const height = Math.max(1, Math.round(Number(heightRaw) || 0))
    if (width === lastViewportWidth && height === lastViewportHeight) {
      return
    }
    lastViewportWidth = width
    lastViewportHeight = height
    app.ports.viewportResized.send({ width, height })
  }

  function measureAndEmitViewportSize() {
    const rect = node.getBoundingClientRect?.()
    if (rect) {
      emitViewportSize(rect.width, rect.height)
    }
  }

  function sendRuntimeEvent(eventType, detail = {}) {
    if (typeof runtimeEventHandler === 'function') {
      runtimeEventHandler({ type: eventType, ...detail })
    }
  }

  function sendRuntimeWarning(code, message, detail = {}) {
    sendRuntimeEvent('runtime-warning', {
      code,
      message,
      workerMode: activeWorkerMode,
      ...detail,
    })
  }

  function pushFileText(text) {
    app.ports.fileContentReceived.send(String(text ?? ''))
  }

  function reportLoadError(message) {
    const normalized = String(message ?? 'Unknown load error')
    app.ports.fileLoadError.send(normalized)
    sendRuntimeEvent('model-error', { message: normalized })
  }

  function terminateGeometryWorker() {
    if (geometryWorker) {
      geometryWorker.terminate()
      geometryWorker = null
    }

    if (geometryWorkerObjectUrl) {
      URL.revokeObjectURL(geometryWorkerObjectUrl)
      geometryWorkerObjectUrl = null
    }
  }

  function handleGeometryResult(cacheKey, resultText) {
    try {
      const parsed = JSON.parse(resultText)
      if (parsed.ok) {
        if (typeof cacheKey === 'string') {
          rememberFlattenResult(cacheKey, resultText)
        }
        app.ports.geometryFlattened.send(resultText)
      } else {
        app.ports.geometryFlattenFailed.send(parsed.error ?? 'Geometry worker failed')
      }
    } catch {
      app.ports.geometryFlattenFailed.send('Invalid geometry worker response')
    }
  }

  function runGeometryFlattenOnMainThread(payload, cacheKey) {
    const resultText = JSON.stringify(flattenGeometryPayloadSafe(payload))
    handleGeometryResult(cacheKey, resultText)
  }

  function fallbackWorkerToOff(reason, error, rerunPayload, rerunCacheKey) {
    terminateGeometryWorker()
    const previousMode = activeWorkerMode
    activeWorkerMode = 'off'
    pendingFlattenKey = null
    pendingFlattenPayload = null

    sendRuntimeWarning(
      'geometry-worker-fallback',
      `Geometry worker disabled (${reason}); using main-thread geometry flattening.`,
      {
        requestedWorkerMode,
        previousWorkerMode: previousMode,
        error: error instanceof Error ? error.message : String(error ?? ''),
      },
    )

    if (typeof rerunPayload === 'string') {
      runGeometryFlattenOnMainThread(rerunPayload, rerunCacheKey)
    }
  }

  function attachGeometryWorkerListeners(worker) {
    addEventListener(worker, 'message', (event) => {
      const cacheKey = pendingFlattenKey
      pendingFlattenKey = null
      pendingFlattenPayload = null
      handleGeometryResult(cacheKey, String(event.data ?? ''))
    })

    addEventListener(worker, 'error', (event) => {
      fallbackWorkerToOff(
        'worker runtime error',
        event?.error ?? event?.message ?? event,
        pendingFlattenPayload,
        pendingFlattenKey,
      )
    })
  }

  function tryActivateWorker(candidateMode) {
    try {
      if (candidateMode === 'inline') {
        const inline = createInlineGeometryWorker()
        geometryWorker = inline.worker
        geometryWorkerObjectUrl = inline.objectUrl
        activeWorkerMode = 'inline'
        attachGeometryWorkerListeners(geometryWorker)
        return true
      }

      if (candidateMode === 'external') {
        const external = createExternalGeometryWorker(configuredWorkerUrl)
        geometryWorker = external.worker
        geometryWorkerObjectUrl = external.objectUrl
        activeWorkerMode = 'external'
        attachGeometryWorkerListeners(geometryWorker)
        return true
      }
    } catch (error) {
      terminateGeometryWorker()
      sendRuntimeWarning(
        'geometry-worker-init-failed',
        `Failed to initialize ${candidateMode} geometry worker.`,
        {
          requestedWorkerMode,
          attemptedWorkerMode: candidateMode,
          error: error instanceof Error ? error.message : String(error),
        },
      )
      return false
    }

    return false
  }

  if (requestedWorkerMode === 'off') {
    activeWorkerMode = 'off'
  } else if (requestedWorkerMode === 'inline') {
    if (!tryActivateWorker('inline')) {
      activeWorkerMode = 'off'
    }
  } else if (requestedWorkerMode === 'external') {
    if (!tryActivateWorker('external')) {
      activeWorkerMode = 'off'
    }
  } else if (!tryActivateWorker('inline')) {
    activeWorkerMode = 'off'
  }

  const { readLdrawFile, loadFromUrl } = createFileReader({ reportLoadError, pushFileText })

  // If the default model is a .io archive, load it now via the binary path
  // (Elm's Http.expectString cannot handle binary ZIP data).
  if (defaultModelIsIo && defaultModel) {
    loadFromUrl(String(defaultModel))
  }

  function loadFromText(text) {
    pushFileText(String(text ?? ''))
  }

  async function loadFromFile(file) {
    readLdrawFile(file)
  }

  app.ports.requestGeometryFlatten.subscribe((payload) => {
    const payloadText = String(payload ?? '')
    const cacheHit = geometryFlattenCache.get(payloadText)
    if (cacheHit) {
      app.ports.geometryFlattened.send(cacheHit)
      return
    }

    if (!geometryWorker || activeWorkerMode === 'off') {
      runGeometryFlattenOnMainThread(payloadText, payloadText)
      return
    }

    pendingFlattenKey = payloadText
    pendingFlattenPayload = payloadText

    try {
      geometryWorker.postMessage(payloadText)
    } catch (error) {
      fallbackWorkerToOff('postMessage failed', error, payloadText, payloadText)
    }
  })

  if (syncUrlHash && typeof window !== 'undefined') {
    app.ports.setUrlHash.subscribe((hash) => {
      const normalized = String(hash ?? '')
      const withPrefix = normalized.startsWith('#') ? normalized : `#${normalized}`
      if (window.location.hash !== withPrefix) {
        window.history.replaceState(null, '', withPrefix)
      }
    })
  }

  if (app.ports.runtimeEvent) {
    app.ports.runtimeEvent.subscribe((payload) => {
      try {
        const parsed = typeof payload === 'string' ? JSON.parse(payload) : payload
        if (parsed && typeof parsed === 'object') {
          sendRuntimeEvent(String(parsed.type ?? 'runtime-event'), parsed)
        }
      } catch {
        sendRuntimeEvent('runtime-event', { payload })
      }
    })
  }

  const ownerDocument = documentRef ?? node.ownerDocument
  if (ownerDocument) {
    const fileInput = ownerDocument.createElement('input')
    fileInput.type = 'file'
    fileInput.accept = '.ldr,.mpd,.dat,.io'
    fileInput.style.display = 'none'
    node.appendChild(fileInput)

    addEventListener(fileInput, 'change', () => {
      const file = fileInput.files?.[0]
      readLdrawFile(file)
    })

    cleanups.push(() => {
      fileInput.remove()
    })

    app.ports.requestFileUpload.subscribe(() => {
      fileInput.value = ''
      fileInput.click()
    })
  }

  function suppressDefaultDragBehavior(event) {
    event.preventDefault()
    event.stopPropagation()
  }

  if (dragDropTarget) {
    addEventListener(dragDropTarget, 'dragenter', suppressDefaultDragBehavior)
    addEventListener(dragDropTarget, 'dragover', suppressDefaultDragBehavior)
    addEventListener(dragDropTarget, 'drop', (event) => {
      suppressDefaultDragBehavior(event)
      const droppedFile = event.dataTransfer?.files?.[0]
      if (!droppedFile) {
        reportLoadError('Drop a .ldr, .mpd, .dat, or .io file to load a model.')
        return
      }
      readLdrawFile(droppedFile)
    })
  }

  function suppressGestureZoom(event) {
    event.preventDefault()
  }

  if (suppressGestureTarget) {
    addEventListener(suppressGestureTarget, 'gesturestart', suppressGestureZoom, { passive: false })
    addEventListener(suppressGestureTarget, 'gesturechange', suppressGestureZoom, { passive: false })
    addEventListener(suppressGestureTarget, 'gestureend', suppressGestureZoom, { passive: false })
  }

  const ownerWindow = ownerDocument?.defaultView ?? (typeof window !== 'undefined' ? window : null)
  let hasResizeObserver = false

  if (typeof ResizeObserver !== 'undefined') {
    const resizeObserver = new ResizeObserver((entries) => {
      const entry = entries.find((candidate) => candidate.target === node) ?? entries[0]
      if (!entry) {
        return
      }
      emitViewportSize(entry.contentRect.width, entry.contentRect.height)
    })
    resizeObserver.observe(node)
    cleanups.push(() => resizeObserver.disconnect())
    hasResizeObserver = true
  }

  if (ownerWindow && !hasResizeObserver) {
    addEventListener(ownerWindow, 'resize', measureAndEmitViewportSize)
    addEventListener(ownerWindow, 'orientationchange', measureAndEmitViewportSize)
  }

  if (!hasResizeObserver && ownerWindow?.setInterval && ownerWindow?.clearInterval) {
    const resizePollId = ownerWindow.setInterval(measureAndEmitViewportSize, RESIZE_POLL_INTERVAL_MS)
    cleanups.push(() => {
      ownerWindow.clearInterval(resizePollId)
    })
  }

  measureAndEmitViewportSize()

  function destroy() {
    terminateGeometryWorker()
    while (cleanups.length > 0) {
      const cleanup = cleanups.pop()
      cleanup()
    }
  }

  return {
    app,
    destroy,
    loadFromText,
    loadFromFile,
    loadFromUrl,
  }
}

export function runtimeDefaults() {
  return {
    defaultLdrawBase,
    defaultFallbackBase,
    defaultModelUrl,
    defaultMaxRpm,
  }
}
