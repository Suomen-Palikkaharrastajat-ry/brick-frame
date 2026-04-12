import './main.css'
import { inflateSync, unzipSync } from 'fflate'
import { Elm } from './src/Main.elm'
import GeometryWorker from './geometry-worker.js?worker'

function pathFromBase(relativePath) {
  const rawBase = String(import.meta.env.BASE_URL ?? '/')
  const base = rawBase.endsWith('/') ? rawBase : `${rawBase}/`
  const relative = String(relativePath ?? '').replace(/^\/+/, '')
  return `${base}${relative}`
}

const defaultLdrawBase = pathFromBase('ldraw')
const defaultFallbackBase = ''
const defaultMaxRpm = 50

const ldrawBase = import.meta.env.VITE_LDRAW_BASE ?? defaultLdrawBase
const ldrawFallbackBase =
  import.meta.env.VITE_LDRAW_FALLBACK_BASE ?? defaultFallbackBase
const maxRpmRaw = Number(import.meta.env.VITE_MAX_RPM ?? defaultMaxRpm)
const maxRpm = Number.isFinite(maxRpmRaw) && maxRpmRaw >= 1 ? maxRpmRaw : defaultMaxRpm

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    ldrawBase,
    ldrawFallbackBase,
    initialHash: window.location.hash ?? '',
    maxRpm,
  },
})

const geometryWorker = new GeometryWorker()
const geometryFlattenCache = new Map()
const GEOMETRY_FLATTEN_CACHE_LIMIT = 8
const STUDIO_IO_PASSWORD = 'soho0909'
let pendingFlattenKey = null

function rememberFlattenResult(cacheKey, resultText) {
  geometryFlattenCache.set(cacheKey, resultText)
  if (geometryFlattenCache.size > GEOMETRY_FLATTEN_CACHE_LIMIT) {
    const oldestKey = geometryFlattenCache.keys().next().value
    if (oldestKey !== undefined) {
      geometryFlattenCache.delete(oldestKey)
    }
  }
}

geometryWorker.addEventListener('message', (event) => {
  const text = String(event.data ?? '')
  try {
    const parsed = JSON.parse(text)
    if (parsed.ok) {
      if (typeof pendingFlattenKey === 'string') {
        rememberFlattenResult(pendingFlattenKey, text)
      }
      pendingFlattenKey = null
      app.ports.geometryFlattened.send(text)
    } else {
      pendingFlattenKey = null
      app.ports.geometryFlattenFailed.send(
        parsed.error ?? 'Geometry worker failed',
      )
    }
  } catch {
    pendingFlattenKey = null
    app.ports.geometryFlattenFailed.send('Invalid geometry worker response')
  }
})

app.ports.requestGeometryFlatten.subscribe((payload) => {
  const cacheHit = geometryFlattenCache.get(payload)
  if (cacheHit) {
    app.ports.geometryFlattened.send(cacheHit)
    return
  }

  pendingFlattenKey = payload
  geometryWorker.postMessage(payload)
})

app.ports.setUrlHash.subscribe((hash) => {
  const normalized = String(hash ?? '')
  const withPrefix = normalized.startsWith('#') ? normalized : `#${normalized}`
  if (window.location.hash !== withPrefix) {
    window.history.replaceState(null, '', withPrefix)
  }
})

// ── File upload port ──────────────────────────────────────────────────────────

// Hidden file input — created once, reused for every upload request.
const fileInput = document.createElement('input')
fileInput.type = 'file'
fileInput.accept = '.ldr,.mpd,.dat,.io'
fileInput.style.display = 'none'
document.body.appendChild(fileInput)

const allowedExtensions = new Set(['.ldr', '.mpd', '.dat', '.io'])

function hasAllowedExtension(filename) {
  const lower = filename.toLowerCase()
  return [...allowedExtensions].some((ext) => lower.endsWith(ext))
}

function reportLoadError(message) {
  app.ports.fileLoadError.send(message)
}

function readLdrawFile(file) {
  if (!file) return

  if (!hasAllowedExtension(file.name)) {
    reportLoadError(
      `Unsupported file type "${file.name}". Please use .ldr, .mpd, .dat, or .io.`,
    )
    return
  }

  const lowerName = file.name.toLowerCase()
  if (lowerName.endsWith('.io')) {
    readStudioIoFile(file)
    return
  }

  const reader = new FileReader()
  reader.onload = (e) => {
    app.ports.fileContentReceived.send(e.target.result)
  }
  reader.onerror = () => {
    reportLoadError(`Failed to read "${file.name}".`)
  }
  reader.readAsText(file)
}

function readStudioIoFile(file) {
  const reader = new FileReader()
  reader.onload = (e) => {
    try {
      const arrayBuffer = e.target?.result
      if (!(arrayBuffer instanceof ArrayBuffer)) {
        throw new Error('Invalid .io file payload')
      }
      const text = extractLdrawFromStudioArchive(arrayBuffer)
      app.ports.fileContentReceived.send(text)
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

function extractLdrawFromStudioArchive(arrayBuffer) {
  const zipBytes = new Uint8Array(arrayBuffer)
  let entries
  try {
    entries = Object.entries(unzipSync(zipBytes))
  } catch {
    entries = Object.entries(unzipWithPassword(zipBytes, STUDIO_IO_PASSWORD))
  }
  return extractPreferredLdraw(entries)
}

function decodeLdrawBytes(bytes) {
  const decoded = new TextDecoder('utf-8').decode(bytes)
  return decoded.charCodeAt(0) === 0xfeff ? decoded.slice(1) : decoded
}

function extractPreferredLdraw(entries) {
  if (entries.length === 0) {
    throw new Error('Archive is empty')
  }

  const normalizedEntries = entries.map(([name, bytes]) => ({
    name,
    normalized: String(name).replaceAll('\\', '/').toLowerCase(),
    bytes,
  }))

  const preferred = [
    'modelv2.ldr',
    'model.ldr',
    'model2.ldr',
  ]

  for (const target of preferred) {
    const found = normalizedEntries.find((entry) => entry.normalized.endsWith(target))
    if (found) {
      return decodeLdrawBytes(found.bytes)
    }
  }

  const anyLdr = normalizedEntries.find((entry) => entry.normalized.endsWith('.ldr'))
  if (anyLdr) {
    return decodeLdrawBytes(anyLdr.bytes)
  }

  throw new Error('No .ldr model found in archive')
}

function unzipWithPassword(data, password) {
  const eocdOffset = findEocdOffset(data)
  const entryCount = readU16LE(data, eocdOffset + 10)
  const centralDirectoryOffset = readU32LE(data, eocdOffset + 16)
  const files = {}

  let cursor = centralDirectoryOffset
  const nameDecoder = new TextDecoder('utf-8')

  for (let i = 0; i < entryCount; i += 1) {
    if (readU32LE(data, cursor) !== 0x02014b50) {
      throw new Error('Invalid central directory entry')
    }

    const generalPurpose = readU16LE(data, cursor + 8)
    const compressionMethod = readU16LE(data, cursor + 10)
    const compressedSize = readU32LE(data, cursor + 20)
    const fileNameLength = readU16LE(data, cursor + 28)
    const extraLength = readU16LE(data, cursor + 30)
    const commentLength = readU16LE(data, cursor + 32)
    const localHeaderOffset = readU32LE(data, cursor + 42)

    const nameStart = cursor + 46
    const fileName = nameDecoder.decode(data.subarray(nameStart, nameStart + fileNameLength))

    const localNameLength = readU16LE(data, localHeaderOffset + 26)
    const localExtraLength = readU16LE(data, localHeaderOffset + 28)
    const fileDataStart = localHeaderOffset + 30 + localNameLength + localExtraLength
    const fileDataEnd = fileDataStart + compressedSize
    if (fileDataEnd > data.length) {
      throw new Error('Invalid ZIP entry bounds')
    }

    let compressedData = data.subarray(fileDataStart, fileDataEnd)
    if ((generalPurpose & 0x1) !== 0) {
      const decrypted = decryptZipCrypto(compressedData, password)
      if (decrypted.length < 12) {
        throw new Error('Encrypted ZIP header too short')
      }
      compressedData = decrypted.subarray(12)
    }

    let fileBytes
    if (compressionMethod === 0) {
      fileBytes = new Uint8Array(compressedData)
    } else if (compressionMethod === 8) {
      fileBytes = inflateSync(compressedData)
    } else {
      throw new Error(`Unsupported ZIP compression method: ${compressionMethod}`)
    }

    files[fileName] = fileBytes
    cursor += 46 + fileNameLength + extraLength + commentLength
  }

  return files
}

function findEocdOffset(data) {
  for (let i = data.length - 22; i >= 0; i -= 1) {
    if (readU32LE(data, i) === 0x06054b50) {
      return i
    }
  }
  throw new Error('Invalid ZIP: EOCD not found')
}

const ZIP_CRYPTO_CRC_TABLE = (() => {
  const table = new Uint32Array(256)
  for (let i = 0; i < 256; i += 1) {
    let c = i
    for (let k = 0; k < 8; k += 1) {
      c = (c & 1) !== 0 ? (0xedb88320 ^ (c >>> 1)) : (c >>> 1)
    }
    table[i] = c >>> 0
  }
  return table
})()

function zipCryptoCrc32Update(crc, byte) {
  return (ZIP_CRYPTO_CRC_TABLE[(crc ^ byte) & 0xff] ^ (crc >>> 8)) >>> 0
}

function zipCryptoUpdateKeys(keys, byteValue) {
  keys[0] = zipCryptoCrc32Update(keys[0], byteValue)
  keys[1] = (Math.imul((keys[1] + (keys[0] & 0xff)) >>> 0, 134775813) + 1) >>> 0
  keys[2] = zipCryptoCrc32Update(keys[2], keys[1] >>> 24)
}

function zipCryptoDecryptByte(keys) {
  const temp = ((keys[2] & 0xffff) | 2) >>> 0
  return (Math.imul(temp, temp ^ 1) >>> 8) & 0xff
}

function decryptZipCrypto(data, password) {
  const keys = new Uint32Array([0x12345678, 0x23456789, 0x34567890])
  for (let i = 0; i < password.length; i += 1) {
    zipCryptoUpdateKeys(keys, password.charCodeAt(i) & 0xff)
  }

  const out = new Uint8Array(data.length)
  for (let i = 0; i < data.length; i += 1) {
    const plainByte = data[i] ^ zipCryptoDecryptByte(keys)
    zipCryptoUpdateKeys(keys, plainByte)
    out[i] = plainByte
  }
  return out
}

function readU16LE(bytes, offset) {
  return bytes[offset] | (bytes[offset + 1] << 8)
}

function readU32LE(bytes, offset) {
  return (
    (bytes[offset])
    | (bytes[offset + 1] << 8)
    | (bytes[offset + 2] << 16)
    | (bytes[offset + 3] << 24)
  ) >>> 0
}

// Elm → JS: open the file picker
app.ports.requestFileUpload.subscribe(() => {
  fileInput.value = '' // reset so the same file can be re-selected
  fileInput.click()
})

// User picked a file → read it and send back to Elm
fileInput.addEventListener('change', () => {
  const file = fileInput.files[0]
  readLdrawFile(file)
})

// Global drag-and-drop support
function suppressDefaultDragBehavior(event) {
  event.preventDefault()
  event.stopPropagation()
}

window.addEventListener('dragenter', suppressDefaultDragBehavior)
window.addEventListener('dragover', suppressDefaultDragBehavior)

window.addEventListener('drop', (event) => {
  suppressDefaultDragBehavior(event)

  const droppedFile = event.dataTransfer?.files?.[0]
  if (!droppedFile) {
    reportLoadError('Drop a .ldr, .mpd, .dat, or .io file to load a model.')
    return
  }

  readLdrawFile(droppedFile)
})

// iOS Safari pinch-zoom gesture events are separate from Touch/Pointer events.
// Prevent default page zoom so model controls remain responsive.
function suppressGestureZoom(event) {
  event.preventDefault()
}

window.addEventListener('gesturestart', suppressGestureZoom, { passive: false })
window.addEventListener('gesturechange', suppressGestureZoom, { passive: false })
window.addEventListener('gestureend', suppressGestureZoom, { passive: false })
