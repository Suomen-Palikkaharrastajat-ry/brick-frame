/**
 * Client-side visual asset pipeline: on-demand isometric thumbnail rendering
 * and IndexedDB persistent cache (`brick-frame-thumbnails`).
 */

const DB_NAME = 'brick-frame-thumbnails'
const DB_VERSION = 1
const STORE_NAME = 'thumbnails'

const COLOR_TABLE = {
  0: [40, 40, 40],
  1: [0, 85, 191],
  2: [37, 122, 60],
  4: [201, 26, 9],
  7: [156, 156, 156],
  8: [109, 110, 108],
  14: [250, 200, 10],
  15: [245, 245, 245],
  19: [228, 205, 158],
  25: [254, 138, 24],
  27: [187, 233, 11],
  28: [149, 138, 115],
  70: [88, 42, 18],
  71: [160, 165, 169],
  72: [108, 110, 104],
  320: [114, 0, 18],
  321: [10, 52, 99],
  322: [24, 70, 50],
}

/** Open or create the IndexedDB cache */
export function openThumbnailDb() {
  if (typeof window === 'undefined' || !window.indexedDB) {
    return Promise.resolve(null)
  }
  return new Promise((resolve) => {
    try {
      const request = window.indexedDB.open(DB_NAME, DB_VERSION)
      request.onupgradeneeded = (event) => {
        const db = event.target.result
        if (!db.objectStoreNames.contains(STORE_NAME)) {
          db.createObjectStore(STORE_NAME, { keyPath: 'key' })
        }
      }
      request.onsuccess = () => resolve(request.result)
      request.onerror = () => resolve(null)
    } catch (_) {
      resolve(null)
    }
  })
}

/** Retrieve cached thumbnail data URL from IndexedDB */
export function getCachedThumbnail(db, file, color) {
  if (!db) return Promise.resolve(null)
  return new Promise((resolve) => {
    try {
      const tx = db.transaction(STORE_NAME, 'readonly')
      const store = tx.objectStore(STORE_NAME)
      const key = `${file}:${color}`
      const req = store.get(key)
      req.onsuccess = () => {
        if (req.result && req.result.url) {
          resolve(req.result.url)
        } else {
          resolve(null)
        }
      }
      req.onerror = () => resolve(null)
    } catch (_) {
      resolve(null)
    }
  })
}

/** Save thumbnail data URL into IndexedDB */
export function storeCachedThumbnail(db, file, color, url) {
  if (!db) return Promise.resolve()
  return new Promise((resolve) => {
    try {
      const tx = db.transaction(STORE_NAME, 'readwrite')
      const store = tx.objectStore(STORE_NAME)
      const key = `${file}:${color}`
      store.put({ key, file, color, url, timestamp: Date.now() })
      tx.oncomplete = () => resolve()
      tx.onerror = () => resolve()
    } catch (_) {
      resolve()
    }
  })
}

function normalizeVector([x, y, z]) {
  const len = Math.sqrt(x * x + y * y + z * z) || 1
  return [x / len, y / len, z / len]
}

function crossProduct([ax, ay, az], [bx, by, bz]) {
  return [
    ay * bz - az * by,
    az * bx - ax * bz,
    ax * by - ay * bx,
  ]
}

/**
 * Render a crisp 96x96 isometric thumbnail of a part in the given color.
 */
export async function renderIsometricThumbnail(file, color, ldrawBase) {
  const size = 96
  const canvas = typeof OffscreenCanvas !== 'undefined'
    ? new OffscreenCanvas(size, size)
    : document.createElement('canvas')
  canvas.width = size
  canvas.height = size
  const ctx = canvas.getContext('2d')

  ctx.clearRect(0, 0, size, size)

  const rgb = COLOR_TABLE[color] || [160, 165, 169]

  // Try to fetch part geometry from ldrawBase
  let datText = null
  try {
    const url = `${ldrawBase.replace(/\/+$/, '')}/parts/${file}`
    const resp = await fetch(url)
    if (resp.ok) {
      datText = await resp.text()
    }
  } catch (_) {}

  const triangles = []
  if (datText) {
    const lines = datText.split(/\r?\n/)
    for (const line of lines) {
      const tokens = line.trim().split(/\s+/)
      if (tokens[0] === '3' && tokens.length >= 11) {
        triangles.push([
          [parseFloat(tokens[2]), parseFloat(tokens[3]), parseFloat(tokens[4])],
          [parseFloat(tokens[5]), parseFloat(tokens[6]), parseFloat(tokens[7])],
          [parseFloat(tokens[8]), parseFloat(tokens[9]), parseFloat(tokens[10])],
        ])
      } else if (tokens[0] === '4' && tokens.length >= 14) {
        const v0 = [parseFloat(tokens[2]), parseFloat(tokens[3]), parseFloat(tokens[4])]
        const v1 = [parseFloat(tokens[5]), parseFloat(tokens[6]), parseFloat(tokens[7])]
        const v2 = [parseFloat(tokens[8]), parseFloat(tokens[9]), parseFloat(tokens[10])]
        const v3 = [parseFloat(tokens[11]), parseFloat(tokens[12]), parseFloat(tokens[13])]
        triangles.push([v0, v1, v2])
        triangles.push([v0, v2, v3])
      }
    }
  }

  // If part has no direct triangles (e.g. primitive subfiles or not found), render synthetic 3D brick
  if (triangles.length === 0) {
    renderSyntheticBrick(ctx, rgb, size)
  } else {
    renderMesh(ctx, triangles, rgb, size)
  }

  if (typeof canvas.convertToBlob === 'function') {
    const blob = await canvas.convertToBlob({ type: 'image/png' })
    return new Promise((resolve) => {
      const reader = new FileReader()
      reader.onloadend = () => resolve(reader.result)
      reader.readAsDataURL(blob)
    })
  }

  return canvas.toDataURL('image/png')
}

function renderSyntheticBrick(ctx, [r, g, b], size) {
  const cx = size / 2
  const cy = size / 2 + 6
  const s = 18

  // Isometric 3D cube / brick preview
  // Top face
  ctx.fillStyle = `rgb(${Math.min(255, r * 1.15)}, ${Math.min(255, g * 1.15)}, ${Math.min(255, b * 1.15)})`
  ctx.strokeStyle = `rgba(0,0,0,0.25)`
  ctx.lineWidth = 1
  ctx.beginPath()
  ctx.moveTo(cx, cy - s)
  ctx.lineTo(cx + s * 1.4, cy - s * 0.3)
  ctx.lineTo(cx, cy + s * 0.4)
  ctx.lineTo(cx - s * 1.4, cy - s * 0.3)
  ctx.closePath()
  ctx.fill()
  ctx.stroke()

  // Left face
  ctx.fillStyle = `rgb(${r * 0.75}, ${g * 0.75}, ${b * 0.75})`
  ctx.beginPath()
  ctx.moveTo(cx - s * 1.4, cy - s * 0.3)
  ctx.lineTo(cx, cy + s * 0.4)
  ctx.lineTo(cx, cy + s * 1.4)
  ctx.lineTo(cx - s * 1.4, cy + s * 0.7)
  ctx.closePath()
  ctx.fill()
  ctx.stroke()

  // Right face
  ctx.fillStyle = `rgb(${r * 0.9}, ${g * 0.9}, ${b * 0.9})`
  ctx.beginPath()
  ctx.moveTo(cx, cy + s * 0.4)
  ctx.lineTo(cx + s * 1.4, cy - s * 0.3)
  ctx.lineTo(cx + s * 1.4, cy + s * 0.7)
  ctx.lineTo(cx, cy + s * 1.4)
  ctx.closePath()
  ctx.fill()
  ctx.stroke()
}

function renderMesh(ctx, triangles, [r, g, b], size) {
  // Compute bounding box
  let minX = Infinity, maxX = -Infinity
  let minY = Infinity, maxY = -Infinity
  let minZ = Infinity, maxZ = -Infinity

  for (const tri of triangles) {
    for (const [x, y, z] of tri) {
      if (x < minX) minX = x
      if (x > maxX) maxX = x
      if (y < minY) minY = y
      if (y > maxY) maxY = y
      if (z < minZ) minZ = z
      if (z > maxZ) maxZ = z
    }
  }

  const cx = (minX + maxX) / 2
  const cy = (minY + maxY) / 2
  const cz = (minZ + maxZ) / 2

  const cos45 = Math.cos(Math.PI / 4)
  const sin45 = Math.sin(Math.PI / 4)
  const cos30 = Math.cos(Math.PI / 6)
  const sin30 = Math.sin(Math.PI / 6)

  // Project vertices
  function project([x, y, z]) {
    const dx = x - cx
    const dy = y - cy
    const dz = z - cz

    const rx = dx * cos45 - dz * sin45
    const rz = dx * sin45 + dz * cos45
    const ry = -dy * cos30 + rz * sin30

    return { x: rx, y: ry, z: rz }
  }

  // Find projected bounds for framing
  let pMinX = Infinity, pMaxX = -Infinity
  let pMinY = Infinity, pMaxY = -Infinity

  const projectedTris = []
  for (const [v0, v1, v2] of triangles) {
    const p0 = project(v0)
    const p1 = project(v1)
    const p2 = project(v2)

    pMinX = Math.min(pMinX, p0.x, p1.x, p2.x)
    pMaxX = Math.max(pMaxX, p0.x, p1.x, p2.x)
    pMinY = Math.min(pMinY, p0.y, p1.y, p2.y)
    pMaxY = Math.max(pMaxY, p0.y, p1.y, p2.y)

    // Compute normal for lighting
    const e1 = [v1[0] - v0[0], v1[1] - v0[1], v1[2] - v0[2]]
    const e2 = [v2[0] - v0[0], v2[1] - v0[1], v2[2] - v0[2]]
    const n = normalizeVector(crossProduct(e1, e2))
    const avgZ = (p0.z + p1.z + p2.z) / 3

    projectedTris.push({ p0, p1, p2, normal: n, z: avgZ })
  }

  const spanX = Math.max(1, pMaxX - pMinX)
  const spanY = Math.max(1, pMaxY - pMinY)
  const scale = (size - 20) / Math.max(spanX, spanY)

  // Sort back-to-front
  projectedTris.sort((a, b) => a.z - b.z)

  // Key light direction
  const light = normalizeVector([0.5, -1.0, 0.7])

  ctx.lineWidth = 0.5
  ctx.strokeStyle = 'rgba(0,0,0,0.2)'

  for (const { p0, p1, p2, normal } of projectedTris) {
    const diff = Math.max(0, normal[0] * light[0] + normal[1] * light[1] + normal[2] * light[2])
    const intensity = 0.35 + 0.65 * diff
    const fillR = Math.min(255, Math.round(r * intensity))
    const fillG = Math.min(255, Math.round(g * intensity))
    const fillB = Math.min(255, Math.round(b * intensity))

    const x0 = size / 2 + p0.x * scale
    const y0 = size / 2 - p0.y * scale
    const x1 = size / 2 + p1.x * scale
    const y1 = size / 2 - p1.y * scale
    const x2 = size / 2 + p2.x * scale
    const y2 = size / 2 - p2.y * scale

    ctx.fillStyle = `rgb(${fillR},${fillG},${fillB})`
    ctx.beginPath()
    ctx.moveTo(x0, y0)
    ctx.lineTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.closePath()
    ctx.fill()
    ctx.stroke()
  }
}
