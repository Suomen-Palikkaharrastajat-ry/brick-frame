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

const IDENTITY_MATRIX = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]

const BUILTIN_PRIMITIVES = {
  'box5.dat': [
    '4 16 -1 -1 -1 -1 1 -1 1 1 -1 1 -1 -1',
    '4 16 1 -1 1 1 1 1 -1 1 1 -1 -1 1',
    '4 16 -1 -1 1 -1 1 1 -1 1 -1 -1 -1 -1',
    '4 16 1 -1 -1 1 1 -1 1 1 1 1 -1 1',
    '4 16 -1 -1 1 -1 -1 -1 1 -1 -1 1 -1 1',
  ].join('\n'),
  'box4.dat': [
    '4 16 -1 -1 -1 -1 1 -1 1 1 -1 1 -1 -1',
    '4 16 1 -1 1 1 1 1 -1 1 1 -1 -1 1',
    '4 16 -1 -1 1 -1 1 1 -1 1 -1 -1 -1 -1',
    '4 16 1 -1 -1 1 1 -1 1 1 1 1 -1 1',
  ].join('\n'),
  'box3u2p.dat': [
    '4 16 1 -1 -1 1 1 -1 1 1 1 1 -1 1',
    '4 16 -1 -1 1 -1 1 1 -1 1 -1 -1 -1 -1',
    '4 16 -1 -1 -1 1 -1 -1 1 -1 1 -1 -1 1',
  ].join('\n'),
  'stud.dat': [
    '4 16 -6 -4 -6 -6 -4 6 6 -4 6 6 -4 -6',
    '4 16 -6 0 -6 -6 -4 -6 6 -4 -6 6 0 -6',
    '4 16 6 0 6 6 -4 6 -6 -4 6 -6 0 6',
    '4 16 -6 0 6 -6 -4 6 -6 -4 -6 -6 0 -6',
    '4 16 6 0 -6 6 -4 -6 6 -4 6 6 0 6',
  ].join('\n'),
}

const datTextCache = new Map()

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

function transformPoint([vx, vy, vz], m) {
  const [x, y, z, a, b, c, d, e, f, g, h, i] = m
  return [
    a * vx + b * vy + c * vz + x,
    d * vx + e * vy + f * vz + y,
    g * vx + h * vy + i * vz + z,
  ]
}

function multiplyMatrices(m1, m2) {
  const [x1, y1, z1, a1, b1, c1, d1, e1, f1, g1, h1, i1] = m1
  const [x2, y2, z2, a2, b2, c2, d2, e2, f2, g2, h2, i2] = m2
  return [
    a1 * x2 + b1 * y2 + c1 * z2 + x1,
    d1 * x2 + e1 * y2 + f1 * z2 + y1,
    g1 * x2 + h1 * y2 + i1 * z2 + z1,
    a1 * a2 + b1 * d2 + c1 * g2,
    a1 * b2 + b1 * e2 + c1 * h2,
    a1 * c2 + b1 * f2 + c1 * i2,
    d1 * a2 + e1 * d2 + f1 * g2,
    d1 * b2 + e1 * e2 + f1 * h2,
    d1 * c2 + e1 * f2 + f1 * i2,
    g1 * a2 + h1 * d2 + i1 * g2,
    g1 * b2 + h1 * e2 + i1 * h2,
    g1 * c2 + h1 * f2 + i1 * i2,
  ]
}

/**
 * Parse direct triangles/quads and subfile references from LDraw text.
 */
export function parseLDrawGeometry(datText, matrix = IDENTITY_MATRIX) {
  const triangles = []
  const subfiles = []
  if (!datText) return { triangles, subfiles }

  const lines = datText.split(/\r?\n/)
  for (const line of lines) {
    const tokens = line.trim().split(/\s+/)
    if (tokens[0] === '1' && tokens.length >= 15) {
      const subMatrix = [
        parseFloat(tokens[2]), parseFloat(tokens[3]), parseFloat(tokens[4]),
        parseFloat(tokens[5]), parseFloat(tokens[6]), parseFloat(tokens[7]),
        parseFloat(tokens[8]), parseFloat(tokens[9]), parseFloat(tokens[10]),
        parseFloat(tokens[11]), parseFloat(tokens[12]), parseFloat(tokens[13]),
      ]
      const subfile = tokens.slice(14).join(' ').toLowerCase().replace(/\\/g, '/')
      subfiles.push({ file: subfile, matrix: multiplyMatrices(matrix, subMatrix) })
    } else if (tokens[0] === '3' && tokens.length >= 11) {
      const v0 = transformPoint([parseFloat(tokens[2]), parseFloat(tokens[3]), parseFloat(tokens[4])], matrix)
      const v1 = transformPoint([parseFloat(tokens[5]), parseFloat(tokens[6]), parseFloat(tokens[7])], matrix)
      const v2 = transformPoint([parseFloat(tokens[8]), parseFloat(tokens[9]), parseFloat(tokens[10])], matrix)
      triangles.push([v0, v1, v2])
    } else if (tokens[0] === '4' && tokens.length >= 14) {
      const v0 = transformPoint([parseFloat(tokens[2]), parseFloat(tokens[3]), parseFloat(tokens[4])], matrix)
      const v1 = transformPoint([parseFloat(tokens[5]), parseFloat(tokens[6]), parseFloat(tokens[7])], matrix)
      const v2 = transformPoint([parseFloat(tokens[8]), parseFloat(tokens[9]), parseFloat(tokens[10])], matrix)
      const v3 = transformPoint([parseFloat(tokens[11]), parseFloat(tokens[12]), parseFloat(tokens[13])], matrix)
      triangles.push([v0, v1, v2])
      triangles.push([v0, v2, v3])
    }
  }

  return { triangles, subfiles }
}

async function fetchDatText(file, ldrawBase) {
  const normalized = file.toLowerCase().replace(/\\/g, '/')
  if (BUILTIN_PRIMITIVES[normalized]) {
    return BUILTIN_PRIMITIVES[normalized]
  }
  const cacheKey = `${ldrawBase}:${normalized}`
  if (datTextCache.has(cacheKey)) {
    return datTextCache.get(cacheKey)
  }

  const base = ldrawBase.replace(/\/+$/, '')
  const urls = [`${base}/parts/${normalized}`, `${base}/p/${normalized}`]
  for (const url of urls) {
    try {
      const resp = await fetch(url)
      if (resp.ok) {
        const text = await resp.text()
        datTextCache.set(cacheKey, text)
        return text
      }
    } catch (_) {}
  }

  datTextCache.set(cacheKey, null)
  return null
}

async function collectPartTriangles(file, ldrawBase, matrix = IDENTITY_MATRIX, depth = 0) {
  const datText = await fetchDatText(file, ldrawBase)
  if (!datText) return []

  const { triangles, subfiles } = parseLDrawGeometry(datText, matrix)
  if (depth < 2) {
    for (const sub of subfiles) {
      if (BUILTIN_PRIMITIVES[sub.file] || sub.file.startsWith('s/')) {
        const subTris = await collectPartTriangles(sub.file, ldrawBase, sub.matrix, depth + 1)
        for (const tri of subTris) {
          triangles.push(tri)
        }
      }
    }
  }
  return triangles
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
  const triangles = await collectPartTriangles(file, ldrawBase)

  // If part has insufficient triangles (e.g. missing subfiles), render synthetic 3D brick
  if (triangles.length < 4) {
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
  const midX = (pMinX + pMaxX) / 2
  const midY = (pMinY + pMaxY) / 2
  const scale = (size - 20) / Math.max(spanX, spanY)

  // Sort back-to-front
  projectedTris.sort((a, b) => a.z - b.z)

  // Key light direction
  const light = normalizeVector([0.5, -1.0, 0.7])

  ctx.lineWidth = 0.5
  ctx.strokeStyle = 'rgba(0,0,0,0.2)'

  for (const { p0, p1, p2, normal } of projectedTris) {
    const diff = Math.abs(normal[0] * light[0] + normal[1] * light[1] + normal[2] * light[2])
    const intensity = 0.35 + 0.65 * diff
    const fillR = Math.min(255, Math.round(r * intensity))
    const fillG = Math.min(255, Math.round(g * intensity))
    const fillB = Math.min(255, Math.round(b * intensity))

    const x0 = size / 2 + (p0.x - midX) * scale
    const y0 = size / 2 - (p0.y - midY) * scale
    const x1 = size / 2 + (p1.x - midX) * scale
    const y1 = size / 2 - (p1.y - midY) * scale
    const x2 = size / 2 + (p2.x - midX) * scale
    const y2 = size / 2 - (p2.y - midY) * scale

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
