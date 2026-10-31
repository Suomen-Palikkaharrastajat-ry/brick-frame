/**
 * Client-side visual asset pipeline: on-demand thumbnail rendering of palette
 * parts and an IndexedDB persistent cache (`brick-frame-thumbnails`).
 *
 * Parts are resolved through the whole LDraw sub-file tree (parts/, parts/s/,
 * p/, p/48/ …) with BFC inverts honoured, and drawn with WebGL and a depth
 * buffer from the same view direction as the studio's part preview, so a card
 * shows the same part the preview does.
 *
 * Shared by the studio palette and the Brick Play dock (kit and Technic
 * catalog): a host calls `initThumbnails(app.ports, ldrawBase)` and
 * declares the `requestThumbnails` / `thumbnailLoaded` ports (see below).
 */

const DB_NAME = 'brick-frame-thumbnails'
const DB_VERSION = 1
const STORE_NAME = 'thumbnails'

// Bump when the renderer's output changes, so images cached by an older
// renderer are not served again.
const RENDER_VERSION = 3

// Output size in device pixels; cards show it at about 84 CSS px.
const SIZE = 192

// Mirrors the preview camera (`Render.Camera.init`): azimuth and elevation in
// render space, where Y is up (LDraw Y negated).
const AZIMUTH = 0.75
const ELEVATION = 0.6

// Hard-coded colours inside part files (glass, rubber, prints). The part's own
// colour (16) comes from the caller, resolved from the full LDConfig table.
const INNER_COLOURS = {
  0: [0.106, 0.165, 0.204],
  7: [0.627, 0.647, 0.663],
  8: [0.388, 0.373, 0.384],
  15: [1, 1, 1],
  47: [0.988, 0.988, 0.988],
  71: [0.627, 0.647, 0.663],
  72: [0.388, 0.373, 0.384],
}

const EDGE_RGB = [0.13, 0.13, 0.13]

// Deep enough for any official part; stops a malformed self-reference.
const MAX_DEPTH = 16

function cacheKey(file, color) {
  return `v${RENDER_VERSION}:${file}:${color}`
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
      const req = store.get(cacheKey(file, color))
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
      store.put({ key: cacheKey(file, color), file, color, url, timestamp: Date.now() })
      tx.oncomplete = () => resolve()
      tx.onerror = () => resolve()
    } catch (_) {
      resolve()
    }
  })
}

// ── LDraw resolution ──────────────────────────────────────────────────────────

// LDraw type-1 matrix layout: [x y z a b c d e f g h i].
const IDENTITY_MATRIX = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]

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

function determinant(m) {
  const [, , , a, b, c, d, e, f, g, h, i] = m
  return a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
}

const datTextCache = new Map()

async function fetchDatText(file, ldrawBase) {
  const normalized = file.toLowerCase().replace(/\\/g, '/')
  const key = `${ldrawBase}:${normalized}`
  if (datTextCache.has(key)) return datTextCache.get(key)

  const pending = (async () => {
    const base = ldrawBase.replace(/\/+$/, '')
    const isPrimitive =
      normalized.startsWith('p/') ||
      normalized.startsWith('48/') ||
      normalized.startsWith('8/') ||
      normalized.includes('-') ||
      /^(stud|ring|edge|cyl|disc|box|tooth|peghole|bush|rect|joint)/.test(normalized)

    const urls = []
    if (normalized.startsWith('parts/') || normalized.startsWith('p/')) {
      urls.push(`${base}/${normalized}`)
    } else if (normalized.startsWith('s/')) {
      urls.push(`${base}/parts/${normalized}`)
    } else if (normalized.startsWith('48/') || normalized.startsWith('8/')) {
      urls.push(`${base}/p/${normalized}`)
    } else if (isPrimitive) {
      urls.push(`${base}/p/${normalized}`, `${base}/parts/${normalized}`, `${base}/parts/s/${normalized}`)
    } else {
      urls.push(`${base}/parts/${normalized}`, `${base}/parts/s/${normalized}`, `${base}/p/${normalized}`)
    }

    for (const url of urls) {
      try {
        const resp = await fetch(url)
        if (!resp.ok) continue
        // A dev server or SPA host answers a missing file with index.html and
        // 200; taking that as the part loses every primitive under p/.
        if ((resp.headers.get('content-type') ?? '').includes('text/html')) continue
        const text = await resp.text()
        if (!text.trimStart().startsWith('<')) return text
      } catch (_) {}
    }
    return null
  })()
  datTextCache.set(key, pending)
  return pending
}

/**
 * Flatten a part into triangles `{ v: [a, b, c], color }` and edge line
 * endpoints (pairs in `lines`), in the part's own LDraw space.
 *
 * `load(file)` resolves a sub-file name to its text (or `null`). Colour 16
 * stays 16 when nothing above overrides it, meaning "the part's colour".
 */
export async function flattenPart(file, load) {
  const out = { triangles: [], lines: [] }
  await flatten(file, load, IDENTITY_MATRIX, 16, false, 0, out)
  return out
}

/**
 * `color` is the colour code inherited from the parent; `inverted` tracks BFC
 * INVERTNEXT and mirroring so the winding of each triangle is consistent.
 */
async function flatten(file, load, matrix, color, inverted, depth, out) {
  if (depth > MAX_DEPTH) return
  const text = await load(file)
  if (!text) return

  let invertNext = false
  for (const raw of text.split(/\r?\n/)) {
    const tokens = raw.trim().split(/\s+/)
    const type = tokens[0]

    if (type === '0') {
      if (tokens[1] === 'BFC' && tokens.includes('INVERTNEXT')) invertNext = true
      continue
    }

    const own = parseInt(tokens[1], 10)
    const lineColor = own === 16 ? color : own

    if (type === '1' && tokens.length >= 15) {
      const n = tokens.slice(2, 14).map(parseFloat)
      const child = multiplyMatrices(matrix, n)
      const childInverted = inverted !== invertNext !== (determinant(n) < 0)
      invertNext = false
      const sub = tokens.slice(14).join(' ')
      await flatten(sub, load, child, lineColor, childInverted, depth + 1, out)
    } else if (type === '2' && tokens.length >= 8) {
      const p = tokens.slice(2, 8).map(parseFloat)
      out.lines.push(transformPoint(p.slice(0, 3), matrix), transformPoint(p.slice(3, 6), matrix))
    } else if (type === '3' && tokens.length >= 11) {
      const p = tokens.slice(2, 11).map(parseFloat)
      const v = [p.slice(0, 3), p.slice(3, 6), p.slice(6, 9)].map((q) => transformPoint(q, matrix))
      pushTriangle(out, v[0], v[1], v[2], lineColor, inverted)
    } else if (type === '4' && tokens.length >= 14) {
      const p = tokens.slice(2, 14).map(parseFloat)
      const v = [p.slice(0, 3), p.slice(3, 6), p.slice(6, 9), p.slice(9, 12)].map((q) => transformPoint(q, matrix))
      pushTriangle(out, v[0], v[1], v[2], lineColor, inverted)
      pushTriangle(out, v[0], v[2], v[3], lineColor, inverted)
    }
    if (type !== '1') invertNext = false
  }
}

function pushTriangle(out, a, b, c, color, inverted) {
  if (inverted) out.triangles.push({ v: [a, c, b], color })
  else out.triangles.push({ v: [a, b, c], color })
}

// ── WebGL rendering ───────────────────────────────────────────────────────────

const VERTEX_SHADER = `
attribute vec3 position;
attribute vec3 color;
uniform mat4 viewProjection;
varying vec3 vColor;
void main() {
  vColor = color;
  gl_Position = viewProjection * vec4(position, 1.0);
}`

const FRAGMENT_SHADER = `
precision mediump float;
varying vec3 vColor;
void main() {
  gl_FragColor = vec4(vColor, 1.0);
}`

let glContext = null

function getGl() {
  if (glContext) return glContext
  const canvas = typeof OffscreenCanvas !== 'undefined'
    ? new OffscreenCanvas(SIZE, SIZE)
    : Object.assign(document.createElement('canvas'), { width: SIZE, height: SIZE })
  const gl = canvas.getContext('webgl', { antialias: true, preserveDrawingBuffer: true, premultipliedAlpha: false })
  if (!gl) return null

  const compile = (kind, source) => {
    const shader = gl.createShader(kind)
    gl.shaderSource(shader, source)
    gl.compileShader(shader)
    return shader
  }
  const program = gl.createProgram()
  gl.attachShader(program, compile(gl.VERTEX_SHADER, VERTEX_SHADER))
  gl.attachShader(program, compile(gl.FRAGMENT_SHADER, FRAGMENT_SHADER))
  gl.linkProgram(program)
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) return null

  glContext = {
    canvas,
    gl,
    program,
    position: gl.getAttribLocation(program, 'position'),
    color: gl.getAttribLocation(program, 'color'),
    viewProjection: gl.getUniformLocation(program, 'viewProjection'),
    buffer: gl.createBuffer(),
  }
  return glContext
}

/** Camera basis for the preview's view direction, in LDraw space (Y down). */
function cameraBasis() {
  // Eye direction in render space (Y up), as `Camera.eyeOffset` computes it.
  const eye = [
    Math.sin(AZIMUTH) * Math.cos(ELEVATION),
    Math.sin(ELEVATION),
    Math.cos(AZIMUTH) * Math.cos(ELEVATION),
  ]
  const toLdraw = ([x, y, z]) => [x, -y, z]
  const normalize = (v) => {
    const l = Math.hypot(v[0], v[1], v[2]) || 1
    return [v[0] / l, v[1] / l, v[2] / l]
  }
  const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
  const back = eye // points from the target towards the eye
  const right = normalize(cross([0, 1, 0], back))
  const up = cross(back, right)
  return { right: toLdraw(right), up: toLdraw(up), back: toLdraw(back) }
}

const BASIS = cameraBasis()
const LIGHT = (() => {
  const l = [0.35, -0.85, 0.4] // LDraw space: mostly from above
  const n = Math.hypot(...l)
  return l.map((c) => c / n)
})()

function dot(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

function shade(tri, base) {
  const [a, b, c] = tri.v
  const e1 = [b[0] - a[0], b[1] - a[1], b[2] - a[2]]
  const e2 = [c[0] - a[0], c[1] - a[1], c[2] - a[2]]
  const n = [e1[1] * e2[2] - e1[2] * e2[1], e1[2] * e2[0] - e1[0] * e2[2], e1[0] * e2[1] - e1[1] * e2[0]]
  const len = Math.hypot(n[0], n[1], n[2]) || 1
  // Uncertified winding is unreliable; light both faces alike.
  const lambert = Math.abs(dot(n, LIGHT)) / len
  const k = 0.45 + 0.55 * lambert
  return [base[0] * k, base[1] * k, base[2] * k]
}

function colourFor(code, partRgb) {
  if (code === 16) return partRgb
  if (code === 24) return EDGE_RGB
  return INNER_COLOURS[code] ?? partRgb
}

function draw(geometry, partRgb) {
  const ctx = getGl()
  if (!ctx) return null
  const { gl, program, buffer } = ctx

  // Project onto the camera basis to frame the part tightly.
  let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity, minZ = Infinity, maxZ = -Infinity
  const grow = (p) => {
    const x = dot(p, BASIS.right), y = dot(p, BASIS.up), z = dot(p, BASIS.back)
    if (x < minX) minX = x
    if (x > maxX) maxX = x
    if (y < minY) minY = y
    if (y > maxY) maxY = y
    if (z < minZ) minZ = z
    if (z > maxZ) maxZ = z
  }
  for (const tri of geometry.triangles) tri.v.forEach(grow)
  if (!Number.isFinite(minX)) return null

  const span = Math.max(maxX - minX, maxY - minY, 1) * 1.08
  const cx = (minX + maxX) / 2, cy = (minY + maxY) / 2
  const depth = Math.max(maxZ - minZ, 1)
  // Orthographic: rows map LDraw points to clip space (column-major for GL).
  const sx = 2 / span, sy = 2 / span, sz = -2 / (depth * 1.2)
  const r = BASIS.right, u = BASIS.up, b = BASIS.back
  const tz = -(minZ + maxZ) / 2
  const viewProjection = new Float32Array([
    r[0] * sx, u[0] * sy, b[0] * sz, 0,
    r[1] * sx, u[1] * sy, b[1] * sz, 0,
    r[2] * sx, u[2] * sy, b[2] * sz, 0,
    -cx * sx, -cy * sy, tz * sz, 1,
  ])

  const triCount = geometry.triangles.length
  const lineCount = geometry.lines.length / 2
  const data = new Float32Array((triCount * 3 + lineCount * 2) * 6)
  let o = 0
  for (const tri of geometry.triangles) {
    const rgb = shade(tri, colourFor(tri.color, partRgb))
    for (const p of tri.v) {
      data.set(p, o)
      data.set(rgb, o + 3)
      o += 6
    }
  }
  for (const p of geometry.lines) {
    // Nudge edges towards the eye so they win the depth test against their faces.
    data.set([p[0] + b[0] * 0.3, p[1] + b[1] * 0.3, p[2] + b[2] * 0.3], o)
    data.set(EDGE_RGB, o + 3)
    o += 6
  }

  gl.viewport(0, 0, SIZE, SIZE)
  gl.clearColor(0, 0, 0, 0)
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT)
  gl.enable(gl.DEPTH_TEST)
  gl.depthFunc(gl.LEQUAL)
  gl.disable(gl.CULL_FACE)
  gl.enable(gl.POLYGON_OFFSET_FILL)
  gl.polygonOffset(1, 1)

  gl.useProgram(program)
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
  gl.bufferData(gl.ARRAY_BUFFER, data, gl.STREAM_DRAW)
  gl.enableVertexAttribArray(ctx.position)
  gl.vertexAttribPointer(ctx.position, 3, gl.FLOAT, false, 24, 0)
  gl.enableVertexAttribArray(ctx.color)
  gl.vertexAttribPointer(ctx.color, 3, gl.FLOAT, false, 24, 12)
  gl.uniformMatrix4fv(ctx.viewProjection, false, viewProjection)

  gl.drawArrays(gl.TRIANGLES, 0, triCount * 3)
  gl.drawArrays(gl.LINES, triCount * 3, lineCount * 2)
  return ctx.canvas
}

async function canvasToDataUrl(canvas) {
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

/**
 * Render a thumbnail of a part in the given colour, as a PNG data URL, or
 * `null` when the part cannot be resolved or WebGL is unavailable.
 *
 * `rgb` is the part colour as `[r, g, b]` in 0–1, resolved by the caller from
 * the LDConfig colour table.
 */
export async function renderIsometricThumbnail(file, color, ldrawBase, rgb) {
  const geometry = await flattenPart(file, (sub) => fetchDatText(sub, ldrawBase))
  if (geometry.triangles.length === 0) return null
  const partRgb = Array.isArray(rgb) && rgb.length === 3 ? rgb : [0.627, 0.647, 0.663]
  const canvas = draw(geometry, partRgb)
  return canvas ? canvasToDataUrl(canvas) : null
}

// Redirects a library file to another part: `0 ~Moved to 3648b`.
const MOVED_TO = /^~Moved to\s+(\S+)$/i

// Most redirect hops followed before giving up on a title.
export const MAX_TITLE_HOPS = 3

/**
 * Strip the LDraw library markers from the front of a title: `~` (moved or
 * obsolete), `=` (alias), `_` (internal) and `|` (logo or sub-part). Runs of
 * spaces collapse to one, so `| Brick 2 x 4` reads `Brick 2 x 4`.
 */
export function cleanPartTitle(raw) {
  return String(raw ?? '')
    .replace(/^[~=_|\s]+/, '')
    .replace(/\s+/g, ' ')
    .trim()
}

/**
 * The raw title of a .dat text: its first non-blank line, which must be a meta
 * line (`0 …`), with the `0` and runs of spaces removed. `null` when there is
 * none.
 */
export function rawTitleOf(text) {
  if (!text) return null
  const first = text.split(/\r?\n/).find((line) => line.trim() !== '')
  if (!first || !/^\s*0\s/.test(first)) return null
  const title = first.trim().replace(/^0\s+/, '').replace(/\s+/g, ' ').trim()
  return title === '' ? null : title
}

/**
 * The readable title of a part, following `~Moved to X` redirects.
 *
 * `load(file)` resolves a .dat name to its text, or `null`, so the logic can be
 * tested with a stub. A redirect is followed to X.dat for at most
 * `MAX_TITLE_HOPS` hops; a chain that is still a redirect after that, or a part
 * that cannot be read, gives `null` (a host then shows the part number).
 */
export async function resolvePartTitle(file, load, maxHops = MAX_TITLE_HOPS) {
  let current = file
  for (let hop = 0; hop <= maxHops; hop++) {
    const raw = rawTitleOf(await load(current))
    if (raw === null) return null
    const moved = raw.match(MOVED_TO)
    if (!moved) {
      const title = cleanPartTitle(raw)
      return title === '' ? null : title
    }
    current = moved[1].toLowerCase().endsWith('.dat') ? moved[1] : `${moved[1]}.dat`
  }
  return null
}

/**
 * The part's title: the first line of its .dat file (`0 Brick  2 x  4`), with
 * runs of spaces collapsed (`Brick 2 x 4`), LDraw markers stripped and moved
 * parts followed to their new file. `null` when it cannot be read.
 *
 * `options.load` replaces the library fetch (for tests).
 */
export async function partTitle(file, ldrawBase, options = {}) {
  const load = options.load ?? ((name) => fetchDatText(name, ldrawBase))
  return resolvePartTitle(file, load, options.maxHops ?? MAX_TITLE_HOPS)
}

/**
 * Wire an Elm app's thumbnail ports to the pipeline.
 *
 * Elm side (both ports optional; nothing happens when either is missing):
 *
 *     port requestThumbnails : List { file : String, color : Int, rgb : List Float } -> Cmd msg
 *     port thumbnailLoaded : ({ file : String, color : Int, url : String } -> msg) -> Sub msg
 *
 * Requests are queued, de-duplicated by file + colour, served from the
 * IndexedDB cache when possible and rendered one at a time otherwise, yielding
 * between renders. A part that cannot be rendered sends nothing.
 *
 * With `{ titles: true }` each `thumbnailLoaded` message also carries
 * `title : String` — the part's .dat title (see `partTitle`), or `""` when
 * unknown — so a host without a part index can show a readable name. Elm
 * ignores the extra field when the port's record type does not declare it.
 *
 * Subscribes synchronously, so call it right after the Elm app starts; requests
 * wait for the cache to open. Returns a promise that resolves once it has.
 */
export function initThumbnails(ports, ldrawBase, { titles = false } = {}) {
  if (!ports || !ports.requestThumbnails || !ports.thumbnailLoaded) return Promise.resolve()

  // Subscribe at once, not after the cache opens: a request sent from the
  // app's `init` would otherwise be dropped.
  const dbReady = openThumbnailDb()

  const queue = []
  let processing = false

  async function processQueue() {
    if (processing || queue.length === 0) return
    processing = true
    const db = await dbReady

    while (queue.length > 0) {
      const item = queue.shift()
      const { file, color, rgb } = item
      try {
        let url = await getCachedThumbnail(db, file, color)
        if (!url) {
          url = await renderIsometricThumbnail(file, color, ldrawBase, rgb)
          if (url) {
            await storeCachedThumbnail(db, file, color, url)
          }
        }
        if (url) {
          if (titles) {
            const title = (await partTitle(file, ldrawBase)) ?? ''
            ports.thumbnailLoaded.send({ file, color, url, title })
          } else {
            ports.thumbnailLoaded.send({ file, color, url })
          }
        }
      } catch (err) {
        console.warn('Thumbnail generation failed for', file, err)
      }
      // Yield briefly between renders to preserve 60fps main thread responsiveness
      await new Promise((r) => setTimeout(r, 8))
    }

    processing = false
  }

  ports.requestThumbnails.subscribe((items) => {
    for (const item of items) {
      if (!queue.some((q) => q.file === item.file && q.color === item.color)) {
        queue.push(item)
      }
    }
    processQueue()
  })
  return dbReady.then(() => undefined)
}
