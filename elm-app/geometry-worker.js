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

// Scalar triple product: det of matrix whose columns are t.x, t.y, t.z.
// Negative result means the transform flips handedness (inverts winding order).
function transformDet(t) {
  return dot(t.x, cross(t.y, t.z))
}

function faceNormal(p1, p2, p3) {
  return normalizeSafe(cross(sub(p2, p1), sub(p3, p1)))
}

function resolveColor(parentColor, lineColor, colorTable) {
  // Studio exports may use -1 for current color (inherit) and -2 for edge color.
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

// windingFlipped tracks the accumulated winding state:
//   - Starts true because toYUp negates Y, which is a reflection (det = -1) that
//     inverts the winding of every triangle.  Compensating for it here means normals
//     point outward and lighting works correctly.
//   - Each BFC INVERTNEXT or negative-determinant sub-file transform XORs this flag,
//     so the two inversions in e.g. "INVERTNEXT + neg-det" cancel out correctly.
function flattenLines(lines, cache, parentColor, transform, colorTable, acc, windingFlipped) {
  let invertNext = false
  for (const line of lines) {
    switch (line.k) {
      case 'comment': {
        if (line.text && line.text.includes('BFC INVERTNEXT')) {
          invertNext = true
        }
        // Comments do not reset invertNext (other BFC or non-BFC comments are ignored).
        continue
      }
      case 'subfile': {
        const loaded = cache[line.file]
        const childColor = (line.color === 16 || line.color === -1) ? parentColor : line.color
        const childTransform = composeTransforms(transform, line.transform)
        // A negative-determinant local transform flips winding; INVERTNEXT also flips.
        const detFlip = transformDet(line.transform) < 0
        const childFlipped = windingFlipped !== (invertNext !== detFlip)
        invertNext = false
        if (!loaded) break
        flattenLines(loaded, cache, childColor, childTransform, colorTable, acc, childFlipped)
        break
      }
      case 'tri': {
        invertNext = false
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        const p3 = toYUp(applyTransform(transform, line.p3))
        // windingFlipped=true → swap p2/p3 so the cross product gives the outward normal.
        const normal = windingFlipped ? faceNormal(p1, p3, p2) : faceNormal(p1, p2, p3)
        const color = resolveColor(parentColor, line.color, colorTable)
        acc.triangles.push([
          { position: p1, normal, color },
          { position: p2, normal, color },
          { position: p3, normal, color },
        ])
        break
      }
      case 'quad': {
        invertNext = false
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        const p3 = toYUp(applyTransform(transform, line.p3))
        const p4 = toYUp(applyTransform(transform, line.p4))
        const color = resolveColor(parentColor, line.color, colorTable)
        const n1 = windingFlipped ? faceNormal(p1, p3, p2) : faceNormal(p1, p2, p3)
        const n2 = windingFlipped ? faceNormal(p1, p4, p3) : faceNormal(p1, p3, p4)
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
        invertNext = false
        const p1 = toYUp(applyTransform(transform, line.p1))
        const p2 = toYUp(applyTransform(transform, line.p2))
        acc.lines.push([p1, p2])
        break
      }
      case 'cond': {
        invertNext = false
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

self.onmessage = (event) => {
  try {
    const payload = JSON.parse(String(event.data ?? '{}'))
    const lines = payload.lines ?? []
    const cache = payload.cache ?? {}
    const parentColor = payload.parentColor ?? 15
    const colorTable = payload.colorTable ?? {}

    const acc = { triangles: [], lines: [], conditionalLines: [] }
    // Start with windingFlipped=true: the toYUp Y-axis reflection (det=-1) inverts
    // winding for every triangle; this flag compensates for that globally.
    flattenLines(lines, cache, parentColor, identityTransform(), colorTable, acc, true)

    self.postMessage(
      JSON.stringify({
        ok: true,
        bfcCertified: hasBfcCertify(lines),
        triangles: acc.triangles,
        lines: acc.lines,
        conditionalLines: acc.conditionalLines,
      }),
    )
  } catch (err) {
    self.postMessage(
      JSON.stringify({
        ok: false,
        error: err instanceof Error ? err.message : String(err),
      }),
    )
  }
}
