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
  const code = lineColor === 16 ? parentColor : lineColor
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
        const childColor = line.color === 16 ? parentColor : line.color
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

self.onmessage = (event) => {
  try {
    const payload = JSON.parse(String(event.data ?? '{}'))
    const lines = payload.lines ?? []
    const cache = payload.cache ?? {}
    const parentColor = payload.parentColor ?? 15
    const colorTable = payload.colorTable ?? {}

    const acc = { triangles: [], lines: [], conditionalLines: [] }
    flattenLines(lines, cache, parentColor, identityTransform(), colorTable, acc)

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
