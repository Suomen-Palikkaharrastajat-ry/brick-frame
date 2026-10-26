#!/usr/bin/env node
// Builds the part index the studio's palette searches: one entry per part in
// <ldraw>/parts, taken from each file's header, written to
// <ldraw>/parts-index.json.
//
//   node scripts/build-part-index.mjs [ldraw-dir]
//
// The directory defaults to $LDRAW_SYNC_DIR, then viewer-app/public/ldraw.
//
// Format (version 1), kept compact because the studio downloads it whole:
//
//   { "version": 1,
//     "parts": [ [ "3001.dat", "Brick 2 x 4", "Brick", "keyword, keyword" ], ... ] }
//
// Each part is [file, description, category] and, only when the file has
// `!KEYWORDS`, a fourth string. The category is the `!CATEGORY` meta when there
// is one, else the first word of the description, as the LDraw library itself
// derives it. Sub-parts, primitives and moved/obsolete stubs are left out.

import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

export const INDEX_VERSION = 1

// A part's header is within its first lines; stop reading there.
const HEADER_LINES = 40

/**
 * Read what the index needs from the text of one part file, or `null` when the
 * file should not be offered in a palette.
 */
export function parsePartHeader(file, text) {
  const lines = text.split(/\r?\n/, HEADER_LINES)
  const first = lines.find((line) => line.trim() !== '')
  if (!first || !first.startsWith('0 ')) return null

  // The library pads descriptions with runs of spaces to line up columns.
  const padded = first.slice(2).trim().replace(/\s+/g, ' ')

  // `~` marks sub-parts and moved/obsolete files, `_` internal helpers, `=`
  // aliases of another part. None of them belong in a palette.
  if (/^[~_=]/.test(padded)) return null

  // `|` marks a logo or sub-part title that is still a real part: keep the name.
  const description = padded.replace(/^\|\s*/, '')
  if (description === '') return null

  let category = null
  let keywords = null
  for (const line of lines) {
    const meta = line.match(/^0\s+!(CATEGORY|KEYWORDS)\s+(.*)$/)
    if (!meta) continue
    if (meta[1] === 'CATEGORY' && category === null) category = meta[2].trim()
    if (meta[1] === 'KEYWORDS') keywords = keywords === null ? meta[2].trim() : `${keywords}, ${meta[2].trim()}`
  }

  if (category === null) category = description.split(/\s+/)[0].replace(/^[^A-Za-z0-9]+/, '')
  if (category === '') category = 'Other'

  const entry = [file, description, category]
  if (keywords) entry.push(keywords)
  return entry
}

export const ELEMENTS_VERSION = 1

export const LDRAW_TO_BRICKLINK_COLORS = {
  0: 11,
  1: 7,
  2: 6,
  3: 39,
  4: 5,
  5: 47,
  6: 8,
  7: 9,
  8: 10,
  9: 62,
  10: 36,
  11: 40,
  12: 25,
  13: 23,
  14: 3,
  15: 1,
  17: 38,
  18: 33,
  19: 2,
  20: 44,
  21: 46,
  22: 24,
  25: 4,
  26: 27,
  27: 34,
  28: 69,
  29: 104,
  30: 157,
  31: 154,
  32: 13,
  33: 14,
  34: 20,
  35: 108,
  36: 17,
  37: 18,
  38: 121,
  39: 113,
  41: 74,
  42: 16,
  43: 15,
  46: 19,
  47: 12,
  68: 96,
  70: 88,
  71: 86,
  72: 85,
  73: 42,
  74: 37,
  77: 56,
  78: 90,
  84: 94,
  85: 89,
  89: 97,
  92: 28,
  110: 96,
  115: 76,
  118: 41,
  120: 35,
  125: 32,
  128: 225,
  134: 84,
  135: 66,
  137: 78,
  142: 61,
  148: 77,
  150: 61,
  151: 99,
  178: 95,
  179: 95,
  191: 110,
  212: 105,
  226: 103,
  272: 63,
  288: 80,
  308: 120,
  313: 72,
  320: 59,
  321: 153,
  322: 156,
  323: 152,
  326: 158,
  330: 155,
  335: 58,
  373: 54,
  378: 48,
  379: 55,
  383: 21,
  462: 31,
  484: 68,
  503: 49,
}

export const COLOR_NAMES = {
  0: 'Black',
  1: 'Blue',
  2: 'Green',
  3: 'Dark Turquoise',
  4: 'Red',
  5: 'Dark Pink',
  6: 'Brown',
  7: 'Light Gray',
  8: 'Dark Gray',
  9: 'Light Blue',
  10: 'Bright Green',
  11: 'Light Turquoise',
  12: 'Salmon',
  13: 'Pink',
  14: 'Yellow',
  15: 'White',
  17: 'Light Green',
  18: 'Light Yellow',
  19: 'Tan',
  22: 'Purple',
  25: 'Orange',
  26: 'Magenta',
  27: 'Lime',
  28: 'Dark Tan',
  29: 'Bright Pink',
  30: 'Medium Lavender',
  31: 'Lavender',
  32: 'Trans-Black',
  33: 'Trans-Dark Blue',
  34: 'Trans-Green',
  36: 'Trans-Red',
  47: 'Trans-Clear',
  70: 'Reddish Brown',
  71: 'Light Bluish Gray',
  72: 'Dark Bluish Gray',
  73: 'Medium Blue',
  74: 'Medium Green',
  78: 'Light Nougat',
  85: 'Dark Purple',
  92: 'Nougat',
  191: 'Bright Light Orange',
  212: 'Bright Light Blue',
  226: 'Bright Light Yellow',
  272: 'Dark Blue',
  288: 'Dark Green',
  320: 'Dark Red',
  321: 'Dark Azure',
  322: 'Medium Azure',
  330: 'Olive Green',
  378: 'Sand Green',
  379: 'Sand Blue',
}

export const SEED_ELEMENTS = {
  '3001:4': ['300121', '3001', 5, 'Red'],
  '3001:15': ['300101', '3001', 1, 'White'],
  '3001:1': ['300123', '3001', 7, 'Blue'],
  '3001:0': ['300126', '3001', 11, 'Black'],
  '3001:14': ['300124', '3001', 3, 'Yellow'],
  '3002:4': ['300221', '3002', 5, 'Red'],
  '3002:15': ['300201', '3002', 1, 'White'],
  '3002:1': ['300223', '3002', 7, 'Blue'],
  '3002:0': ['300226', '3002', 11, 'Black'],
  '3002:14': ['300224', '3002', 3, 'Yellow'],
  '3003:4': ['300321', '3003', 5, 'Red'],
  '3003:15': ['300301', '3003', 1, 'White'],
  '3003:1': ['300323', '3003', 7, 'Blue'],
  '3003:0': ['300326', '3003', 11, 'Black'],
  '3003:14': ['300324', '3003', 3, 'Yellow'],
  '3004:4': ['300421', '3004', 5, 'Red'],
  '3004:15': ['300401', '3004', 1, 'White'],
  '3004:1': ['300423', '3004', 7, 'Blue'],
  '3004:0': ['300426', '3004', 11, 'Black'],
  '3004:14': ['300424', '3004', 3, 'Yellow'],
  '3005:4': ['300521', '3005', 5, 'Red'],
  '3005:15': ['300501', '3005', 1, 'White'],
  '3005:1': ['300523', '3005', 7, 'Blue'],
  '3005:0': ['300526', '3005', 11, 'Black'],
  '3005:14': ['300524', '3005', 3, 'Yellow'],
  '3020:4': ['302021', '3020', 5, 'Red'],
  '3020:15': ['302001', '3020', 1, 'White'],
  '3020:1': ['302023', '3020', 7, 'Blue'],
  '3020:0': ['302026', '3020', 11, 'Black'],
  '3021:4': ['302121', '3021', 5, 'Red'],
  '3021:15': ['302101', '3021', 1, 'White'],
  '3022:4': ['302221', '3022', 5, 'Red'],
  '3022:15': ['302201', '3022', 1, 'White'],
  '3023:4': ['302321', '3023', 5, 'Red'],
  '3023:15': ['302301', '3023', 1, 'White'],
  '3023:1': ['302323', '3023', 7, 'Blue'],
  '3023:0': ['302326', '3023', 11, 'Black'],
  '3024:4': ['302421', '3024', 5, 'Red'],
  '3024:15': ['302401', '3024', 1, 'White'],
  '3700:0': ['370026', '3700', 11, 'Black'],
  '3700:71': ['4211397', '3700', 86, 'Light Bluish Gray'],
  '3701:0': ['370126', '3701', 11, 'Black'],
  '3701:71': ['4211395', '3701', 86, 'Light Bluish Gray'],
  '3647:71': ['4211815', '3647', 86, 'Light Bluish Gray'],
  '3647:72': ['4211063', '3647', 85, 'Dark Bluish Gray'],
  '3648:71': ['4211434', '3648', 86, 'Light Bluish Gray'],
  '3648:72': ['4210794', '3648', 85, 'Dark Bluish Gray'],
  '3649:71': ['4285634', '3649', 86, 'Light Bluish Gray'],
  '3705:0': ['370526', '3705', 11, 'Black'],
  '3706:0': ['370626', '3706', 11, 'Black'],
  '2780:0': ['4121715', '2780', 11, 'Black'],
  '3673:71': ['4211807', '3673', 86, 'Light Bluish Gray'],
}

/**
 * Ingest Rebrickable elements.csv into an element catalog dictionary.
 * Format: element_id,part_num,color_id,design_id
 */
export function parseElementsCsv(csvText) {
  const elements = { ...SEED_ELEMENTS }
  const lines = csvText.split(/\r?\n/)
  if (lines.length < 2) return { version: ELEMENTS_VERSION, elements }

  const header = lines[0].split(',')
  const elemIdx = header.indexOf('element_id')
  const partIdx = header.indexOf('part_num')
  const colorIdx = header.indexOf('color_id')

  if (elemIdx === -1 || partIdx === -1 || colorIdx === -1) {
    return { version: ELEMENTS_VERSION, elements }
  }

  for (let i = 1; i < lines.length; i++) {
    const line = lines[i].trim()
    if (!line) continue
    const cols = line.split(',')
    const elementId = cols[elemIdx]?.trim()
    const partNum = cols[partIdx]?.trim().toLowerCase()
    const colorId = parseInt(cols[colorIdx]?.trim(), 10)
    if (!elementId || !partNum || isNaN(colorId)) continue

    const key = `${partNum}:${colorId}`
    const blPart = partNum
    const blColor = LDRAW_TO_BRICKLINK_COLORS[colorId] ?? colorId
    const colorName = COLOR_NAMES[colorId] ?? `Color ${colorId}`
    elements[key] = [elementId, blPart, blColor, colorName]
  }

  return { version: ELEMENTS_VERSION, elements }
}

export function buildElementsCatalog(ldrawDir) {
  const candidates = [
    path.join(ldrawDir, 'elements.csv'),
    path.join(ldrawDir, '..', 'elements.csv'),
    'elements.csv',
  ]
  for (const candidate of candidates) {
    if (fs.existsSync(candidate) && fs.statSync(candidate).isFile()) {
      return parseElementsCsv(fs.readFileSync(candidate, 'utf8'))
    }
  }
  return { version: ELEMENTS_VERSION, elements: { ...SEED_ELEMENTS } }
}

export function writeElementsIndex(ldrawDir) {
  const catalog = buildElementsCatalog(ldrawDir)
  const target = path.join(ldrawDir, 'elements-index.json')
  fs.writeFileSync(target, JSON.stringify(catalog))
  return { target, count: Object.keys(catalog.elements).length }
}

/**
 * Extract connection points from LDraw text by detecting standard subfiles
 * (stud, stud4/tube, axlehole, axle, pinhole, pin, clip, bar, joint8ball, joint8socket).
 */
export function extractConnectionsFromText(text) {
  const connections = []
  const lines = text.split(/\r?\n/)
  for (const line of lines) {
    const trimmed = line.trim()
    if (!trimmed.startsWith('1 ')) continue
    const parts = trimmed.split(/\s+/)
    if (parts.length < 15) continue
    const subfile = parts.slice(14).join(' ').toLowerCase().replace(/\\/g, '/')
    const subBase = path.basename(subfile)

    const x = parseFloat(parts[2])
    const y = parseFloat(parts[3])
    const z = parseFloat(parts[4])
    const a = parseFloat(parts[5]), b = parseFloat(parts[6]), c = parseFloat(parts[7])
    const d = parseFloat(parts[8]), e = parseFloat(parts[9]), f = parseFloat(parts[10])
    const g = parseFloat(parts[11]), h = parseFloat(parts[12]), i = parseFloat(parts[13])

    function multRot(nx, ny, nz) {
      const rx = a * nx + b * ny + c * nz
      const ry = d * nx + e * ny + f * nz
      const rz = g * nx + h * ny + i * nz
      const len = Math.hypot(rx, ry, rz) || 1
      return [rx / len, ry / len, rz / len]
    }

    if (subBase === 'stud.dat' || subBase === 'stud2.dat' || subBase.startsWith('stud-logo')) {
      connections.push({ type: 'Stud', position: [x, y, z], normal: multRot(0, -1, 0) })
    } else if (subBase === 'stud4.dat' || subBase === 'stud3.dat' || subBase === 'tube.dat') {
      connections.push({ type: 'AntiStud', position: [x, y, z], normal: multRot(0, 1, 0) })
    } else if (subBase.startsWith('axlehole') || subBase.startsWith('axlehol')) {
      connections.push({ type: 'TechnicAxleFemale', position: [x, y, z], normal: multRot(0, 0, 1) })
    } else if (subBase === 'axle.dat' || subBase.startsWith('axleend')) {
      connections.push({ type: 'TechnicAxleMale', position: [x, y, z], normal: multRot(0, 0, 1) })
    } else if (subBase.includes('pinhole') || subBase === 'peghole.dat') {
      connections.push({ type: 'TechnicPinFemale', position: [x, y, z], normal: multRot(0, 0, 1) })
    } else if (subBase.includes('confric') || subBase === 'connect.dat' || subBase === 'pin.dat') {
      connections.push({ type: 'TechnicPinMale', position: [x, y, z], normal: multRot(0, 0, 1) })
    } else if (subBase.includes('clip')) {
      connections.push({ type: 'ClipMale', position: [x, y, z], normal: multRot(0, 0, 1) })
    } else if (subBase.includes('joint8ball') || subBase.includes('towball')) {
      connections.push({ type: 'BallMale', position: [x, y, z], normal: multRot(0, -1, 0) })
    } else if (subBase.includes('joint8socket')) {
      connections.push({ type: 'BallSocketFemale', position: [x, y, z], normal: multRot(0, 1, 0) })
    }
  }
  return connections
}

/** Build the index object for the `parts` directory of an LDraw library. */
export function buildIndex(ldrawDir) {
  const partsDir = path.join(ldrawDir, 'parts')
  const parts = []
  for (const name of fs.readdirSync(partsDir).sort()) {
    if (!name.endsWith('.dat')) continue
    const full = path.join(partsDir, name)
    if (!fs.statSync(full).isFile()) continue
    const entry = parsePartHeader(name.toLowerCase(), fs.readFileSync(full, 'utf8'))
    if (entry) parts.push(entry)
  }
  return { version: INDEX_VERSION, parts }
}

export function writeIndex(ldrawDir) {
  const index = buildIndex(ldrawDir)
  const target = path.join(ldrawDir, 'parts-index.json')
  fs.writeFileSync(target, JSON.stringify(index))

  const elementsTarget = writeElementsIndex(ldrawDir)

  return { target, count: index.parts.length, elementsTarget }
}

/**
 * Ingest Rebrickable part_colors.csv data.
 * Columns: part_num,color_id,num_sets,num_set_parts,is_spare
 */
export function parseRebrickablePartColors(csvText) {
  const result = new Map()
  const lines = csvText.trim().split(/\r?\n/)
  if (lines.length <= 1) return result

  for (let i = 1; i < lines.length; i++) {
    const line = lines[i].trim()
    if (!line) continue
    const [partNum, colorId, numSets] = line.split(',')
    if (!partNum || !colorId) continue

    const part = partNum.toLowerCase()
    const color = parseInt(colorId, 10)
    const sets = parseInt(numSets, 10) || 0

    if (!result.has(part)) {
      result.set(part, [])
    }
    result.get(part).push({ color, numSets: sets })
  }
  return result
}

/**
 * Render a clean SVG isometric thumbnail for build-time asset pipelines.
 */
export function renderSvgThumbnail(file, description = '', rgb = [201, 26, 9], size = 96) {
  const [r, g, b] = rgb
  const topColor = `rgb(${Math.min(255, Math.round(r * 1.15))},${Math.min(255, Math.round(g * 1.15))},${Math.min(255, Math.round(b * 1.15))})`
  const leftColor = `rgb(${Math.round(r * 0.75)},${Math.round(g * 0.75)},${Math.round(b * 0.75)})`
  const rightColor = `rgb(${Math.round(r * 0.9)},${Math.round(g * 0.9)},${Math.round(b * 0.9)})`

  const cx = size / 2
  const cy = size / 2 + 6
  const s = 18

  return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${size} ${size}" width="${size}" height="${size}">
  <polygon points="${cx},${cy - s} ${cx + s * 1.4},${cy - s * 0.3} ${cx},${cy + s * 0.4} ${cx - s * 1.4},${cy - s * 0.3}" fill="${topColor}" stroke="rgba(0,0,0,0.2)" stroke-width="0.8" />
  <polygon points="${cx - s * 1.4},${cy - s * 0.3} ${cx},${cy + s * 0.4} ${cx},${cy + s * 1.4} ${cx - s * 1.4},${cy + s * 0.7}" fill="${leftColor}" stroke="rgba(0,0,0,0.2)" stroke-width="0.8" />
  <polygon points="${cx},${cy + s * 0.4} ${cx + s * 1.4},${cy - s * 0.3} ${cx + s * 1.4},${cy + s * 0.7} ${cx},${cy + s * 1.4}" fill="${rightColor}" stroke="rgba(0,0,0,0.2)" stroke-width="0.8" />
</svg>`
}

/**
 * Batch render SVG thumbnails into an output directory.
 */
export function batchRenderThumbnails(parts, outputDir, rgb = [201, 26, 9]) {
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true })
  }
  let count = 0
  for (const part of parts) {
    const file = Array.isArray(part) ? part[0] : part
    const desc = Array.isArray(part) ? part[1] : ''
    const svg = renderSvgThumbnail(file, desc, rgb)
    const outPath = path.join(outputDir, `${file.replace(/\.dat$/i, '')}.svg`)
    fs.writeFileSync(outPath, svg, 'utf8')
    count++
  }
  return count
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const dir = process.argv[2] ?? process.env.LDRAW_SYNC_DIR ?? 'viewer-app/public/ldraw'
  if (!fs.existsSync(path.join(dir, 'parts'))) {
    console.error(`build-part-index: no parts directory in ${dir}`)
    process.exit(1)
  }
  const { target, count, elementsTarget } = writeIndex(dir)
  console.log(`build-part-index: wrote ${count} parts to ${target}`)
  console.log(`build-part-index: wrote ${elementsTarget.count} elements to ${elementsTarget.target}`)
}

