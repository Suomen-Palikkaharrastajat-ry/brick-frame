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
  const description = first.slice(2).trim().replace(/\s+/g, ' ')
  if (description === '') return null

  // `~` marks sub-parts and moved/obsolete files, `_` internal helpers, `=`
  // aliases of another part. None of them belong in a palette.
  if (/^[~_=]/.test(description)) return null

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
    const subfile = parts[14].toLowerCase().replace(/\\/g, '/')
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
  return { target, count: index.parts.length }
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const dir = process.argv[2] ?? process.env.LDRAW_SYNC_DIR ?? 'viewer-app/public/ldraw'
  if (!fs.existsSync(path.join(dir, 'parts'))) {
    console.error(`build-part-index: no parts directory in ${dir}`)
    process.exit(1)
  }
  const { target, count } = writeIndex(dir)
  console.log(`build-part-index: wrote ${count} parts to ${target}`)
}
