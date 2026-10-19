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
  const { target, count } = writeIndex(dir)
  console.log(`build-part-index: wrote ${count} parts to ${target}`)
}
