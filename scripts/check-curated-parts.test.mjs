import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import test from 'node:test'
import { fileURLToPath } from 'node:url'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const rootDir = path.resolve(__dirname, '..')

export function extractCuratedEssentials(elmSource) {
  const lines = elmSource.split('\n')
  let inEssentials = false
  const essentials = []

  for (const line of lines) {
    if (line.includes('essentials =')) {
      inEssentials = true
    }
    if (inEssentials) {
      const matches = [...line.matchAll(/"([^"]+)"/g)].map((m) => m[1])
      essentials.push(...matches)
      if (line.includes('}') && !line.includes('++')) {
        inEssentials = false
      }
    }
  }

  return essentials
}

test('every curated essential part in BuildGroups.elm exists in parts-index.json', () => {
  const indexPath = path.join(rootDir, 'viewer-app/public/ldraw/parts-index.json')
  const studioPkgBuildGroups = path.join(rootDir, 'packages/brick-frame-studio/src/Studio/BuildGroups.elm')
  const studioAppBuildGroups = path.join(rootDir, 'studio-app/src/Studio/BuildGroups.elm')
  const buildGroupsPath = fs.existsSync(studioPkgBuildGroups) ? studioPkgBuildGroups : studioAppBuildGroups

  assert(fs.existsSync(indexPath), `parts-index.json must exist at ${indexPath}`)
  assert(fs.existsSync(buildGroupsPath), `BuildGroups.elm must exist at ${buildGroupsPath}`)

  const indexData = JSON.parse(fs.readFileSync(indexPath, 'utf8'))
  const partStems = new Set(
    indexData.parts.map(([file]) => (file.endsWith('.dat') ? file.slice(0, -4) : file))
  )

  const elmSource = fs.readFileSync(buildGroupsPath, 'utf8')
  const essentials = extractCuratedEssentials(elmSource)

  assert(essentials.length >= 100, `Expected at least 100 curated essentials, found ${essentials.length}`)

  const missing = essentials.filter((id) => !partStems.has(id))
  assert.deepEqual(
    missing,
    [],
    `Curated parts in BuildGroups.elm missing from parts-index.json: ${missing.join(', ')}`
  )
})
