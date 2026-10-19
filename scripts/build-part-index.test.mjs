import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import test from 'node:test'
import { buildIndex, parsePartHeader, writeIndex } from './build-part-index.mjs'

const brick = `0 Brick  2 x  4
0 Name: 3001.dat
0 Author: someone
0 !LDRAW_ORG Part UPDATE 2004-03
0 !LICENSE Licensed under CC BY 2.0 and CC BY 4.0
0 !CATEGORY Brick
0 !KEYWORDS 2x4, rectangular
0 !KEYWORDS studs
1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat
`

test('reads description, category and keywords', () => {
  assert.deepEqual(parsePartHeader('3001.dat', brick), ['3001.dat', 'Brick 2 x 4', 'Brick', '2x4, rectangular, studs'])
})

test('falls back to the first word of the description for the category', () => {
  const entry = parsePartHeader('3023.dat', '0 Plate 1 x 2\n0 Name: 3023.dat\n')
  assert.deepEqual(entry, ['3023.dat', 'Plate 1 x 2', 'Plate'])
})

test('skips sub-parts, moved parts, aliases and files without a description', () => {
  assert.equal(parsePartHeader('s/x.dat', '0 ~Brick 2 x 4 Stud Cutout\n'), null)
  assert.equal(parsePartHeader('1.dat', '0 ~Moved to 3001\n'), null)
  assert.equal(parsePartHeader('2.dat', '0 _Internal\n'), null)
  assert.equal(parsePartHeader('3.dat', '0 =Alias\n'), null)
  assert.equal(parsePartHeader('4.dat', '1 16 0 0 0 1 0 0 0 1 0 0 0 1 x.dat\n'), null)
  assert.equal(parsePartHeader('5.dat', ''), null)
})

test('copes with CRLF files and leading blank lines', () => {
  const entry = parsePartHeader('x.dat', '\r\n0 Technic Gear 8 Tooth\r\n0 Name: x.dat\r\n0 !CATEGORY Technic\r\n')
  assert.deepEqual(entry, ['x.dat', 'Technic Gear 8 Tooth', 'Technic'])
})

test('builds an index from a parts directory, sorted, ignoring other files', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'ldraw-'))
  fs.mkdirSync(path.join(dir, 'parts', 's'), { recursive: true })
  fs.writeFileSync(path.join(dir, 'parts', '3023.dat'), '0 Plate 1 x 2\n')
  fs.writeFileSync(path.join(dir, 'parts', '3001.DAT'.toLowerCase()), brick)
  fs.writeFileSync(path.join(dir, 'parts', 'readme.txt'), 'not a part')
  fs.writeFileSync(path.join(dir, 'parts', 's', '3001s01.dat'), '0 ~Sub-part\n')
  const index = buildIndex(dir)
  assert.equal(index.version, 1)
  assert.deepEqual(index.parts.map((p) => p[0]), ['3001.dat', '3023.dat'])
})

test('writes parts-index.json next to the parts', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'ldraw-'))
  fs.mkdirSync(path.join(dir, 'parts'))
  fs.writeFileSync(path.join(dir, 'parts', '3001.dat'), brick)
  const { target, count } = writeIndex(dir)
  assert.equal(count, 1)
  assert.equal(JSON.parse(fs.readFileSync(target, 'utf8')).parts.length, 1)
})
