import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import test from 'node:test'
import { buildIndex, extractConnectionsFromText, parsePartHeader, writeIndex, parseRebrickablePartColors, renderSvgThumbnail, batchRenderThumbnails } from './build-part-index.mjs'
import { parseLDrawGeometry } from '../studio-app/thumbnail-pipeline.js'

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

test('parseRebrickablePartColors ingests csv into map of part colors', () => {
  const csv = `part_num,color_id,num_sets,num_set_parts,is_spare
3001,4,1240,5000,f
3001,0,1120,4500,f
3647,71,380,800,f
`
  const map = parseRebrickablePartColors(csv)
  assert.equal(map.has('3001'), true)
  assert.equal(map.get('3001').length, 2)
  assert.deepEqual(map.get('3001')[0], { color: 4, numSets: 1240 })
  assert.equal(map.has('3647'), true)
  assert.equal(map.get('3647')[0].color, 71)
})

test('renderSvgThumbnail generates valid isometric SVG with dimensions', () => {
  const svg = renderSvgThumbnail('3001.dat', 'Brick 2 x 4', [201, 26, 9], 96)
  assert.ok(svg.startsWith('<svg'))
  assert.ok(svg.includes('viewBox="0 0 96 96"'))
  assert.ok(svg.includes('polygon points='))
  assert.ok(svg.endsWith('</svg>'))
})

test('batchRenderThumbnails generates thumbnail files in output directory', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'thumbnails-'))
  const parts = [['3001.dat', 'Brick 2 x 4'], ['3023.dat', 'Plate 1 x 2']]
  const count = batchRenderThumbnails(parts, dir)
  assert.equal(count, 2)
  assert.ok(fs.existsSync(path.join(dir, '3001.svg')))
  assert.ok(fs.existsSync(path.join(dir, '3023.svg')))
})

test('parseLDrawGeometry extracts triangles, quads and normalized subfile references', () => {
  const dat = `1 16 10 20 30 1 0 0 0 1 0 0 0 1 s\\3001s01.dat
3 16 0 0 0 10 0 0 0 10 0
4 16 -40 0 -20 -40 24 -20 40 24 -20 40 0 -20
`
  const { triangles, subfiles } = parseLDrawGeometry(dat)
  assert.equal(triangles.length, 3)
  assert.equal(subfiles.length, 1)
  assert.equal(subfiles[0].file, 's/3001s01.dat')
  assert.deepEqual(subfiles[0].matrix.slice(0, 3), [10, 20, 30])
})

test('extractConnectionsFromText identifies studs, tubes, pins, axles, clips and sockets', () => {
  const modelText = [
    '0 Sample Part',
    '1 16 10 0 20 1 0 0 0 1 0 0 0 1 stud.dat',
    '1 16 0 24 0 1 0 0 0 1 0 0 0 1 stud4.dat',
    '1 16 0 0 0 1 0 0 0 1 0 0 0 1 axlehole.dat',
    '1 16 0 0 10 1 0 0 0 1 0 0 0 1 pin.dat',
    '1 16 0 4 -10 1 0 0 0 1 0 0 0 1 clip2.dat',
    '1 16 0 4 -20 1 0 0 0 1 0 0 0 1 joint8ball.dat',
  ].join('\n')

  const conns = extractConnectionsFromText(modelText)
  assert.equal(conns.length, 6)
  assert.equal(conns[0].type, 'Stud')
  assert.deepEqual(conns[0].position, [10, 0, 20])
  assert.equal(conns[1].type, 'AntiStud')
  assert.equal(conns[2].type, 'TechnicAxleFemale')
  assert.equal(conns[3].type, 'TechnicPinMale')
  assert.equal(conns[4].type, 'ClipMale')
  assert.equal(conns[5].type, 'BallMale')
})
