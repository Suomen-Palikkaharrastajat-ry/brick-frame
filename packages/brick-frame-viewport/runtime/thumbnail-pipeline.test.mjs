import assert from 'node:assert/strict'
import test from 'node:test'
import { cleanPartTitle, partTitle, resolvePartTitle, MAX_TITLE_HOPS } from './thumbnail-pipeline.js'

// A stub library: file name -> .dat text. Lookups are recorded to check hops.
function stubLoader(files) {
  const requested = []
  const load = async (name) => {
    requested.push(name)
    return Object.prototype.hasOwnProperty.call(files, name) ? files[name] : null
  }
  return { load, requested }
}

test('cleanPartTitle strips the LDraw markers and collapses spaces', () => {
  assert.equal(cleanPartTitle('=Technic Bush with Two Flanges'), 'Technic Bush with Two Flanges')
  assert.equal(cleanPartTitle('~Brick 2 x 4'), 'Brick 2 x 4')
  assert.equal(cleanPartTitle('_Internal  helper'), 'Internal helper')
  assert.equal(cleanPartTitle('| BuWizz 2.0 Hub'), 'BuWizz 2.0 Hub')
  assert.equal(cleanPartTitle('=~|Brick'), 'Brick')
  assert.equal(cleanPartTitle('Plate 1 x 2'), 'Plate 1 x 2')
})

test('a plain title is returned with runs of spaces collapsed', async () => {
  const { load } = stubLoader({ '3001.dat': '0 Brick  2 x  4\n0 Name: 3001.dat\n' })
  assert.equal(await partTitle('3001.dat', '', { load }), 'Brick 2 x 4')
})

test('an alias marker is stripped from the title', async () => {
  const { load } = stubLoader({ 'x.dat': '0 =Technic Bush with Two Flanges\n' })
  assert.equal(await partTitle('x.dat', '', { load }), 'Technic Bush with Two Flanges')
})

test('a moved part takes the title of the file it moved to', async () => {
  const { load, requested } = stubLoader({
    '3648.dat': '0 ~Moved to 3648b\n0 Name: 3648.dat\n',
    '3648b.dat': '0 Gear 24 Tooth Double Bevel\n',
  })
  assert.equal(await partTitle('3648.dat', '', { load }), 'Gear 24 Tooth Double Bevel')
  assert.deepEqual(requested, ['3648.dat', '3648b.dat'])
})

test('a redirect chain is followed for at most three hops', async () => {
  const files = {
    'a.dat': '0 ~Moved to b\n',
    'b.dat': '0 ~Moved to c\n',
    'c.dat': '0 ~Moved to d\n',
    'd.dat': '0 Final Name\n',
  }
  const { load, requested } = stubLoader(files)
  assert.equal(MAX_TITLE_HOPS, 3)
  assert.equal(await partTitle('a.dat', '', { load }), 'Final Name')
  assert.deepEqual(requested, ['a.dat', 'b.dat', 'c.dat', 'd.dat'])
})

test('a chain longer than three hops gives no title rather than a marker', async () => {
  const files = {
    'a.dat': '0 ~Moved to b\n',
    'b.dat': '0 ~Moved to c\n',
    'c.dat': '0 ~Moved to d\n',
    'd.dat': '0 ~Moved to e\n',
    'e.dat': '0 Too Far\n',
  }
  const { load, requested } = stubLoader(files)
  assert.equal(await partTitle('a.dat', '', { load }), null)
  assert.equal(requested.length, 4)
})

test('a redirect to a missing file, a cycle or an absent part gives null', async () => {
  const missing = stubLoader({ 'm.dat': '0 ~Moved to nowhere\n' })
  assert.equal(await partTitle('m.dat', '', { load: missing.load }), null)

  const cycle = stubLoader({ 'p.dat': '0 ~Moved to q\n', 'q.dat': '0 ~Moved to p\n' })
  assert.equal(await partTitle('p.dat', '', { load: cycle.load }), null)

  const absent = stubLoader({})
  assert.equal(await partTitle('none.dat', '', { load: absent.load }), null)
})

test('a file that does not start with a meta line has no title', async () => {
  const { load } = stubLoader({ 'z.dat': '1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat\n' })
  assert.equal(await partTitle('z.dat', '', { load }), null)
})

test('resolvePartTitle works with a bare loader function', async () => {
  const load = async (name) => (name === 'only.dat' ? '0 Only One\n' : null)
  assert.equal(await resolvePartTitle('only.dat', load), 'Only One')
})
