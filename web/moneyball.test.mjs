// Explicitly synthetic schema fixtures, never published NBA observations.
import test from 'node:test';
import assert from 'node:assert/strict';
import { validateBundle, interpolate, validateIndex, validateViewBundle, createViewLoader } from './moneyball.ts';
const contrast = (supported, estimate = null) => ({ supported, estimate, low: supported ? estimate - 0.1 : null, high: supported ? estimate + 0.1 : null, nStart: 3, nEnd: 3, reason: supported ? null : 'Synthetic unsupported endpoint' });
function fixture() {
  const model = { name: 'Synthetic linear test', method: 'Test fixture only', validation: 'Test fixture only', intervalLabel: 'Synthetic conditional interval', curve: [{ x: 0, y: -1, low: -2, high: 0, extraMillion: contrast(true, .1) }, { x: .1, y: 0, low: -1, high: 1, extraMillion: contrast(true, .1) }, { x: .2, y: 1, low: 0, high: 2, extraMillion: contrast(false) }] };
  return { schemaVersion: 'moneyball.v1', dataVersion: 'synthetic-test-only', defaultView: '2025-1', publication: { status: 'private-preview', note: 'Synthetic test fixture only' }, sources: [{ label: 'Synthetic source', url: 'https://example.com/test', version: 'fixture', retrieved: '2026-10-23' }], definitions: { salary: 'Synthetic salary definition', eligibility: 'Synthetic eligibility', uncertainty: 'Synthetic uncertainty', limitations: ['Test fixture only'] }, views: [{ id: '2025-1', endSeason: 2025, window: 1, seasons: [2025], referenceCapUsd: 100000000, representation: 'player-season', coverage: { eligible: 1, matched: 1, includedKnownSalaryUsd: 5000000, allKnownActiveSalaryUsd: 6000000, unknownSalaryPlayerSeasons: 1, sourcePlayerSeasons: 2, eligiblePlayerSeasons: 1, excluded: [], note: 'Test only' }, players: [{ id: '999999999', name: 'SYNTHETIC TEST PLAYER', teams: ['TEST'], observedSeasons: [2025], minutes: 1, possessions: 2, salaryCapShare: .05, salaryUsd: 5000000, lebron: -1, war: -2, fitted: { lebron: -.5, war: -.5 }, residual: { lebron: -.5, war: -1.5 }, label: false, flags: ['Synthetic fixture only'] }], models: { lebron: structuredClone(model), war: structuredClone(model) }, teams: [{ team: 'TEST', salaryUsd: null, war: null, status: 'unavailable', attributablePlayerSeasons: 0, eligiblePlayerSeasons: 1, excludedTradeCount: 1, excludedMissingCount: 0, note: 'Synthetic unavailable allocation' }] }] };
}
test('accepts negative outcomes and unknown team attribution as null', () => { assert.equal(validateBundle(fixture()).views[0].players[0].war, -2); });
test('interpolates the visible curve and never extrapolates', () => { const c = fixture().views[0].models.war.curve; assert.equal(interpolate(c, .05), -.5); assert.ok(Number.isNaN(interpolate(c, .3))); });
const cases = {
  'salary coverage over denominator': b => { b.views[0].coverage.includedKnownSalaryUsd = 7000000; },
  'unknown salaries over source population': b => { b.views[0].coverage.unknownSalaryPlayerSeasons = 3; },
  'missing salary coverage': b => { delete b.views[0].coverage.includedKnownSalaryUsd; },
  'wrong schema': b => { b.schemaVersion = 'wrong'; },
  'mixed default identity': b => { b.defaultView = '2026-1'; },
  'duplicate NBA identity': b => { b.views[0].players.push(structuredClone(b.views[0].players[0])); b.views[0].coverage.matched = 2; b.views[0].coverage.eligible = 2; },
  'mismatched dollars': b => { b.views[0].players[0].salaryUsd = 1; },
  'nonfinite outcome': b => { b.views[0].players[0].war = NaN; },
  'wrong observed season': b => { b.views[0].players[0].observedSeasons = [2024]; },
  'missing active season window identity': b => { b.views[0].window = 3; },
  'wrong residual': b => { b.views[0].players[0].residual.war = 2; },
  'hidden different fitted reference': b => { b.views[0].players[0].fitted.war = 2; },
  'descending curve': b => { b.views[0].models.war.curve.reverse(); },
  'reversed interval': b => { b.views[0].models.war.curve[0].low = 5; },
  'mixed interval availability': b => { b.views[0].models.war.curve[0].low = null; b.views[0].models.war.curve[0].high = null; },
  'unsupported result leakage': b => { b.views[0].models.war.curve[2].extraMillion.estimate = 0; },
  'supported contrast without local records': b => { b.views[0].models.war.curve[0].extraMillion.nStart = 0; },
  'independent contrast reference': b => { b.views[0].models.war.curve[0].extraMillion.estimate = .5; },
  'contrast beyond curve domain': b => { b.views[0].models.war.curve[2].extraMillion = contrast(true, .1); },
  'unknown team fabricated zero': b => { b.views[0].teams[0].salaryUsd = 0; b.views[0].teams[0].war = 0; },
  'unlabelled attribution state': b => { delete b.views[0].teams[0].status; },
  'salary-derived provider outcome': b => { b.views[0].players[0].fair_salary = 1; },
  'unsafe source URL': b => { b.sources[0].url = 'javascript:alert(1)'; },
};
for (const [label, mutate] of Object.entries(cases)) test(`rejects ${label}`, () => { const b = fixture(); mutate(b); assert.throws(() => validateBundle(b)); });
test('percentile intervals can exclude point estimates while remaining ordered', () => { const b = fixture(); b.views[0].models.war.curve[0].low = 1; b.views[0].models.war.curve[0].high = 2; assert.doesNotThrow(() => validateBundle(b)); });
test('one invalid nondefault view rejects the entire bundle', () => { const b = fixture(); const v = structuredClone(b.views[0]); v.endSeason = 2024; v.id = '2024-1'; v.seasons = [2024]; b.views.push(v); assert.throws(() => validateBundle(b)); });
test('multi-year point uses its own observed matched season count', () => { const b = fixture(); const v = b.views[0]; v.endSeason = 2025; v.window = 3; v.id = '2025-3'; v.seasons = [2023, 2024, 2025]; v.representation = 'player-window'; v.players[0].observedSeasons = [2023, 2025]; b.defaultView = v.id; assert.doesNotThrow(() => validateBundle(b)); });

function indexFixture() { return { schemaVersion: 'moneyball.index.v1', dataVersion: 'synthetic-test-only', defaultView: '2025-1', views: [{ id: '2025-1', endSeason: 2025, window: 1, url: '/data/moneyball/v1/views/2025-1.json' }, { id: '2024-1', endSeason: 2024, window: 1, url: '/data/moneyball/v1/views/2024-1.json' }] }; }
function olderFixture() { const b = fixture(); b.defaultView = '2024-1'; const v = b.views[0]; v.id = '2024-1'; v.endSeason = 2024; v.seasons = [2024]; v.players[0].observedSeasons = [2024]; return b; }
test('validates an availability index without loading its observations', () => { assert.equal(validateIndex(indexFixture()).views.length, 2); });
for (const url of ['https://example.com/2025-1.json', '//example.com/2025-1.json', '/data/moneyball/v1/views/../2025-1.json', '/data/moneyball/v1/views/%32%30%32%35-1.json', '/data/moneyball/v1/views/2025-1.json?version=2', '/data/moneyball/v1/views/2025-1.json#x', '/data/moneyball/v1/views/2024-1.json']) test(`rejects unsafe or mismatched manifest path ${url}`, () => { const i = indexFixture(); i.views[0].url = url; assert.throws(() => validateIndex(i)); });
test('rejects duplicate manifest views and missing default', () => { const i = indexFixture(); i.views.push(i.views[0]); assert.throws(() => validateIndex(i)); i.views.pop(); i.defaultView = '2023-1'; assert.throws(() => validateIndex(i)); });
test('rejects manifest identity and window mismatch', () => { const i = indexFixture(); i.views[0].window = 3; assert.throws(() => validateIndex(i)); });
test('view boundary checks data version and selected identity', () => { const i = validateIndex(indexFixture()); assert.throws(() => validateViewBundle(fixture(), i.views[0], 'other-version')); assert.throws(() => validateViewBundle(fixture(), i.views[1], i.dataVersion)); });
test('view file cannot smuggle additional views', () => { const b = fixture(); b.views.push(olderFixture().views[0]); const i = indexFixture(); assert.throws(() => validateViewBundle(b, i.views[0], i.dataVersion)); });
test('loader caches only validated view files', async () => { let reads = 0; const loader = createViewLoader(validateIndex(indexFixture()), async () => { reads++; return fixture(); }); assert.ok(await loader.load('2025-1')); assert.ok(await loader.load('2025-1')); assert.equal(reads, 1); });
test('invalid view is never cached and can recover on retry', async () => { let reads = 0; const loader = createViewLoader(validateIndex(indexFixture()), async () => { reads++; return reads === 1 ? { bad: true } : fixture(); }); await assert.rejects(loader.load('2025-1')); assert.ok(await loader.load('2025-1')); assert.equal(reads, 2); });
test('slower prior view cannot replace the latest request', async () => { let finish; const loader = createViewLoader(validateIndex(indexFixture()), url => url.includes('2025-1') ? new Promise(resolve => { finish = resolve; }) : Promise.resolve(olderFixture())); const first = loader.load('2025-1'); const latest = await loader.load('2024-1'); finish(fixture()); assert.equal(latest.defaultView, '2024-1'); assert.equal(await first, null); });
test('stale load error cannot clear a newer valid view', async () => { let reject; const loader = createViewLoader(validateIndex(indexFixture()), url => url.includes('2025-1') ? new Promise((_, no) => { reject = no; }) : Promise.resolve(olderFixture())); const first = loader.load('2025-1'); assert.ok(await loader.load('2024-1')); reject(new Error('Synthetic stale network failure')); assert.equal(await first, null); });
test('cancelled loader cannot render its pending completion', async () => { let finish; const loader = createViewLoader(validateIndex(indexFixture()), () => new Promise(resolve => { finish = resolve; })); const first = loader.load('2025-1'); loader.cancel(); finish(fixture()); assert.equal(await first, null); });
