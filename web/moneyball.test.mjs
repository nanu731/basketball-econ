// Explicitly synthetic schema fixtures, never published NBA observations.
import test from 'node:test';
import assert from 'node:assert/strict';
import { validateBundle as validateBundleAt, interpolate, validateIndex as validateIndexAt, validateViewBundle as validateViewBundleAt, createViewLoader as createViewLoaderAt, parseJSONBytes, readJSONResponse, INDEX_BYTE_LIMIT, VIEW_BYTE_LIMIT, lastCompletedSeason, teamName, readableReason, initialContrastIndex, niceTicks, salaryCoverage, resolvePlayerSelection, chartLabels, money, decimal, valuePercentiles, percentile, filterPlayers, playerComparisonCopy, placeChartLabels, warOptionLabel, normalizePlayerSearch, playerSuggestions, createPlayerSuggestions, resolveSearchChoice, salaryTicks, salaryComparison, examplePlayers } from './moneyball.ts';
const AS_OF = new Date('2026-10-23T12:00:00Z');
const validateBundle = (input, asOf = AS_OF) => validateBundleAt(input, asOf);
const validateIndex = (input, asOf = AS_OF) => validateIndexAt(input, asOf);
const validateViewBundle = (input, expected, version, asOf = AS_OF) => validateViewBundleAt(input, expected, version, asOf);
const createViewLoader = (index, read, asOf = AS_OF) => createViewLoaderAt(index, read, asOf);
const contrast = (supported, estimate = null) => ({ supported, estimate, low: supported ? estimate - 0.1 : null, high: supported ? estimate + 0.1 : null, nStart: 3, nEnd: 3, reason: supported ? null : 'Synthetic unsupported endpoint' });
function fixture() {
  const model = { name: 'Synthetic linear test', method: 'Test fixture only', validation: 'Test fixture only', intervalLabel: 'Synthetic conditional interval', curve: [{ x: 0, y: -1, low: -2, high: 0, extraMillion: contrast(true, .1) }, { x: .1, y: 0, low: -1, high: 1, extraMillion: contrast(true, .1) }, { x: .2, y: 1, low: 0, high: 2, extraMillion: contrast(false) }] };
  return { schemaVersion: 'moneyball.v1', dataVersion: 'synthetic-test-only', defaultView: '2025-1', publication: { status: 'private-preview', note: 'Synthetic test fixture only' }, sources: [{ label: 'Synthetic source', url: 'https://example.com/test', version: 'fixture', retrieved: '2026-10-23' }], definitions: { salary: 'Synthetic salary definition', eligibility: 'Synthetic eligibility', uncertainty: 'Synthetic uncertainty', limitations: ['Test fixture only'] }, views: [{ id: '2025-1', endSeason: 2025, window: 1, seasons: [2025], referenceCapUsd: 100000000, representation: 'player-season', coverage: { eligible: 1, matched: 1, includedKnownSalaryUsd: 5000000, allKnownActiveSalaryUsd: 6000000, unknownSalaryPlayerSeasons: 1, sourcePlayerSeasons: 2, eligiblePlayerSeasons: 1, excluded: [], note: 'Test only' }, players: [{ id: '999999999', name: 'SYNTHETIC TEST PLAYER', teams: ['TEST'], observedSeasons: [2025], minutes: 1, possessions: 2, salaryCapShare: .05, salaryUsd: 5000000, lebron: -1, war: -2, fitted: { lebron: -.5, war: -.5 }, residual: { lebron: -.5, war: -1.5 }, valuePercentile: { lebron: 50, war: 50 }, label: false, flags: ['Synthetic fixture only'] }], models: { lebron: structuredClone(model), war: structuredClone(model) }, teams: [{ team: 'TEST', salaryUsd: null, war: null, status: 'unavailable', attributablePlayerSeasons: 0, eligiblePlayerSeasons: 1, excludedTradeCount: 1, excludedMissingCount: 0, note: 'Synthetic unavailable allocation' }] }] };
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

// Boundary regressions from the independent review. All contents remain synthetic.
const encode = value => new TextEncoder().encode(JSON.stringify(value));
test('build byte parser counts multibyte UTF-8 for the index and view hard limits', () => {
  for (const [value, limit, count] of [[indexFixture(), INDEX_BYTE_LIMIT, 60000], [fixture(), VIEW_BYTE_LIMIT, 1000000]]) {
    value.syntheticPadding = 'é'.repeat(count);
    assert.ok(JSON.stringify(value).length < limit);
    assert.ok(encode(value).byteLength > limit);
    assert.throws(() => parseJSONBytes(encode(value), limit), /payload byte limit/);
  }
});
test('byte parser accepts the exact UTF-8 boundary and rejects the next byte', () => {
  const value = { synthetic: 'é' }, bytes = encode(value);
  assert.deepEqual(parseJSONBytes(bytes, bytes.byteLength), value);
  assert.throws(() => parseJSONBytes(bytes, bytes.byteLength - 1), /payload byte limit/);
});
test('streamed index and view bodies enforce actual UTF-8 bytes without a length header', async () => {
  for (const [value, limit, count] of [[indexFixture(), INDEX_BYTE_LIMIT, 60000], [fixture(), VIEW_BYTE_LIMIT, 1000000]]) {
    value.syntheticPadding = 'é'.repeat(count);
    await assert.rejects(readJSONResponse(new Response(encode(value)), limit), /payload byte limit/);
  }
});
test('stream reader cancels an oversized body before consuming the remainder', async () => {
  let pulls = 0, cancelled = false;
  const stream = new ReadableStream({ pull(controller) { pulls++; controller.enqueue(new Uint8Array(40)); }, cancel() { cancelled = true; } });
  await assert.rejects(readJSONResponse(new Response(stream), 100), /payload byte limit/);
  assert.ok(cancelled); assert.ok(pulls <= 4);
});
test('content length is checked early but a false smaller length cannot bypass byte limits', async () => {
  let cancelled = false;
  const stream = new ReadableStream({ cancel() { cancelled = true; } });
  await assert.rejects(readJSONResponse(new Response(stream, { headers: { 'Content-Length': '101' } }), 100), /response length/);
  assert.ok(cancelled);
  await assert.rejects(readJSONResponse(new Response(new Uint8Array(101), { headers: { 'Content-Length': '1' } }), 100), /payload byte limit/);
});
test('streamed UTF-8 can split a character across chunks at the exact boundary', async () => {
  const value = { synthetic: 'é' }, bytes = encode(value), split = bytes.indexOf(195) + 1;
  const stream = new ReadableStream({ start(controller) { controller.enqueue(bytes.slice(0, split)); controller.enqueue(bytes.slice(split)); controller.close(); } });
  assert.deepEqual(await readJSONResponse(new Response(stream), bytes.length), value);
});
test('rejects a finite salary with an overflowing cap-share product', () => {
  const b = fixture(), v = b.views[0], p = v.players[0]; v.referenceCapUsd = 1e308; p.salaryCapShare = 2; p.salaryUsd = 1;
  p.fitted = { lebron: 0, war: 0 }; p.residual = { lebron: -1, war: -2 };
  for (const m of ['lebron', 'war']) v.models[m].curve.forEach((k, i) => { k.x = i * 2; k.extraMillion = contrast(false); });
  assert.throws(() => validateBundle(b), /money units/);
});
test('interpolation never returns infinity from finite endpoints', () => {
  assert.ok(Number.isNaN(interpolate([{ x: 0, y: -1e308 }, { x: 1, y: 1e308 }], .5)));
  const b = fixture();
  for (const m of ['lebron', 'war']) b.views[0].models[m].curve.forEach((k, i) => { k.y = i === 0 ? -1e308 : 1e308; k.extraMillion = contrast(false); });
  assert.throws(() => validateBundle(b), /curve order or arithmetic/);
});
test('overflowing residual arithmetic cannot pass tolerance comparison', () => {
  const b = fixture(), v = b.views[0], p = v.players[0];
  for (const m of ['lebron', 'war']) v.models[m].curve.forEach(k => { k.y = -1e308; k.extraMillion = contrast(false); });
  p.lebron = 1e308; p.fitted = { lebron: -1e308, war: -1e308 }; p.residual = { lebron: 1, war: 1e308 };
  assert.throws(() => validateBundle(b), /visible reference or residual mismatch/);
});
test('zero attributable observations require unavailable null totals', () => {
  const b = fixture(), t = b.views[0].teams[0]; t.status = 'attributable-sample'; t.salaryUsd = 0; t.war = 0;
  assert.throws(() => validateBundle(b), /finite team totals need attributed observations/);
  t.status = 'unavailable'; t.salaryUsd = null; t.war = null;
  assert.doesNotThrow(() => validateBundle(b));
});
test('genuine measured zero totals remain valid with an attributed observation', () => {
  const b = fixture(), t = b.views[0].teams[0]; t.status = 'attributable-sample'; t.salaryUsd = 0; t.war = 0; t.attributablePlayerSeasons = 1;
  assert.doesNotThrow(() => validateBundle(b));
});
function seasonFixture(end) { const b = fixture(), v = b.views[0]; b.defaultView = v.id = `${end}-1`; v.endSeason = end; v.seasons = [end]; v.players[0].observedSeasons = [end]; return b; }
function seasonIndex(end) { return { ...indexFixture(), defaultView: `${end}-1`, views: [{ id: `${end}-1`, endSeason: end, window: 1, url: `/data/moneyball/v1/views/${end}-1.json` }] }; }
test('July 1 UTC completion gate handles early calendar dates and year boundaries', () => {
  assert.equal(lastCompletedSeason(new Date('2026-01-01T00:00:00Z')), 2025);
  assert.equal(lastCompletedSeason(new Date('2026-06-30T23:59:59Z')), 2025);
  assert.equal(lastCompletedSeason(new Date('2026-07-01T00:00:00Z')), 2026);
  assert.throws(() => lastCompletedSeason(new Date('invalid')), /as-of date/);
});
test('both index and bundle reject incomplete or future ending seasons', () => {
  for (const validate of [(end, asOf) => validateBundle(seasonFixture(end), asOf), (end, asOf) => validateIndex(seasonIndex(end), asOf)]) {
    assert.doesNotThrow(() => validate(2025, new Date('2026-01-01T00:00:00Z')));
    assert.throws(() => validate(2026, new Date('2026-01-01T00:00:00Z')));
    assert.throws(() => validate(2026, new Date('2026-06-30T23:59:59Z')));
    assert.doesNotThrow(() => validate(2026, new Date('2026-07-01T00:00:00Z')));
    assert.throws(() => validate(2027, AS_OF));
  }
});
test('view loader applies its explicit as-of date to the loaded season', async () => {
  const index = validateIndex(seasonIndex(2026), AS_OF);
  const loader = createViewLoader(index, async () => seasonFixture(2026), new Date('2026-01-01T00:00:00Z'));
  await assert.rejects(loader.load('2026-1'), /season/);
});

test('team labels expand current and historical codes without guessing unknown names', () => {
  assert.equal(teamName('NYK', 2026), 'New York Knicks');
  assert.equal(teamName('BRK', 2026), 'Brooklyn Nets');
  assert.equal(teamName('CHA', 2014), 'Charlotte Bobcats');
  assert.equal(teamName('CHA', 2026), 'Charlotte Hornets');
  assert.equal(teamName('SYNTHETIC'), 'SYNTHETIC');
});
test('unavailable contrast reasons explain the observed-data restriction', () => {
  assert.match(readableReason('insufficient_bracketing'), /not enough nearby salaries on both sides/);
  assert.match(readableReason('outside_observed_range'), /outside the observed salary range/);
  assert.equal(readableReason('synthetic_unknown_reason'), 'Synthetic unknown reason');
});
test('initial extra-million position is supported when a supported knot exists', () => {
  const c = fixture().views[0].models.war.curve;
  c[0].extraMillion = contrast(false);
  assert.equal(initialContrastIndex(c), 1);
  c[1].extraMillion = contrast(false);
  assert.equal(initialContrastIndex(c), 0);
});
test('initial extra-million position chooses the middle supported knot across gaps', () => {
  const c = Array.from({ length: 7 }, (_, i) => ({ extraMillion: contrast([1, 3, 6].includes(i), .1) }));
  assert.equal(initialContrastIndex(c), 3);
});
test('nice y ticks include zero, stay inside the domain and use readable steps', () => {
  assert.deepEqual(niceTicks(-3.7, 15.8), [0, 5, 10, 15]);
  assert.deepEqual(niceTicks(-.11, .23), [-.1, 0, .1, .2]);
  assert.deepEqual(niceTicks(1.1, 9.2), [2, 4, 6, 8]);
});
test('nice y ticks handle negative and flat domains without fabricating zero outside them', () => {
  const ticks = niceTicks(-9, -1);
  assert.ok(ticks.length > 0 && ticks.every(t => t >= -9 && t <= -1));
  assert.ok(!ticks.includes(0));
  assert.deepEqual(niceTicks(1, 1), []);
  assert.deepEqual(niceTicks(-Infinity, Infinity), []);
});

test('default WAR-scale domain keeps readable five-win ticks on both sides of zero', () => {
  assert.deepEqual(niceTicks(-5.5, 14.7), [-5, 0, 5, 10]);
});

test('salary coverage distinguishes retained matched seasons from playing-time eligibility', () => {
  const view = fixture().views[0]; view.coverage.eligible = 9;
  const copy = salaryCoverage(view);
  assert.match(copy, /1 matched active player-seasons retained from 2 active source player-seasons/);
  assert.match(copy, /1 player-seasons have unknown salary/);
  assert.doesNotMatch(copy, /eligible/);
  assert.equal(copy.match(/unknown salary/g)?.length, 1);
});

test('typing an exact or unique match never commits a selection', () => {
  assert.equal(resolvePlayerSelection([{ id: '11' }], ''), '');
  assert.equal(resolvePlayerSelection([{ id: '11' }], '22'), '');
});
test('an explicit selected ID survives only while valid in the new cohort/filter', () => {
  assert.equal(resolvePlayerSelection([{ id: '22' }, { id: '11' }], '11'), '11');
  assert.equal(resolvePlayerSelection([{ id: '22' }], '11'), '');
  assert.equal(resolvePlayerSelection([], '11'), '');
});
test('phone chart labels show only the selected player while desktop retains automatic labels', () => {
  const source = fixture().views[0].players[0];
  const players = Array.from({ length: 8 }, (_, i) => ({ ...source, id: String(i + 1), label: true }));
  assert.deepEqual(chartLabels(players, '2', 375).map(p => p.id), ['2']);
  assert.deepEqual(chartLabels(players, '', 375), []);
  const desktop = chartLabels(players, '2', 800);
  assert.equal(desktop.length, 6); assert.equal(desktop.at(-1).id, '2');
  assert.equal(new Set(desktop.map(p => p.id)).size, desktop.length);
});

test('cached number formatters preserve rounding, negatives, large values and missing-value rejection', () => {
  for (const value of [1234.567, -1234.567, 987654321.5, -0, 0]) {
    assert.equal(decimal(value), value.toLocaleString('en-US', { maximumFractionDigits: 2, minimumFractionDigits: 2 }));
    assert.equal(money(value), value.toLocaleString('en-US', { style: 'currency', currency: 'USD', maximumFractionDigits: 0 }));
  }
  assert.equal(decimal(-1234.567), '-1,234.57');
  assert.equal(money(987654321.5), '$987,654,322');
  for (const missing of [null, undefined]) { assert.throws(() => decimal(missing), TypeError); assert.throws(() => money(missing), TypeError); }
});

test('desktop labels prioritize the three requested players before earlier outliers', () => {
  const source = fixture().views[0].players[0];
  const players = ['1', '2', '3', '4', '5', '6', '203999', '1641705', '1629029', '999'].map(id => ({ ...source, id, label: true }));
  const labels = chartLabels(players, '999', 800).map(p => p.id);
  assert.equal(labels.length, 6); assert.equal(labels.at(-1), '999');
  for (const id of ['203999', '1641705', '1629029']) assert.ok(labels.slice(0, 5).includes(id));
  assert.deepEqual(labels.slice(3, 5), ['1', '2']);
  assert.deepEqual(chartLabels(players, '999', 375).map(p => p.id), ['999']);
  players.find(p => p.id === '203999').label = false;
  assert.ok(!chartLabels(players, '', 800).some(p => p.id === '203999'));
});

test('value-for-pay ranks use exact tie midpoints, negatives and self without forced endpoints', () => {
  assert.deepEqual(valuePercentiles([]), []);
  assert.deepEqual(valuePercentiles([7]), [50]);
  assert.deepEqual(valuePercentiles([-2, -2, -2]), [50, 50, 50]);
  assert.deepEqual(valuePercentiles([-3, 0, 0, 2]), [12.5, 50, 50, 87.5]);
  assert.deepEqual(valuePercentiles([-3, -2, 0, 9]), [12.5, 37.5, 62.5, 87.5]);
  for (const invalid of [NaN, Infinity, -Infinity, null, undefined, '2']) assert.throws(() => valuePercentiles([0, invalid]));
});
test('stored ties differ from displayed ties and do not use an epsilon', () => {
  const x = .0574276985275538;
  assert.deepEqual(valuePercentiles([x, x + 1e-17]), [25, 75]);
  const stored = parseJSONBytes(new TextEncoder().encode('[-1,0.0574276985275538,0.0574276985275538,0.0574286985275538]'), 200);
  assert.deepEqual(valuePercentiles(stored), [12.5, 50, 50, 87.5]);
});
function rankedFixture(gaps = [-3, 0, 0, 2]) {
  const b = fixture(), v = b.views[0], source = v.players[0];
  const war = [...gaps].reverse();
  // Independent definition, intentionally not the production ranking helper.
  const rank = (values, r) => 100 * (values.filter(x => x < r).length + .5 * values.filter(x => x === r).length) / values.length;
  v.players = gaps.map((gap, i) => ({ ...structuredClone(source), id: String(999999990 + i), name: `SYNTHETIC ${i}`, teams: [i % 2 ? 'OTHER' : 'TEST'],
    lebron: -.5 + gap, war: -.5 + war[i], residual: { lebron: gap, war: war[i] },
    valuePercentile: { lebron: rank(gaps, gap), war: rank(war, war[i]) } }));
  Object.assign(v.coverage, { matched: gaps.length, eligible: gaps.length, eligiblePlayerSeasons: gaps.length, sourcePlayerSeasons: gaps.length + 1 });
  return b;
}
test('each metric and window validates its own full matched cohort ranks', () => {
  const b = rankedFixture();
  const other = rankedFixture([-2, 3, 8]).views[0];
  other.id = '2025-3'; other.window = 3; other.representation = 'player-window'; other.seasons = [2023, 2024, 2025];
  b.views.push(other);
  assert.doesNotThrow(() => validateBundle(b));
  assert.notEqual(b.views[0].players[0].valuePercentile.lebron, b.views[0].players[0].valuePercentile.war);
  assert.notEqual(b.views[0].players[0].valuePercentile.lebron, other.players[0].valuePercentile.lebron);
});
test('team and search selection retain the full-cohort scores and denominator', () => {
  const v = validateBundle(rankedFixture()).views[0];
  const original = structuredClone(v.players);
  const shown = filterPlayers(v, 'TEST', '  SYNTHETIC 0  ');
  assert.equal(resolvePlayerSelection(shown, ''), '');
  assert.equal(resolveSearchChoice(v, 'TEST', 'SYNTHETIC 0', shown[0].id)?.id, shown[0].id);
  assert.equal(shown[0].valuePercentile.lebron, 12.5);
  assert.equal(v.players.length, 4); assert.deepEqual(v.players, original);
});
for (const [label, mutate] of Object.entries({
  missing: p => { delete p.valuePercentile; },
  'missing metric': p => { delete p.valuePercentile.war; },
  nonfinite: p => { p.valuePercentile.war = NaN; },
  negative: p => { p.valuePercentile.war = -1; },
  'over 100': p => { p.valuePercentile.war = 101; },
  'wrong rank': p => { p.valuePercentile.war = 50; },
  'swapped metrics': p => { [p.valuePercentile.lebron, p.valuePercentile.war] = [p.valuePercentile.war, p.valuePercentile.lebron]; },
})) test(`rejects ${label} value percentile`, () => { const b = rankedFixture(); mutate(b.views[0].players[0]); assert.throws(() => validateBundle(b)); });
test('display rounding is not accepted as the stored percentile', () => {
  const b = rankedFixture([-2, 0, 4]); b.views[0].players[0].valuePercentile.lebron = 16.7;
  assert.throws(() => validateBundle(b), /value percentile/);
});
test('percentile formatting rounds only the display to one decimal', () => {
  assert.equal(percentile(100 / 6), '16.7'); assert.equal(percentile(50), '50.0'); assert.equal(percentile(87.5), '87.5');
  for (const invalid of [null, undefined, NaN, Infinity, -1, 101]) assert.throws(() => percentile(invalid));
});

test('selected comparison preserves native-unit gaps and distinguishes player and observed-season denominators', () => {
  const view = validateBundle(rankedFixture([-3, 0, 0, 2])).views[0];
  const player = filterPlayers(view, 'TEST', 'SYNTHETIC 0')[0];
  const annual = playerComparisonCopy(player, 'war', view);
  assert.equal(annual.percentile, '87.5 / 100'); assert.equal(annual.reference, '-0.50 WAR'); assert.equal(annual.gap, '+2.00 WAR');
  assert.match(annual.contribution, /1.50 WAR/); assert.match(annual.salary, /season/);
  assert.match(annual.cohort, /Among 4 eligible matched players/); assert.equal(annual.observed, '');
  const multi = structuredClone(view); multi.window = 3; multi.seasons = [2023, 2024, 2025];
  player.observedSeasons = [2023, 2025];
  const rate = playerComparisonCopy(player, 'lebron', multi);
  assert.equal(rate.percentile, '12.5 / 100'); assert.equal(rate.reference, '-0.50 points/100 possessions'); assert.equal(rate.gap, '−3.00 points/100 possessions');
  assert.equal(rate.contribution, '-3.50 points/100 possessions');
  assert.match(rate.cohort, /Among 4 eligible matched players/); assert.equal(rate.observed, 'Based on 2 of 3 seasons');
  assert.match(rate.salary, /average\/year/); assert.match(rate.details, /2022–23, 2024–25/);
  assert.match(rate.details, /minutes; .* possessions/);
  assert.match(playerComparisonCopy(player, 'war', multi).contribution, /WAR\/observed season/);
});

test('WAR control labels distinguish totals and averages without the long unit phrase', () => {
  assert.equal(warOptionLabel(1), 'Season contribution · WAR');
  assert.equal(warOptionLabel(3), 'Average season contribution · WAR');
  assert.equal(warOptionLabel(5), 'Average season contribution · WAR');
});

// Pixel coordinates and names below are synthetic collision fixtures.
const syntheticLabels = count => Array.from({ length: count }, (_, i) => ({ id: String(i), name: `SYNTHETIC PLAYER ${i}`, x: 460, y: 70 }));
const overlaps = (a, b) => a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top;
test('clustered labels use bounded nearby positions and omit unresolved lower-priority names', () => {
  const points = syntheticLabels(6), before = structuredClone(points);
  const labels = placeChartLabels(points, '', 800, 340);
  assert.ok(labels.length > 0 && labels.length < points.length);
  assert.equal(labels[0].id, points[0].id);
  for (let i = 0; i < labels.length; i++) for (let j = i + 1; j < labels.length; j++) assert.ok(!overlaps(labels[i], labels[j]));
  assert.deepEqual(points, before); assert.deepEqual(placeChartLabels(points, '', 800, 340), labels);
});
test('selected label wins a crowded position and is always preserved', () => {
  const labels = placeChartLabels(syntheticLabels(6), '5', 800, 340);
  assert.ok(labels.some(p => p.id === '5')); assert.equal(labels.at(-1).id, '5');
  for (let i = 0; i < labels.length; i++) for (let j = i + 1; j < labels.length; j++) assert.ok(!overlaps(labels[i], labels[j]));
});
test('label boxes stay inside plot bounds at top and side edges', () => {
  const points = [{ id: '1', name: 'SYNTHETIC EDGE', x: 46, y: 26 }, { id: '2', name: 'SYNTHETIC RIGHT', x: 786, y: 26 }];
  const labels = placeChartLabels(points, '2', 800, 340);
  assert.equal(labels.length, 2);
  for (const p of labels) assert.ok(p.left >= 46 && p.right <= 786 && p.top >= 26 && p.bottom <= 300);
});
test('phone selected labels remain available and long names fit without layout measurement', () => {
  const point = { id: '1', name: 'SYNTHETIC VERY LONG SELECTED PLAYER NAME', x: 200, y: 45 };
  const labels = placeChartLabels([point], '1', 300, 340);
  assert.equal(labels.length, 1); assert.equal(labels[0].name, point.name);
  assert.ok(labels[0].left >= 46 && labels[0].right <= 286);
  assert.ok(labels[0].textWidth <= 240);
});

function syntheticSearchView() {
  const view = rankedFixture([-3, 0, 0, 2]).views[0];
  const names = ['Joël SYNTHETIC', 'A. Jo-SYNTHETIC', 'Jokić SYNTHETIC', 'JOEL SYNTHETIC'];
  view.players.forEach((p, i) => { p.name = names[i]; });
  return view;
}
test('search follows the incumbent accent/punctuation normalization and compact matching', () => {
  assert.equal(normalizePlayerSearch('  JoëL—SYNTHETIC  '), 'joel synthetic');
  assert.equal(normalizePlayerSearch('Jo\u0065\u0308l'), 'joel');
  const view = syntheticSearchView();
  assert.deepEqual(playerSuggestions(view, '', 'joel').map(p => p.id), [view.players[0].id, view.players[3].id]);
  assert.deepEqual(playerSuggestions(view, '', 'ajosy').map(p => p.id), [view.players[1].id]);
  assert.equal(filterPlayers(view, '', 'JOKIC')[0].id, view.players[2].id);
});
test('suggestions require two normalized non-space characters and sort prefixes before substrings', () => {
  const view = syntheticSearchView(), before = structuredClone(view);
  for (const query of ['', ' ', '-', 'j', ' j- ', 'é', 'e\u0301-']) assert.deepEqual(playerSuggestions(view, '', query), []);
  const suggestions = playerSuggestions(view, '', 'jo');
  assert.deepEqual(suggestions.map(p => p.id), [view.players[0].id, view.players[3].id, view.players[2].id, view.players[1].id]);
  assert.deepEqual(playerSuggestions(view, '', 'jo'), suggestions);
  assert.deepEqual(view, before);
});
test('keyboard suggestion state wraps, requires an active option for Enter and clears on dismissal', () => {
  const menu = createPlayerSuggestions(), view = syntheticSearchView();
  menu.update(view, '', 'j'); menu.move(1);
  assert.equal(menu.state.open, false); assert.equal(menu.choice(), undefined);
  menu.update(view, '', 'jo');
  assert.equal(menu.state.open, true); assert.equal(menu.state.index, -1); assert.equal(menu.choice(), undefined);
  menu.move(-1); assert.equal(menu.choice()?.id, menu.state.matches.at(-1).id);
  menu.move(1); assert.equal(menu.state.index, 0);
  menu.move(-1); assert.equal(menu.state.index, menu.state.matches.length - 1);
  menu.close(); assert.equal(menu.state.open, false); assert.equal(menu.state.index, -1); assert.equal(menu.choice(), undefined); assert.deepEqual(menu.state.matches, []);
  menu.update(view, '', 'joel'); assert.equal(menu.choice(), undefined);
});
test('empty matches and changed context cannot retain a navigable stale option', () => {
  const menu = createPlayerSuggestions(), view = syntheticSearchView();
  menu.update(view, '', 'jo'); menu.move(1); assert.ok(menu.choice());
  menu.update(view, '', 'nothing synthetic'); menu.move(1);
  assert.equal(menu.state.open, true); assert.equal(menu.state.index, -1); assert.equal(menu.choice(), undefined);
  menu.update(view, '', 'jo'); menu.move(1);
  menu.update(null, '', 'jo'); assert.equal(menu.state.open, false); assert.equal(menu.choice(), undefined);
  menu.update(view, '', 'jo'); menu.move(1);
  menu.update(view, 'MISSING_SYNTHETIC_TEAM', 'jo'); assert.equal(menu.choice(), undefined); assert.equal(menu.state.index, -1);
});
test('committing a suggestion rechecks the current query team and view without changing cohort data', () => {
  const view = syntheticSearchView(), before = structuredClone(view), id = view.players[0].id;
  assert.equal(resolveSearchChoice(view, 'TEST', 'joel', id)?.id, id);
  assert.equal(resolveSearchChoice(view, 'OTHER', 'joel', id), undefined);
  assert.equal(resolveSearchChoice(view, '', 'jokic', id), undefined);
  assert.equal(resolveSearchChoice({ ...view, players: view.players.slice(1) }, '', 'joel', id), undefined);
  assert.equal(resolveSearchChoice(null, '', 'joel', id), undefined);
  assert.deepEqual(view, before);
});

test('salary ticks use readable millions or cap percentages within the unchanged domain', () => {
  const usd = salaryTicks(0, .47, 154647000, 'usd', 5);
  assert.deepEqual(usd.map(t => t.label), ['$0m', '$20m', '$40m', '$60m']);
  assert.ok(usd.every(t => t.x >= 0 && t.x <= .47));
  assert.ok(Math.abs(usd[1].x * 154647000 - 20000000) < 1e-8);
  const share = salaryTicks(0, .47, 154647000, 'share', 5);
  assert.deepEqual(share.map(t => t.label), ['0%', '10%', '20%', '30%', '40%']);
  assert.deepEqual(salaryTicks(.1, .1, 154647000, 'usd', 5), []);
});

test('extra-million comparison shows exact salary scenarios and signed native units', () => {
  const view = fixture().views[0], knot = structuredClone(view.models.war.curve[0]);
  knot.x = .07518518; knot.extraMillion = contrast(true, .1);
  const before = structuredClone(knot), result = salaryComparison(view, 'war', knot);
  assert.equal(result.lower, '$7,518,518'); assert.equal(result.higher, '$8,518,518');
  assert.equal(result.difference, '+0.10 estimated wins');
  assert.match(result.lowerContribution, /^About -0.25 estimated wins$/); assert.match(result.higherContribution, /^About -0.15 estimated wins$/);
  assert.match(result.aria, /Lower salary \$7,518,518.*higher salary \$8,518,518/);
  assert.match(result.support, /3 near the lower salary; 3 near the higher salary/);
  assert.deepEqual(knot, before);
  knot.extraMillion = contrast(true, -.25);
  assert.equal(salaryComparison(view, 'war', knot).difference, '−0.25 estimated wins');
  knot.extraMillion = contrast(true, 0);
  assert.equal(salaryComparison(view, 'war', knot).difference, '0.00 estimated wins');
});

test('extra-million rate and multi-year labels preserve their actual units and dollar basis', () => {
  const view = fixture().views[0], knot = structuredClone(view.models.lebron.curve[0]);
  knot.extraMillion = contrast(true, .02);
  assert.equal(salaryComparison(view, 'lebron', knot).difference, '+0.02 impact points/100 possessions');
  view.window = 3;
  assert.match(salaryComparison(view, 'war', knot).difference, /estimated wins\/observed season/);
  assert.match(salaryComparison(view, 'war', knot).context, /Average annual salary.*cap-equivalent/);
});

test('unsupported comparisons never carry zero results and null ranges remain unavailable', () => {
  const view = fixture().views[0], knot = structuredClone(view.models.war.curve[0]);
  knot.extraMillion = contrast(false); knot.extraMillion.reason = 'insufficient_bracketing';
  const unsupported = salaryComparison(view, 'war', knot);
  assert.match(unsupported.difference, /Comparison unavailable.*not enough nearby salaries/);
  assert.doesNotMatch(unsupported.difference, /0\.00/); assert.equal(unsupported.range, 'Unavailable');
  knot.extraMillion = { ...contrast(true, -.25), low: null, high: null };
  const noRange = salaryComparison(view, 'war', knot);
  assert.equal(noRange.difference, '−0.25 estimated wins'); assert.equal(noRange.range, 'Unavailable');
});

// All guided-player and comparison observations here are synthetic fixtures.
test('guided examples use only labeled eligible players from the active team without mutation', () => {
  const view = fixture().views[0];
  view.players = Array.from({ length: 6 }, (_, i) => ({ ...structuredClone(view.players[0]), id: String(i), name: `Synthetic ${i}`, teams: [i === 0 ? 'OTHER' : 'TEST'], label: i !== 1 }));
  const before = structuredClone(view);
  assert.deepEqual(examplePlayers(view, 'TEST').map(p => p.id), ['2', '3', '4']);
  assert.equal(examplePlayers(view, '').length, 3);
  assert.deepEqual(examplePlayers(view, 'ABSENT'), []); assert.deepEqual(view, before);
  view.players.forEach(p => p.label = false); assert.deepEqual(examplePlayers(view, ''), []);
});
test('selected reference and signed gap follow the visible interpolant and preserve stored rank', () => {
  const view = fixture().views[0], p = view.players[0];
  // A stored fitted value can differ within the boundary tolerance; presentation uses the painted line.
  p.fitted.war = -0.50000000001; p.residual.war = -1.49999999999;
  const before = structuredClone(p), copy = playerComparisonCopy(p, 'war', view);
  assert.equal(copy.reference, '-0.50 WAR'); assert.equal(copy.gap, '−1.50 WAR');
  assert.equal(copy.percentile, '50.0 / 100'); assert.deepEqual(p, before);
  p.war = -.5; assert.equal(playerComparisonCopy(p, 'war', view).gap, '0.00 WAR');
  view.window = 3; assert.match(playerComparisonCopy(p, 'war', view).reference, /WAR\/observed season/);
});
test('slider starting amount is exact and retained for unsupported comparisons', () => {
  const view = fixture().views[0], knot = view.models.war.curve[2];
  assert.equal(salaryComparison(view, 'war', knot).start, '$20,000,000');
  assert.equal(salaryComparison(view, 'war', knot).difference.startsWith('Comparison unavailable'), true);
});

test('paired salary estimates interpolate the visible curve, retain negative rates and preserve exported rounding', () => {
  const view = fixture().views[0];
  const knot = view.models.lebron.curve[0];
  knot.extraMillion = contrast(true, .07563);
  // Synthetic endpoint values intentionally round differently from the saved difference.
  view.models.lebron.curve[0].y = -.632708; view.models.lebron.curve[1].y = .123592;
  const before = structuredClone(view), copy = salaryComparison(view, 'lebron', knot);
  assert.equal(copy.lowerContribution, 'About -0.63 impact points/100 possessions');
  assert.equal(copy.higherContribution, 'About -0.56 impact points/100 possessions');
  assert.equal(copy.difference, '+0.08 impact points/100 possessions');
  assert.match(copy.precision, /exported difference: 0.07563/); assert.match(copy.unitNote, /not points actually scored/);
  assert.deepEqual(view, before);
  view.window = 3; const multi = salaryComparison(view, 'war', view.models.war.curve[0]);
  assert.match(multi.lowerContribution, /estimated wins\/observed season/);
  assert.match(multi.context, /WAR.*Average annual salary.*cap-equivalent/);
});
test('unsupported scenarios retain exact salaries but hide both endpoints and joint results', () => {
  const view = fixture().views[0], knot = view.models.war.curve[2];
  knot.extraMillion.reason = 'insufficient_bracketing';
  const copy = salaryComparison(view, 'war', knot);
  assert.equal(copy.lower, '$20,000,000'); assert.equal(copy.higher, '$21,000,000');
  for (const field of ['lowerContribution', 'higherContribution', 'range', 'precision']) assert.equal(copy[field], 'Unavailable');
  assert.match(copy.difference, /not enough nearby salaries/);
  assert.equal([copy.lowerContribution, copy.higherContribution, copy.difference, copy.range, copy.precision].join(' ').match(/not enough nearby salaries/g).length, 1);
  const supported = view.models.war.curve[0]; supported.extraMillion.low = null; supported.extraMillion.high = null;
  const noRange = salaryComparison(view, 'war', supported);
  assert.match(noRange.lowerContribution, /^About/); assert.match(noRange.higherContribution, /^About/); assert.equal(noRange.range, 'Unavailable');
});
test('constant-step explanation is conditional on the authoritative model name', () => {
  const view = fixture().views[0], knot = view.models.war.curve[0];
  assert.doesNotMatch(salaryComparison(view, 'war', knot).shape, /same at every/);
  view.models.war.name = 'Straight line'; assert.match(salaryComparison(view, 'war', knot).shape, /same at every supported salary.*straight line/);
  view.models.war.name = 'Unconstrained smooth'; assert.doesNotMatch(salaryComparison(view, 'war', knot).shape, /same at every/);
});
