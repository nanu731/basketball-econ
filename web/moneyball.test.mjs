// Explicitly synthetic schema fixtures, never published NBA observations.
import test from 'node:test';
import assert from 'node:assert/strict';
import { validateBundle as validateBundleAt, interpolate, validateIndex as validateIndexAt, validateViewBundle as validateViewBundleAt, createViewLoader as createViewLoaderAt, parseJSONBytes, readJSONResponse, INDEX_BYTE_LIMIT, VIEW_BYTE_LIMIT, lastCompletedSeason, teamName, readableReason, initialContrastIndex, niceTicks, salaryCoverage, resolvePlayerSelection, chartLabels, money, decimal } from './moneyball.ts';
const AS_OF = new Date('2026-10-23T12:00:00Z');
const validateBundle = (input, asOf = AS_OF) => validateBundleAt(input, asOf);
const validateIndex = (input, asOf = AS_OF) => validateIndexAt(input, asOf);
const validateViewBundle = (input, expected, version, asOf = AS_OF) => validateViewBundleAt(input, expected, version, asOf);
const createViewLoader = (index, read, asOf = AS_OF) => createViewLoaderAt(index, read, asOf);
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

test('a unique nonempty name search selects its player without another action', () => {
  assert.equal(resolvePlayerSelection([{ id: '11' }], '', 'Synthetic name'), '11');
  assert.equal(resolvePlayerSelection([{ id: '11' }], '22', 'Synthetic name'), '11');
});
test('multiple search matches require an explicit choice and preserve a valid prior choice', () => {
  const shown = [{ id: '11' }, { id: '22' }];
  assert.equal(resolvePlayerSelection(shown, '', 'Synthetic'), '');
  assert.equal(resolvePlayerSelection(shown, '22', 'Synthetic'), '22');
});
test('an empty search does not auto-select the only remaining player', () => {
  assert.equal(resolvePlayerSelection([{ id: '11' }], '', ''), '');
  assert.equal(resolvePlayerSelection([{ id: '11' }], '', '  '), '');
});
test('selected NBA identity survives a changed view only while it passes the new filters', () => {
  const nextViewRows = [{ id: '22' }, { id: '11' }];
  assert.equal(resolvePlayerSelection(nextViewRows, '11', ''), '11');
  assert.equal(resolvePlayerSelection([{ id: '22' }], '11', ''), '');
  assert.equal(resolvePlayerSelection([], '11', 'Synthetic'), '');
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
