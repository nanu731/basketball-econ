/** Shared build/browser boundary. Source values and fitted results come from R. */
export type Metric = 'lebron' | 'war';
export interface Contrast { supported: boolean; estimate: number | null; low: number | null; high: number | null; nStart: number; nEnd: number; reason: string | null }
export interface Knot { x: number; y: number; low: number | null; high: number | null; extraMillion: Contrast }
export interface Model { name: string; method: string; validation: string; intervalLabel: string; curve: Knot[] }
export interface GapScale { method: 'median-absolute-visible-gap-v1'; cohortN: number; scale: number | null; status: 'available' | 'unavailable'; reason: 'zero-scale' | 'invalid-scale' | 'unrepresentable-ratio' | null }
export interface Player { id: string; name: string; teams: string[]; observedSeasons: number[]; minutes: number; possessions: number; salaryCapShare: number; salaryUsd: number; lebron: number; war: number; fitted: Record<Metric, number>; residual: Record<Metric, number>; valuePercentile: Record<Metric, number>; gapMultiple: Record<Metric, number | null>; label: boolean; flags: string[] }
export interface Team { team: string; salaryUsd: number | null; war: number | null; status: 'attributable-sample' | 'unavailable'; attributablePlayerSeasons: number; eligiblePlayerSeasons: number; excludedTradeCount: number; excludedMissingCount: number; note: string }
export interface View { id: string; endSeason: number; window: 1 | 3 | 5; seasons: number[]; referenceCapUsd: number; representation: string; coverage: { eligible: number; matched: number; includedKnownSalaryUsd: number; allKnownActiveSalaryUsd: number; unknownSalaryPlayerSeasons: number; sourcePlayerSeasons: number; eligiblePlayerSeasons: number; excluded: { reason: string; count: number }[]; note: string }; players: Player[]; gapSize: Record<Metric, GapScale>; models: Record<Metric, Model>; teams: Team[] }
export interface Bundle { schemaVersion: 'moneyball.v1'; dataVersion: string; defaultView: string; publication: { status: 'private-preview' | 'cleared'; note: string }; sources: { label: string; url: string; version: string; retrieved: string }[]; definitions: { salary: string; eligibility: string; uncertainty: string; limitations: string[] }; views: View[] }
export interface IndexView { id: string; endSeason: number; window: 1 | 3 | 5; url: string }
export interface ViewIndex { schemaVersion: 'moneyball.index.v1'; dataVersion: string; defaultView: string; views: IndexView[] }
export const INDEX_URL = '/data/moneyball/v1/index.json';
export const INDEX_BYTE_LIMIT = 100_000;
export const VIEW_BYTE_LIMIT = 2_000_000;
const metrics: Metric[] = ['lebron', 'war'];
const fail = (reason: string): never => { throw new Error(`Invalid Moneyball data: ${reason}`); };
const obj = (v: unknown): any => v !== null && typeof v === 'object' && !Array.isArray(v) ? v : fail('object required');
const str = (v: unknown, max = 5000): string => typeof v === 'string' && v.trim().length > 0 && v.length <= max ? v : fail('text required');
const num = (v: unknown): number => typeof v === 'number' && Number.isFinite(v) ? v : fail('finite number required');
const nonneg = (v: unknown): number => num(v) >= 0 ? v as number : fail('negative amount');
const int = (v: unknown): number => Number.isSafeInteger(v) && nonneg(v) === v ? v as number : fail('nonnegative integer required');
const arr = (v: unknown, max = 10000): any[] => Array.isArray(v) && v.length <= max ? v : fail('bounded array required');
const unique = (v: unknown[]): void => { if (new Set(v).size !== v.length) fail('duplicate identity'); };
const close = (a: number, b: number): boolean => Number.isFinite(a) && Number.isFinite(b) && Number.isFinite(a - b) && Math.abs(a - b) <= 1e-6 * Math.max(1, Math.abs(a), Math.abs(b));
const nullablePair = (lo: unknown, hi: unknown): void => {
  if (lo === null && hi === null) return;
  if (num(lo) > num(hi)) fail('interval endpoint order');
};
/** Exact stored-value ties; a full cohort is rejected if any residual is invalid. */
export function valuePercentiles(residuals: number[]): number[] {
  const values = arr(residuals).map(num);
  const sorted = [...values].sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
  const ranks = new Map<number, number>();
  for (let start = 0; start < sorted.length;) {
    let end = start + 1;
    while (end < sorted.length && sorted[end] === sorted[start]) end++;
    ranks.set(sorted[start], 100 * (start + .5 * (end - start)) / sorted.length);
    start = end;
  }
  return values.map(value => ranks.get(value)!);
}
/** Independent full-cohort check of R's descriptive score, not a distributional model. */
export function gapSizes(residuals: number[]): { metadata: GapScale; multiples: (number | null)[] } {
  const values = arr(residuals).map(num); if (!values.length) fail('empty gap-size cohort');
  // R's canonical jsonlite encoding uses 15 significant digits; guard its overflow boundary too.
  if (values.some(r => !Number.isFinite(Number(r.toPrecision(15))))) fail('gap-size cohort cannot be encoded as finite JSON');
  const ordered = values.map(Math.abs).sort((a, b) => a < b ? -1 : a > b ? 1 : 0), mid = Math.floor(ordered.length / 2);
  const a = ordered[Math.max(0, mid - 1)], b = ordered[mid];
  const scale = ordered.length % 2 ? b : a <= Number.MAX_VALUE - b ? (a + b) / 2 : a / 2 + b / 2;
  let reason: GapScale['reason'] = !Number.isFinite(scale) ? 'invalid-scale' : scale <= 0 ? 'zero-scale' : null;
  const ratios = reason ? [] : values.map(r => r / scale);
  if (!reason && ratios.some((ratio, i) => !Number.isFinite(ratio) || !Number.isFinite(Number(ratio.toPrecision(15))) || (values[i] !== 0 && ratio === 0))) reason = 'unrepresentable-ratio';
  return { metadata: { method: 'median-absolute-visible-gap-v1', cohortN: values.length, scale: reason ? null : scale, status: reason ? 'unavailable' : 'available', reason }, multiples: values.map((_, i) => reason ? null : ratios[i]) };
}
const sameStoredNumber = (actual: unknown, expected: number): boolean =>
  typeof actual === 'number' && Number.isFinite(actual) && (expected === 0 ? actual === 0 : Math.abs(actual / expected - 1) <= 1e-12);

export function interpolate(curve: Knot[], x: number): number {
  if (!Number.isFinite(x) || x < curve[0].x || x > curve[curve.length - 1].x) return NaN;
  const j = curve.findIndex(k => k.x >= x);
  if (j === 0) return curve[0].y;
  const a = curve[j - 1], b = curve[j];
  const rise = b.y - a.y, offset = x - a.x, run = b.x - a.x;
  const value = a.y + rise * (offset / run);
  return [rise, offset, run, value].every(Number.isFinite) ? value : NaN;
}
/** Conservative completion gate: a season ending this calendar year opens July 1 UTC. */
export function lastCompletedSeason(asOf: Date = new Date()): number {
  if (!(asOf instanceof Date) || !Number.isFinite(asOf.getTime())) fail('as-of date');
  return asOf.getUTCFullYear() - (asOf.getUTCMonth() < 6 ? 1 : 0);
}
/** Shared by build files and streamed browser responses; limits count UTF-8 bytes. */
export function parseJSONBytes(bytes: Uint8Array, limit: number): unknown {
  if (bytes.byteLength > limit) fail('payload byte limit');
  return JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes));
}
export async function readJSONResponse(response: Response, limit: number): Promise<unknown> {
  if (!response.ok || !response.body) fail('missing response body');
  const length = response.headers.get('Content-Length');
  if (length !== null && (!/^\d+$/.test(length) || !Number.isSafeInteger(Number(length)) || Number(length) > limit)) {
    await response.body.cancel(); fail('response length');
  }
  const reader = response.body.getReader(), chunks: Uint8Array[] = [];
  let total = 0;
  try {
    while (true) {
      const { value, done } = await reader.read();
      if (done) break;
      if (value.byteLength > limit - total) { await reader.cancel(); fail('payload byte limit'); }
      chunks.push(value); total += value.byteLength;
    }
  } finally { reader.releaseLock(); }
  const bytes = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) { bytes.set(chunk, offset); offset += chunk.byteLength; }
  return parseJSONBytes(bytes, limit);
}
export function validateBundle(input: unknown, asOf: Date = new Date()): Bundle {
  const b = obj(input);
  // Reject provider salary-derived outcomes anywhere, even in otherwise unused metadata.
  const forbidden = /fair.?salary|surplus|wins.?over.?contract/i;
  const scan = (v: unknown, depth = 0): void => {
    if (depth > 16) fail('excessive nesting');
    if (v && typeof v === 'object') for (const [k, value] of Object.entries(v)) { if (forbidden.test(k)) fail('salary-derived outcome'); scan(value, depth + 1); }
  };
  scan(b);
  if (b.schemaVersion !== 'moneyball.v1') fail('schema version');
  str(b.dataVersion, 200); str(b.defaultView, 30);
  const publication = obj(b.publication);
  if (!['private-preview', 'cleared'].includes(publication.status)) fail('publication status');
  str(publication.note);
  const sources = arr(b.sources, 30); if (!sources.length) fail('sources missing');
  sources.forEach(s => { obj(s); str(s.label); str(s.version); if (!/^\d{4}-\d{2}-\d{2}$/.test(str(s.retrieved, 10))) fail('retrieval date'); const u = new URL(str(s.url)); if (u.protocol !== 'https:' || u.username || u.password) fail('source URL'); });
  const d = obj(b.definitions); ['salary', 'eligibility', 'uncertainty'].forEach(k => str(d[k])); arr(d.limitations, 40).forEach(x => str(x));
  const views = arr(b.views, 200); if (!views.length) fail('no views'); unique(views.map(v => obj(v).id));
  views.forEach(v => {
    obj(v); const end = int(v.endSeason); if (end < 2010 || end > lastCompletedSeason(asOf)) fail('season');
    if (![1, 3, 5].includes(v.window) || v.id !== `${end}-${v.window}`) fail('window identity');
    if (v.representation !== (v.window === 1 ? 'player-season' : 'player-window')) fail('representation');
    if (arr(v.seasons, 5).length !== v.window || v.seasons.some((s: unknown, i: number) => s !== end - v.window + i + 1)) fail('window seasons');
    if (num(v.referenceCapUsd) <= 0) fail('cap');
    const c = obj(v.coverage); int(c.eligible); int(c.matched); str(c.note);
    nonneg(c.includedKnownSalaryUsd); nonneg(c.allKnownActiveSalaryUsd); int(c.unknownSalaryPlayerSeasons); int(c.sourcePlayerSeasons); int(c.eligiblePlayerSeasons);
    if (c.includedKnownSalaryUsd > c.allKnownActiveSalaryUsd || c.unknownSalaryPlayerSeasons > c.sourcePlayerSeasons || c.eligiblePlayerSeasons > c.sourcePlayerSeasons) fail('salary coverage accounting'); arr(c.excluded, 100).forEach(e => { obj(e); str(e.reason); int(e.count); });
    const players = arr(v.players); if (!players.length || players.length !== c.matched || c.eligible < c.matched) fail('matched coverage'); unique(players.map(p => obj(p).id));
    obj(v.models);
    metrics.forEach(m => {
      const model = obj(v.models[m]); ['name', 'method', 'validation', 'intervalLabel'].forEach(k => str(model[k]));
      const curve = arr(model.curve, 2000); if (curve.length < 2) fail('curve missing');
      curve.forEach((k, i) => {
        obj(k); nonneg(k.x); num(k.y); if (i && (k.x <= curve[i - 1].x || !Number.isFinite(k.y - curve[i - 1].y))) fail('curve order or arithmetic');
        nullablePair(k.low, k.high); if ((k.low === null) !== (curve[0].low === null)) fail('mixed curve interval availability');
        const e = obj(k.extraMillion); int(e.nStart); int(e.nEnd);
        if (e.supported === true) {
          if (!e.nStart || !e.nEnd || e.reason !== null) fail('contrast support');
          num(e.estimate); nullablePair(e.low, e.high);
        } else if (e.supported === false) {
          if (e.estimate !== null || e.low !== null || e.high !== null) fail('unsupported contrast carries result'); str(e.reason);
        } else fail('contrast support flag');
      });
      curve.forEach(k => {
        if (k.extraMillion.supported) {
          const finish = num(k.x + num(1e6 / v.referenceCapUsd));
          if (finish > curve[curve.length - 1].x || !close(k.extraMillion.estimate, interpolate(curve, finish) - k.y)) fail('contrast differs from visible reference');
        }
      });
    });
    players.forEach(p => {
      obj(p); if (!/^\d+$/.test(str(p.id, 20))) fail('NBA identity'); str(p.name, 200);
      const teams = arr(p.teams, 40); if (!teams.length) fail('player teams'); teams.forEach(t => str(t, 30)); unique(teams);
      const seasons = arr(p.observedSeasons, 5); if (!seasons.length || seasons.some(s => !v.seasons.includes(s))) fail('observed seasons'); unique(seasons);
      nonneg(p.minutes); nonneg(p.possessions); nonneg(p.salaryCapShare); nonneg(p.salaryUsd);
      if (!close(p.salaryUsd, p.salaryCapShare * v.referenceCapUsd)) fail('money units');
      obj(p.fitted); obj(p.residual); obj(p.valuePercentile); obj(p.gapMultiple); if (typeof p.label !== 'boolean') fail('label flag'); arr(p.flags, 30).forEach(f => str(f));
      metrics.forEach(m => { num(p[m]); num(p.fitted[m]); num(p.residual[m]); if (!close(p.fitted[m], interpolate(v.models[m].curve, p.salaryCapShare)) || !close(p.residual[m], p[m] - p.fitted[m])) fail('visible reference or residual mismatch'); });
    });
    metrics.forEach(m => {
      const magnitude = gapSizes(players.map(p => p.residual[m])), scale = obj(obj(v.gapSize)[m]), expectedScale = magnitude.metadata;
      if (scale.method !== expectedScale.method || scale.cohortN !== expectedScale.cohortN || scale.status !== expectedScale.status || scale.reason !== expectedScale.reason ||
          (expectedScale.scale === null ? scale.scale !== null : !sameStoredNumber(scale.scale, expectedScale.scale))) fail('gap-size scale differs from full matched cohort');
      players.forEach((p, i) => {
        const expected = magnitude.multiples[i], actual = p.gapMultiple[m];
        if (expected === null ? actual !== null : !sameStoredNumber(actual, expected)) fail('gap-size multiple differs from full matched cohort');
      });
      const expected = valuePercentiles(players.map(p => p.residual[m]));
      players.forEach((p, i) => {
        const score = num(p.valuePercentile[m]);
        // This tolerance covers JSON encoding of the score, never residual ties.
        if (score < 0 || score > 100 || Math.abs(score - expected[i]) > 1e-12) fail('value percentile differs from full matched cohort');
      });
    });
    const teams = arr(v.teams, 40); unique(teams.map(t => obj(t).team));
    teams.forEach(t => {
      str(t.team, 30); int(t.attributablePlayerSeasons); int(t.eligiblePlayerSeasons);
      if (t.status === 'unavailable') {
        if (t.salaryUsd !== null || t.war !== null) fail('unavailable team carries totals');
      } else if (t.status === 'attributable-sample') {
        if (t.attributablePlayerSeasons === 0) fail('finite team totals need attributed observations');
        nonneg(t.salaryUsd); num(t.war);
      } else fail('team attribution status');
      int(t.excludedTradeCount); int(t.excludedMissingCount); str(t.note);
      if (t.attributablePlayerSeasons > t.eligiblePlayerSeasons) fail('team coverage');
    });
  });
  if (!views.some(v => v.id === b.defaultView)) fail('default view unavailable');
  return b as Bundle;
}
/** The manifest lists availability only; unloaded observations have not been checked. */
export function validateIndex(input: unknown, asOf: Date = new Date()): ViewIndex {
  const index = obj(input);
  if (index.schemaVersion !== 'moneyball.index.v1') fail('index schema');
  str(index.dataVersion, 200); str(index.defaultView, 30);
  const views = arr(index.views, 200); if (!views.length) fail('empty index');
  unique(views.map(v => obj(v).id));
  views.forEach(v => {
    const end = int(v.endSeason);
    if (end < 2010 || end > lastCompletedSeason(asOf) || ![1, 3, 5].includes(v.window) || v.id !== `${end}-${v.window}`) fail('index view identity');
    if (v.url !== `/data/moneyball/v1/views/${v.id}.json`) fail('index URL');
  });
  if (!views.some(v => v.id === index.defaultView)) fail('index default unavailable');
  return index as ViewIndex;
}
export function validateViewBundle(input: unknown, expected: IndexView, dataVersion: string, asOf: Date = new Date()): Bundle {
  const bundle = validateBundle(input, asOf);
  if (bundle.dataVersion !== dataVersion || bundle.views.length !== 1 || bundle.defaultView !== expected.id) fail('view version or identity');
  const view = bundle.views[0];
  if (view.id !== expected.id || view.endSeason !== expected.endSeason || view.window !== expected.window) fail('view differs from index');
  return bundle;
}
/** Used by the UI: stale completions can populate the validated cache but never render. */
export function createViewLoader(index: ViewIndex, read: (url: string) => Promise<unknown>, asOf: Date = new Date()) {
  let generation = 0;
  const cache = new Map<string, Bundle>();
  return {
    cancel() { generation++; },
    async load(id: string): Promise<Bundle | null> {
      const ticket = ++generation;
      const expected = index.views.find(v => v.id === id);
      if (!expected) fail('requested view unavailable');
      try {
        let bundle = cache.get(id);
        if (!bundle) { bundle = validateViewBundle(await read(expected.url), expected, index.dataVersion, asOf); cache.set(id, bundle); }
        return ticket === generation ? bundle : null;
      } catch (error) { if (ticket === generation) throw error; return null; }
    },
  };
}
export const seasonLabel = (end: number): string => `${end - 1}–${String(end).slice(-2)}`;
export const viewLabel = (v: View): string => v.window === 1 ? seasonLabel(v.endSeason) : `${seasonLabel(v.seasons[0])} to ${seasonLabel(v.endSeason)} · ${v.window} seasons`;
// Reuse formatters: rebuilding locale/currency rules for every dot slows filtering.
const decimalFormatter = new Intl.NumberFormat('en-US', { maximumFractionDigits: 2, minimumFractionDigits: 2 });
const moneyFormatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', maximumFractionDigits: 0 });
const compactMoneyFormatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', notation: 'compact', maximumFractionDigits: 2 });
const preciseSalaryFormatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', minimumFractionDigits: 0, maximumFractionDigits: 2 });
const percentileFormatter = new Intl.NumberFormat('en-US', { maximumFractionDigits: 1, minimumFractionDigits: 1 });
export const percentile = (v: number): string => {
  const score = num(v); if (score < 0 || score > 100) fail('percentile range');
  return percentileFormatter.format(score);
};
export const decimal = (v: number): string => {
  if (v == null) throw new TypeError('Cannot format a missing number');
  return decimalFormatter.format(v);
};
export const money = (v: number): string => {
  if (v == null) throw new TypeError('Cannot format missing compensation');
  return moneyFormatter.format(v);
};
export const salaryCoverage = (v: View): string => { const c = v.coverage; return `Known active compensation included: ${money(c.includedKnownSalaryUsd)} of ${money(c.allKnownActiveSalaryUsd)} (${c.allKnownActiveSalaryUsd > 0 ? `${decimal(c.includedKnownSalaryUsd / c.allKnownActiveSalaryUsd * 100)}%` : 'coverage undefined: no known-dollar denominator'}). ${c.unknownSalaryPlayerSeasons} player-seasons have unknown salary; ${c.eligiblePlayerSeasons} matched active player-seasons retained from ${c.sourcePlayerSeasons} active source player-seasons. ${v.window > 1 ? 'Both dollar totals are annual ending-season cap-equivalent amounts across the full window. ' : ''}This is known matched-sample coverage, not whole payroll.`; };
export const metricLabel = (m: Metric, v?: View): string => m === 'lebron' ? 'LEBRON · points / 100 possessions' : v && v.window > 1 ? 'WAR · average / observed season' : 'WAR · season total';
export const warOptionLabel = (window: View['window']): string => window === 1 ? 'Wins incl. playing time' : 'Avg wins incl. playing time';

function gapNumber(value: number): string {
  const magnitude = Math.abs(num(value));
  return magnitude !== 0 && (magnitude < .005 || magnitude >= 1e6) ? Number(magnitude.toPrecision(3)).toString() : decimal(magnitude);
}
export function gapMultiple(value: number | null, withUnit = true): string {
  return value === null ? 'Unavailable' : `${value > 0 ? '+' : value < 0 ? '−' : ''}${gapNumber(value)}${withUnit ? ' × typical league gap' : ''}`;
}
export function gapScaleCopy(metric: Metric, view: View): string {
  const scale = view.gapSize[metric];
  const unit = metric === 'war' ? view.window > 1 ? 'WAR/observed season' : 'WAR' : 'impact points/100 possessions';
  return scale.status === 'available' ? `Typical league gap (median distance): about ${gapNumber(scale.scale!)} ${unit}.` : `Gap size unavailable: ${scale.reason === 'zero-scale' ? 'the median distance from the line is zero' : scale.reason === 'unrepresentable-ratio' ? 'the ratio cannot be represented safely' : 'the scale is invalid'}.`;
}

export function normalizePlayerSearch(value: string): string {
  return value.normalize('NFD').replace(/\p{M}/gu, '').toLocaleLowerCase('en-US')
    .replace(/[\p{P}\p{S}]+/gu, ' ').replace(/\s+/g, ' ').trim();
}
const compactSearch = (value: string): string => normalizePlayerSearch(value).replace(/\s/g, '');
const suggestionsReady = (query: string): boolean => Array.from(compactSearch(query)).length >= 2;
export function filterPlayers(view: View, team: string, query: string): Player[] {
  const search = normalizePlayerSearch(query), compact = compactSearch(query);
  return view.players.filter(p => (!team || p.teams.includes(team)) &&
    (normalizePlayerSearch(p.name).includes(search) || compactSearch(p.name).includes(compact)));
}
export function sortGapRows(players: Player[], metric: Metric): Player[] {
  return [...players].sort((a, b) => a.residual[metric] === b.residual[metric] ? a.id.localeCompare(b.id, 'en') : a.residual[metric] > b.residual[metric] ? -1 : 1);
}

export function playerSuggestions(view: View, team: string, query: string): Player[] {
  if (!suggestionsReady(query)) return [];
  const search = normalizePlayerSearch(query), compact = compactSearch(query);
  const starts = (p: Player) => normalizePlayerSearch(p.name).startsWith(search) || compactSearch(p.name).startsWith(compact);
  const compare = (a: string, b: string) => a < b ? -1 : a > b ? 1 : 0;
  return filterPlayers(view, team, query).sort((a, b) => Number(starts(b)) - Number(starts(a)) ||
    compare(normalizePlayerSearch(a.name), normalizePlayerSearch(b.name)) || compare(a.id, b.id));
}
/** Shared menu state for keyboard handling and context/failure dismissal. */
export function createPlayerSuggestions() {
  let state = { open: false, matches: [] as Player[], index: -1 };
  return {
    get state(): Readonly<{ open: boolean; matches: readonly Player[]; index: number }> { return state; },
    update(view: View | null, team: string, query: string) {
      state = { open: !!view && suggestionsReady(query), matches: view ? playerSuggestions(view, team, query) : [], index: -1 };
    },
    close() { state = { open: false, matches: [], index: -1 }; },
    move(step: -1 | 1) {
      if (!state.open || !state.matches.length) return;
      state = { ...state, index: state.index < 0 ? step === 1 ? 0 : state.matches.length - 1 :
        (state.index + step + state.matches.length) % state.matches.length };
    },
    choice(): Player | undefined { return state.open && state.index >= 0 ? state.matches[state.index] : undefined; },
  };
}
export function resolveSearchChoice(view: View | null, team: string, query: string, id: string): Player | undefined {
  return view ? filterPlayers(view, team, query).find(p => p.id === id) : undefined;
}
/** Read existing scores; presentation and filters never calculate a new rank. */
export function playerComparisonCopy(p: Player, metric: Metric, view: View, units: 'usd' | 'share' = 'usd') {
  const reference = interpolate(view.models[metric].curve, p.salaryCapShare);
  const gap = p[metric] - reference;
  const contributionUnit = metric === 'war' ? view.window > 1 ? 'estimated wins per observed season' : 'estimated wins' : 'impact points/100 possessions';
  const values = { contribution: decimal(p[metric]), reference: decimal(reference), gap: `${gap > 0 ? '+' : gap < 0 ? '−' : ''}${decimal(Math.abs(gap))}` };
  return {
    name: p.name, values,
    unit: `${metric.toUpperCase()} · ${contributionUnit}${metric === 'war' ? ' · includes playing time' : ' · impact when playing'}`,
    direction: gap > 0 ? 'Above salary reference' : gap < 0 ? 'Below salary reference' : 'On salary reference',
    salary: units === 'share' ? `${decimal(p.salaryCapShare * 100)}%` : compactMoneyFormatter.format(p.salaryUsd),
    salaryBasis: units === 'share' ? view.window > 1 ? 'Average share of each observed season’s cap' : 'Share of the season cap' : view.window > 1 ? `Average annual ${seasonLabel(view.endSeason)} cap-equivalent dollars` : 'Nominal dollars for this season',
    contribution: `${values.contribution} ${contributionUnit}`,
    reference: `${values.reference} ${contributionUnit}`,
    gap: `${values.gap} ${contributionUnit}`,
    percentile: `${percentile(p.valuePercentile[metric])}th percentile of league gaps`,
    magnitude: gapMultiple(p.gapMultiple[metric]), scale: gapScaleCopy(metric, view),
    cohort: `Among ${view.players.length} eligible matched players`,
    observed: view.window > 1 ? `Based on ${p.observedSeasons.length} of ${view.window} seasons` : '',
    details: `${metric.toUpperCase()} · ${viewLabel(view)}. ${p.teams.map(t => teamName(t, view.endSeason)).join(', ')}. Observed seasons: ${p.observedSeasons.map(seasonLabel).join(', ')}. ${decimal(p.minutes)} minutes; ${decimal(p.possessions)} possessions. Salary: ${preciseSalaryFormatter.format(p.salaryUsd)} ${view.window > 1 ? "average annual ending-season cap-equivalent dollars" : "nominal dollars"}; ${decimal(p.salaryCapShare * 100)}% ${view.window > 1 ? "average share of each observed season’s cap" : "of the season cap"}. ${p.flags.map(readableReason).join('. ')}`,
  };
}

const TEAM_NAMES: Record<string, string> = {
  ATL: 'Atlanta Hawks', BOS: 'Boston Celtics', BKN: 'Brooklyn Nets', BRK: 'Brooklyn Nets',
  CHA: 'Charlotte Hornets', CHO: 'Charlotte Hornets', CHI: 'Chicago Bulls', CLE: 'Cleveland Cavaliers',
  DAL: 'Dallas Mavericks', DEN: 'Denver Nuggets', DET: 'Detroit Pistons', GSW: 'Golden State Warriors',
  HOU: 'Houston Rockets', IND: 'Indiana Pacers', LAC: 'Los Angeles Clippers', LAL: 'Los Angeles Lakers',
  MEM: 'Memphis Grizzlies', MIA: 'Miami Heat', MIL: 'Milwaukee Bucks', MIN: 'Minnesota Timberwolves',
  NJN: 'New Jersey Nets', NOH: 'New Orleans Hornets', NOP: 'New Orleans Pelicans', NYK: 'New York Knicks',
  OKC: 'Oklahoma City Thunder', ORL: 'Orlando Magic', PHI: 'Philadelphia 76ers', PHO: 'Phoenix Suns',
  PHX: 'Phoenix Suns', POR: 'Portland Trail Blazers', SAC: 'Sacramento Kings', SAS: 'San Antonio Spurs',
  TOR: 'Toronto Raptors', UTA: 'Utah Jazz', WAS: 'Washington Wizards',
};
export function teamName(code: string, endSeason?: number): string {
  if (code === 'CHA' && endSeason !== undefined && endSeason <= 2014) return 'Charlotte Bobcats';
  return TEAM_NAMES[code] ?? code;
}
export function readableReason(reason: string | null): string {
  const known: Record<string, string> = {
    insufficient_bracketing: 'There are not enough nearby salaries on both sides of the two amounts being compared.',
    outside_observed_range: 'One of the two salary amounts falls outside the observed salary range.',
    insufficient_local_support: 'Too few player records have salaries near one or both amounts.',
    missing_salary: 'The salary record is missing.',
    missing_active_salary: 'An active season has no matched salary record.',
    missing_contribution: 'The contribution estimate is missing.',
    unknown_team_allocation: 'The salary and contribution could not be assigned to one team.',
    traded_unknown_allocation: 'A trade prevents assigning the full salary and contribution to one team.',
  };
  if (!reason) return 'No explanation was supplied for this missing comparison.';
  if (known[reason]) return known[reason];
  // Preserve unrecognized reasons without inventing a statistical explanation.
  const words = reason.replace(/[_-]+/g, ' ').trim();
  return words.charAt(0).toUpperCase() + words.slice(1);
}
/** Regular, readable ticks within the unchanged full-sample domain. */
export function niceTicks(min: number, max: number, count = 5): number[] {
  const rawStep = (max - min) / Math.max(1, count - 1);
  if (![min, max, rawStep].every(Number.isFinite) || rawStep <= 0) return [];
  const power = 10 ** Math.floor(Math.log10(rawStep)), fraction = rawStep / power;
  // Midpoints select the nearest readable step rather than always rounding up.
  const multiple = fraction < 1.5 ? 1 : fraction < 2.25 ? 2 : fraction < 3.75 ? 2.5 : fraction < 7.5 ? 5 : 10;
  const step = power * multiple;
  if (!Number.isFinite(step) || step <= 0) return [];
  const first = Math.ceil(min / step), last = Math.floor(max / step), ticks: number[] = [];
  for (let i = first; i <= last && ticks.length < 20; i++) ticks.push(Number((i * step).toPrecision(12)));
  if (min <= 0 && max >= 0 && !ticks.includes(0)) ticks.push(0);
  return ticks.sort((a, b) => a - b);
}

export function salaryTicks(min: number, max: number, cap: number, units: string, count: number) {
  const scale = units === 'share' ? 100 : cap / 1e6;
  return niceTicks(min * scale, max * scale, count).map(value => ({
    x: value / scale, label: units === 'share' ? `${value}%` : `$${value}m`,
  }));
}

/** Summarize only an explicitly straight-line reference using a saved supported contrast. */
export function millionSummary(view: View, metric: Metric) {
  const model = view.models[metric], knot = model.curve.find(k => k.extraMillion.supported);
  const unit = metric === 'war' ? view.window > 1 ? 'estimated wins/observed season' : 'estimated wins' : 'impact points/100 possessions';
  const pay = view.window === 1 ? 'annual salary' : `average annual pay in ${seasonLabel(view.endSeason)} cap-equivalent dollars`;
  const context = `${viewLabel(view)} · ${metric.toUpperCase()} · ${pay} (US dollars). ${model.name}.${model.name !== 'Straight line' ? ' This is a local comparison; a single constant step is not assumed.' : ''}`;
  const signed = (value: number) => `${value > 0 ? '+' : value < 0 ? '−' : ''}${gapNumber(value)}`;
  if (!knot) return {
    summary: `The $1 million comparison is unavailable in this view: ${readableReason(model.curve[0].extraMillion.reason)}`,
    context, anchor: 'Unavailable', difference: 'Unavailable', range: 'Unavailable', support: 'Unavailable', precision: 'Unavailable',
  };
  const result = knot.extraMillion, difference = `${signed(num(result.estimate))} ${unit}`;
  return {
    summary: model.name === 'Straight line' ? `In ${viewLabel(view)}, $1 million more in ${pay} corresponds to about ${difference} on the fitted league line; this association does not predict the effect of a raise.` : 'This view does not use a straight-line reference, so a single $1 million association is not summarized.',
    context,
    anchor: `${preciseSalaryFormatter.format(knot.x * view.referenceCapUsd)} → ${preciseSalaryFormatter.format(knot.x * view.referenceCapUsd + 1e6)}`,
    difference,
    range: result.low === null ? 'Unavailable' : `${signed(result.low)} to ${signed(result.high!)} ${unit}`,
    support: `${result.nStart} near the lower salary; ${result.nEnd} near the higher salary`,
    precision: `Exported difference: ${result.estimate}; joint range: ${result.low === null ? 'unavailable' : `${result.low} to ${result.high}`}.`,
  };
}

/** Preserve only an explicit choice that remains valid; typing never commits. */
export function resolvePlayerSelection(shown: Pick<Player, 'id'>[], current: string): string {
  return shown.some(p => p.id === current) ? current : '';
}
const REQUESTED_LABEL_IDS = new Set(['203999', '1641705', '1629029']);
export function chartLabels(shown: Player[], selected: string, width: number): Player[] {
  const chosen = shown.find(p => p.id === selected);
  const automatic = width >= 500 ? shown.filter(p => p.label && p.id !== selected)
    .sort((a, b) => Number(REQUESTED_LABEL_IDS.has(b.id)) - Number(REQUESTED_LABEL_IDS.has(a.id))).slice(0, 5) : [];
  // The selected name is painted last so it stays above automatic desktop labels.
  return chosen ? [...automatic, chosen] : automatic;
}

/** Guided choices come only from labeled eligible players in the loaded team/view. */
export function examplePlayers(view: View, team: string): Player[] {
  return chartLabels(filterPlayers(view, team, ''), '', 800).slice(0, 3);
}

interface LabelPoint { id: string; name: string; x: number; y: number }
interface PlacedLabel extends LabelPoint { left: number; right: number; top: number; bottom: number; textWidth: number; compressed: boolean }
/** At most six labels and three nearby positions each; no DOM measurements. */
export function placeChartLabels(points: LabelPoint[], selected: string, width: number, height: number): PlacedLabel[] {
  const L = 46, R = width - 14, T = 26, B = height - 40;
  const chosen = points.find(p => p.id === selected);
  const automatic = points.filter(p => p.id !== selected).slice(0, 5);
  const ordered = chosen ? [chosen, ...automatic] : automatic;
  const placed: PlacedLabel[] = [];
  for (const point of ordered) {
    // shortcut: conservative 14px text bounds; use font metrics if collisions recur.
    const fullWidth = point.name.length * 8.75 + 4, textWidth = Math.min(fullWidth, R - L);
    const right = point.x > width * (width < 500 ? .5 : .7);
    const left = Math.max(L, Math.min(R - textWidth, point.x + (right ? -9 - textWidth : 9)));
    for (const offset of [-10, -36, 20]) {
      const baseline = Math.max(T + 16, Math.min(B - 5, point.y + offset));
      const candidate = { ...point, x: left + 2, y: baseline, left, right: left + textWidth, top: baseline - 16, bottom: baseline + 5, textWidth, compressed: fullWidth > textWidth };
      if (placed.some(p => candidate.left < p.right + 2 && candidate.right + 2 > p.left && candidate.top < p.bottom + 2 && candidate.bottom + 2 > p.top)) continue;
      placed.push(candidate); break;
    }
  }
  // It was reserved first; paint the selected name last as well.
  return placed.sort((a, b) => Number(a.id === selected) - Number(b.id === selected));
}

export function mountMoneyball(root: HTMLElement): void {
  const get = <T extends HTMLElement>(key: string): T => root.querySelector<T>(`[data-mb-${key}]`)!;
  // Put the project skip link before site navigation without editing the shared header.
  document.body.prepend(get('skip'));
  const controls = get<HTMLFieldSetElement>('controls'), status = get('status');
  const season = get<HTMLSelectElement>('season'), windowSelect = get<HTMLSelectElement>('window');
  const metric = get<HTMLSelectElement>('metric'), units = get<HTMLSelectElement>('units');
  const search = get<HTMLInputElement>('search'), team = get<HTMLSelectElement>('team');
  const searchShell = get('search-shell'), suggestions = get('suggestions'), suggestionModel = createPlayerSuggestions();
  const chart = get<SVGSVGElement & HTMLElement>('chart');
  const table = get<HTMLTableSectionElement>('rows');
  const records = get<HTMLDetailsElement>('live-table');
  let tableDirty = true;
  let index: ViewIndex | null = null, bundle: Bundle | null = null, active: View | null = null, selected = '', exampleSelection = '', request = 0;
  let loader: ReturnType<typeof createViewLoader> | null = null;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = (name: string, attrs: Record<string, string | number> = {}, text?: string): SVGElement => {
    const el = document.createElementNS(ns, name); Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v))); if (text !== undefined) el.textContent = text; return el;
  };
  const txt = (key: string, text: string) => { get(key).textContent = text; };
  const option = (value: string, label: string) => { const o = document.createElement('option'); o.value = value; o.textContent = label; return o; };
  function setControls(ready: boolean) {
    controls.disabled = !index; get<HTMLButtonElement>('reset').disabled = !index; get('guide').hidden = !ready; get('period').hidden = !ready; get('caption').hidden = !ready;
    for (const el of [metric, units, search, team, get<HTMLButtonElement>('find-player')]) el.disabled = !ready;
    for (const link of root.querySelectorAll<HTMLAnchorElement>('[data-mb-return]')) {
      link.href = ready ? '#mb-player-comparison' : '#mb-snapshot-title';
      link.textContent = ready ? 'Back to player comparison' : 'Back to saved records';
    }
  }
  function renderSuggestions(scroll = false) {
    const state = suggestionModel.state;
    suggestions.hidden = !state.open; search.setAttribute('aria-expanded', String(state.open));
    search.removeAttribute('aria-activedescendant');
    if (!state.open) { suggestions.replaceChildren(); return; }
    const options = state.matches.map((p, index) => {
      const option = document.createElement('div'); option.id = `${suggestions.id}-option-${p.id}`;
      option.className = 'suggestion-option'; option.setAttribute('role', 'option');
      option.setAttribute('aria-selected', String(index === state.index)); option.dataset.suggestionIndex = String(index);
      const name = document.createElement('span'), context = document.createElement('small');
      name.dataset.suggestionName = ''; name.textContent = p.name;
      context.textContent = p.teams.map(t => teamName(t, active!.endSeason)).join(' · ');
      option.append(name, context); return option;
    });
    if (!options.length) {
      const empty = document.createElement('p'); empty.className = 'no-results'; empty.setAttribute('role', 'option');
      empty.setAttribute('aria-disabled', 'true'); empty.setAttribute('aria-selected', 'false');
      empty.textContent = 'No players found. Try another name, team or season.'; suggestions.replaceChildren(empty);
    } else suggestions.replaceChildren(...options);
    const chosen = state.matches[state.index];
    if (chosen) {
      const id = `${suggestions.id}-option-${chosen.id}`; search.setAttribute('aria-activedescendant', id);
      if (scroll) suggestions.querySelector<HTMLElement>(`[data-suggestion-index="${state.index}"]`)?.scrollIntoView({ block: 'nearest' });
    }
  }
  function closeSuggestions() { suggestionModel.close(); renderSuggestions(); }
  function searchStatus() {
    const chosen = active?.players.find(p => p.id === selected);
    const count = active ? filterPlayers(active, team.value, search.value).length : 0;
    txt('search-result', chosen ? `${chosen.id === exampleSelection ? 'Example' : 'Selected'} ${chosen.name} · ${playerComparisonCopy(chosen, metric.value as Metric, active!).gap} vs salary reference.` : !suggestionsReady(search.value) ?
      '' : count === 0 ?
      'No players found. Try another name, team or season.' : suggestionModel.state.open ?
      `${count} ${count === 1 ? 'match' : 'matches'}. Use the arrow keys and Enter, or choose a name.` : 'Choose a suggested name to select a player.');
  }
  function updateSuggestions() {
    suggestionModel.update(search.disabled ? null : active, team.value, search.value);
    renderSuggestions(); searchStatus();
  }
  function commitPlayer(id: string, fromSearch = false, asExample = false): boolean {
    const entry = resolveSearchChoice(active, team.value, fromSearch ? search.value : '', id);
    if (!entry) { closeSuggestions(); searchStatus(); return false; }
    selected = entry.id; exampleSelection = asExample ? entry.id : ''; search.value = entry.name;
    if (fromSearch) search.focus({ preventScroll: true });
    closeSuggestions(); render(); return true;
  }
  function failure(message: string, preserveChoice = false) {
    if (!preserveChoice) { selected = ''; exampleSelection = ''; search.value = ''; }
    closeSuggestions();
    bundle = null; active = null; setControls(false);
    chart.replaceChildren(); table.replaceChildren(); tableDirty = true;
    txt('search-result', message.startsWith('Loading') ? 'Checking the selected view…' : 'Search is unavailable until a view loads.');
    get('view-comparison').hidden = true; get('no-matches').hidden = true;
    txt('empty-title', message.startsWith('Loading') ? 'Loading player comparison…' : 'Player comparison unavailable');
    txt('empty-copy', message.startsWith('Loading') ? 'The selected records are being checked.' : 'Retry or change the season. Saved records are available below.');
    get('retry').hidden = message.startsWith('Loading');
    txt('selection-name', 'Player comparison'); get('selection-name').hidden = true; txt('selection-context', ''); get('selection-context').hidden = true; get('selection').hidden = false; get('comparison').hidden = true;
    for (const key of ['selected-unit', 'selected-direction', 'selected-salary-basis', 'selected-salary', 'selected-contribution', 'selected-reference', 'selected-gap', 'selected-percentile', 'selected-magnitude', 'selected-scale', 'selected-cohort', 'selected-observed', 'selected-details']) txt(key, '');
    for (const key of ['gap', 'contribution', 'reference', 'salary']) get(`selected-${key}`).closest('dd')!.removeAttribute('aria-label');
    txt('selection', 'Player details unavailable.'); txt('team-summary', ''); get('team-summary').hidden = true; get('team-audit-panel').hidden = true; txt('team-audit', 'No team audit is loaded.');
    for (const field of ['summary', 'context', 'anchor', 'difference', 'range', 'support', 'precision']) txt(`million-${field}`, '');
    txt('count', 'No interactive player records');
    get('example-buttons').replaceChildren(); get('examples').hidden = true;
    get('selected-key').hidden = true; get('peer-key').dataset.hollow = 'false';
    txt('metric-note', 'Contribution baseline appears after this view loads.'); txt('period', 'Regular seasons · awaiting selected view'); txt('coverage-summary', '');
    txt('coverage', ''); txt('model', ''); txt('view-evidence', 'No interactive view loaded.');
    for (const field of ['view-method', 'view-validation', 'view-interval']) txt(field, '');
    txt('source-context', 'Interactive sources unavailable; saved sources are in the technical records.');
    get('view-sources').replaceChildren(); get('view-source-versions').replaceChildren();
    txt('y-label', 'Contribution · awaiting selected view'); txt('x-label', 'Salary · awaiting selected view');
    status.textContent = message; get('empty').hidden = false; get('live-table').hidden = true; get('snapshot').hidden = false;
  }
  async function readJSON(path: string, limit = VIEW_BYTE_LIMIT): Promise<unknown> {
    const url = new URL(path, location.origin); if (url.origin !== location.origin) throw new Error('origin');
    const response = await fetch(url, { credentials: 'same-origin', cache: 'no-store', redirect: 'error' });
    return readJSONResponse(response, limit);
  }
  async function loadIndex() {
    const ticket = ++request; loader?.cancel(); index = null; loader = null;
    failure('Loading available seasons and windows…');
    try {
      const catalog = validateIndex(await readJSON(INDEX_URL, INDEX_BYTE_LIMIT)); if (ticket !== request) return;
      index = catalog; loader = createViewLoader(catalog, readJSON);
      const initial = catalog.views.find(v => v.id === catalog.defaultView)!;
      season.replaceChildren(...[...new Set(catalog.views.map(v => v.endSeason))].sort((a, b) => b - a).map(y => option(String(y), seasonLabel(y))));
      season.value = String(initial.endSeason); windowSelect.value = String(initial.window); metric.value = 'war'; units.value = 'usd'; search.value = ''; selected = '';
      await changeView();
      showDefaultExample();
    } catch { if (ticket === request) failure('The season index is unavailable or invalid. Retry or read the labeled default build snapshot below.'); }
  }
  function showDefaultExample() {
    const example = active && active.id === index?.defaultView && metric.value === 'war' && units.value === 'usd' && !team.value && !search.value ? examplePlayers(active, '')[0] : undefined;
    if (example) commitPlayer(example.id, false, true);
  }
  async function changeView() {
    if (!index || !loader) return;
    const allowed = index.views.filter(v => v.endSeason === Number(season.value));
    for (const o of windowSelect.options) o.disabled = !allowed.some(v => v.window === Number(o.value));
    const requestedWindow = Number(windowSelect.value), adjustedWindow = !allowed.some(v => v.window === requestedWindow);
    if (adjustedWindow) windowSelect.value = String(allowed[0].window);
    const desired = allowed.find(v => v.window === Number(windowSelect.value))!;
    failure(`Loading ${seasonLabel(desired.endSeason)} · ${desired.window}-season window…`, true);
    txt('window-note', `Requested: ending ${seasonLabel(desired.endSeason)}, ${desired.window}-season window. Results appear only after this view passes validation.`);
    try {
      const data = await loader.load(desired.id); if (!data) return;
      bundle = data; active = data.views[0];
      setControls(true); get('empty').hidden = true; get('snapshot').hidden = true; get('live-table').hidden = false;
      status.textContent = `${data.publication.status === 'private-preview' ? 'Private preview' : 'Reviewed export'}${adjustedWindow ? `. The ${requestedWindow}-season span is unavailable here; showing ${active.window} season${active.window === 1 ? '' : 's'}.` : ''}`;
      get('view-sources').replaceChildren(...data.sources.map(source => {
        const li = document.createElement('li'), link = document.createElement('a');
        link.href = source.url; link.rel = 'noreferrer'; link.textContent = source.label;
        li.append(link); return li;
      }));
      get('view-source-versions').replaceChildren(...data.sources.map(source => {
        const li = document.createElement('li'), code = document.createElement('code'); code.textContent = source.version;
        li.append(`${source.label} · retrieved ${source.retrieved} · `, code); return li;
      }));
      txt('source-context', `Sources for ${viewLabel(active)}.`);
      const previousTeam = team.value; team.replaceChildren(option('', 'All teams'), ...[...new Set(active.players.flatMap(p => p.teams))].sort((a, b) => teamName(a, active!.endSeason).localeCompare(teamName(b, active!.endSeason))).map(t => option(t, teamName(t, active!.endSeason))));
      team.value = [...team.options].some(o => o.value === previousTeam) ? previousTeam : '';
      const retained = active.players.find(p => p.id === selected && (!team.value || p.teams.includes(team.value)));
      selected = retained?.id ?? ''; search.value = retained?.name ?? '';
      txt('window-note', active.window === 1 ? '' : 'Averages cover available matched seasons; check each player’s observed count.');
      txt('coverage', `${active.coverage.matched} matched salary records among ${active.coverage.eligible} player records meeting playing-time and contribution-metric rules. ${salaryCoverage(active)}${active.coverage.excluded.length ? ` Primary exclusion reasons: ${active.coverage.excluded.map(e => `${e.count}: ${readableReason(e.reason)}`).join('; ')}` : ''}`);
      renderExamples(); render();
    } catch { failure(`${seasonLabel(desired.endSeason)} view unavailable or failed validation. Retry or change season; saved records remain below.`); }
  }
  function rows(): Player[] { return active ? filterPlayers(active, team.value, selected ? '' : search.value) : []; }
  function render() {
    if (!active) return;
    const shown = rows(), m = metric.value as Metric;
    selected = resolvePlayerSelection(shown, selected); searchStatus();
    txt('count', `${shown.length} ${search.value && !selected ? 'search matches' : 'chart players'} · ${active.players.length} matched league players`);
    get('no-matches').hidden = shown.length > 0;
    get('war-help').hidden = m !== 'war'; get('lebron-help').hidden = m !== 'lebron';
    metric.options[0].textContent = warOptionLabel(active.window);
    metric.options[1].textContent = 'Impact when playing';
    txt('season-label', active.window > 1 ? 'Season ending' : 'Season');
    const context = `${viewLabel(active)} · Regular season${active.window === 1 ? '' : 's · averages over available matched seasons'}`;
    txt('period', ` · ${context}`);
    txt('metric-note', m === 'war' ? 'Zero: a readily available fill-in player.' : 'Zero: league-average impact.');
    txt('coverage-summary', `League coverage: ${active.coverage.matched} matched of ${active.coverage.eligible} eligible player records.`);
    get('selected-key').hidden = !selected; get('peer-key').dataset.hollow = String(!!selected);
    for (const button of get('example-buttons').querySelectorAll<HTMLButtonElement>('[data-player]')) button.setAttribute('aria-pressed', String(button.dataset.player === selected));
    txt('y-label', m === 'war' ? active.window > 1 ? 'Estimated wins · average per observed season' : 'Estimated wins · season total' : 'Impact points per 100 possessions');
    txt('x-label', units.value === 'share' ? active.window === 1 ? 'Salary · % of season cap' : 'Salary · average % of observed season caps' : active.window === 1 ? 'Salary · US dollars' : `Average annual salary · ${seasonLabel(active.endSeason)} cap-equivalent dollars`);
    txt('model', active.models[m].curve[0].low === null ? 'No estimated range for the league curve is supplied.' : 'The shaded range concerns the fitted league curve.');
    if (bundle) txt('view-evidence', `${viewLabel(active)} · ${m.toUpperCase()} · ${active.models[m].name} · ${bundle.dataVersion}`);
    txt('view-method', active.models[m].method); txt('view-interval', active.models[m].intervalLabel);
    get('view-validation').replaceChildren(...active.models[m].validation.split(' | ').map(clause => { const li = document.createElement('li'); li.textContent = clause; return li; }));
    const t = active.teams.find(t => t.team === team.value);
    const shownPlayers = `${shown.length} ${shown.length === 1 ? 'player' : 'players'}`;
    get('team-summary').hidden = !team.value;
    get('team-audit-panel').hidden = !team.value || !t;
    txt('team-summary', !team.value ? '' : !t || t.status === 'unavailable' ? `${teamName(team.value, active.endSeason)} · matched sample only: ${shownPlayers} in the chart; attributable totals unavailable.` : `${teamName(t.team, active.endSeason)} · matched sample only: ${shownPlayers} in the chart; ${t.attributablePlayerSeasons} assigned player-seasons contribute ${money(t.salaryUsd!)} and ${decimal(t.war!)} WAR ${active.window === 1 ? 'in this season' : `per year across the ${active.window}-season span`}.`);
    if (team.value) get('team-summary').append(document.createTextNode(' Dots use each player’s full matched record across teams in this span. Partial team totals use only assignable records across the whole matched team sample and do not change with player search.'));
    txt('team-audit', t ? `Matched team sample: ${t.attributablePlayerSeasons} assigned of ${t.eligiblePlayerSeasons} matched player-seasons. Broader active-source team rows have ${t.excludedTradeCount} trade/unknown-allocation flags and ${t.excludedMissingCount} missing-data flags. Those audit counts may overlap and include ineligible source rows; do not add them as separate groups or subtract them from the chart-player count. ${t.note}` : 'No team audit is loaded.');
    // Hidden rows are built on demand instead of on every search keystroke.
    tableDirty = true;
    if (records.open) renderTable();
    else if (table.childElementCount) table.replaceChildren();
    txt('table-cost', units.value === 'share' ? 'Pay (% cap)' : active.window === 1 ? 'Pay (USD)' : 'Avg pay (end-cap USD)');
    txt('table-contribution', metricLabel(m, active)); txt('table-residual', `Gap (${m.toUpperCase()})`); txt('table-order', `${viewLabel(active)} · ${metricLabel(m, active)}. Gap = measured contribution minus salary reference; highest first.`);
    const p = active.players.find(p => p.id === selected);
    get('view-comparison').hidden = !p; txt('selection-context', p ? context : ''); get('selection-context').hidden = !p;
    txt('selection-name', p ? `${p.id === exampleSelection ? 'Example: ' : ''}${p.name}` : 'Player comparison'); get('selection-name').hidden = !p; get('selection').hidden = !!p; get('comparison').hidden = !p;
    if (p) {
      const copy = playerComparisonCopy(p, m, active, units.value as 'usd' | 'share');
      for (const field of ['salary', 'salaryBasis', 'unit', 'direction', 'percentile', 'magnitude', 'scale', 'cohort', 'observed', 'details'] as const) txt(`selected-${field === 'salaryBasis' ? 'salary-basis' : field}`, copy[field]);
      for (const field of ['contribution', 'reference', 'gap'] as const) { txt(`selected-${field}`, copy.values[field]); get(`selected-${field}`).closest('dd')!.setAttribute('aria-label', copy[field]); }
      get('selected-salary').closest('dd')!.setAttribute('aria-label', `${copy.salary}. ${copy.salaryBasis}`);
      get('selected-observed').hidden = active.window === 1;
    } else { txt('selection', 'Select a dot, choose an example or search for a player.'); txt('selected-magnitude', ''); txt('selected-scale', ''); }
    draw(); millionReadout();
  }
  function renderExamples() {
    const players = active ? examplePlayers(active, team.value) : [];
    get('examples').hidden = !players.length;
    get('example-buttons').replaceChildren(...players.map(p => {
      const button = document.createElement('button'); button.type = 'button'; button.dataset.player = p.id; button.textContent = p.name; return button;
    }));
  }
  function renderTable() {
    if (!active || !records.open || !tableDirty) return;
    const m = metric.value as Metric, shown = sortGapRows(rows(), m);
    table.replaceChildren(...shown.map(p => {
      const tr = document.createElement('tr'), name = document.createElement('th'); name.scope = 'row';
      const b = document.createElement('button'); b.type = 'button'; b.textContent = p.name; b.dataset.player = p.id; b.setAttribute('aria-pressed', String(p.id === selected)); name.append(b); tr.append(name);
      for (const [index, value] of [decimal(p.residual[m]), units.value === 'share' ? `${decimal(p.salaryCapShare * 100)}%` : compactMoneyFormatter.format(p.salaryUsd), decimal(p[m]), p.teams.join(', '), `${p.observedSeasons.length}/${active!.window}`].entries()) { const td = document.createElement('td'); td.textContent = value; if (index !== 3) td.className = 'mb-number'; tr.append(td); }
      return tr;
    }));
    if (!shown.length) { const tr = document.createElement('tr'); const td = document.createElement('td'); td.colSpan = 6; td.textContent = 'No players match these filters. Clear the search or reset the view.'; tr.append(td); table.append(tr); }
    tableDirty = false;
  }
  function clearHover() { chart.querySelector('.mb-hover')?.remove(); }
  function draw() {
    clearHover();
    if (!active) return;
    const m = metric.value as Metric, curve = active.models[m].curve, all = active.players, shown = rows();
    const bounds = chart.getBoundingClientRect(), width = Math.max(260, Math.round(bounds.width)), height = Math.max(340, Math.round(bounds.height));
    chart.setAttribute('viewBox', `0 0 ${width} ${height}`);
    const L = 46, R = 14, T = 26, B = 40, xmax = curve[curve.length - 1].x, xmin = Math.min(0, curve[0].x);
    const values = [...all.map(p => p[m]), ...curve.flatMap(k => [k.y, k.low ?? k.y, k.high ?? k.y])];
    const min = Math.min(0, ...values), max = Math.max(0, ...values), span = max - min || 1, ymin = min - span * .08, ymax = max + span * .08;
    const x = (v: number) => L + (v - xmin) / (xmax - xmin || 1) * (width - L - R), y = (v: number) => T + (ymax - v) / (ymax - ymin) * (height - T - B);
    chart.setAttribute('aria-label', `Salary and ${metricLabel(m, active)}`);
    const children: SVGElement[] = [svg('desc', {}, 'Dots represent matched players. The solid line is the fixed salary reference. A selected player has a vertical connector to that same line. Use player search or the table for keyboard access.')];
    for (const v of niceTicks(ymin, ymax)) children.push(svg('line', { x1: L, x2: width - R, y1: y(v), y2: y(v), class: 'mb-grid' }), svg('text', { x: L - 7, y: y(v) + 4, 'text-anchor': 'end' }, String(Number(v.toPrecision(8)))));
    for (const tick of salaryTicks(xmin, xmax, active.referenceCapUsd, units.value, width < 500 ? 3 : 5)) children.push(svg('text', { x: x(tick.x), y: height - 13, 'text-anchor': tick.x === xmin ? 'start' : x(tick.x) > width - 40 ? 'end' : 'middle' }, tick.label));
    children.push(svg('line', { x1: L, x2: width - R, y1: y(0), y2: y(0), class: 'mb-zero' }));
    const path = (points: [number, number][]) => points.map(([a, b], i) => `${i ? 'L' : 'M'}${x(a)},${y(b)}`).join(' ');
    if (curve[0].low !== null) children.push(svg('path', { d: `${path(curve.map(k => [k.x, k.low!]))} ${curve.slice().reverse().map(k => `L${x(k.x)},${y(k.high!)}`).join(' ')} Z`, class: 'mb-band' }));
    const playerDot = (p: Player) => {
      const dot = svg('circle', { cx: x(p.salaryCapShare), cy: y(p[m]), r: p.id === selected ? 6 : 3.5, class: p.id === selected ? 'mb-dot mb-selected' : selected ? 'mb-dot mb-peer' : 'mb-dot', 'data-player': p.id, role: 'img', 'aria-label': `${p.name}: ${decimal(p[m])}; ${money(p.salaryUsd)}` });
      return dot;
    };
    for (const p of shown) if (p.id !== selected) children.push(playerDot(p));
    const referencePath = path(curve.map(k => [k.x, k.y]));
    children.push(svg('path', { d: referencePath, class: 'mb-reference-halo' }), svg('path', { d: referencePath, class: 'mb-reference' }));
    const chosen = shown.find(p => p.id === selected);
    if (chosen) {
      const reference = interpolate(curve, chosen.salaryCapShare);
      children.push(svg('line', { x1: x(chosen.salaryCapShare), x2: x(chosen.salaryCapShare), y1: y(chosen[m]), y2: y(reference), class: 'mb-gap' }),
        svg('circle', { cx: x(chosen.salaryCapShare), cy: y(reference), r: 3.5, class: 'mb-gap-base' }), playerDot(chosen));
    }
    const labels = chartLabels(shown, selected, width).map(p => ({ id: p.id, name: p.name, x: x(p.salaryCapShare), y: y(p[m]) }));
    for (const p of placeChartLabels(labels, selected, width, height)) {
      const attrs: Record<string, string | number> = { x: p.x, y: p.y, 'text-anchor': 'start', class: p.id === selected ? 'mb-name mb-selected-name' : 'mb-name' };
      if (p.compressed) { attrs.textLength = Math.max(1, p.textWidth - 4); attrs.lengthAdjust = 'spacingAndGlyphs'; }
      children.push(svg('text', attrs, p.name));
    }
    chart.replaceChildren(...children);
  }
  function millionReadout() {
    if (!active) return;
    const copy = millionSummary(active, metric.value as Metric);
    for (const field of ['summary', 'context', 'anchor', 'difference', 'range', 'support', 'precision'] as const) txt(`million-${field}`, copy[field]);
  }
  root.querySelector('form')?.addEventListener('submit', e => e.preventDefault());
  get('retry').addEventListener('click', () => { void (index ? changeView() : loadIndex()); });
  season.addEventListener('change', changeView); windowSelect.addEventListener('change', changeView);
  metric.addEventListener('change', () => { closeSuggestions(); render(); });
  units.addEventListener('change', () => { closeSuggestions(); render(); });
  team.addEventListener('change', () => {
    closeSuggestions();
    if (selected && active && !active.players.some(p => p.id === selected && (!team.value || p.teams.includes(team.value)))) { selected = ''; search.value = ''; }
    renderExamples(); render();
  });
  search.addEventListener('input', () => { selected = ''; exampleSelection = ''; render(); updateSuggestions(); });
  search.addEventListener('focus', updateSuggestions); search.addEventListener('click', updateSuggestions);
  search.addEventListener('keydown', event => {
    if (event.isComposing || search.disabled || !active) return;
    if (event.key === 'Tab' || event.key === 'Escape') {
      if (event.key === 'Escape') event.preventDefault(); closeSuggestions(); searchStatus(); return;
    }
    if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
      if (!suggestionsReady(search.value)) return;
      event.preventDefault(); if (!suggestionModel.state.open) updateSuggestions();
      suggestionModel.move(event.key === 'ArrowDown' ? 1 : -1); renderSuggestions(true); searchStatus(); return;
    }
    if (event.key === 'Enter' && suggestionModel.state.open) {
      event.preventDefault(); const entry = suggestionModel.choice(); if (entry) commitPlayer(entry.id, true);
    }
  });
  suggestions.addEventListener('mousedown', event => { if (event.target instanceof Element && event.target.closest('[data-suggestion-index]')) event.preventDefault(); });
  suggestions.addEventListener('click', event => {
    const option = event.target instanceof Element ? event.target.closest<HTMLElement>('[data-suggestion-index]') : null;
    const index = Number(option?.dataset.suggestionIndex);
    const entry = suggestionModel.state.open && Number.isSafeInteger(index) ? suggestionModel.state.matches[index] : undefined;
    if (entry) commitPlayer(entry.id, true);
  });
  document.addEventListener('pointerdown', event => {
    if (suggestionModel.state.open && event.target instanceof Node && !searchShell.contains(event.target)) { closeSuggestions(); searchStatus(); }
  });
  searchShell.addEventListener('focusout', event => {
    if (event.relatedTarget instanceof Node && !searchShell.contains(event.relatedTarget)) { closeSuggestions(); if (active) searchStatus(); }
  });
  records.addEventListener('toggle', () => { if (records.open) renderTable(); });
  const choose = (e: Event) => {
    const target = e.target instanceof Element ? e.target.closest<HTMLElement>('[data-player]') : null;
    if (target && commitPlayer(target.dataset.player ?? '') && e.currentTarget === table) {
      const panel = get('selection-panel'); panel.focus({ preventScroll: true }); panel.scrollIntoView({ block: 'start', behavior: 'instant' });
    }
  };
  chart.addEventListener('pointerover', event => {
    clearHover();
    if (event.pointerType === 'touch' || !active) return;
    const dot = event.target instanceof Element ? event.target.closest<SVGCircleElement>('circle[data-player]') : null;
    const player = dot && chart.contains(dot) ? active.players.find(p => p.id === dot.dataset.player) : undefined;
    if (!dot || !player) return;
    const { width, height } = chart.viewBox.baseVal;
    const label = svg('g', { class: 'mb-hover', 'aria-hidden': 'true' });
    const text = svg('text', { x: 8, y: 19 }, player.name); label.append(text); chart.append(label);
    const measured = text.getComputedTextLength(), boxWidth = Math.min(width - 16, measured + 16);
    if (measured > boxWidth - 16) { text.setAttribute('textLength', String(boxWidth - 16)); text.setAttribute('lengthAdjust', 'spacingAndGlyphs'); }
    const x = Math.max(8, Math.min(width - boxWidth - 8, Number(dot.getAttribute('cx')) - boxWidth / 2));
    const y = Math.max(8, Math.min(height - 36, Number(dot.getAttribute('cy')) - 38));
    label.setAttribute('transform', `translate(${x},${y})`);
    label.prepend(svg('rect', { width: boxWidth, height: 28 }));
  });
  chart.addEventListener('pointerout', clearHover);
  chart.addEventListener('pointerleave', clearHover);
  chart.addEventListener('pointerdown', event => { if (event.pointerType === 'touch') clearHover(); });
  document.addEventListener('keydown', event => { if (event.key === 'Escape') clearHover(); });
  chart.addEventListener('click', choose); table.addEventListener('click', choose); get('example-buttons').addEventListener('click', choose);
  get('clear-search').addEventListener('click', () => { selected = ''; exampleSelection = ''; search.value = ''; closeSuggestions(); render(); search.focus({ preventScroll: true }); });
  get('find-player').addEventListener('click', () => { search.focus({ preventScroll: true }); search.scrollIntoView({ block: 'center', behavior: 'instant' }); });
  get('reset').addEventListener('click', async () => { if (!index) return; const v = index.views.find(v => v.id === index!.defaultView)!; season.value = String(v.endSeason); windowSelect.value = String(v.window); metric.value = 'war'; units.value = 'usd'; search.value = ''; team.value = ''; selected = ''; exampleSelection = ''; await changeView(); showDefaultExample(); });
  let frame = 0;
  new ResizeObserver(() => { cancelAnimationFrame(frame); frame = requestAnimationFrame(draw); }).observe(chart.parentElement!);
  void loadIndex();
}
