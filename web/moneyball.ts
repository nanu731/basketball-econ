/** Shared build/browser boundary. Source values and fitted results come from R. */
export type Metric = 'lebron' | 'war';
export interface Contrast { supported: boolean; estimate: number | null; low: number | null; high: number | null; nStart: number; nEnd: number; reason: string | null }
export interface Knot { x: number; y: number; low: number | null; high: number | null; extraMillion: Contrast }
export interface Model { name: string; method: string; validation: string; intervalLabel: string; curve: Knot[] }
export interface Player { id: string; name: string; teams: string[]; observedSeasons: number[]; minutes: number; possessions: number; salaryCapShare: number; salaryUsd: number; lebron: number; war: number; fitted: Record<Metric, number>; residual: Record<Metric, number>; label: boolean; flags: string[] }
export interface Team { team: string; salaryUsd: number | null; war: number | null; status: 'attributable-sample' | 'unavailable'; attributablePlayerSeasons: number; eligiblePlayerSeasons: number; excludedTradeCount: number; excludedMissingCount: number; note: string }
export interface View { id: string; endSeason: number; window: 1 | 3 | 5; seasons: number[]; referenceCapUsd: number; representation: string; coverage: { eligible: number; matched: number; includedKnownSalaryUsd: number; allKnownActiveSalaryUsd: number; unknownSalaryPlayerSeasons: number; sourcePlayerSeasons: number; eligiblePlayerSeasons: number; excluded: { reason: string; count: number }[]; note: string }; players: Player[]; models: Record<Metric, Model>; teams: Team[] }
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
      obj(p.fitted); obj(p.residual); if (typeof p.label !== 'boolean') fail('label flag'); arr(p.flags, 30).forEach(f => str(f));
      metrics.forEach(m => { num(p[m]); num(p.fitted[m]); num(p.residual[m]); if (!close(p.fitted[m], interpolate(v.models[m].curve, p.salaryCapShare)) || !close(p.residual[m], p[m] - p.fitted[m])) fail('visible reference or residual mismatch'); });
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
export function initialContrastIndex(curve: Knot[]): number {
  const supported = curve.flatMap((k, i) => k.extraMillion.supported ? [i] : []);
  return supported.length ? supported[Math.floor((supported.length - 1) / 2)] : 0;
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

/** Reuse an explicit choice across views; a unique name search needs no second click. */
export function resolvePlayerSelection(shown: Pick<Player, 'id'>[], current: string, query: string): string {
  if (query.trim() && shown.length === 1) return shown[0].id;
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

export function mountMoneyball(root: HTMLElement): void {
  const get = <T extends HTMLElement>(key: string): T => root.querySelector<T>(`[data-mb-${key}]`)!;
  const controls = get<HTMLFieldSetElement>('controls'), status = get('status');
  const season = get<HTMLSelectElement>('season'), windowSelect = get<HTMLSelectElement>('window');
  const metric = get<HTMLSelectElement>('metric'), units = get<HTMLSelectElement>('units');
  const search = get<HTMLInputElement>('search'), team = get<HTMLSelectElement>('team'), pick = get<HTMLSelectElement>('player');
  const chart = get<SVGSVGElement & HTMLElement>('chart'), contrastChart = get<SVGSVGElement & HTMLElement>('contrast');
  const table = get<HTMLTableSectionElement>('rows'), slider = get<HTMLInputElement>('spending');
  const records = get<HTMLDetailsElement>('live-table'), extra = get<HTMLDetailsElement>('extra');
  let tableDirty = true;
  let index: ViewIndex | null = null, bundle: Bundle | null = null, active: View | null = null, selected = '', request = 0;
  let loader: ReturnType<typeof createViewLoader> | null = null;
  const ns = 'http://www.w3.org/2000/svg';
  const svg = (name: string, attrs: Record<string, string | number> = {}, text?: string): SVGElement => {
    const el = document.createElementNS(ns, name); Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, String(v))); if (text !== undefined) el.textContent = text; return el;
  };
  const txt = (key: string, text: string) => { get(key).textContent = text; };
  const option = (value: string, label: string) => { const o = document.createElement('option'); o.value = value; o.textContent = label; return o; };
  function setControls(ready: boolean) {
    controls.disabled = !index;
    for (const el of [metric, units, search, team, pick, slider]) el.disabled = !ready;
  }
  function failure(message: string) {
    bundle = null; active = null; setControls(false);
    chart.replaceChildren(); contrastChart.replaceChildren(); table.replaceChildren(); tableDirty = true;
    pick.replaceChildren(option('', 'Choose a player')); txt('search-result', 'Player choices will return after this view loads.');
    txt('selection', 'Player details unavailable.'); txt('team-summary', 'Team totals unavailable.'); get('team-audit-panel').hidden = true; txt('team-audit', 'No team audit is loaded.'); txt('contrast-readout', 'Extra $1m comparison unavailable.'); txt('count', 'No interactive player records');
    txt('coverage', ''); txt('model', ''); txt('view-evidence', 'No interactive view evidence loaded.');
    txt('y-label', 'Contribution · awaiting selected view'); txt('x-label', 'Compensation · awaiting selected view');
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
    } catch { if (ticket === request) failure('The season index is unavailable or invalid. Retry or read the labeled default build snapshot below.'); }
  }
  async function changeView() {
    if (!index || !loader) return;
    const allowed = index.views.filter(v => v.endSeason === Number(season.value));
    for (const o of windowSelect.options) o.disabled = !allowed.some(v => v.window === Number(o.value));
    if (!allowed.some(v => v.window === Number(windowSelect.value))) windowSelect.value = String(allowed[0].window);
    const desired = allowed.find(v => v.window === Number(windowSelect.value))!;
    failure(`Loading ${seasonLabel(desired.endSeason)} · ${desired.window}-season window…`);
    txt('window-note', `Requested: ending ${seasonLabel(desired.endSeason)}, ${desired.window}-season window. Results appear only after this view passes validation.`);
    try {
      const data = await loader.load(desired.id); if (!data) return;
      bundle = data; active = data.views[0];
      setControls(true); get('empty').hidden = true; get('snapshot').hidden = true; get('live-table').hidden = false;
      status.textContent = `${data.publication.status === 'private-preview' ? 'Private preview' : 'Reviewed export'} · ${viewLabel(active)}`;
      const previousTeam = team.value; team.replaceChildren(option('', 'All teams'), ...[...new Set(active.players.flatMap(p => p.teams))].sort((a, b) => teamName(a, active!.endSeason).localeCompare(teamName(b, active!.endSeason))).map(t => option(t, `${teamName(t, active!.endSeason)} (${t})`)));
      team.value = [...team.options].some(o => o.value === previousTeam) ? previousTeam : '';
      resetContrast();
      txt('window-note', active.window === 1 ? `${viewLabel(active)}. Each dot is one matched player-season. Dollars are nominal annual compensation.` : `${viewLabel(active)}. Each dot averages the matched active seasons shown for that player. Pay is adjusted to the ${seasonLabel(active.endSeason)} salary cap; WAR is average annual contribution. LEBRON is a playing-time-weighted rate.`);
      txt('coverage', `${active.coverage.matched} matched salary records among ${active.coverage.eligible} player records meeting playing-time and contribution-metric rules. ${salaryCoverage(active)}${active.coverage.excluded.length ? ` Primary exclusion reasons: ${active.coverage.excluded.map(e => `${e.count}: ${readableReason(e.reason)}`).join('; ')}` : ''}`);
      render();
    } catch { failure(`${seasonLabel(desired.endSeason)} · ${desired.window}-season window is unavailable or failed validation. Change season/window, reset, or retry. The default build snapshot below is separately labeled.`); }
  }
  function rows(): Player[] { return active?.players.filter(p => (!team.value || p.teams.includes(team.value)) && p.name.toLocaleLowerCase().includes(search.value.trim().toLocaleLowerCase())) ?? []; }
  function render() {
    if (!active) return;
    const shown = rows(), m = metric.value as Metric;
    selected = resolvePlayerSelection(shown, selected, search.value);
    pick.replaceChildren(option('', 'Choose a player'), ...shown.map(p => option(p.id, p.name))); pick.value = selected;
    txt('search-result', search.value.trim() ? shown.length === 1 ? '1 match; player selected.' : shown.length ? `${shown.length} matches; choose a player.` : 'No matching players.' : 'Search by name, then choose if several players match.');
    txt('count', `${shown.length} of ${active.players.length} matched players shown`);
    get('war-help').hidden = m !== 'war'; get('lebron-help').hidden = m !== 'lebron';
    metric.options[0].textContent = active.window > 1 ? 'WAR · avg estimated wins' : 'WAR · estimated wins';
    txt('y-label', m === 'war' ? active.window > 1 ? 'estimated wins / observed season' : 'estimated wins · season total' : 'impact points / 100 possessions'); txt('x-label', units.value === 'share' ? 'Compensation · % of season cap →' : active.window === 1 ? 'Annual compensation · US dollars →' : `Average annual compensation · ${seasonLabel(active.endSeason)} cap-equivalent dollars →`);
    txt('model', active.models[m].curve[0].low === null ? 'No estimated range for the league curve is supplied.' : 'The shaded range concerns the fitted league curve.');
    if (bundle) txt('view-evidence', `${viewLabel(active)} · ${bundle.dataVersion}. ${bundle.publication.note} ${active.models[m].name}. ${active.models[m].intervalLabel} ${active.models[m].method} ${active.models[m].validation} Salary: ${bundle.definitions.salary} Window representation: salary cap share and WAR are averages over exactly the observed matched active seasons; LEBRON is weighted by provider possession counts. Multi-season dollars are average annual compensation using the ending season cap, and team averages divide attributable player-season totals by the full window length. Eligibility: ${bundle.definitions.eligibility} Coverage note: ${active.coverage.note} Uncertainty: ${bundle.definitions.uncertainty} Limitations: ${bundle.definitions.limitations.join('; ')} Sources: ${bundle.sources.map(s => `${s.label}, ${s.url}, ${s.version}, retrieved ${s.retrieved}`).join('; ')}`);
    const t = active.teams.find(t => t.team === team.value);
    get('team-audit-panel').hidden = !team.value || !t;
    txt('team-summary', !team.value ? 'Choose a team to compare its chart players with the matched records we can assign to the club.' : !t || t.status === 'unavailable' ? `${teamName(team.value, active.endSeason)}: ${shown.length} players in the chart; team salary and WAR totals are unavailable.` : `${teamName(t.team, active.endSeason)}: ${shown.length} players in the chart; totals cover ${t.attributablePlayerSeasons} matched player-seasons we can assign to this club. ${money(t.salaryUsd!)} and ${decimal(t.war!)} WAR ${active.window === 1 ? 'in the selected season' : `per year across the full ${active.window}-season window`}. Team totals stay fixed under player search.`);
    txt('team-audit', t ? `Matched team sample: ${t.attributablePlayerSeasons} assigned of ${t.eligiblePlayerSeasons} matched player-seasons. Broader active-source team rows have ${t.excludedTradeCount} trade/unknown-allocation flags and ${t.excludedMissingCount} missing-data flags. Those audit counts may overlap and include ineligible source rows; do not add them as separate groups or subtract them from the chart-player count. ${t.note}` : 'No team audit is loaded.');
    // Hidden rows are built on demand instead of on every search keystroke.
    tableDirty = true;
    if (records.open) renderTable();
    else if (table.childElementCount) table.replaceChildren();
    txt('table-cost', units.value === 'share' ? 'Compensation (% cap)' : active.window === 1 ? 'Compensation (USD)' : 'Avg compensation (end-cap USD)');
    txt('table-war', active.window === 1 ? 'WAR total' : 'Avg WAR / observed season'); txt('table-residual', `Above / below reference (${m === 'war' ? 'WAR' : 'LEBRON'})`);
    const p = active.players.find(p => p.id === selected);
    txt('selection', p ? `${p.name} · ${p.teams.map(t => teamName(t, active!.endSeason)).join(', ')}. ${money(p.salaryUsd)}${active.window > 1 ? ' average annual end-season cap-equivalent compensation' : ' annual compensation'} (${decimal(p.salaryCapShare * 100)}% cap). ${metricLabel(m, active)}: ${decimal(p[m])}. League reference: ${decimal(p.fitted[m])}. ${decimal(Math.abs(p.residual[m]))} ${p.residual[m] >= 0 ? 'above' : 'below'} the visible reference. Observed seasons: ${p.observedSeasons.map(seasonLabel).join(', ')} (${p.observedSeasons.length}/${active.window}). ${decimal(p.minutes)} minutes; ${decimal(p.possessions)} possessions. ${p.flags.map(readableReason).join(' ')}` : 'Search for a player or tap a dot. Use the player selector for an exact comparison.');
    draw(chart, false); if (extra.open) draw(contrastChart, true); contrastReadout();
  }
  function renderTable() {
    if (!active || !records.open || !tableDirty) return;
    const shown = rows(), m = metric.value as Metric;
    table.replaceChildren(...shown.map(p => {
      const tr = document.createElement('tr'), name = document.createElement('th'); name.scope = 'row';
      const b = document.createElement('button'); b.type = 'button'; b.textContent = p.name; b.dataset.player = p.id; b.setAttribute('aria-pressed', String(p.id === selected)); name.append(b); tr.append(name);
      for (const value of [p.teams.join(', '), `${p.observedSeasons.length}/${active!.window}`, units.value === 'share' ? `${decimal(p.salaryCapShare * 100)}%` : money(p.salaryUsd), decimal(p.lebron), decimal(p.war), decimal(p.residual[m])]) { const td = document.createElement('td'); td.textContent = value; tr.append(td); }
      return tr;
    }));
    if (!shown.length) { const tr = document.createElement('tr'); const td = document.createElement('td'); td.colSpan = 7; td.textContent = 'No players match these filters. Clear the search or reset the view.'; tr.append(td); table.append(tr); }
    tableDirty = false;
  }
  function draw(target: SVGSVGElement & HTMLElement, extra: boolean) {
    if (!active) return;
    const m = metric.value as Metric, curve = active.models[m].curve, all = active.players, shown = rows();
    const width = Math.max(260, Math.round(target.getBoundingClientRect().width)), height = extra ? 200 : 340;
    target.setAttribute('viewBox', `0 0 ${width} ${height}`);
    const L = 46, R = 14, T = 26, B = 40, xmax = curve[curve.length - 1].x, xmin = Math.min(0, curve[0].x);
    const values = extra ? curve.filter(k => k.extraMillion.supported).flatMap(k => [k.extraMillion.estimate!, k.extraMillion.low ?? k.extraMillion.estimate!, k.extraMillion.high ?? k.extraMillion.estimate!]) : [...all.map(p => p[m]), ...curve.flatMap(k => [k.y, k.low ?? k.y, k.high ?? k.y])];
    if (!values.length) { target.replaceChildren(svg('title', {}, 'No locally supported extra $1m comparisons'), svg('text', { x: 12, y: 40, 'font-size': 13 }, 'No supported comparisons. See explanation below.')); return; }
    const min = Math.min(0, ...values), max = Math.max(0, ...values), span = max - min || 1, ymin = min - span * .08, ymax = max + span * .08;
    const x = (v: number) => L + (v - xmin) / (xmax - xmin || 1) * (width - L - R), y = (v: number) => T + (ymax - v) / (ymax - ymin) * (height - T - B);
    const children: SVGElement[] = [svg('title', {}, extra ? 'Change in fitted contribution associated with an extra $1 million' : `Salary and ${metricLabel(m, active)}`), svg('desc', {}, extra ? 'Only supported spending positions are drawn. Intervals describe conditional curve differences.' : 'Dots represent matched players. Dashed line is the fixed league reference. Use the player selector or table for keyboard access.')];
    for (const v of niceTicks(ymin, ymax)) { children.push(svg('line', { x1: L, x2: width - R, y1: y(v), y2: y(v), class: 'mb-grid' }), svg('text', { x: L - 7, y: y(v) + 4, 'text-anchor': 'end' }, String(Number(v.toPrecision(8))))); }
    const ticks = width < 500 ? 2 : 4;
    for (let i = 0; i <= ticks; i++) { const v = xmin + (xmax - xmin) * i / ticks; children.push(svg('text', { x: x(v), y: height - 13, 'text-anchor': i === 0 ? 'start' : i === ticks ? 'end' : 'middle' }, units.value === 'share' ? `${Math.round(v * 100)}%` : `$${(v * active.referenceCapUsd / 1e6).toFixed(0)}m`)); }
    children.push(svg('line', { x1: L, x2: width - R, y1: y(0), y2: y(0), class: 'mb-zero' }));
    const path = (points: [number, number][]) => points.map(([a, b], i) => `${i ? 'L' : 'M'}${x(a)},${y(b)}`).join(' ');
    if (!extra) {
      if (curve[0].low !== null) children.push(svg('path', { d: `${path(curve.map(k => [k.x, k.low!]))} ${curve.slice().reverse().map(k => `L${x(k.x)},${y(k.high!)}`).join(' ')} Z`, class: 'mb-band' }));
      children.push(svg('path', { d: path(curve.map(k => [k.x, k.y])), class: 'mb-reference' }));
      for (const p of shown) { const dot = svg('circle', { cx: x(p.salaryCapShare), cy: y(p[m]), r: p.id === selected ? 7 : 4, class: p.id === selected ? 'mb-dot mb-selected' : 'mb-dot', 'data-player': p.id }); dot.append(svg('title', {}, `${p.name}: ${decimal(p[m])}; ${money(p.salaryUsd)}`)); children.push(dot); }
      chartLabels(shown, selected, width).forEach((p, i) => {
        const right = x(p.salaryCapShare) > width * (width < 500 ? .5 : .7);
        children.push(svg('text', { x: x(p.salaryCapShare) + (right ? -9 : 9), y: Math.max(T + 12, Math.min(height - B - 4, y(p[m]) - 10 - (i % 2) * 12)), 'text-anchor': right ? 'end' : 'start', class: p.id === selected ? 'mb-name mb-selected-name' : 'mb-name' }, p.name));
      });
    } else {
      let segment: Knot[] = [];
      const flush = () => { if (!segment.length) return; children.push(svg('path', { d: path(segment.map(k => [k.x, k.extraMillion.estimate!])), class: 'mb-reference' })); for (const k of segment) { if (k.extraMillion.low !== null) children.push(svg('line', { x1: x(k.x), x2: x(k.x), y1: y(k.extraMillion.low!), y2: y(k.extraMillion.high!), class: 'mb-interval' })); children.push(svg('circle', { cx: x(k.x), cy: y(k.extraMillion.estimate!), r: 2.5, class: 'mb-dot' })); } segment = []; };
      curve.forEach(k => { if (k.extraMillion.supported) segment.push(k); else flush(); }); flush();
      const knot = curve[Math.min(curve.length - 1, Number(slider.value))]; children.push(svg('line', { x1: x(knot.x), x2: x(knot.x), y1: T, y2: height - B, class: 'mb-cursor' }));
    }
    target.setAttribute('viewBox', `0 0 ${width} ${height}`); target.replaceChildren(...children);
  }
  function resetContrast() {
    if (!active) return;
    const curve = active.models[metric.value as Metric].curve;
    slider.max = String(curve.length - 1); slider.value = String(initialContrastIndex(curve));
  }
  function contrastReadout() {
    if (!active) return;
    const curve = active.models[metric.value as Metric].curve; slider.max = String(curve.length - 1);
    const k = curve[Math.min(Number(slider.value), curve.length - 1)], e = k.extraMillion;
    slider.setAttribute('aria-valuetext', `Starting compensation ${money(k.x * active.referenceCapUsd)}; ${e.supported ? `${decimal(e.estimate!)} ${metric.value === 'war' ? 'WAR' : 'LEBRON points per 100 possessions'} for the curve difference` : `comparison unavailable. ${readableReason(e.reason)}`}`);
    txt('contrast-readout', `Starting at ${money(k.x * active.referenceCapUsd)}${active.window > 1 ? ` average annual ${seasonLabel(active.endSeason)} cap-equivalent compensation` : ''}: ${e.supported ? `${decimal(e.estimate!)} ${metric.value === 'war' ? 'WAR' : 'LEBRON points / 100 possessions'} associated with an extra $1m${e.low !== null ? `; estimated range for this curve difference: ${decimal(e.low)} to ${decimal(e.high!)}` : '; uncertainty range unavailable'}. Nearby records at the two amounts: ${e.nStart} and ${e.nEnd}.` : `Unavailable: ${readableReason(e.reason)}`}`);
  }
  root.querySelector('form')?.addEventListener('submit', e => e.preventDefault());
  get('retry').addEventListener('click', () => { void (index ? changeView() : loadIndex()); });
  season.addEventListener('change', changeView); windowSelect.addEventListener('change', changeView);
  metric.addEventListener('change', () => { resetContrast(); render(); });
  for (const el of [units, team]) el.addEventListener('change', render);
  search.addEventListener('input', render); pick.addEventListener('change', () => { selected = pick.value; render(); });
  slider.addEventListener('input', () => { contrastReadout(); if (extra.open) draw(contrastChart, true); });
  records.addEventListener('toggle', () => { if (records.open) renderTable(); });
  extra.addEventListener('toggle', () => { if (extra.open) draw(contrastChart, true); });
  const choose = (e: Event) => { const target = e.target instanceof Element ? e.target.closest('[data-player]') : null; if (target) { selected = (target as HTMLElement).dataset.player ?? ''; render(); } };
  chart.addEventListener('click', choose); table.addEventListener('click', choose);
  get('reset').addEventListener('click', () => { if (!index) return; const v = index.views.find(v => v.id === index!.defaultView)!; season.value = String(v.endSeason); windowSelect.value = String(v.window); metric.value = 'war'; units.value = 'usd'; search.value = ''; team.value = ''; selected = ''; changeView(); });
  let frame = 0;
  new ResizeObserver(() => { cancelAnimationFrame(frame); frame = requestAnimationFrame(() => { draw(chart, false); if (extra.open) draw(contrastChart, true); }); }).observe(chart.parentElement!);
  void loadIndex();
}
