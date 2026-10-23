// Run against a fresh private build: MONEYBALL_BUILD_DIR=/absolute/build node --test web/moneyball.browser.test.mjs
// Optional MONEYBALL_CHROME_BIN overrides the installed macOS Chrome executable. No build/data files are written.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile, realpath, mkdtemp, rm } from 'node:fs/promises';
import { resolve, sep, extname, join } from 'node:path';
import { tmpdir } from 'node:os';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';

async function mountedScenario() {
  const root = document.querySelector('[data-moneyball]'), q = key => root.querySelector(`[data-mb-${key}]`);
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  const wait = async (condition, message) => {
    for (let i = 0; i < 100; i++) { if (condition()) return; await new Promise(r => setTimeout(r, 50)); }
    throw new Error(message);
  };
  const json = async url => { const response = await fetch(url); check(response.ok, `Missing ${url}`); return response.json(); };
  const change = (key, value) => { q(key).value = value; q(key).dispatchEvent(new Event('change', { bubbles: true })); };
  const dots = () => [...q('chart').querySelectorAll('circle[data-player]')];
  const reference = () => q('chart').querySelector('.mb-reference')?.getAttribute('d');
  const fields = ['start', 'lower', 'higher', 'lowerContribution', 'higherContribution', 'unit', 'context', 'range', 'support', 'shape', 'unitNote', 'precision'];
  const choose = player => {
    q('search').value = player.name; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    const option = [...q('suggestions').querySelectorAll('[data-suggestion-index]')].find(el => el.textContent === player.name);
    check(option, 'Actual player suggestion missing'); option.click();
  };
  const connector = () => {
    const gap = q('chart').querySelector('.mb-gap'), dot = q('chart').querySelector('.mb-selected');
    check(gap && dot, 'Selected connector missing');
    const n = (el, key) => Number(el.getAttribute(key)), x = n(dot, 'cx');
    check(n(gap, 'x1') === x && n(gap, 'x2') === x && n(gap, 'y1') === n(dot, 'cy'), 'Connector misses player');
    const points = [...reference().matchAll(/[ML]([^,ML]+),([^ML]+)/g)].map(m => [Number(m[1]), Number(m[2])]);
    const index = points.findIndex(p => p[0] >= x); check(index > 0, 'Player must be inside the plotted range');
    const a = points[index - 1], b = points[index], expected = a[1] + (b[1] - a[1]) * (x - a[0]) / (b[0] - a[0]);
    check(Math.abs(n(gap, 'y2') - expected) < 1e-6, 'Connector misses the painted interpolated reference');
  };
  try {
    document.body.dataset.moneyballTest = 'running';
    const index = await json('/data/moneyball/v1/index.json'), initial = index.views.find(v => v.id === index.defaultView);
    const three = index.views.find(v => v.endSeason === initial.endSeason && v.window === 3);
    check(initial.window === 1 && three && index.views.some(v => v.endSeason === initial.endSeason && v.window === 5), 'Build needs one-, three- and five-season views');
    const one = (await json(initial.url)).views[0], multi = (await json(three.url)).views[0];
    const common = one.players.filter(p => multi.players.some(v => v.id === p.id) && p.salaryCapShare > one.models.war.curve[0].x);
    const player = common.find(p => p.id === '203999') ?? common.find(p => !p.teams.includes('NYK'));
    const peer = common.find(p => p.id !== player?.id); check(player && peer, 'Actual comparison players missing');
    // Real exports include every player salary as a knot; between-knot interpolation has a separate unit regression.
    await wait(() => !q('search').disabled && reference() && fields.every(f => q(`contrast-${f}`).textContent.trim()), 'Mount did not finish');
    await document.fonts.ready;
    const baseline = reference(); check(dots().length === one.players.length, 'Initial cohort count differs');
    const chartTop = q('chart').getBoundingClientRect().top + scrollY;
    check(chartTop - (root.getBoundingClientRect().top + scrollY) < Math.min(650, innerHeight * .8), 'Default graph remains too far below controls');
    if (innerWidth >= 1100) check(chartTop < innerHeight * .75, 'Desktop first graph remains too far below the fold');
    check(document.documentElement.scrollWidth <= innerWidth + 1, 'Default page overflows horizontally');
    const readableMetric = () => {
      const select = q('metric'), canvas = document.createElement('canvas'), context = canvas.getContext('2d'); context.font = getComputedStyle(select).font;
      check(select.clientWidth >= context.measureText(select.selectedOptions[0].textContent).width + 26, 'Active long metric label is clipped');
    };
    readableMetric();
    check(q('period').textContent.includes('Regular seasons') && q('metric-note').textContent.includes('fill-in'), 'Season scope or plain zero baseline missing');
    const contextPanel = q('context-controls');
    check(contextPanel.tagName === 'DETAILS' && !contextPanel.open && contextPanel.querySelector('summary').textContent === 'Change season, team or time span', 'Named native context disclosure missing or initially open');
    check(['season', 'team', 'window'].every(k => root.querySelectorAll(`[data-mb-${k}]`).length === 1 && contextPanel.contains(q(k))), 'Context controls duplicated or outside the named panel');
    check(q('period').textContent.includes('All teams') && !q('period').closest('details') && !q('window-note').closest('details'), 'Collapsed active context is hidden');
    const hero = document.querySelector('.question-band.band');
    check(hero && parseFloat(getComputedStyle(hero).paddingTop) <= 12.1 && parseFloat(getComputedStyle(hero).paddingBottom) <= 12.1, 'Project compact hero padding override did not apply');
    const hover = (dot, pointerType = 'mouse') => dot.dispatchEvent(new PointerEvent('pointerover', { bubbles: true, pointerType }));
    const hoveredName = () => q('chart').querySelector('.mb-hover text')?.textContent;
    const unlabelled = one.players.find(p => !p.label), unlabelledDot = dots().find(d => d.dataset.player === unlabelled?.id);
    check(unlabelledDot, 'Unlabeled actual player dot missing');
    hover(unlabelledDot);
    check(hoveredName() === unlabelled.name && !q('chart').querySelector('.mb-selected') && q('search').value === '' && dots().length === one.players.length && reference() === baseline, 'Hover failed to name an unlabeled dot without selecting/filtering');
    check(unlabelledDot.getAttribute('aria-label').includes(unlabelled.name) && !unlabelledDot.querySelector('title') && getComputedStyle(q('chart').querySelector('.mb-hover')).pointerEvents === 'none', 'Hover lost accessible information or duplicates/intercepts the tooltip');
    unlabelledDot.dispatchEvent(new PointerEvent('pointerout', { bubbles: true, pointerType: 'mouse' })); check(!hoveredName(), 'Pointer exit retained hover');
    hover(unlabelledDot); document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true })); check(!hoveredName(), 'Escape retained hover');
    hover(unlabelledDot, 'touch'); check(!hoveredName(), 'Touch created sticky hover');
    hover(unlabelledDot); change('units', 'share'); check(!hoveredName(), 'Redraw retained hover'); change('units', 'usd');
    const edge = dots().reduce((a, b) => Number(a.getAttribute('cx')) > Number(b.getAttribute('cx')) ? a : b); hover(edge);
    const hoverBounds = q('chart').querySelector('.mb-hover').getBoundingClientRect(), chartBounds = q('chart').getBoundingClientRect();
    check(hoverBounds.left >= chartBounds.left && hoverBounds.right <= chartBounds.right && hoverBounds.top >= chartBounds.top && hoverBounds.bottom <= chartBounds.bottom, 'Boundary hover escapes chart');
    edge.dispatchEvent(new PointerEvent('pointerout', { bubbles: true, pointerType: 'mouse' }));
    const examples = [...q('example-buttons').querySelectorAll('button')];
    check(examples.length > 0 && examples.length <= 3 && !q('chart').querySelector('.mb-selected'), 'Examples missing or auto-selected');
    check(examples.every(b => one.players.some(p => p.id === b.dataset.player && p.label)), 'Example was not an eligible labeled player');
    check(q('examples').closest('.mb-selection'), 'Examples compete with the pre-chart hierarchy');
    check(examples.every(b => b.getBoundingClientRect().height >= 44 && getComputedStyle(b).fontFamily === getComputedStyle(q('examples')).fontFamily && getComputedStyle(b).borderTopStyle === 'solid'), 'Example buttons lost house styling or 44px targets');
    examples[0].click(); check(q('chart').querySelector('.mb-selected')?.dataset.player === examples[0].dataset.player && dots().length === one.players.length, 'Example did not commit with peers');
    const beforeShortcut = q('search').value; q('find-player').click();
    check(document.activeElement === q('search') && q('search').value === beforeShortcut && root.querySelectorAll('input[type=search]').length === 1, 'Find shortcut lost continuity or duplicated search');
    const pairedComparison = (view, metric) => {
      const knot = view.models[metric].curve[Number(q('spending').value)], result = knot.extraMillion;
      const currency = v => new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', minimumFractionDigits: 0, maximumFractionDigits: 2 }).format(v);
      const dec = v => new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(v);
      const signed = v => `${v > 0 ? '+' : v < 0 ? '−' : ''}${dec(Math.abs(v))}`;
      check(q('contrast-lower').textContent === currency(knot.x * view.referenceCapUsd) && q('contrast-higher').textContent === currency(knot.x * view.referenceCapUsd + 1e6), 'Exact salary pair differs from export');
      if (result.supported) {
        const x = knot.x + 1e6 / view.referenceCapUsd, curve = view.models[metric].curve, i = curve.findIndex(k => k.x >= x), a = curve[i - 1], b = curve[i];
        const higher = a.y + (b.y - a.y) * (x - a.x) / (b.x - a.x);
        check(q('contrast-lowerContribution').textContent === `About ${dec(knot.y)}` && q('contrast-higherContribution').textContent === `About ${dec(higher)}`, 'Paired estimates differ from visible interpolation');
        check(q('contrast-readout').textContent.startsWith(`${signed(result.estimate)} `), 'Difference is not the exported joint contrast');
        check(result.low === null ? q('contrast-range').textContent === 'Unavailable' : q('contrast-range').textContent.startsWith(`${signed(result.low)} to ${signed(result.high)} `), 'Range is not the exported joint interval');
      } else {
        check(['lowerContribution', 'higherContribution', 'range', 'precision'].every(f => q(`contrast-${f}`).textContent === 'Unavailable'), 'Unsupported numeric estimates or repeated warning leaked');
        const reason = q('contrast-readout').textContent.replace('Comparison unavailable. ', '');
        check(reason && q('extra').textContent.split(reason).length === 2, 'Unsupported reason is missing or repeated');
      }
      check(q('contrast-unit').textContent === (metric === 'lebron' ? 'impact points/100 possessions' : view.window > 1 ? 'estimated wins/observed season' : 'estimated wins') && q('contrast-unit').closest('th')?.id === 'mb-scenario-contribution', 'Shared contribution units are missing or wrong');
      const pairTable = q('extra').querySelector('.mb-scenarios'), lowerBox = q('contrast-lower').getBoundingClientRect(), higherBox = q('contrast-higher').getBoundingClientRect();
      check(getComputedStyle(pairTable).display === 'table' && Math.abs(lowerBox.width - higherBox.width) < 1 && Math.abs(lowerBox.width + higherBox.width - pairTable.getBoundingClientRect().width) < 2, 'Scenario table lost native full-width equal columns');
      check(Math.abs(q('contrast-lower').getBoundingClientRect().top - q('contrast-higher').getBoundingClientRect().top) < 1 && Math.abs(q('contrast-lowerContribution').getBoundingClientRect().top - q('contrast-higherContribution').getBoundingClientRect().top) < 1, 'Paired salary or contribution values lost shared-row alignment');
    };
    q('extra').open = true; pairedComparison(one, 'war');
    check(document.documentElement.scrollWidth <= innerWidth + 1 && q('extra').querySelector('.mb-scenarios').getBoundingClientRect().width <= q('extra').getBoundingClientRect().width, 'Worked comparison overflows the page');
    const start = q('contrast-start').textContent; q('spending').value = q('spending').value === '0' ? q('spending').max : '0'; q('spending').dispatchEvent(new Event('input', { bubbles: true }));
    check(q('contrast-start').textContent && q('contrast-start').textContent !== start, 'Starting salary did not update with slider'); pairedComparison(one, 'war');
    const gapSize = (chosen, metric, view) => {
      const abs = view.players.map(p => Math.abs(p.residual[metric])).sort((a, b) => a - b), mid = Math.floor(abs.length / 2);
      const scale = abs.length % 2 ? abs[mid] : (abs[mid - 1] + abs[mid]) / 2, ratio = chosen.residual[metric] / scale;
      const magnitude = Math.abs(ratio), formatted = magnitude !== 0 && (magnitude < .005 || magnitude >= 1e6) ? String(Number(magnitude.toPrecision(3))) : new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(magnitude);
      check(q('selected-magnitude').textContent.startsWith(`${ratio > 0 ? '+' : ratio < 0 ? '−' : ''}${formatted} × median absolute league gap`), 'Selected magnitude differs from the full league cohort');
      check(q('selected-scale').textContent.includes(String(Number(scale.toPrecision(6)))) && q('selected-scale').textContent.includes(`${view.players.length} matched players`), 'Native scale/full cohort context missing');
    };
    choose(player); connector(); gapSize(player, 'war', one);
    const pinnedRank = q('selected-percentile').textContent; hover(dots().find(d => d.dataset.player === peer.id));
    check(hoveredName() === peer.name && q('chart').querySelector('.mb-selected')?.dataset.player === player.id && q('search').value === player.name && q('selected-percentile').textContent === pinnedRank, 'Peer hover replaced the pinned comparison');
    q('chart').dispatchEvent(new PointerEvent('pointerleave', { pointerType: 'mouse' })); check(!hoveredName(), 'Chart exit retained hover');
    check(q('selected-reference').textContent.includes(new Intl.NumberFormat('en-US', { maximumFractionDigits: 2, minimumFractionDigits: 2 }).format(player.fitted.war)), 'Selected reference estimate missing');
    check(/^[+−]/.test(q('selected-gap').textContent) && q('peer-key').dataset.hollow === 'true' && !q('selected-key').hidden, 'Signed gap or selected legend missing');
    check(dots().length === one.players.length && reference() === baseline, 'Selection lost peers or changed the reference');
    q('live-table').open = true; await wait(() => q('rows').querySelectorAll('[data-player]').length === one.players.length, 'Selected table lost peers');
    dots().find(el => el.dataset.player === peer.id).dispatchEvent(new MouseEvent('click', { bubbles: true }));
    check(q('chart').querySelector('.mb-selected')?.dataset.player === peer.id && q('search').value === peer.name && dots().length === one.players.length, 'Visible peer cannot be selected'); connector();
    q('search').value = 'zzzz'; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    check(q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Draft input retained gap size');
    check(!q('chart').querySelector('.mb-selected') && dots().length === 0 && !q('rows').querySelector('[data-player]'), 'Typing did not clear selection and filter');
    choose(player); contextPanel.querySelector('summary').click(); check(contextPanel.open && q('team').getBoundingClientRect().height >= 44, 'Native panel did not expose usable controls'); change('team', 'NYK');
    const team = one.players.filter(p => p.teams.includes('NYK')); check(team.length, 'NYK cohort missing'); choose(team[0]);
    const expectedRank = new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(team[0].valuePercentile.war);
    check(dots().length === team.length && reference() === baseline && q('selected-percentile').textContent === `${expectedRank} / 100`, 'Team filter changed cohort reference/rank');
    contextPanel.querySelector('summary').click(); check(!contextPanel.open && q('period').textContent.includes('New York Knicks'), 'Collapsed team context stayed stale');
    check([...q('example-buttons').querySelectorAll('button')].every(b => team.some(p => p.id === b.dataset.player && p.label)), 'Team examples stayed stale');
    check(q('coverage-summary').textContent.startsWith(`League coverage: ${one.coverage.matched} matched of ${one.coverage.eligible} eligible`) && !q('coverage-summary').textContent.includes('Knicks'), 'League coverage reads as selected-team coverage');
    check(q('selected-cohort').textContent.includes(String(one.players.length)), 'Rank denominator changed'); gapSize(team[0], 'war', one);
    change('team', ''); choose(player); change('metric', 'lebron'); connector();
    gapSize(player, 'lebron', one);
    check(q('metric-note').textContent.includes('league-average'), 'Rate zero baseline missing'); pairedComparison(one, 'lebron'); check(q('contrast-unitNote').textContent.includes('not points actually scored'), 'Rate estimate units are unexplained');
    change('window', '3');
    check(fields.every(f => q(`contrast-${f}`).textContent === '') && !q('example-buttons').childElementCount && q('find-player').disabled && q('selected-reference').textContent === '' && q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Loading retained stale guided/result fields');
    await wait(() => !q('search').disabled && dots().length === multi.players.length, 'Three-season view did not mount'); connector();
    const multiPlayer = multi.players.find(p => p.id === player.id); gapSize(multiPlayer, 'lebron', multi);
    check(q('selected-percentile').textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(multiPlayer.valuePercentile.lebron) + ' / 100', 'Window/metric rank stayed stale');
    check(q('selected-observed').textContent.includes('of 3 seasons'), 'Observed-season context missing');
    pairedComparison(multi, 'lebron'); check(q('contrast-context').textContent.includes('cap-equivalent') && q('contrast-context').textContent.includes('LEBRON'), 'Multi-year comparison basis stayed stale');
    readableMetric(); check(q('period').textContent.includes('averages over observed seasons') && !q('window-note').closest('details'), 'Average context was hidden');
    const armed = await fetch('/__moneyball-arm', { method: 'POST' }); check(armed.ok, 'Fault was not armed'); hover(dots()[0]); change('window', '5'); check(!hoveredName(), 'Loading retained hover');
    await wait(() => q('search').disabled && /failed validation/.test(q('status').textContent), 'Corrupt view was not rejected');
    check(!hoveredName(), 'Failed view retained hover');
    check(fields.every(f => q(`contrast-${f}`).textContent === ''), 'A contrast field stayed stale');
    check(!q('example-buttons').childElementCount && q('examples').hidden && q('find-player').disabled && q('selected-reference').textContent === '' && q('coverage-summary').textContent === '' && q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Failed view retained new results/actions');
    check(!q('reset').disabled && q('units').disabled, 'Reset lost recovery availability outside the controls fieldset');
    check(q('spending').disabled && !q('snapshot').hidden && q('snapshot').querySelector('tbody tr'), 'Disabled controls or SSR fallback missing');
    check(q('search').value === '' && q('suggestions').hidden && !reference(), 'Stale search or chart survived failure');
    q('retry').click(); await wait(() => !q('search').disabled && reference() && fields.every(f => q(`contrast-${f}`).textContent.trim()), 'Retry did not recover');
    check(!q('spending').disabled && !q('units').disabled && !q('reset').disabled && q('snapshot').hidden, 'Recovered view stayed disabled');
    const fiveView = index.views.find(v => v.endSeason === initial.endSeason && v.window === 5); pairedComparison((await json(fiveView.url)).views[0], 'lebron');
    q('reset').click(); await wait(() => !q('search').disabled && q('window').value === '1' && dots().length === one.players.length, 'Reset did not return to default view');
    check(q('metric').value === 'war' && q('units').value === 'usd' && q('team').value === '' && q('search').value === '', 'Reset did not restore the default controls');
    check(q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Reset retained selected gap size');
    check(q('snapshot').querySelector('thead').textContent.includes('gap size') && q('rows').closest('table').querySelector('thead').textContent.includes('Gap size'), 'No-JS or live table lacks gap-size equivalent');
    const priorEntry = index.views.find(v => v.endSeason === initial.endSeason - 1 && v.window === 1), prior = (await json(priorEntry.url)).views[0];
    change('season', String(prior.endSeason)); await wait(() => !q('search').disabled && dots().length === prior.players.length, 'Prior year did not load');
    choose(prior.players[0]); gapSize(prior.players[0], 'war', prior);
    q('reset').click(); await wait(() => !q('search').disabled && q('season').value === String(initial.endSeason) && dots().length === one.players.length, 'Reset from prior year failed');
    check(q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Year reset retained gap size');
    document.body.dataset.moneyballTest = 'passed';
  } catch (error) {
    document.body.dataset.moneyballTest = 'failed'; document.body.dataset.moneyballError = encodeURIComponent(error.stack ?? String(error));
  }
}

test('real mounted Moneyball retains comparison context and clears/retries corrupt views', { skip: !process.env.MONEYBALL_BUILD_DIR, timeout: 60000 }, async t => {
  const build = await realpath(resolve(process.env.MONEYBALL_BUILD_DIR));
  const page = '/projects/moneyball-basketball-edition/';
  const html = await readFile(join(build, page, 'index.html'), 'utf8');
  assert.ok(html.includes("script-src 'self'") && html.includes('</body>'), 'Use the actual strict-CSP private build');
  const module = `(${mountedScenario.toString()})();`, profile = await mkdtemp(join(tmpdir(), 'moneyball-browser-'));
  const mime = { '.html': 'text/html', '.js': 'text/javascript', '.mjs': 'text/javascript', '.json': 'application/json', '.css': 'text/css', '.svg': 'image/svg+xml', '.woff2': 'font/woff2', '.png': 'image/png', '.webp': 'image/webp' };
  let armed = false, corruptions = 0, browser;
  const server = createServer(async (req, res) => {
    try {
      const pathname = decodeURIComponent(new URL(req.url, 'http://127.0.0.1').pathname);
      if (pathname === '/__moneyball-arm' && req.method === 'POST') { armed = true; res.writeHead(204); res.end(); return; }
      if (req.method !== 'GET') { res.writeHead(405); res.end(); return; }
      let body, type;
      if (pathname === '/__moneyball-test.mjs') { body = module; type = 'text/javascript'; }
      else {
        const target = resolve(build, `.${pathname}`, pathname.endsWith('/') ? 'index.html' : '');
        assert.ok(target.startsWith(`${build}${sep}`)); const file = await realpath(target); assert.ok(file.startsWith(`${build}${sep}`));
        body = await readFile(file); type = mime[extname(file)] ?? 'application/octet-stream';
        if (pathname === page) body = html.replace('</body>', '<script type="module" src="/__moneyball-test.mjs"></script></body>');
        if (armed && /^\/data\/moneyball\/v1\/views\/\d{4}-(1|3|5)\.json$/.test(pathname)) {
          const invalid = JSON.parse(body); invalid.views[0].gapSize.war.scale *= 2;
          body = JSON.stringify(invalid); armed = false; corruptions++;
        }
      }
      res.writeHead(200, { 'Content-Type': type, 'Content-Length': Buffer.byteLength(body), 'Cache-Control': 'no-store', 'X-Content-Type-Options': 'nosniff' }); res.end(body);
    } catch { res.writeHead(404); res.end('Not found'); }
  });
  t.after(async () => {
    if (browser?.child?.exitCode === null) browser.child.kill('SIGTERM');
    server.closeAllConnections(); if (server.listening) await new Promise(done => server.close(done));
    await rm(profile, { recursive: true, force: true, maxRetries: 5, retryDelay: 100 });
  });
  await new Promise((done, reject) => { server.once('error', reject); server.listen(0, '127.0.0.1', done); });
  const executable = process.env.MONEYBALL_CHROME_BIN ?? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
  browser = promisify(execFile)(executable, ['--headless', '--window-size=1440,1000', '--disable-gpu', '--disable-background-networking', '--no-first-run', '--no-default-browser-check', `--user-data-dir=${profile}`, '--dump-dom', '--virtual-time-budget=10000', `http://127.0.0.1:${server.address().port}${page}`], { timeout: 45000, maxBuffer: 8 * 1024 * 1024, signal: t.signal });
  const { stdout, stderr } = await browser;
  const result = stdout.match(/data-moneyball-test="([^"]*)"/)?.[1], error = stdout.match(/data-moneyball-error="([^"]*)"/)?.[1];
  assert.equal(result, 'passed', error ? decodeURIComponent(error) : `Mounted test did not finish. ${stderr.slice(-2000)}`);
  assert.equal(corruptions, 1, 'Expected exactly one real view-validation failure');
});
