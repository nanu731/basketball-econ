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
  const fields = ['salaries', 'context', 'exact', 'range', 'support'];
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
    const baseline = reference(); check(dots().length === one.players.length, 'Initial cohort count differs');
    choose(player); connector(); check(dots().length === one.players.length && reference() === baseline, 'Selection lost peers or changed the reference');
    q('live-table').open = true; await wait(() => q('rows').querySelectorAll('[data-player]').length === one.players.length, 'Selected table lost peers');
    dots().find(el => el.dataset.player === peer.id).dispatchEvent(new MouseEvent('click', { bubbles: true }));
    check(q('chart').querySelector('.mb-selected')?.dataset.player === peer.id && q('search').value === peer.name && dots().length === one.players.length, 'Visible peer cannot be selected'); connector();
    q('search').value = 'zzzz'; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    check(!q('chart').querySelector('.mb-selected') && dots().length === 0 && !q('rows').querySelector('[data-player]'), 'Typing did not clear selection and filter');
    choose(player); change('team', 'NYK');
    const team = one.players.filter(p => p.teams.includes('NYK')); check(team.length, 'NYK cohort missing'); choose(team[0]);
    const expectedRank = new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(team[0].valuePercentile.war);
    check(dots().length === team.length && reference() === baseline && q('selected-percentile').textContent === expectedRank, 'Team filter changed cohort reference/rank');
    check(q('selected-cohort').textContent.includes(String(one.players.length)), 'Rank denominator changed');
    change('team', ''); choose(player); change('metric', 'lebron'); connector();
    change('window', '3');
    await wait(() => !q('search').disabled && dots().length === multi.players.length, 'Three-season view did not mount'); connector();
    const multiPlayer = multi.players.find(p => p.id === player.id);
    check(q('selected-percentile').textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(multiPlayer.valuePercentile.lebron), 'Window/metric rank stayed stale');
    check(q('selected-observed').textContent.includes('of 3 seasons'), 'Observed-season context missing');
    const armed = await fetch('/__moneyball-arm', { method: 'POST' }); check(armed.ok, 'Fault was not armed'); change('window', '5');
    await wait(() => q('search').disabled && /failed validation/.test(q('status').textContent), 'Corrupt view was not rejected');
    check(fields.every(f => q(`contrast-${f}`).textContent === ''), 'A contrast field stayed stale');
    check(q('spending').disabled && !q('snapshot').hidden && q('snapshot').querySelector('tbody tr'), 'Disabled controls or SSR fallback missing');
    check(q('search').value === '' && q('suggestions').hidden && !reference(), 'Stale search or chart survived failure');
    q('retry').click(); await wait(() => !q('search').disabled && reference() && fields.every(f => q(`contrast-${f}`).textContent.trim()), 'Retry did not recover');
    check(!q('spending').disabled && q('snapshot').hidden, 'Recovered view stayed disabled');
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
          const invalid = JSON.parse(body); invalid.views[0].players[0].valuePercentile.war = 0;
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
  browser = promisify(execFile)(executable, ['--headless', '--disable-gpu', '--disable-background-networking', '--no-first-run', '--no-default-browser-check', `--user-data-dir=${profile}`, '--dump-dom', '--virtual-time-budget=10000', `http://127.0.0.1:${server.address().port}${page}`], { timeout: 45000, maxBuffer: 8 * 1024 * 1024, signal: t.signal });
  const { stdout, stderr } = await browser;
  const result = stdout.match(/data-moneyball-test="([^"]*)"/)?.[1], error = stdout.match(/data-moneyball-error="([^"]*)"/)?.[1];
  assert.equal(result, 'passed', error ? decodeURIComponent(error) : `Mounted test did not finish. ${stderr.slice(-2000)}`);
  assert.equal(corruptions, 1, 'Expected exactly one real view-validation failure');
});
