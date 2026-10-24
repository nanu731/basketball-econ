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
  const checkReturns = async ready => {
    const links = [...root.querySelectorAll('[data-mb-return]')];
    check(links.length === 3 && links.filter(link => link.dataset.mbReturn === 'controls').length === 1, 'Two calculation returns and one footer controls return are required');
    for (const link of links) {
      const toControls = link.dataset.mbReturn === 'controls';
      const expected = ready ? (toControls ? '#mb-controls' : '#mb-player-comparison') : '#mb-snapshot-title';
      const label = ready ? (toControls ? 'Back to controls' : 'Back to player comparison') : 'Back to saved records';
      const target = document.querySelector(expected);
      check(target?.checkVisibility(), 'Return destination is missing or hidden');
      check(link.getAttribute('href') === expected && link.textContent === label, 'Return link does not match its route and readiness');
      for (let parent = link.parentElement; parent; parent = parent.parentElement) if (parent instanceof HTMLDetailsElement) parent.open = true;
      link.click(); await wait(() => document.activeElement === target, 'Return action did not focus its visible destination');
    }
  };
  const json = async url => { const response = await fetch(url); check(response.ok, `Missing ${url}`); return response.json(); };
  const change = (key, value) => { q(key).value = value; q(key).dispatchEvent(new Event('change', { bubbles: true })); };
  const dots = () => [...q('chart').querySelectorAll('circle[data-player]')];
  const reference = () => q('chart').querySelector('.mb-reference')?.getAttribute('d');
  const fields = ['summary', 'context', 'anchor', 'difference', 'range', 'support', 'precision'];
  const choose = player => {
    q('search').value = player.name; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    const option = [...q('suggestions').querySelectorAll('[data-suggestion-index]')].find(el => el.querySelector('[data-suggestion-name]')?.textContent === player.name);
    check(option && option.querySelector('small')?.textContent.trim(), 'Actual player suggestion or team context missing'); option.click();
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
    await wait(() => !q('search').disabled && reference() && fields.every(f => q(`million-${f}`).textContent.trim()), 'Mount did not finish');
    await document.fonts.ready;
    const baseline = reference(); check(dots().length === one.players.length, 'Initial cohort count differs');
    const chartTop = q('chart').getBoundingClientRect().top + scrollY;
    check(document.documentElement.scrollWidth <= innerWidth + 1, 'Initial example widens the page');
    const initialExample = one.players.find(p => p.id === q('chart').querySelector('.mb-selected')?.dataset.player);
    check(q('search').value === '' && q('search').placeholder === 'Search by name' && root.querySelector('label[for="mb-player-search"]').textContent.startsWith('Search player'), 'Initial search is not an empty labeled name search');
    check(initialExample?.label && q('search-result').textContent.startsWith(`Example ${initialExample.name}`) && q('selection-name').textContent.startsWith('Example:'), 'Initial comparison is not a clearly labeled validated example');
    check(scrollY === 0 && document.activeElement !== q('selection-panel'), 'Initial example auto-scrolled or focused the result');
    const exampleBounds = q('example-buttons').getBoundingClientRect();
    check([...q('example-buttons').querySelectorAll('button')].every(button => { const r = button.getBoundingClientRect(); return r.height >= 44 && r.left >= exampleBounds.left - 1 && r.right <= exampleBounds.right + 1; }), 'Examples require horizontal scrolling or have undersized targets');
    q('search').value = ''; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    check(!q('chart').querySelector('.mb-selected') && !q('search-result').textContent.startsWith('Example'), 'Editing did not clear the initial example');
    check(chartTop - (root.getBoundingClientRect().top + scrollY) < Math.min(650, innerHeight * .8), 'Default graph remains too far below controls');
    if (innerWidth >= 1100) check(chartTop < innerHeight * .75, 'Desktop first graph remains too far below the fold');
    check(document.documentElement.scrollWidth <= innerWidth + 1, 'Default page overflows horizontally');
    const readableMetric = () => {
      const select = q('metric'), canvas = document.createElement('canvas'), context = canvas.getContext('2d'); context.font = getComputedStyle(select).font;
      check(select.clientWidth >= context.measureText(select.selectedOptions[0].textContent).width + 26, 'Active long metric label is clipped');
    };
    readableMetric();
    if (innerWidth <= 704) {
      const bounds = key => q(key).getBoundingClientRect(), metricBounds = bounds('metric'), seasonBounds = bounds('season'), teamBounds = bounds('team'), resetBounds = bounds('reset');
      check(bounds('search').width > root.clientWidth * .9 && Math.abs(metricBounds.top - seasonBounds.top) < 1 && Math.abs(teamBounds.top - resetBounds.top) < 1, 'Phone controls are not full-width search followed by paired contribution/season and team/reset');
      check(metricBounds.bottom <= teamBounds.top && seasonBounds.bottom <= resetBounds.top, 'Phone control rows overlap');
    }
    check(q('period').textContent.includes('Regular season') && q('metric-note').textContent.includes('fill-in'), 'Season scope or plain zero baseline missing');
    const contextPanel = q('context-controls');
    check(contextPanel.tagName === 'DETAILS' && !contextPanel.open && contextPanel.querySelector('summary').textContent === 'Time span', 'Named native context disclosure missing or initially open');
    check(['season', 'team', 'window', 'reset', 'units'].every(k => root.querySelectorAll(`[data-mb-${k}]`).length === 1), 'Controls are duplicated');
    check(['season', 'team', 'reset', 'units'].every(k => !q(k).closest('details')) && contextPanel.contains(q('window')) && !contextPanel.contains(q('units')) && q('units').checkVisibility(), 'Primary context/reset is hidden or advanced controls are misplaced');
    check(q('team').value === '' && !q('period').closest('details') && !q('window-note').closest('details'), 'Collapsed active context is hidden');
    const hero = document.querySelector('.question-band.band');
    check(hero && parseFloat(getComputedStyle(hero).paddingTop) === 16 && parseFloat(getComputedStyle(hero).paddingBottom) === 16, 'Project question-band breathing room did not apply');
    if (innerWidth > 704) { const nav = document.querySelector('body > header nav'); check(parseFloat(getComputedStyle(nav).paddingTop) === 8 && parseFloat(getComputedStyle(nav).paddingBottom) === 8, 'Roomier desktop navigation padding did not apply'); }
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
    check(!q('examples').closest('fieldset') && q('examples').previousElementSibling === q('selection-panel') && q('examples').getBoundingClientRect().top >= q('selection-panel').getBoundingClientRect().bottom + 8, 'Examples are not after the result with enough room for its focus outline');
    check(examples.every(b => b.getBoundingClientRect().height >= 44 && getComputedStyle(b).fontFamily === getComputedStyle(q('examples')).fontFamily && getComputedStyle(b).borderTopStyle === 'solid'), 'Example buttons lost house styling or 44px targets');
    examples[0].click(); check(q('chart').querySelector('.mb-selected')?.dataset.player === examples[0].dataset.player && dots().length === one.players.length, 'Example did not commit with peers');
    check(document.activeElement === q('selection-panel') && q('selection-panel').getBoundingClientRect().top >= -1 && q('selection-panel').getBoundingClientRect().top < innerHeight, 'Example selection did not focus and reveal the changed result');
    const beforeShortcut = q('search').value; q('find-player').click();
    check(document.activeElement === q('search') && q('search').value === beforeShortcut && root.querySelectorAll('input[type=search]').length === 1, 'Find shortcut lost continuity or duplicated search');
    const association = (view, metric) => {
      const knot = view.models[metric].curve.find(k => k.extraMillion.supported), result = knot.extraMillion;
      const currency = v => new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', minimumFractionDigits: 0, maximumFractionDigits: 2 }).format(v);
      const signed = v => `${v > 0 ? '+' : v < 0 ? '−' : ''}${Math.abs(v) !== 0 && (Math.abs(v) < .005 || Math.abs(v) >= 1e6) ? String(Number(Math.abs(v).toPrecision(3))) : new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(Math.abs(v))}`;
      const unit = metric === 'lebron' ? 'impact points/100 possessions' : view.window > 1 ? 'estimated wins/observed season' : 'estimated wins';
      check(q('million-summary').textContent.includes(`corresponds to about ${signed(result.estimate)} ${unit} on the fitted league line`) && q('million-summary').textContent.includes('does not predict the effect of a raise'), 'Association sentence differs from the saved supported contrast or lacks its causal limit');
      check(view.window === 1 ? q('million-summary').textContent.includes('annual salary') : q('million-summary').textContent.includes('average annual pay') && q('million-summary').textContent.includes('cap-equivalent dollars'), 'Association salary basis stayed stale');
      check(q('million-anchor').textContent === `${currency(knot.x * view.referenceCapUsd)} → ${currency(knot.x * view.referenceCapUsd + 1e6)}`, 'Technical anchor differs from saved supported pair');
      check(q('million-difference').textContent === `${signed(result.estimate)} ${unit}` && (result.low === null ? q('million-range').textContent === 'Unavailable' : q('million-range').textContent === `${signed(result.low)} to ${signed(result.high)} ${unit}`), 'Technical difference/range differ from exported joint result');
      check(q('million-support').textContent === `${result.nStart} near the lower salary; ${result.nEnd} near the higher salary`, 'Technical support counts differ');
      check(!root.querySelector('input[type=range], .mb-scenarios') && !q('million-summary').children.length && q('million-summary').closest('.mb-methods') && q('million-details').closest('.mb-methods'), 'Retired slider/pair survived or evidence is outside methodology');
    };
    association(one, 'war'); const defaultAssociation = q('million-summary').textContent;
    const gapSize = (chosen, metric, view) => {
      const abs = view.players.map(p => Math.abs(p.residual[metric])).sort((a, b) => a - b), mid = Math.floor(abs.length / 2);
      const scale = abs.length % 2 ? abs[mid] : (abs[mid - 1] + abs[mid]) / 2, ratio = chosen.residual[metric] / scale;
      const magnitude = Math.abs(ratio), formatted = magnitude !== 0 && (magnitude < .005 || magnitude >= 1e6) ? String(Number(magnitude.toPrecision(3))) : new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(magnitude);
      const ending = `${view.endSeason - 1}–${String(view.endSeason % 100).padStart(2, '0')}`;
      check(!q('selection-context').hidden && q('selection-context').textContent.includes(ending) && q('period').textContent.includes(ending) && q('selection-context').textContent.includes('Regular season'), 'Selected result or chart lacks its active season basis');
      if (view.window > 1) check(q('selection-context').textContent.includes(`${view.window} seasons`) && q('selected-unit').textContent.includes(metric === 'war' ? 'per observed season' : 'average impact') && q('selected-observed').textContent.includes(`of ${view.window} seasons`), 'Selected multi-season basis is incomplete');
      check(q('selected-magnitude').textContent.startsWith(`${ratio > 0 ? '+' : ratio < 0 ? '−' : ''}${formatted} × typical league gap`), 'Selected magnitude differs from the full league cohort');
      check(q('search-result').textContent.includes(chosen.name) && q('search-result').textContent.includes(q('selected-gap').closest('dd').getAttribute('aria-label')) && q('search-result').textContent.includes('vs salary reference'), 'Search feedback lacks the selected native gap');
      check(q('selected-unit').textContent.includes(metric.toUpperCase()) && q('selected-unit').textContent.includes(metric === 'war' ? 'includes playing time' : 'impact points/100 possessions') && !/[a-z]/i.test(q('selected-contribution').textContent), 'Shared result unit is missing or values repeat it');
      check(q('table-contribution')?.textContent.includes(metric.toUpperCase()), 'Active live-table contribution metric is missing or stale');
      const scaleText = scale < .005 || scale >= 1e6 ? String(Number(scale.toPrecision(3))) : new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(scale);
      check(q('selected-scale').textContent.includes(`about ${scaleText}`) && q('selected-scale').textContent.includes('median distance') && !q('selected-scale').textContent.includes('matched players') && q('selected-cohort').textContent.includes(`${view.players.length} eligible matched players`), 'Rounded native scale or single full cohort context missing');
      check(getComputedStyle(q('selected-magnitude')).fontSize === '18px' && getComputedStyle(q('selected-magnitude')).fontWeight === '600', 'Gap-size number hierarchy missing');
    };
    choose(player); connector();
    check(!q('comparison-details').open && q('selected-percentile').closest('details') === q('comparison-details') && !q('selected-gap').closest('details'), 'Primary gap or secondary comparison hierarchy is wrong');
    check(q('search-result').getAttribute('role') === 'status' && getComputedStyle(q('search-result')).position === 'absolute' && getComputedStyle(q('search-result')).clipPath === 'inset(50%)' && q('search-instructions').textContent === '' && !root.querySelector('[data-mb-view-comparison]'), 'Selected feedback is not screen-reader-only or a visible comparison line remains');
    check(q('guide').textContent.startsWith('Select a dot or search.') && q('guide').getBoundingClientRect().top < q('chart').getBoundingClientRect().top && root.querySelector('.mb-ranked-link').getBoundingClientRect().top < q('chart').getBoundingClientRect().top, 'Invitation or ranked route is below the plot');
    check([...root.querySelector('.mb-comparison').children].every(group => group.tagName === 'DIV' && [...group.children].map(el => el.tagName).join('|') === 'DT|DD'), 'Comparison description-list groups contain invalid children');
    check(q('selected-salary-basis').parentElement === q('selected-salary').parentElement && q('selected-salary-basis').parentElement.tagName === 'DD', 'Salary and its basis are not in the same pay definition');
    const basisBounds = q('selected-salary-basis').getBoundingClientRect(), payBounds = q('selected-salary').getBoundingClientRect();
    if (innerWidth <= 704) check(basisBounds.width > q('comparison').getBoundingClientRect().width * .9, 'Phone salary basis is trapped in a narrow value column');
    else check(Math.abs(basisBounds.left - payBounds.left) < 1 && basisBounds.top >= payBounds.bottom, 'Desktop salary basis is not aligned below its pay value');
    const legend = q('chart').closest('.mb-chart-layout').querySelector('.mb-legend');
    check(legend.getBoundingClientRect().top < q('selection-panel').getBoundingClientRect().top && legend.contains(q('find-player')), 'Legend/search alternative is not between plot and facts');
    q('comparison-details').open = true; gapSize(player, 'war', one); check(q('million-summary').textContent === defaultAssociation, 'Player search changed league association');
    const pinnedRank = q('selected-percentile').textContent; hover(dots().find(d => d.dataset.player === peer.id));
    check(hoveredName() === peer.name && q('chart').querySelector('.mb-selected')?.dataset.player === player.id && q('search').value === player.name && q('selected-percentile').textContent === pinnedRank, 'Peer hover replaced the pinned comparison');
    q('chart').dispatchEvent(new PointerEvent('pointerleave', { pointerType: 'mouse' })); check(!hoveredName(), 'Chart exit retained hover');
    check(q('selected-reference').textContent.includes(new Intl.NumberFormat('en-US', { maximumFractionDigits: 2, minimumFractionDigits: 2 }).format(player.fitted.war)), 'Selected reference estimate missing');
    check(/^[+−]/.test(q('selected-gap').textContent) && q('peer-key').dataset.hollow === 'true' && !q('selected-key').hidden, 'Signed gap or selected legend missing');
    check(dots().length === one.players.length && reference() === baseline, 'Selection lost peers or changed the reference');
    q('live-table').open = true; await wait(() => q('rows').querySelectorAll('[data-player]').length === one.players.length, 'Selected table lost peers');
    const firstRecord = one.players.find(p => p.id === q('rows').querySelector('[data-player]').dataset.player), firstCells = q('rows').firstElementChild.cells;
    check(firstCells.length === 6 && firstCells[1].textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(firstRecord.residual.war) && firstCells[2].textContent.startsWith('$') && firstCells[3].textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(firstRecord.war), 'Economic table columns are missing or in the wrong order');
    check(getComputedStyle(firstCells[1]).textAlign === 'right' && getComputedStyle(firstCells[2]).textAlign === 'right', 'Economic cells are not right aligned');
    const region = q('rows').closest('.mb-table-scroll'), recordTable = q('rows').closest('table');
    // Force horizontal overflow independently of the test window; root also inspects the actual 375px viewport.
    region.style.maxWidth = '343px'; region.focus();
    check(getComputedStyle(recordTable).display === 'table' && getComputedStyle(recordTable).overflowX === 'visible' && region.scrollWidth > region.clientWidth, 'Labeled wrapper is not the sole horizontal scroll owner');
    check(region.clientHeight <= Math.min(parseFloat(getComputedStyle(document.documentElement).fontSize) * 28, innerHeight * .7) + 1 && region.scrollHeight > region.clientHeight && q('rows').querySelector('button').getBoundingClientRect().height >= 44, 'Interactive table is unbounded or player targets too small');
    check(document.activeElement === region, 'Labeled table region did not receive focus');
    // Native ArrowRight is verified separately in root's real Chrome check; this checks actual scroll ownership.
    region.scrollLeft = 80; check(region.scrollLeft > 0, 'Focused labeled table region does not own horizontal scroll');
    check(Math.abs(recordTable.querySelector('tbody th').getBoundingClientRect().left - region.getBoundingClientRect().left) < 2, 'Player identity did not stay visible during horizontal scroll');
    region.scrollTop = 160;
    check(Math.abs(recordTable.querySelector('thead th').getBoundingClientRect().top - region.getBoundingClientRect().top) < 2, 'Table header did not stay visible while scrolling');
    region.scrollTop = 0; region.scrollLeft = 0; region.style.maxWidth = '';
    const order = [...q('rows').querySelectorAll('[data-player]')].map(button => one.players.find(p => p.id === button.dataset.player).residual.war);
    check(order.every((value, i) => i === 0 || order[i - 1] >= value) && q('table-order').textContent.includes('WAR'), 'Table is not ordered/labeled by active native gap');
    check([...q('rows').querySelectorAll('tr')].every(row => !row.lastElementChild.textContent.includes('×')), 'Table repeats the multiple unit in each row');
    q('rows').querySelector(`[data-player="${peer.id}"]`).click();
    check(document.activeElement === q('selection-panel') && q('selection-panel').getBoundingClientRect().top >= -1 && q('selection-panel').getBoundingClientRect().top < innerHeight && q('search').value === peer.name, 'Table selection did not reveal/focus the comparison');
    choose(player);
    check(document.documentElement.scrollWidth <= innerWidth + 1, 'Record table overflow escaped onto the page');
    dots().find(el => el.dataset.player === peer.id).dispatchEvent(new MouseEvent('click', { bubbles: true }));
    check(q('chart').querySelector('.mb-selected')?.dataset.player === peer.id && q('search').value === peer.name && dots().length === one.players.length, 'Visible peer cannot be selected'); connector();
    q('search').value = 'zzzz'; q('search').dispatchEvent(new Event('input', { bubbles: true }));
    check(q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '', 'Draft input retained gap size');
    check(!q('chart').querySelector('.mb-selected') && dots().length === 0 && !q('rows').querySelector('[data-player]'), 'Typing did not clear selection and filter');
    check(!q('no-matches').hidden && q('count').textContent.includes('0 search matches') && q('search-instructions').textContent.includes('No players found') && q('search-result').textContent === q('search-instructions').textContent, 'No-match state is not visible and announced beside the plot');
    q('clear-search').click(); check(q('search').value === '' && q('no-matches').hidden && dots().length === one.players.length, 'Clear search did not restore chart peers');
    choose(player); check(q('team').getBoundingClientRect().height >= 44, 'Visible team control is unusable'); change('team', 'NYK');
    const team = one.players.filter(p => p.teams.includes('NYK')); check(team.length, 'NYK cohort missing'); choose(team[0]);
    const expectedRank = new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(team[0].valuePercentile.war);
    check(dots().length === team.length && reference() === baseline && q('selected-percentile').textContent === expectedRank, 'Team filter changed cohort reference/rank');
    check(q('team-scope').textContent.includes('full matched records across teams') && q('team-basis').textContent.includes('fixed during player search') && q('team-context').getBoundingClientRect().top < q('chart').getBoundingClientRect().top, 'Team population or search scope is missing near the chart controls');
    const teamTotal = one.teams.find(t => t.team === 'NYK'), teamValues = [...q('team-summary').querySelectorAll('dd')].map(el => el.textContent);
    check(teamValues[0] === `${teamTotal.attributablePlayerSeasons} of ${teamTotal.eligiblePlayerSeasons} player-seasons` && teamValues[1] === new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD', maximumFractionDigits: 0 }).format(teamTotal.salaryUsd), 'Structured team totals differ from saved assigned sample');
    const fixedTotals = q('team-summary').textContent; q('search').value = team[0].name.slice(0, 3); q('search').dispatchEvent(new Event('input', { bubbles: true })); check(q('team-summary').textContent === fixedTotals, 'Draft player search changed fixed team totals'); choose(team[0]);
    check(!contextPanel.open && q('team').selectedOptions[0].textContent === 'New York Knicks', 'Visible team context stayed stale');
    check([...q('example-buttons').querySelectorAll('button')].every(b => team.some(p => p.id === b.dataset.player && p.label)), 'Team examples stayed stale');
    check(q('coverage-summary').textContent.startsWith(`League coverage: ${one.coverage.matched} matched of ${one.coverage.eligible} eligible`) && !q('coverage-summary').textContent.includes('Knicks'), 'League coverage reads as selected-team coverage');
    check(q('selected-cohort').textContent.includes(String(one.players.length)), 'Rank denominator changed'); gapSize(team[0], 'war', one); association(one, 'war'); check(q('million-summary').textContent === defaultAssociation, 'Team filter changed league association');
    change('team', ''); choose(player); change('metric', 'lebron'); connector();
    gapSize(player, 'lebron', one);
    check(q('metric-note').textContent.includes('league-average'), 'Rate zero baseline missing'); association(one, 'lebron');
    contextPanel.querySelector('summary').click(); check(contextPanel.open && q('window').getBoundingClientRect().height >= 44, 'Advanced span control is unusable');
    change('window', '3');
    check(['coverage-facts', 'coverage-exclusions', 'view-model-name', 'view-context', 'team-scope', 'team-basis'].every(k => q(k).textContent === '') && q('team-context').hidden, 'Loading retained current population or team facts');
    check(['selected-unit', 'selected-salary-basis', 'selected-direction'].every(k => q(k).textContent === '') && !q('selected-gap').closest('dd').hasAttribute('aria-label'), 'Loading retained result units or an accessibility value');
    check(fields.every(f => q(`million-${f}`).textContent === '') && !q('example-buttons').childElementCount && q('find-player').disabled && q('selected-reference').textContent === '' && q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '' && q('selection-context').hidden && q('selection-context').textContent === '', 'Loading retained stale guided/result fields');
    await wait(() => !q('search').disabled && dots().length === multi.players.length, 'Three-season view did not mount'); connector();
    const multiPlayer = multi.players.find(p => p.id === player.id); gapSize(multiPlayer, 'lebron', multi);
    check(q('selected-percentile').textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).format(multiPlayer.valuePercentile.lebron), 'Window/metric rank stayed stale');
    check(q('selected-observed').textContent.includes('of 3 seasons'), 'Observed-season context missing');
    association(multi, 'lebron'); check(q('million-context').textContent.includes('cap-equivalent') && q('million-context').textContent.includes('LEBRON'), 'Multi-year comparison basis stayed stale');
    check(q('season-label').textContent === 'Season ending' && q('selected-salary-basis').textContent.includes('Average annual pay, adjusted to') && q('selected-salary-basis').textContent.includes('salary cap'), 'Multi-year salary or season-ending basis is ambiguous');
    change('units', 'share'); check(q('selected-salary').textContent === new Intl.NumberFormat('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(multiPlayer.salaryCapShare * 100) + '%' && q('selected-salary-basis').textContent.includes('each observed season'), 'Selected pay ignored cap-share units'); change('units', 'usd');
    readableMetric(); check(q('period').textContent.includes('3 seasons') && q('window-note').hidden && q('window-note').textContent === '' && q('selected-unit').textContent.includes('average impact'), 'Average context was hidden');
    const armed = await fetch('/__moneyball-arm', { method: 'POST' }); check(armed.ok, 'Fault was not armed'); hover(dots()[0]); change('window', '5'); check(!hoveredName(), 'Loading retained hover');
    await wait(() => q('search').disabled && /failed validation/.test(q('status').textContent), 'Corrupt view was not rejected');
    check(!hoveredName(), 'Failed view retained hover');
    check(fields.every(f => q(`million-${f}`).textContent === ''), 'A contrast field stayed stale');
    check(!q('example-buttons').childElementCount && q('examples').hidden && q('find-player').disabled && q('selected-reference').textContent === '' && q('coverage-summary').textContent === '' && q('selected-magnitude').textContent === '' && q('selected-scale').textContent === '' && q('selection-context').hidden && q('selection-context').textContent === '', 'Failed view retained new results/actions');
    check(!q('reset').disabled && q('units').disabled, 'Reset lost recovery availability outside the controls fieldset');
    check(q('search').disabled && !q('snapshot').hidden && q('snapshot').querySelector('tbody tr'), 'Disabled controls or SSR fallback missing');
    check(q('empty').getBoundingClientRect().height > 0 && getComputedStyle(q('chart')).display === 'none' && getComputedStyle(q('selection-panel')).display === 'none' && getComputedStyle(q('chart').closest('.mb-plot').querySelector('.mb-y')).display === 'none' && q('chart').closest('.mb-stage').getBoundingClientRect().height < 300, 'Unavailable chart did not compact to an honest recovery panel');
    check(q('retry').getBoundingClientRect().height >= 44 && q('reset').getBoundingClientRect().height >= 44, 'Recovery actions are not visible');
    check(q('guide').hidden && q('no-matches').hidden && !q('retry').hidden && q('snapshot').querySelector('h3').textContent.includes('2025–26'), 'Failure guidance or saved record label is wrong');
    check(q('search').value === '' && q('suggestions').hidden && !reference(), 'Stale search or chart survived failure');
    await checkReturns(false);
    q('retry').click(); await wait(() => !q('search').disabled && reference() && fields.every(f => q(`million-${f}`).textContent.trim()), 'Retry did not recover');
    check(!q('search').disabled && !q('units').disabled && !q('reset').disabled && q('snapshot').hidden, 'Recovered view stayed disabled');
    const fiveView = index.views.find(v => v.endSeason === initial.endSeason && v.window === 5); association((await json(fiveView.url)).views[0], 'lebron');
    q('reset').click(); await wait(() => !q('search').disabled && q('window').value === '1' && dots().length === one.players.length, 'Reset did not return to default view');
    check(q('metric').value === 'war' && q('units').value === 'usd' && q('team').value === '' && q('search').value === '', 'Reset did not restore the default controls and empty search');
    check(q('selection-name').textContent === `Example: ${initialExample.name}`, 'Reset did not restore the labeled default example'); gapSize(initialExample, 'war', one);
    check(q('snapshot').querySelector('thead').textContent.includes('gap size') && q('snapshot').querySelector('thead').textContent.includes('LEBRON') && q('rows').closest('table').querySelectorAll('thead th').length === 6, 'Full saved records or the simplified active-metric live table is wrong');
    const priorEntry = index.views.find(v => v.endSeason === initial.endSeason - 1 && v.window === 1), prior = (await json(priorEntry.url)).views[0];
    change('season', String(prior.endSeason)); await wait(() => !q('search').disabled && dots().length === prior.players.length, 'Prior year did not load');
    choose(prior.players[0]); gapSize(prior.players[0], 'war', prior); association(prior, 'war');
    q('reset').click(); await wait(() => !q('search').disabled && q('season').value === String(initial.endSeason) && dots().length === one.players.length, 'Reset from prior year failed');
    check(q('selection-name').textContent === `Example: ${initialExample.name}` && q('search').value === '', 'Year reset did not restore the labeled default example'); gapSize(initialExample, 'war', one); association(one, 'war');
    change('window', '5'); await wait(() => !q('search').disabled && q('window').value === '5', 'Five-season setup did not recover');
    check(q('search').value === '', 'Retaining a starter comparison filled the search query');
    const earliest = index.views.filter(v => v.window === 1).sort((a, b) => a.endSeason - b.endSeason)[0];
    change('season', String(earliest.endSeason)); await wait(() => !q('search').disabled && q('window').value === '1', 'Unsupported time span did not fall back safely');
    check(q('status').textContent.includes('5-season span is unavailable') && q('status').textContent.includes('showing 1 season'), 'Automatic span fallback was silent');
    q('reset').click(); await wait(() => !q('search').disabled && q('season').value === String(initial.endSeason) && dots().length === one.players.length, 'Final reset failed');
    check(document.getElementById('mb-ranked-records').tabIndex >= 0, 'Player records summary is missing its native sequential Tab stop');
    const rankedLink = root.querySelector('a[href="#mb-ranked-records"]'); q('live-table').open = false; rankedLink.click(); await wait(() => q('live-table').open && document.activeElement === document.getElementById('mb-ranked-records'), 'Ranked comparisons link did not open/focus records');
    const bypass = root.querySelector('#mb-scroll-help a'); bypass.click(); await wait(() => document.activeElement === document.getElementById('mb-about-title'), 'Table bypass did not focus About');
    root.querySelector('.mb-controls-return').click(); await wait(() => document.activeElement === document.getElementById('mb-controls'), 'Controls return did not focus controls');
    const skip = document.querySelector('[data-mb-skip]'); check(document.body.firstElementChild === skip, 'Project skip link is not before navigation'); skip.click(); await wait(() => document.activeElement === root, 'Skip link did not focus explorer');
    const methodPanels = [...root.querySelectorAll('.mb-methods > details')];
    check(methodPanels.length === 3 && methodPanels.map(panel => panel.querySelector('summary').textContent).join('|') === 'Reading the chart|Methods and limitations|Sources and code', 'Methods/source destinations were not consolidated');
    check(q('view-sources').querySelectorAll('a').length === 3 && !/retrieved|[a-f0-9]{64}/.test(q('view-sources').textContent) && q('view-source-versions').textContent.includes('retrieved'), 'Source attribution/provenance layers are wrong');
    const coverageValues = [...q('coverage-facts').querySelectorAll('dd')].map(el => el.textContent);
    check(coverageValues.slice(0, 5).join('|') === [one.coverage.eligible, one.coverage.matched, one.coverage.sourcePlayerSeasons, one.coverage.eligiblePlayerSeasons, one.coverage.unknownSalaryPlayerSeasons].join('|'), 'Labeled coverage populations differ from the saved fields');
    check([...q('coverage-exclusions').querySelectorAll('dd')].map(el => el.textContent).join('|') === one.coverage.excluded.map(e => e.count).join('|'), 'Reported exclusion counts changed');
    check(q('view-model-name').textContent === one.models.war.name && q('view-method').closest('details') !== q('view-model-name').closest('details'), 'Human model context and exact records are not separated');
    check(q('view-method').textContent === one.models.war.method && [...q('view-validation').children].map(li => li.textContent).join(' | ') === one.models.war.validation, 'Labeled technical method/validation differs from the export');
    for (const list of root.querySelectorAll('[data-mb-saved-validation]')) check([...list.children].map(li => li.textContent).join(' | ') === one.models[list.getAttribute('data-mb-saved-validation')].validation, 'Saved validation clauses differ from the export');
    check(getComputedStyle(q('snapshot').querySelector('.mb-table-scroll')).maxHeight === 'none', 'Saved no-JS record access was bounded');
    for (const [term, targetId] of [['mb-gap-size', 'mb-gap-size-calculation'], ['mb-value-percentile', 'mb-percentile-calculation']]) {
      const link = root.querySelector(`definition-help[data-term="${term}"] [data-detail-target]`), target = document.getElementById(targetId);
      check(link?.dataset.detailTarget === targetId && target?.matches('h4[data-reference-target][tabindex="-1"]'), 'Full calculation link does not target its own focusable formula heading');
      const back = target.nextElementSibling; check(back?.matches('a[href="#mb-player-comparison"]') && back.textContent === 'Back to player comparison', 'Calculation lacks a nearby native return link');
    }
    await checkReturns(true);
    document.body.dataset.moneyballTest = 'passed';
  } catch (error) {
    document.body.dataset.moneyballTest = 'failed'; document.body.dataset.moneyballError = encodeURIComponent(error.stack ?? String(error));
  }
}

test('real mounted Moneyball retains comparison context and clears/retries corrupt views', { skip: !process.env.MONEYBALL_BUILD_DIR, timeout: 60000 }, async t => {
  const build = await realpath(resolve(process.env.MONEYBALL_BUILD_DIR));
  const page = '/projects/moneyball-basketball-edition/';
  const html = await readFile(join(build, page, 'index.html'), 'utf8');
  assert.ok(html.match(/<p\b[^>]*data-mb-guide[^>]*>/)?.[0].includes('hidden'), 'No-JS guide must not link to unavailable interactive records');
  assert.match(html, /data-mb-million-summary[^>]*>In [^<]+\$1 million more[^<]+fitted league line/, 'Default association must be server-rendered for no-JS');
  const savedReturns = html.match(/<a\b[^>]*data-mb-return[^>]*>Back to saved records<\/a>/g) ?? [];
  assert.equal(savedReturns.length, 3, 'All three no-JS return links must name saved records');
  assert.ok(savedReturns.every(link => link.includes('href="#mb-snapshot-title"')), 'No-JS return links must target the visible saved records');
  assert.ok(html.match(/<h3\b[^>]*id="mb-snapshot-title"[^>]*>/)?.[0].includes('tabindex="-1"'), 'Saved heading must accept native return focus');
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
