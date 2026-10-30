# Moneyball: Basketball Edition

How does an NBA player's contribution compare with his pay? This R project pairs
salary records with BBall Index's LEBRON contribution rate and LEBRON WAR season
totals, then fits a league reference for the salary–contribution relationship.

The interactive explorer lets you inspect a player's position relative to that
reference and the modeled difference associated with another $1 million in
spending. Those are descriptive comparisons. They do not measure team revenue,
a player's complete value or the causal return from spending more.

## Start here

- Open the [salary explorer](https://www.nslanalytics.com/projects/nba-economics/)
  to explore the comparisons.
- Read [Methods and limitations](#methods-and-limitations) to understand the
  reference curve, value-for-pay percentile and uncertainty bands.
- Follow [Getting started](#getting-started) to check the code or acquire inputs
  for a local analysis.

The project covers completed regular seasons. Playoff analysis is deferred.
Source data and generated exports are not included, so a fresh clone starts with
the code and tests rather than a ready-to-run NBA dataset.

## What's included

| Location | Contents |
| --- | --- |
| `R/` | Source matching, salary windows, models, bootstrap uncertainty and diagnostics. |
| `scripts/` | Acquisition, analysis, checks and export commands. |
| `tests/` | Tests and artificial fixtures. |
| `web/` | The Astro component, TypeScript chart and project-page source. |

The website files integrate with the [portfolio](https://github.com/nanu731/portfolio).
They expect a validated, versioned data bundle from the same website origin. The
browser loads a small availability index and one selected view at a time; missing
or invalid data produces an unavailable state.

## Getting started

Run scripts from the repository root. The pipeline uses R 4.6 or newer (for its checksum helper) with `dplyr`, `tibble`, `jsonlite`, `httr2` and `mgcv`; diagnostic plots and tables also use `ggplot2` and `readr`. It does not install packages automatically. Source snapshots, manifests and analysis outputs are not included in the repository.

Run the offline syntax and boundary checks:

```sh
Rscript scripts/check.R
```

The 30 October 2026 code checks reproduced and fixed an earnings-parser bug that
treated a JSON boolean as a dollar amount. Indexed salary-history lookups preserve
the complete import result while avoiding a full history scan per player-season.
Offline regressions cover malformed amounts, duplicate records and missing data;
these checks do not reverify upstream data or published findings.
Independent review also found that a cached bootstrap could retain a previous
call's timing-pilot flag. Cache replay now applies the current call's flag while
reusing identical draws; regressions check both call orders and interval gates.

Acquire a metric snapshot, preserving the request, retrieval time and response checksum:

```sh
Rscript scripts/acquire-metrics.R 2025-26
```

Collect historical earnings documents for the metric cohort. Requests are sequential, cached and bounded; acquisition stops on an access or rate-limit response. Repeating the command reuses existing snapshots.

```sh
Rscript scripts/acquire-salaries.R --season=2025-26 --max-requests=25 --delay=2
```

The request limit is an explicit acquisition budget, not an eligibility threshold. Increase it only when completing the intended cohort. To audit already cached inputs without network access:

```sh
Rscript scripts/acquire-salaries.R --season=2025-26 --offline
```

The scripts print output locations and missing-data counts. Test fixtures contain artificial examples, not NBA results.

The browser boundary tests use Node 24's native TypeScript support:

```sh
node --test web/moneyball.test.mjs
```

After acquisition completes, run the offline analysis and create browser exports:

```sh
Rscript scripts/analyze.R
```

`--pilot-only` times a small bootstrap run without creating browser results. `--all-views` requires cached metric snapshots and completed document acquisition for every supported season from 2009–10 through 2025–26. It builds the available one-, three- and five-season windows. An unavailable annual salary remains an explicit exclusion even when its document was successfully acquired.

After completing the historical cache, run the prespecified sensitivity and held-out-season checks:

```sh
Rscript scripts/sensitivity.R --all-views
```

The script writes diagnostics for broader and stricter playing-time rules, multi-season rate weighting and local salary support. Separate held-out-season checks retain annual outcomes. Review these results alongside the analysis residual plots and salary-group tables. A successful run alone does not establish model suitability.

<details>
<summary>Export a reviewed dataset for the website</summary>

The export step requires the exact reviewed data version and explicit data-reuse
clearance. It does not deploy the website:

```sh
Rscript scripts/publish-browser.R outputs/private/browser-v1/moneyball-v1-4592fef1aab01a76fd87 --confirm-clearance
```

The script checks the source fingerprint and writes a separate version under
`outputs/released/browser-v1/`. It changes publication metadata and the resulting
fingerprint/version, while preserving source files and all view values. It refuses
synthetic exports and requires the confirmation flag. The recorded clearance is
the project owner's confirmation on 2026-10-24, not a new provider license or an
independent verification of rights.

Keep review data out of a website's public directory until release checks pass;
marking a page as a draft does not make its data files private. Snapshots refresh
through an explicit acquisition and review, preserving previous versions. There
is no automatic refresh. A future current-season view would need year-to-date
labels, eligibility and staleness checks; weekly refresh remains a proposal.

</details>

## Methods and limitations

### Comparing salary and contribution

The main sample rule requires 200 total minutes and five games across the selected
window. It is a practical threshold, not an established guarantee of rating
reliability. Broader and stricter rules test how much the result depends on it.

Each metric and window compares an intercept-only baseline, a straight line and
an unconstrained five-basis spline. Five-fold validation holds out whole players,
uses the same splits for competing models and favors simpler fits when errors are
close. A second fixed split checks nonconstant selections. The method neither
forces contribution to rise with salary nor assumes diminishing returns.

Single-season points pair nominal salary with that season's contribution.
Multi-season points use average salary-cap share, provider-exposure-weighted
LEBRON and average WAR over the same observed active seasons. Missing salary in
any of those seasons excludes the player's aggregate. Reference dollars use the
ending season's cap, and shorter observed histories remain visible.

### Reading the chart

A player's above/below-reference value is the vertical distance from the curve.
The extra-$1m result compares two points on that curve. Both spending positions
need nearby observations and bracketing support; sparse ranges stay unavailable.
Player and team filters do not refit the league reference.

The value-for-pay percentile ranks that gap within the full eligible matched
league cohort for the selected metric and window. It counts gaps below the
player's, adds half of exact ties including the player, divides by the cohort
size and multiplies by 100. Ties use the exported precision; the display rounds
to one decimal. Filters preserve both the score and denominator. A zero gap need
not rank at 50, and the score does not represent overall talent, return per dollar
or statistical surprise.

Whole-player bootstrap refits give approximate pointwise bands, conditional on
the supplied scores, matched sample and selected model family. They omit errors
in individual player ratings and full model-selection uncertainty. Eligibility,
weighting, local support, influence and held-out-season diagnostics address
separate limits; these comparisons are not forecasts.

### Data limits

Stable NBA player IDs join the sources. Full-season player records remain intact
across trades; team membership does not establish which team paid a salary.
Team accounting must disclose unsupported allocations and sample coverage.

An above-reference point means more measured contribution than the fitted
reference predicts for that salary in this sample. It does not establish a bad
contract or an unrestricted market price. Rookie scales, maximum salaries,
injuries, roles and data availability all shape the comparison.

## Data and license

- [BBall Index's public LEBRON dashboard](https://fanspo.com/bbi-role-explorer/lebron)
  supplies the contribution rate and WAR. Salary-derived provider scores are
  excluded from these contribution measures.
- Fanspo's parent-linked historical earnings records supply Fanspo-reported season
  salary. These amounts are not independent verification of cash paid or cap
  charges. Missing, unsupported zero and conflicting records remain unresolved.
- Official NBA season caps put salaries on a common cap-share scale. Source links
  remain in the analysis code.

The [MIT license](LICENSE) covers original code. Upstream data, metrics, names,
logos and other third-party material retain their own rights. Free access to
source material does not grant permission to redistribute a dataset.
