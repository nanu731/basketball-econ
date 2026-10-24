# Moneyball: Basketball Edition

## Overview

An R analysis and interactive NBA salary explorer for [nslanalytics.com](https://www.nslanalytics.com/).

How does player contribution compare with pay, and how much additional measured contribution accompanies another $1 million in spending? The project compares salary with BBall Index's **LEBRON** contribution rate and supplied **LEBRON WAR** season total. These measures estimate basketball contribution. They do not measure revenue or a player's complete value.

The first release covers completed regular seasons; playoffs remain deferred. Salary relationships describe associations and do not establish that another dollar causes a change in contribution. The model comparison does not assume diminishing returns.

## Project details

### Analysis protocol

Players enter the main sample with 200 total minutes and five games across the selected window. This practical rule has not been established as a threshold for reliable player ratings. Broader and stricter rules test sensitivity.

For each outcome and window, compare an intercept-only baseline, a straight line and an unconstrained five-basis spline. Five-fold validation holds out entire players, uses the same partitions for competing models and prefers a simpler fit when errors are close. A second fixed partition checks nonconstant selections. The analysis neither forces a rising curve nor assumes diminishing returns.

Single-season points pair nominal salary with that season's contribution. Multi-season points use average salary cap share, provider-exposure-weighted LEBRON and average WAR over exactly the same observed active seasons. Missing salary in any observed active season excludes that player's aggregate. Reference dollars translate average cap share using the ending season's cap. A shorter observed history stays visible beside the point.

The above/below-reference value measures vertical distance from the displayed curve. The extra-$1m result compares two points on that curve. Both spending positions need local observations and bracketing support, so sparse ranges stay unavailable. Player/team filters leave the reference fit unchanged.

**Value for pay percentile** ranks that gap within the full eligible matched league cohort for the selected metric and window. It counts gaps strictly below the player's plus half of exact ties (including the player), divides by the cohort size and multiplies by 100. Ties use residuals at their actual JSON export precision; only the displayed percentile is rounded to one decimal. Team/search filters preserve the score and denominator. It is not overall talent, causal value, return per dollar or statistical surprise, and a zero gap need not rank at 50.

Whole-player bootstrap refits provide approximate pointwise resampling bands, conditional on supplied scores, the matched sample and selected model family. They do not measure errors in individual player scores, establish causal spending effects or include full model-selection uncertainty. Eligibility, weighting, support, influence and separate out-of-season diagnostics assess different limits; they are not forecasts.

### Data and interpretation

- [BBall Index's public LEBRON dashboard](https://fanspo.com/bbi-role-explorer/lebron) supplies the rate and WAR fields. Salary-derived provider scores are excluded from the contribution measures.
- Fanspo's parent-linked historical player earnings records supply **Fanspo-reported season salary**. A reported amount is not independently verified cash paid or a team's cap charge. Missing, unsupported zero and conflicting records stay unresolved.
- Official NBA season caps, with source links retained in the analysis code, put different seasons' salaries on a common cap-share scale.

The analysis joins sources by stable NBA player IDs and retains full-season player records across trades. Team membership alone does not establish which team paid a salary. Team accounting must disclose unsupported allocations and the coverage of its eligible sample.

A point above the league reference means more measured contribution than the reference predicts for that salary within the selected sample. It does not prove the contract was a mistake or establish a player's unrestricted market price. Rookie scales, maximum salaries, injuries, roles and the availability of source data shape this comparison.

## Usage

### Reproduction

Run scripts from the repository root. The pipeline uses R 4.6 or newer (for its checksum helper) with `dplyr`, `tibble`, `jsonlite`, `httr2` and `mgcv`; diagnostic plots and tables also use `ggplot2` and `readr`. It does not install packages automatically. Provider snapshots, manifests and private analysis artifacts remain outside Git.

Run the offline syntax and boundary checks:

```sh
Rscript scripts/check.R
```

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

The scripts print private artifact paths and missingness counts. Tests use clearly marked artificial fixtures; those fixtures are not NBA results and must never become a published analysis.

The browser boundary tests use Node 24's native TypeScript support:

```sh
node --test web/moneyball.test.mjs
```

After acquisition completes, run the offline analysis and create private browser exports:

```sh
Rscript scripts/analyze.R
```

`--pilot-only` times a small bootstrap run without creating browser results. `--all-views` requires cached metric snapshots and completed document acquisition for every supported season from 2009–10 through 2025–26. It builds the available one-, three- and five-season windows. An unavailable annual salary remains an explicit exclusion even when its document was successfully acquired.

After completing the historical cache, run the prespecified sensitivity and held-out-season checks:

```sh
Rscript scripts/sensitivity.R --all-views
```

The script writes private diagnostics for broader and stricter playing-time rules, multi-season rate weighting and local salary support. Separate held-out-season checks retain annual outcomes. Review these results alongside the analysis residual plots and salary-group tables. A successful run alone does not establish model suitability.

### Website integration

The `web/` directory contains the portable Astro component, native TypeScript chart and project entry. The chart expects a validated, versioned same-origin data bundle. Missing or invalid data must produce an explicit unavailable state; no demonstration NBA results are substituted. Analysis and publication checks precede the release of any real browser bundle.

The browser loads a small availability index and one selected view at a time. Private exports remain in ignored analysis outputs. A draft page alone does not protect files in a website's public directory: private review data must stay in an isolated local preview until release checks pass.

To promote the frozen reviewed export after the project owner's explicit data reuse clearance, run from the repository root:

```sh
Rscript scripts/publish-browser.R outputs/private/browser-v1/moneyball-v1-4592fef1aab01a76fd87 --confirm-clearance
```

This offline step checks the pinned source fingerprint and writes a separate immutable version under ignored `outputs/released/browser-v1/`. Only publication metadata and its resulting fingerprint/version change; source files and all view values stay unchanged. The recorded clearance is the owner's confirmation on 2026-10-24, not a new provider license or an independent rights verification. Omitting the opt-in fails; synthetic exports remain unwritable through either publication writer. Website integration and deployment are separate steps.

Refreshing completed-season snapshots requires an explicit acquisition and review, with previous versions preserved. No automatic refresh is enabled. A future current-season mode would need a separate year-to-date label, eligibility and staleness checks. A weekly refresh remains a proposed starting schedule.

## License

The MIT license covers original code in this repository. It does not grant rights to upstream data, metrics, names, logos or third-party material. Free viewing or acquisition is separate from permission to redistribute a dataset.
