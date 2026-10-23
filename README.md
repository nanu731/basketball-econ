# Moneyball: Basketball Edition

An R analysis and interactive NBA salary explorer for [nslanalytics.com](https://www.nslanalytics.com/).

The project asks how player contribution compares with pay, and how much additional measured contribution accompanies another $1 million in spending. It compares salary with BBall Index's **LEBRON** contribution rate and supplied **LEBRON WAR** season total. These are estimates of basketball contribution, not revenue or a player's complete value.

The first release covers completed regular seasons. Playoffs are deferred. Salary relationships are descriptive: spending another dollar does not necessarily cause a change in contribution. The model comparison does not assume diminishing returns.

## Reproduction

Run scripts from the repository root. The pipeline uses R with `dplyr`, `tibble`, `jsonlite`, `httr2` and `mgcv`. It does not install packages automatically. Provider snapshots, manifests and private analysis artifacts remain outside Git.

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

## Data and interpretation

- [BBall Index's public LEBRON dashboard](https://fanspo.com/bbi-role-explorer/lebron) supplies the rate and WAR fields. Salary-derived provider scores are excluded from the contribution measures.
- Fanspo's parent-linked historical player earnings records supply **Fanspo-reported season salary**. A reported amount is not independently verified cash paid or a team's cap charge. Missing, unsupported zero and conflicting records stay unresolved.
- Official NBA season caps, with source links retained in the analysis code, put different seasons' salaries on a common cap-share scale.

Stable NBA player IDs join the sources. Full-season player records are retained across trades; team membership alone does not establish which team paid a salary. Team accounting must disclose unsupported allocations and the coverage of its eligible sample.

A point above the league reference means more measured contribution than the reference predicts for that salary within the selected sample. It does not prove the contract was a mistake or establish a player's unrestricted market price. Rookie scales, maximum salaries, injuries, roles and the availability of source data shape this comparison.

## Website integration

The `web/` directory contains the portable Astro component, native TypeScript chart and project entry. The chart expects a validated, versioned same-origin data bundle. Missing or invalid data must produce an explicit unavailable state; no demonstration NBA results are substituted. Analysis and publication checks precede the release of any real browser bundle.

## License

The MIT license covers original code in this repository. It does not grant rights to upstream data, metrics, names, logos or third-party material. Free viewing or acquisition is separate from permission to redistribute a dataset.
