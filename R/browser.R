assert_observed_export_provenance <- function(window) {
  if (!identical(window$data_origin, "observed")) stop("Browser export requires explicit observed data origin; fixtures/unverified origins are forbidden", call. = FALSE)
  provenance <- window$source_provenance
  metric <- provenance$metrics
  if (is.null(metric) || !identical(metric$source_url, LEBRON_SOURCE_URL) ||
      !is.character(metric$response_sha256) || !grepl("^[a-f0-9]{64}$", metric$response_sha256)) stop("Missing observed metric snapshot provenance", call. = FALSE)
  if (length(provenance$salaries) != 1L ||
      !identical(provenance$salaries[[1]]$salary_definition_id, FANSPO_SALARY_DEFINITION) ||
      !length(provenance$salaries[[1]]$snapshots)) stop("Missing observed earnings document provenance", call. = FALSE)
  snapshots <- provenance$salaries[[1]]$snapshots
  for (source in snapshots) {
    if (!is.character(source$response_sha256) || !grepl("^[a-f0-9]{64}$", source$response_sha256) ||
        !startsWith(source$source_url, "https://fanspo.com/nba/players/") ||
        identical(source$publication_status, "test_only")) stop("Invalid observed earnings provenance", call. = FALSE)
  }
  if (any(grepl("TEST_FIXTURE|SYNTHETIC", window$points$name, ignore.case = TRUE))) stop("Fixture player names are forbidden in observed exports", call. = FALSE)
  invisible(TRUE)
}

browser_model <- function(curve) {
  if (length(curve$knots) > 2000L) stop("Browser curve exceeds the reviewed 2000-knot limit", call. = FALSE)
  fit <- curve$fit
  if (!identical(fit$observation_unit, "player-window")) stop("Transport fits cannot become the main displayed reference", call. = FALSE)
  cv <- vapply(fit$partitions, function(partition) {
    scores <- vapply(seq_len(nrow(partition$scores)), function(i) {
      row <- partition$scores[i, ]
      paste0(row$family, " MSE ", if (row$available) format(signif(row$mse, 5), trim = TRUE) else "unavailable")
    }, character(1))
    paste0("Seed ", partition$seed, ": ", paste(scores, collapse = "; "), "; one-SE choice ", partition$selected_family)
  }, character(1))
  knots <- lapply(seq_along(curve$knots), function(i) {
    support <- curve$support[i, ]
    list(x = curve$knots[i], y = curve$expected[i], low = curve$low[i], high = curve$high[i],
      extraMillion = list(supported = support$supported, estimate = curve$contrast[i],
        low = curve$contrast_low[i], high = curve$contrast_high[i], nStart = support$n_start, nEnd = support$n_end,
        reason = if (support$supported) NULL else support$reason))
  })
  list(name = switch(fit$selected_family, intercept = "League mean", linear = "Straight line", smooth = "Unconstrained smooth"),
    method = "Equal-player fit; intercept/linear/cr-GAM k=5; training-only REML; one-SE and fixed second-partition stability rule. Filters preserve this league reference.",
    validation = paste(c(cv, paste0("Final choice: ", fit$selected_family),
      paste0("Bootstrap: ", curve$bootstrap$valid_refits, "/", curve$bootstrap$planned_draws, " valid; seed ", curve$bootstrap$seed),
      paste0("Interval status: ", curve$bootstrap$interval_status)), collapse = " | "),
    intervalLabel = curve$interval_method, curve = knots)
}

browser_view <- function(analysis) {
  if (!isTRUE(analysis$available) || isTRUE(analysis$pilot_only)) stop("An unavailable model or timing pilot cannot become a browser view", call. = FALSE)
  window <- analysis$window
  if (!identical(window$exposure_rule$id, prespecified_exposure_rules()$main$id) ||
      !identical(window$rate_weight, "provider_possessions")) stop("Sensitivity rules cannot silently replace the main browser reference", call. = FALSE)
  for (model in analysis$models) {
    if (!setequal(window$points$nba_id, model$fit$nba_ids)) stop("A filtered player subset cannot become a full league browser bundle", call. = FALSE)
  }
  if (!window$window_length %in% c(1L, 3L, 5L)) stop("Browser supports only 1, 3, and 5-year windows", call. = FALSE)
  points <- window$points
  expected <- lapply(analysis$models, function(model) visible_interpolation(model$knots, model$expected, points$salary_cap_share))
  residual <- list(lebron = points$lebron - expected$lebron, war = points$war - expected$war)
  label_ids <- unique(c(points$nba_id[head(order(points$reference_salary_usd, decreasing = TRUE), 3)],
    points$nba_id[head(order(abs(residual$lebron), decreasing = TRUE), 3)],
    points$nba_id[head(order(abs(residual$war), decreasing = TRUE), 3)]))
  players <- lapply(seq_len(nrow(points)), function(i) {
    list(id = points$nba_id[i], name = points$name[i], teams = as.list(points$teams[[i]]),
      observedSeasons = as.list(points$observed_season_ends[[i]]), minutes = points$minutes[i], possessions = points$possessions[i],
      salaryCapShare = points$salary_cap_share[i], salaryUsd = points$reference_salary_usd[i],
      lebron = points$lebron[i], war = points$war[i],
      fitted = list(lebron = expected$lebron[i], war = expected$war[i]),
      residual = list(lebron = residual$lebron[i], war = residual$war[i]),
      label = points$nba_id[i] %in% label_ids, flags = as.list(points$flags[[i]]))
  })
  excluded <- if (nrow(window$excluded)) {
    reasons <- vapply(window$excluded$reasons, `[`, character(1), 1L)
    counts <- table(reasons)
    lapply(names(counts), function(reason) list(reason = reason, count = unname(as.integer(counts[reason]))))
  } else list()
  coverage <- window$coverage
  team_rows <- analysis$team_summaries
  teams <- lapply(seq_len(nrow(team_rows)), function(i) {
    row <- team_rows[i, ]
    list(team = row$team, salaryUsd = row$reported_salary_average_annual_reference_usd, war = row$war_average_annual,
      status = if (row$status == "attributable_sample_only") "attributable-sample" else "unavailable",
      attributablePlayerSeasons = row$attributable_player_seasons, eligiblePlayerSeasons = row$eligible_player_seasons,
      excludedTradeCount = row$excluded_trade_player_seasons,
      excludedMissingCount = row$excluded_missing_player_seasons + row$excluded_unknown_attribution_player_seasons,
      note = "Eligible single-team records whose unique historical salary team agrees with metric membership. Reported salary, not verified payroll; multi-team allocations remain unavailable.")
  })
  list(id = paste(window$end_season_year, window$window_length, sep = "-"), endSeason = window$end_season_year,
    window = window$window_length, seasons = as.list(as.integer(substr(window$seasons, 1, 4)) + 1L),
    referenceCapUsd = window$reference_cap_usd,
    representation = if (window$window_length == 1L) "player-season" else "player-window",
    coverage = list(eligible = coverage$exposure_eligible_before_salary, matched = nrow(points),
      includedKnownSalaryUsd = coverage$included_known_salary_usd, allKnownActiveSalaryUsd = coverage$all_known_active_salary_usd,
      unknownSalaryPlayerSeasons = coverage$missing_salary_player_seasons, sourcePlayerSeasons = coverage$active_player_seasons,
      eligiblePlayerSeasons = coverage$eligible_player_seasons, excluded = excluded,
      note = paste0(nrow(points), " of ", coverage$exposure_eligible_before_salary,
        " exposure-eligible players have complete matched inputs. Salary coverage uses known comparable amounts only; ",
        coverage$missing_salary_player_seasons, " active player-season salaries are unknown. This is not whole-league or whole-payroll efficiency.")),
    players = players, models = lapply(analysis$models, browser_model), teams = teams)
}

build_browser_exports <- function(analyses, allow_test_fixture = FALSE) {
  if (!length(analyses)) stop("No completed analyses to export", call. = FALSE)
  origins <- vapply(analyses, function(x) {
    if (is.null(x$window$data_origin)) "unverified" else x$window$data_origin
  }, character(1))
  test_only <- all(origins == "test_fixture")
  if (test_only && !allow_test_fixture) stop("Test fixtures cannot enter browser exports", call. = FALSE)
  if (!test_only) for (analysis in analyses) assert_observed_export_provenance(analysis$window)
  views <- lapply(analyses, browser_view)
  ids <- vapply(views, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Duplicate browser view", call. = FALSE)
  order <- order(vapply(views, `[[`, integer(1), "endSeason"), -vapply(views, `[[`, integer(1), "window"), decreasing = TRUE)
  views <- views[order]; analyses <- analyses[order]; ids <- ids[order]
  provenance <- analyses[[1]]$window$source_provenance
  sources <- if (test_only) list(list(label = "SYNTHETIC TEST FIXTURE ONLY", url = "https://example.invalid/test-fixture", version = "test-only", retrieved = "2026-10-23")) else {
    salary <- provenance$salaries[[1]]
    dates <- vapply(salary$snapshots, `[[`, character(1), "retrieved_utc")
    list(list(label = "BBall Index LEBRON and WAR via Fanspo", url = "https://fanspo.com/bbi-role-explorer/lebron",
      version = provenance$metrics$response_sha256, retrieved = substr(provenance$metrics$retrieved_utc, 1, 10)),
      list(label = "Fanspo historical earnings", url = "https://fanspo.com/nba",
        version = sha256_bytes(json_bytes(salary$snapshots)), retrieved = substr(max(dates), 1, 10)),
      list(label = "NBA announced season salary caps", url = "https://pr.nba.com/nba-salary-cap-2025-26-season/",
        version = sha256_bytes(json_bytes(official_caps())), retrieved = "2026-10-23"))
  }
  definitions <- list(salary = "Fanspo-reported historical season salary. One year uses nominal dollars; multi-year views average annual salary cap shares and show end-season cap-equivalent dollars. These are not independently verified cash payments.",
    eligibility = "At least 200 total minutes and 5 games across the selected observed active seasons, with complete comparable salary and metric inputs in every such season. This operational rule does not guarantee individual-score reliability.",
    uncertainty = "Pointwise 95% whole-player case-bootstrap SE intervals, conditional on fixed provider scores, matched sample, exposure rules, representation and selected model family. These are not individual impact intervals, causal effects, simultaneous bands or full post-selection coverage. Unresolved intervals are withheld.",
    limitations = as.list(c("Salary and team filters preserve the full eligible league reference and support counts.",
      "An extra $1m is an association with contribution, not a causal purchase. Multi-year contrasts mean an extra $1m in average annual end-season cap-equivalent salary.",
      "WAR includes availability. Multi-year WAR is an average across observed matched active seasons; fully absent seasons are not imputed as zero.",
      "Provider tTmPoss weights rate averages; its exact possession convention is not owner-documented. Provider-score uncertainty is unavailable.",
      "Missing, zero-placeholder and multiple-record salaries remain unresolved. The matched sample is not the entire NBA or team payroll.",
      "Bootstrap resampling cannot recover dependence from jointly estimated provider ratings or shared team environments.")))
  common <- list(schemaVersion = "moneyball.v1", publication = list(status = "private-preview",
    note = if (test_only) "SYNTHETIC TEST FIXTURE; NEVER PUBLISH" else "Private review only. Public reuse and release remain unapproved."), sources = sources, definitions = definitions)
  fingerprint <- list(common = common, views = views)
  version <- paste0("moneyball-v1-", substr(sha256_bytes(json_bytes(fingerprint)), 1, 20))
  bundles <- lapply(seq_along(views), function(i) c(common, list(dataVersion = version, defaultView = ids[i], views = list(views[[i]]))))
  names(bundles) <- ids
  index <- list(schemaVersion = "moneyball.index.v1", dataVersion = version, defaultView = ids[1],
    views = lapply(seq_along(views), function(i) list(id = ids[i], endSeason = views[[i]]$endSeason,
      window = views[[i]]$window, url = paste0("/data/moneyball/v1/views/", ids[i], ".json"))))
  list(index = index, view_bundles = bundles, data_origin = if (test_only) "test_fixture" else "observed")
}

write_private_browser_exports <- function(exports, output_dir = "outputs/private/browser-v1") {
  if (!identical(exports$data_origin, "observed")) stop("Writing synthetic or unverified browser data is forbidden", call. = FALSE)
  for (bundle in exports$view_bundles) {
    if (!identical(bundle$publication$status, "private-preview") ||
        grepl("SYNTHETIC TEST FIXTURE|TEST_FIXTURE_", rawToChar(json_bytes(bundle)))) {
      stop("Fixture content or an unapproved release label cannot be written", call. = FALSE)
    }
  }
  destination <- file.path(output_dir, exports$index$dataVersion)
  write_immutable <- function(value, path, limit) {
    bytes <- json_bytes(value)
    if (length(bytes) > limit) stop("Browser payload exceeds byte limit: ", basename(path), call. = FALSE)
    if (file.exists(path)) {
      old <- readBin(path, "raw", n = file.info(path)$size)
      if (!identical(old, bytes)) stop("Refusing to overwrite a different browser artifact", call. = FALSE)
    } else write_new_bytes(bytes, path)
    length(bytes)
  }
  index_bytes <- write_immutable(exports$index, file.path(destination, "index.json"), 100000L)
  sizes <- vapply(names(exports$view_bundles), function(id) write_immutable(exports$view_bundles[[id]],
    file.path(destination, "views", paste0(id, ".json")), if (id == exports$index$defaultView) 1000000L else 2000000L), integer(1))
  list(directory = destination, index_path = file.path(destination, "index.json"), index_bytes = index_bytes,
       view_bytes = sizes, publication_status = "private-preview")
}
