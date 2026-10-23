# Offline boundary tests. All artificial observations stay under tests/.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
source("tests/fixtures/analysis-fixtures.R")

test_count <- 0L
test <- function(label, expression) {
  force(expression)
  test_count <<- test_count + 1L
  cat(sprintf("PASS %02d %s\n", test_count, label))
}
expect_error <- function(expression, pattern) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}
fixture_bytes <- readBin("tests/fixtures/lebron.json", "raw", n = 100000L)

test("normalization preserves missing and negative contribution and trade membership", {
  result <- normalize_lebron(fixture_bytes, seasons = "2025-26")
  stopifnot(nrow(result$players) == 2L,
            result$players$war[result$players$nba_id == 1641705] == -0.5,
            is.na(result$players$lebron[result$players$nba_id == 1629029]),
            result$players$lebron_status[result$players$nba_id == 1629029] == "missing",
            !any(c("salary_over_expected", "wins_over_contract") %in% names(result$players)),
            identical(sort(result$teams$team[result$teams$nba_id == 1629029]), c("DAL", "LAL")))
})

test("cached acquisition replays exact bytes offline and rejects corruption", {
  cache <- tempfile("basketball-test-cache-")
  snapshot <- acquire_lebron("2025-26", cache_dir = cache,
                            transport = function(request) fixture_bytes,
                            as_of = as.Date("2026-10-23"))
  replay <- acquire_lebron("2025-26", cache_dir = cache, offline = TRUE,
                          transport = function(request) stop("Network must not be used"),
                          as_of = as.Date("2026-10-23"))
  stopifnot(identical(snapshot$players, replay$players),
            identical(snapshot$provenance$response_sha256, replay$provenance$response_sha256),
            replay$provenance$source_url == "https://fanspo.com/bbi-role-explorer/api/lebron_dashboard_data",
            replay$provenance$request$minMinutes == 0,
            replay$provenance$request$minMpg == 0)
  writeBin(charToRaw("{}"), file.path(cache, snapshot$provenance$response_file))
  expect_error(read_lebron_snapshot(snapshot$manifest_path), "checksum")
})

test("duplicate identities and wrong-season responses cannot enter normalized data", {
  bad <- jsonlite::fromJSON(rawToChar(fixture_bytes))
  bad$players <- rbind(bad$players, bad$players[1, ])
  expect_error(normalize_lebron(json_bytes(bad), "2025-26"), "Duplicate player-season")
  expect_error(normalize_lebron(fixture_bytes, "2024-25"), "seasons")
  bad <- jsonlite::fromJSON(rawToChar(fixture_bytes))
  bad$players$nba_id[1] <- 1.5
  expect_error(normalize_lebron(json_bytes(bad), "2025-26"), "nba_id")
})

test("normalization is independent of provider row order", {
  reversed <- jsonlite::fromJSON(rawToChar(fixture_bytes))
  reversed$players <- reversed$players[2:1, ]
  a <- normalize_lebron(fixture_bytes, "2025-26")
  b <- normalize_lebron(json_bytes(reversed), "2025-26")
  stopifnot(identical(a$players, b$players), identical(a$teams, b$teams))
})

test("future seasons and unavailable offline snapshots fail explicitly", {
  expect_error(acquire_lebron("2026-27", cache_dir = tempfile(), offline = TRUE,
                             as_of = as.Date("2026-10-23")), "completion gate")
  expect_error(acquire_lebron("2025-26", cache_dir = tempfile(), offline = TRUE,
                             as_of = as.Date("2026-10-23")), "No cached")
  expect_error(normalize_lebron(charToRaw("<html>upstream error</html>"), "2025-26"), "Invalid LEBRON JSON")
})

test("cached request scope cannot silently introduce a provider minutes filter", {
  cache <- tempfile("basketball-test-cache-")
  snapshot <- acquire_lebron("2025-26", cache_dir = cache,
                            transport = function(request) fixture_bytes,
                            as_of = as.Date("2026-10-23"))
  meta <- jsonlite::fromJSON(snapshot$manifest_path, simplifyVector = FALSE)
  meta$request$minMinutes <- 200L
  writeBin(json_bytes(meta), snapshot$manifest_path)
  expect_error(read_lebron_snapshot(snapshot$manifest_path), "source/request mismatch")
})

test("salary import retains unknown identities and missing two-way amounts", {
  salaries <- read_salary_import("tests/fixtures/salaries.csv", "tests/fixtures/salaries.metadata.json")
  stopifnot(nrow(salaries$records) == 3L,
            salaries$records$salary_reported_usd[salaries$records$source_player_id == "TEST_FIXTURE_A"] == 15464700,
            is.na(salaries$records$salary_reported_usd[salaries$records$source_player_id == "TEST_FIXTURE_B"]),
            salaries$records$contract_type[salaries$records$source_player_id == "TEST_FIXTURE_B"] == "two_way",
            salaries$records$identity_status[salaries$records$source_player_id == "TEST_FIXTURE_UNMAPPED"] == "unmapped")
})

test("salary disagreement stays unresolved instead of selecting a convenient source", {
  a <- read_salary_import("tests/fixtures/salaries.csv", "tests/fixtures/salaries.metadata.json")
  csv_b <- tempfile(fileext = ".csv")
  meta_b <- tempfile(fileext = ".json")
  writeLines(c("season,nba_id,source_player_id,salary_reported_usd,contract_type",
               "2025-26,1641705,TEST_FIXTURE_A_SECOND_SOURCE,20000000,rookie"), csv_b)
  metadata <- jsonlite::fromJSON("tests/fixtures/salaries.metadata.json", simplifyVector = FALSE)
  metadata$source_id <- "test_fixture_b"
  writeBin(json_bytes(metadata), meta_b)
  b <- read_salary_import(csv_b, meta_b)
  result <- reconcile_salary_imports(list(a, b), "test_annual_compensation")
  row <- result$players[result$players$nba_id == 1641705, ]
  stopifnot(row$salary_status == "conflict", is.na(row$salary_reported_usd),
            row$reported_sources == 2L, nrow(result$unmapped) == 1L)
  expect_error(reconcile_salary_imports(list(a, b), "another_definition"), "different salary definitions")
})

test("matching uses the official season cap and never duplicates a traded player's annual record", {
  metric <- normalize_lebron(fixture_bytes, "2025-26")
  salaries <- reconcile_salary_imports(list(read_salary_import("tests/fixtures/salaries.csv", "tests/fixtures/salaries.metadata.json")), "test_annual_compensation")
  result <- match_player_seasons(metric, salaries)
  a <- result$players[result$players$nba_id == 1641705, ]
  b <- result$players[result$players$nba_id == 1629029, ]
  stopifnot(nrow(result$players) == 2L, a$salary_cap_share == 0.10, a$war == -0.5,
            a$data_status == "matched_inputs", is.na(b$salary_reported_usd),
            b$team_attribution_status == "unallocated_multi_team",
            result$coverage$salary_matched_active == 1L,
            result$coverage$missing_salary_active == 1L)
  caps <- official_caps()
  stopifnot(nrow(caps) == 17L, caps$cap_usd[caps$season == "2009-10"] == 57700000,
            caps$cap_usd[caps$season == "2025-26"] == 154647000)
})

test("salary units are explicit and a reported zero remains distinct from missing", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("season,nba_id,source_player_id,salary_reported_usd,contract_type",
               "2025-26,1641705,TEST_FIXTURE_ZERO,0,unknown"), csv)
  zero <- read_salary_import(csv, "tests/fixtures/salaries.metadata.json")
  stopifnot(zero$records$salary_reported_usd == 0, !is.na(zero$records$salary_reported_usd))
  writeLines(c("season,nba_id,source_player_id,salary_reported_usd,contract_type",
               "2025-26,1641705,TEST_FIXTURE_BAD_UNIT,15.46m,unknown"), csv)
  expect_error(read_salary_import(csv, "tests/fixtures/salaries.metadata.json"), "no unit guessing")
})

test("private export round-trip preserves unavailable salaries and prohibits release claims", {
  metric <- normalize_lebron(fixture_bytes, "2025-26")
  dataset <- match_player_seasons(metric, reconcile_salary_imports())
  files <- write_private_dataset(dataset, tempfile("basketball-private-test-"))
  restored <- readRDS(files$dataset_path)
  summary <- jsonlite::fromJSON(files$summary_path)
  stopifnot(identical(restored, dataset), all(is.na(restored$players$salary_reported_usd)),
            restored$coverage$salary_matched_active == 0L,
            is.na(restored$coverage$reported_salary_matched_sample_usd),
            summary$model_status == "not_fitted",
            summary$publication_status == "unreviewed_do_not_publish")
  again <- write_private_dataset(dataset, dirname(files$dataset_path))
  stopifnot(identical(files$dataset_sha256, again$dataset_sha256))
  writeBin(charToRaw("{}"), files$summary_path)
  expect_error(write_private_dataset(dataset, dirname(files$dataset_path)), "summary checksum")
})

test("earnings parser follows only the requested parent's historical salary references", {
  bytes <- readBin("tests/fixtures/earnings.html", "raw", n = 100000L)
  result <- normalize_earnings(bytes, "1641705")
  stopifnot(nrow(result$records) == 2L,
            setequal(result$records$salary_record_id, c("fixture-2025", "fixture-2026")),
            result$records$salary_reported_usd[result$records$season == "2024-25"] == 1234567,
            is.na(result$records$salary_reported_usd[result$records$season == "2025-26"]),
            result$records$source_salary_status[result$records$season == "2025-26"] == "unsupported_zero",
            result$data_origin == "test_fixture")
  expect_error(normalize_earnings(bytes, "2544"), "requested NBA player")
})

test("earnings snapshots replay offline and reject altered source bodies", {
  bytes <- readBin("tests/fixtures/earnings.html", "raw", n = 100000L)
  cache <- tempfile("basketball-earnings-test-")
  snapshot <- acquire_earnings("1641705", "TEST_FIXTURE_PLAYER_A", cache_dir = cache,
    transport = function(url) list(bytes = bytes, response_url = url))
  replay <- acquire_earnings("1641705", "TEST_FIXTURE_PLAYER_A", cache_dir = cache, offline = TRUE)
  stopifnot(identical(snapshot$records, replay$records))
  writeBin(charToRaw("changed"), file.path(cache, snapshot$provenance$response_file))
  expect_error(read_earnings_snapshot(snapshot$manifest_path), "checksum")
})

test("multiple historical amounts remain unresolved and zero placeholders stay missing", {
  bytes <- readBin("tests/fixtures/earnings.html", "raw", n = 100000L)
  snapshot <- normalize_earnings(bytes, "1641705")
  snapshot$provenance <- list(source_url = "https://example.invalid/synthetic-fixture", publication_status = "test_only")
  duplicate <- snapshot$records[snapshot$records$season == "2024-25", ]
  duplicate$salary_record_id <- "TEST_FIXTURE_SECOND_ANNUAL_RECORD"
  duplicate$salary_reported_usd <- 500000
  snapshot$records <- rbind(snapshot$records, duplicate)
  cohort <- tibble::tibble(season = c("2024-25", "2025-26"), nba_id = c(1641705L, 1641705L))
  imported <- earnings_salary_import(list(snapshot), cohort, c("2024-25", "2025-26"))
  reconciled <- reconcile_salary_imports(list(imported), FANSPO_SALARY_DEFINITION)
  stopifnot(all(is.na(reconciled$players$salary_reported_usd)),
            reconciled$players$salary_status[reconciled$players$season == "2024-25"] == "multiple_source_records",
            reconciled$players$salary_status[reconciled$players$season == "2025-26"] == "unsupported_zero")
})

test("standardized windows keep identical seasons for cost and both outcomes", {
  window <- make_player_window(synthetic_window_dataset(), "2025-26", 2L,
    list(id = "TEST_POSITIVE_EXPOSURE", minimum_minutes = 0, minimum_possessions = 1, minimum_games = 1))
  a <- window$points[window$points$nba_id == "1", ]
  b <- window$points[window$points$nba_id == "2", ]
  stopifnot(nrow(window$points) == 2L, a$lebron == 5, a$war == 6,
            abs(a$salary_cap_share - .15) < 1e-12,
            abs(a$reference_salary_usd - 23197050) < 1e-6,
            a$observed_seasons == 2L, b$observed_seasons == 1L, b$war == 2,
            window$excluded$nba_id == "3",
            "missing_or_unresolved_salary" %in% window$excluded$reasons[[1]])
})

test("team annual summaries use the full window denominator and require attribution evidence", {
  dataset <- synthetic_window_dataset()
  # A one-season player is attributable to LAL in this artificial worked example.
  dataset$players$team_count[dataset$players$nba_id == 2] <- 1L
  dataset$players$team_attribution_verified[dataset$players$nba_id == 2] <- TRUE
  dataset$players$attributed_team[dataset$players$nba_id == 2] <- "LAL"
  dataset$teams <- dataset$teams[!(dataset$teams$nba_id == 2 & dataset$teams$team == "DAL"), ]
  window <- make_player_window(dataset, "2025-26", 2L,
    list(id = "TEST_POSITIVE_EXPOSURE", minimum_minutes = 0, minimum_possessions = 1, minimum_games = 1))
  teams <- summarize_attributable_teams(window)
  lal <- teams[teams$team == "LAL", ]
  sas <- teams[teams$team == "SAS", ]
  stopifnot(lal$war_average_annual == 1, lal$reported_salary_average_annual_reference_usd == 7732350,
            lal$attributable_seasons == 1L, sas$war_average_annual == 6)
  dataset$players$team_attribution_verified <- FALSE
  unverified <- summarize_attributable_teams(make_player_window(dataset, "2025-26", 2L,
    list(id = "TEST_POSITIVE_EXPOSURE", minimum_minutes = 0, minimum_possessions = 1, minimum_games = 1)))
  stopifnot(all(is.na(unverified$war_average_annual)))
})

test("team validation rejects aggregate, unknown and malformed memberships with historical aliases preserved", {
  for (code in c("TOT", "ZZZ", "SAS/", "/SAS", "SAS//LAL")) {
    bad <- jsonlite::fromJSON(rawToChar(fixture_bytes))
    bad$players$team_name[1] <- code
    expect_error(normalize_lebron(json_bytes(bad), "2025-26"), "team")
  }
  stopifnot(identical(canonical_team_codes("NJN/NOH", "2009-10"), c("NJN", "NOH")),
            identical(canonical_team_codes("BRK/CHO/PHO", "2025-26"), c("BKN", "CHA", "PHX")))
  expect_error(canonical_team_codes("NJN", "2025-26"), "Unsupported")
})

test("a valid older manifest under a misleading cache filename cannot satisfy a new-season request", {
  cache <- tempfile("basketball-wrong-season-cache-")
  older <- jsonlite::fromJSON(rawToChar(fixture_bytes))
  older$players$Season <- "2024-25"; older$players$year <- 2025L
  snapshot <- acquire_lebron("2024-25", cache_dir = cache,
    transport = function(request) json_bytes(older), as_of = as.Date("2026-10-23"))
  misleading <- file.path(cache, sub("lebron-2024-25-", "lebron-2025-26-", basename(snapshot$manifest_path), fixed = TRUE))
  stopifnot(file.copy(snapshot$manifest_path, misleading))
  expect_error(acquire_lebron("2025-26", cache_dir = cache, offline = TRUE,
    as_of = as.Date("2026-10-23")), "caller request")
})

test("private matched artifacts preserve mapped candidate identities and conflicting amounts", {
  a <- read_salary_import("tests/fixtures/salaries.csv", "tests/fixtures/salaries.metadata.json")
  b <- a; b$provenance$source_id <- "TEST_SECOND_SOURCE"
  b$records$source_id <- "TEST_SECOND_SOURCE"
  b$records$source_player_id <- paste0("SECOND_", b$records$source_player_id)
  b$records$salary_reported_usd[b$records$nba_id %in% 1641705L] <- 20000000
  salary <- reconcile_salary_imports(list(a, b), "test_annual_compensation")
  matched <- match_player_seasons(normalize_lebron(fixture_bytes, "2025-26"), salary)
  saved <- write_private_dataset(matched, tempfile("basketball-private-lineage-"))
  restored <- readRDS(saved$dataset_path)
  candidates <- restored$salary_candidates[restored$salary_candidates$nba_id %in% 1641705L, ]
  stopifnot(nrow(candidates) == 2L,
            setequal(candidates$source_player_id, c("TEST_FIXTURE_A", "SECOND_TEST_FIXTURE_A")),
            setequal(candidates$salary_reported_usd, c(15464700, 20000000)))
})

test("earnings cache identity is bound to the caller during offline replay and refresh", {
  first <- readBin("tests/fixtures/earnings.html", "raw", n = 100000L)
  other <- charToRaw(gsub("1641705", "2544", rawToChar(first), fixed = TRUE))
  cache <- tempfile("basketball-wrong-player-cache-")
  source <- acquire_earnings("2544", "TEST_OTHER", cache_dir = cache,
    transport = function(url) list(bytes = other, response_url = url))
  misleading <- file.path(cache, sub("earnings-2544-", "earnings-1641705-", basename(source$manifest_path), fixed = TRUE))
  stopifnot(file.copy(source$manifest_path, misleading))
  expect_error(acquire_earnings("1641705", "TEST_FIRST", cache_dir = cache, offline = TRUE), "caller request")
  refresh_path <- file.path(cache, paste0("earnings-1641705-", sha256_bytes(first), ".manifest.json"))
  stopifnot(file.copy(source$manifest_path, refresh_path))
  expect_error(acquire_earnings("1641705", "TEST_FIRST", cache_dir = cache, refresh = TRUE,
    transport = function(url) list(bytes = first, response_url = url)), "caller request")
})

test("main window eligibility uses total minutes and games without dropping low-exposure constituent seasons", {
  rules <- prespecified_exposure_rules()
  view <- make_player_window(synthetic_window_dataset(), "2025-26", 2L, rules$main)
  stopifnot(identical(view$points$nba_id, "1"), view$points$lebron == 5,
            view$points$war == 6, view$points$games == 6,
            "includes_low_exposure_season" %in% view$points$flags[[1]])
  counts <- eligibility_sensitivity(synthetic_window_dataset(), "2025-26", 2L, rules)
  stopifnot(identical(counts$eligible_players, c(2L, 1L, 0L)))
})

test("held-out native-unit comparison recovers a line and uses stable player folds for both outcomes", {
  points <- synthetic_relationship_points("linear")
  fit <- fit_salary_relationship(points, "war")
  other <- fit_salary_relationship(points[nrow(points):1, ], "lebron")
  predicted <- predict_relationship(fit, c(.10, .30))
  stopifnot(fit$selected_family == "linear", max(abs(predicted - c(-1, 1))) < 1e-8,
            identical(fit$partitions[[1]]$fold_map, other$partitions[[1]]$fold_map),
            all(fit$partitions[[1]]$scores$scored_players[fit$partitions[[1]]$scores$available] == nrow(points)),
            all(fit$partitions[[1]]$scores$scored_players[!fit$partitions[[1]]$scores$available] == 0L))
})

test("curved visible-line contrasts and bootstrap differences use the same interpolation", {
  knots <- c(0, .10, .30)
  central <- c(0, .01, .09)
  # At .15 the visible line is .03, not the quadratic's .0225.
  stopifnot(abs(visible_interpolation(knots, central, .15) - .03) < 1e-12)
  draws <- outer(seq(-1, 1, length.out = 499), c(1, 1, 1)) + matrix(rep(central, each = 499), nrow = 499)
  result <- bootstrap_visible_intervals(knots, central, draws, at = .10, endpoint = .15)
  stopifnot(abs(result$contrast - .02) < 1e-12, result$contrast_se < 1e-12,
            all(result$curve_se > .5), result$valid_refits == 499L)
})

test("local contrasts require original-data density, bracketing and distinct salaries", {
  dense <- seq(.10, .15, length.out = 201)
  supported <- local_contrast_support(dense, c(.10, .125, .149), 154647000)
  stopifnot(identical(supported$supported, c(FALSE, TRUE, FALSE)),
            supported$reason[1] == "insufficient_bracketing", supported$reason[3] == "outside_observed_range")
  sparse_values <- rep(c(.11, .12, .13, .14), each = 10)
  repeated <- local_contrast_support(sparse_values, .12, 154647000)
  stopifnot(!repeated$supported, repeated$reason == "too_few_salary_values")
})

test("an unconstrained selected curve can decrease then increase while a constant contrast stays unavailable", {
  points <- synthetic_relationship_points("curved", 240L)
  points$war <- points$war + .02 * sin(seq_len(nrow(points)))
  fit <- fit_salary_relationship(points, "war")
  stopifnot(fit$selected_family == "smooth")
  curve <- relationship_curve(fit, 154647000)
  left <- which.min(abs(curve$knots - .10)); right <- which.min(abs(curve$knots - .30))
  stopifnot(curve$support$supported[left], curve$support$supported[right],
            curve$contrast[left] < 0, curve$contrast[right] > 0)
  constant <- fit_salary_relationship(synthetic_relationship_points("constant"), "war")
  flat <- relationship_curve(constant, 154647000)
  stopifnot(constant$selected_family == "intercept", all(is.na(flat$contrast)),
            all(flat$support$reason == "constant_model_no_resolved_salary_effect"))
})

test("coverage distinguishes annual source records from player-window points and known salary amounts", {
  window <- make_player_window(synthetic_window_dataset(), "2025-26", 2L, prespecified_exposure_rules()$broad)
  stopifnot(window$coverage$active_player_seasons == 5L, window$coverage$eligible_players == 2L,
            window$coverage$eligible_player_seasons == 3L,
            abs(window$coverage$included_known_salary_usd - 30929400) < 1e-6,
            abs(window$coverage$all_known_active_salary_usd - 31702635) < 1e-6,
            window$coverage$missing_salary_player_seasons == 1L)
})

test("single-team attribution requires agreement with one genuine historical salary team", {
  bytes <- readBin("tests/fixtures/earnings.html", "raw", n = 100000L)
  earnings <- normalize_earnings(bytes, "1641705")
  earnings$provenance <- list(source_url = "https://example.invalid/test", publication_status = "test_only")
  metric <- jsonlite::fromJSON(rawToChar(fixture_bytes))
  metric$players$Season <- "2024-25"; metric$players$year <- 2025
  metric <- normalize_lebron(json_bytes(metric), "2024-25")
  imported <- earnings_salary_import(list(earnings), metric$players, "2024-25")
  salary <- reconcile_salary_imports(list(imported), FANSPO_SALARY_DEFINITION)
  result <- match_player_seasons(metric, salary)
  a <- result$players[result$players$nba_id == 1641705, ]
  stopifnot(a$team_attribution_verified, a$attributed_team == "SAS")
  salary$source_records$source_team_name <- "Los Angeles Lakers"
  wrong <- match_player_seasons(metric, salary)
  stopifnot(!wrong$players$team_attribution_verified[wrong$players$nba_id == 1641705])
})

test("provider CHO alias is valid from 2013-14 without allowing unrelated historical franchises", {
  stopifnot(identical(canonical_team_codes("CHO/PHO", "2013-14"), c("CHA", "PHX")),
            identical(team_name_franchise_code(c("San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards", "New Jersey Nets")),
                      c("SAS", "TOR", "UTA", "WAS", "BKN")))
  expect_error(canonical_team_codes("CHO", "2012-13"), "Unsupported")
  expect_error(canonical_team_codes("BKN", "2009-10"), "Unsupported")
})

test("bootstrap timing pilots are reproducible cache replays and cannot supply primary intervals", {
  fit <- fit_salary_relationship(synthetic_relationship_points("linear"), "war")
  curve <- relationship_curve(fit, 154647000)
  cache <- tempfile("basketball-bootstrap-test-")
  pilot <- run_relationship_bootstrap(curve, 2026L, 1L, draws = 25L, cache_dir = cache, pilot = TRUE)
  replay <- run_relationship_bootstrap(curve, 2026L, 1L, draws = 25L, cache_dir = cache, pilot = TRUE)
  result <- attach_bootstrap_intervals(curve, pilot)
  stopifnot(identical(pilot$predictions, replay$predictions), pilot$valid_refits == 25L,
            all(is.na(result$low)), result$bootstrap$interval_status == "timing_pilot_only")
})

test("team filtering preserves the full league reference and support counts", {
  analysis <- analyze_player_window(synthetic_analysis_window(), pilot_only = TRUE,
                                    cache_dir = tempfile("basketball-analysis-test-"), influence = FALSE)
  filtered <- select_analysis_players(analysis, team = "SAS")
  stopifnot(nrow(filtered$window$points) == 40L, identical(filtered$models, analysis$models),
            identical(filtered$window$coverage, analysis$window$coverage))
})

test("browser export matches the visible reference, splits views and refuses synthetic writes", {
  analysis <- analyze_player_window(synthetic_analysis_window(), sentinel = FALSE,
    cache_dir = tempfile("basketball-export-test-"), influence = FALSE)
  expect_error(build_browser_exports(list(analysis)), "Test fixtures")
  exports <- build_browser_exports(list(analysis), allow_test_fixture = TRUE)
  bundle <- jsonlite::fromJSON(rawToChar(json_bytes(exports$view_bundles[[1]])), simplifyVector = FALSE)
  view <- bundle$views[[1]]
  stopifnot(exports$index$schemaVersion == "moneyball.index.v1", length(bundle$views) == 1L,
            exports$index$views[[1]]$url == "/data/moneyball/v1/views/2026-1.json",
            length(view$players) == 80L, is.list(view$players[[1]]$teams),
            abs(view$players[[1]]$residual$war) < 1e-8)
  text <- rawToChar(json_bytes(bundle))
  stopifnot(!grepl("salary_candidates|source_player_id|private_use_basis|salary_source_records", text))
  expect_error(write_private_browser_exports(exports, tempfile("basketball-denied-export-")), "synthetic or unverified")
  exports$data_origin <- "observed"
  expect_error(write_private_browser_exports(exports, tempfile("basketball-forged-origin-")), "Fixture content")
  analysis$window$data_origin <- NULL
  expect_error(build_browser_exports(list(analysis)), "explicit observed data origin")
  filtered <- select_analysis_players(analysis, team = "SAS")
  expect_error(browser_view(filtered), "filtered player subset")
})

test("transport validation groups repeated players and identifies its annual target separately", {
  points <- synthetic_relationship_points("linear", 30L)
  panel <- dplyr::bind_rows(lapply(c("2023-24", "2024-25", "2025-26"), function(season) {
    x <- points; x$season <- season; x
  }))
  expect_error(fit_salary_relationship(panel, "war"), "one equal-weight point")
  fit <- fit_salary_relationship(panel, "war", observation_unit = "player-season-transport")
  predictions <- fit$partitions[[1]]$predictions
  groups <- split(predictions$fold, predictions$nba_id)
  stopifnot(all(vapply(groups, function(x) length(unique(x)) == 1L, logical(1))),
            all(fit$partitions[[1]]$scores$scored_players[fit$partitions[[1]]$scores$available] == 30L),
            all(fit$partitions[[1]]$scores$scored_observations[fit$partitions[[1]]$scores$available] == 90L))
  root <- synthetic_window_dataset()
  root$players <- dplyr::bind_rows(lapply(c("2023-24", "2024-25", "2025-26"), function(season) {
    tibble::tibble(season = season, nba_id = as.integer(points$nba_id), played = TRUE,
      minutes = 1000, games = 40, provider_possessions = 2000,
      salary_cap_share = points$salary_cap_share, lebron = points$lebron, war = points$war)
  }))
  result <- out_of_season_robustness(root)
  stopifnot(result$available, length(result$results) == 6L)
  for (value in result$results) stopifnot(!value$held_out_season %in% value$training_seasons, value$mse < 1e-12)
})

test("a single-salary cohort preserves raw points without inventing a reference domain", {
  window <- synthetic_analysis_window()
  window$points$salary_cap_share <- .10
  window$points$salary_reported_usd <- window$points$reference_salary_usd <- 15464700
  result <- analyze_player_window(window, pilot_only = TRUE, cache_dir = tempfile("basketball-single-salary-"))
  stopifnot(!result$available, result$reason == "constant_model_no_resolved_salary_effect",
            nrow(result$window$points) == 80L, is.null(result$models))
})

test("detected nonfinite analytic covariance withholds intervals and preserves bootstrap evidence and estimates", {
  analysis <- analyze_player_window(synthetic_analysis_window(), cache_dir = tempfile("basketball-covariance-gate-"), influence = FALSE)
  curve <- analysis$models$war
  stopifnot(any(is.finite(curve$low)), curve$bootstrap$valid_refits == 499L)
  curve$analytic_crosscheck <- list(available = FALSE, reason = "nonfinite_analytic_covariance")
  gated <- apply_curve_uncertainty_gates(curve)
  stopifnot(all(is.na(gated$low)), all(is.na(gated$high)), all(is.na(gated$contrast_low)),
            identical(gated$expected, curve$expected), identical(gated$contrast, curve$contrast),
            gated$bootstrap$valid_refits == 499L,
            gated$bootstrap$interval_status == "nonfinite_analytic_covariance_interval_withheld")
})

test("the primary player bootstrap refuses raw repeated-season transport rows", {
  points <- synthetic_relationship_points("linear", 30L)
  rows <- dplyr::bind_rows(lapply(c("2024-25", "2025-26"), function(season) {
    x <- points; x$season <- season; x
  }))
  fit <- fit_salary_relationship(rows, "war", observation_unit = "player-season-transport")
  curve <- relationship_curve(fit, 154647000)
  expect_error(run_relationship_bootstrap(curve, 2026, 1, draws = 25, pilot = TRUE,
    cache_dir = tempfile("basketball-wrong-bootstrap-unit-")), "not repeated season rows")
})

cat(sprintf("\n%d offline boundary tests passed.\n", test_count))
