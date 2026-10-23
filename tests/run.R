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

cat(sprintf("\n%d offline boundary tests passed.\n", test_count))
