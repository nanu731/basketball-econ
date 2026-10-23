# Offline boundary tests. All artificial observations stay under tests/.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)

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

cat(sprintf("\n%d offline boundary tests passed.\n", test_count))
