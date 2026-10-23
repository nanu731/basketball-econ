# Artificial observations for independent worked examples. Never source in production.
synthetic_window_dataset <- function() {
  rows <- tibble::tibble(
    season = c("2024-25", "2025-26", "2025-26", "2024-25", "2025-26"),
    nba_id = c(1L, 1L, 2L, 3L, 3L),
    player_name = c("TEST_FIXTURE_A", "TEST_FIXTURE_A", "TEST_FIXTURE_B", "TEST_FIXTURE_C", "TEST_FIXTURE_C"),
    played = TRUE, minutes = c(50, 150, 100, 100, 100), games = c(2L, 4L, 3L, 3L, 3L),
    provider_possessions = c(100, 300, 200, 200, 200), lebron = c(2, 6, -1, 0, 1),
    war = c(4, 8, 2, 1, 2), cap_usd = c(140588000, 154647000, 154647000, 140588000, 154647000),
    salary_reported_usd = c(14058800, 30929400, 15464700, 1405880, NA_real_),
    salary_cap_share = c(.10, .20, .10, .01, NA_real_),
    salary_status = c("single_source", "single_source", "single_source", "single_source", "missing_salary"),
    data_status = c("matched_inputs", "matched_inputs", "matched_inputs", "matched_inputs", "missing_salary"),
    team_count = c(1L, 1L, 2L, 1L, 1L),
    team_attribution_verified = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    attributed_team = c("SAS", "SAS", NA_character_, "CHI", "CHI"))
  teams <- tibble::tibble(season = c("2024-25", "2025-26", "2025-26", "2025-26", "2024-25", "2025-26"),
                          nba_id = c(1L, 1L, 2L, 2L, 3L, 3L), team = c("SAS", "SAS", "DAL", "LAL", "CHI", "CHI"))
  list(schema_version = "matched-player-seasons-v1", players = rows, teams = teams,
       coverage = tibble::tibble(season = c("2024-25", "2025-26")),
       data_origin = "test_fixture", source_provenance = list(),
       salary_label = "SYNTHETIC TEST FIXTURE", publication_status = "unreviewed_do_not_publish")
}

synthetic_relationship_points <- function(shape = c("linear", "constant", "curved"), n = 80L) {
  shape <- match.arg(shape)
  x <- seq(.01, .40, length.out = n)
  y <- switch(shape, linear = -2 + 10 * x, constant = rep(3, n), curved = 100 * (x - .20)^2)
  tibble::tibble(nba_id = as.character(seq_len(n)), salary_cap_share = x, war = y, lebron = y / 2)
}

synthetic_analysis_window <- function() {
  points <- synthetic_relationship_points("linear")
  rows <- tibble::tibble(season = "2025-26", nba_id = as.integer(points$nba_id),
    player_name = paste0("TEST_FIXTURE_", points$nba_id), played = TRUE, minutes = 1000,
    games = 40L, provider_possessions = 2000, salary_reported_usd = points$salary_cap_share * 154647000,
    salary_cap_share = points$salary_cap_share, lebron = points$lebron, war = points$war,
    team_count = 1L, salary_status = "single_source")
  dataset <- list(players = rows, teams = tibble::tibble(season = "2025-26", nba_id = rows$nba_id,
    team = rep(c("SAS", "BOS"), each = 40L)), source_provenance = list(),
    data_origin = "test_fixture", publication_status = "unreviewed_do_not_publish")
  make_player_window(dataset, "2025-26", 1L, prespecified_exposure_rules()$main)
}
