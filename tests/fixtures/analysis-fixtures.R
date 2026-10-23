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
