# Keep every metric player-season visible in the audit. No inner join, invented
# salary, eligibility cutoff, or allocation of annual pay across traded teams.
match_player_seasons <- function(metrics, salaries, caps = official_caps()) {
  require_columns(metrics$players, c("season", "nba_id", "played", "lebron", "war", "team_count"), "Normalized metrics")
  require_columns(salaries$players, c("season", "nba_id", "salary_reported_usd", "salary_status"), "Reconciled salaries")
  require_columns(caps, c("season", "season_end", "cap_usd", "cap_source_url"), "Salary caps")
  if (anyDuplicated(caps$season)) stop("Duplicate season in cap table", call. = FALSE)
  require_numeric(caps$cap_usd, "cap_usd", minimum = 1)
  for (data in list(metrics$players, salaries$players)) {
    if (anyDuplicated(paste(data$season, data$nba_id))) stop("Duplicate player-season at matching boundary", call. = FALSE)
  }
  rows <- dplyr::left_join(metrics$players, salaries$players, by = c("season", "nba_id"))
  rows <- dplyr::left_join(rows, caps, by = "season")
  rows$salary_status[is.na(rows$salary_status)] <- "missing_salary_record"
  rows$salary_cap_share <- rows$salary_reported_usd / rows$cap_usd
  rows$team_attribution_status <- ifelse(rows$team_count > 1L, "unallocated_multi_team", "single_team_membership_only")
  rows <- annotate_single_team_attribution(rows, salaries$source_records)
  # This is input completeness, not a statistically validated exposure threshold.
  rows$data_status <- dplyr::case_when(
    !rows$played ~ "no_playing_time",
    is.na(rows$lebron) | is.na(rows$war) ~ "missing_metric",
    rows$salary_status == "conflict" ~ "salary_conflict",
    is.na(rows$salary_reported_usd) ~ "missing_salary",
    is.na(rows$cap_usd) ~ "missing_cap",
    TRUE ~ "matched_inputs"
  )
  rows$eligibility_status <- "not_assessed"
  rows <- rows[order(rows$season, rows$nba_id), ]
  coverage <- dplyr::bind_rows(lapply(sort(unique(rows$season)), function(season) {
    r <- rows[rows$season == season, ]
    active <- r$played
    salary_known <- active & !is.na(r$salary_reported_usd)
    tibble::tibble(season = season, metric_player_seasons = nrow(r), active_player_seasons = sum(active),
                   salary_matched_active = sum(salary_known),
                   zero_salary_active = sum(salary_known & r$salary_reported_usd == 0, na.rm = TRUE),
                   missing_salary_active = sum(active & is.na(r$salary_reported_usd)),
                   conflicting_salary_active = sum(active & r$salary_status == "conflict"),
                   missing_metric_active = sum(active & (is.na(r$lebron) | is.na(r$war))),
                   complete_input_active = sum(r$data_status == "matched_inputs"),
                   multi_team_active = sum(active & r$team_count > 1L),
                   reported_salary_matched_sample_usd = if (any(salary_known)) sum(r$salary_reported_usd[salary_known]) else NA_real_)
  }))
  salary_only <- dplyr::anti_join(salaries$players, metrics$players[, c("season", "nba_id")], by = c("season", "nba_id"))
  list(schema_version = "matched-player-seasons-v1", players = rows, teams = metrics$teams,
       data_origin = if (identical(metrics$data_origin, "test_fixture") || identical(salaries$data_origin, "test_fixture") ||
         any(vapply(salaries$provenance, function(x) identical(x$publication_status, "test_only"), logical(1)))) "test_fixture" else "observed",
       coverage = coverage, salary_only = salary_only, unmapped_salaries = salaries$unmapped,
       salary_candidates = salaries$records, salary_source_records = salaries$source_records,
       source_provenance = list(metrics = metrics$provenance, salaries = salaries$provenance,
                                salary_definition_id = salaries$definition_id),
       salary_label = if (length(salaries$provenance) == 1L && !is.null(salaries$provenance[[1]]$display_label)) salaries$provenance[[1]]$display_label else "Reported season salary",
       model_status = "not_fitted", eligibility_status = "not_assessed",
       publication_status = "unreviewed_do_not_publish")
}

write_private_dataset <- function(dataset, output_dir = "outputs/private") {
  if (!identical(dataset$schema_version, "matched-player-seasons-v1") ||
      !identical(dataset$publication_status, "unreviewed_do_not_publish")) stop("Unexpected private dataset contract", call. = FALSE)
  bytes <- serialize(dataset, NULL, version = 3)
  hash <- sha256_bytes(bytes)
  path <- file.path(output_dir, paste0("player-seasons-", hash, ".rds"))
  if (file.exists(path)) {
    if (!identical(sha256_bytes(readBin(path, "raw", n = file.info(path)$size)), hash)) stop("Private artifact checksum mismatch", call. = FALSE)
  } else write_new_bytes(bytes, path)
  summary <- list(schema_version = dataset$schema_version, dataset_sha256 = hash,
                  dataset_file = basename(path), coverage = dataset$coverage,
                  salary_label = dataset$salary_label, model_status = dataset$model_status,
                  eligibility_status = dataset$eligibility_status, publication_status = dataset$publication_status)
  summary_path <- sub("\\.rds$", ".summary.json", path)
  summary_bytes <- json_bytes(summary)
  if (file.exists(summary_path)) {
    existing <- readBin(summary_path, "raw", n = file.info(summary_path)$size)
    if (!identical(existing, summary_bytes)) stop("Private summary checksum mismatch", call. = FALSE)
  } else write_new_bytes(summary_bytes, summary_path)
  list(dataset_path = path, summary_path = summary_path, dataset_sha256 = hash)
}
