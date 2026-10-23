#!/usr/bin/env Rscript
# Offline scientific analysis. Every output remains ignored and private.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %in% c("--all-views", "--pilot-only"))) stop("Unknown analysis option")
all_views <- "--all-views" %in% args
seasons <- if (all_views) sprintf("%d-%02d", 2009:2025, (2010:2026) %% 100L) else "2025-26"
metrics <- acquire_lebron(seasons, offline = TRUE)
imported <- cached_earnings_import(metrics, require_complete_acquisition = TRUE)
salary <- reconcile_salary_imports(list(imported), FANSPO_SALARY_DEFINITION)
dataset <- match_player_seasons(metrics, salary)
private_data <- write_private_dataset(dataset)
analyses <- list()
ends <- if (all_views) 2010:2026 else 2026L
for (end in rev(ends)) for (window_length in c(1L, 3L, 5L)) {
  if (end - window_length + 1L < min(ends)) next
  season <- sprintf("%d-%02d", end - 1L, end %% 100L)
  window <- make_player_window(dataset, season, window_length, prespecified_exposure_rules()$main)
  sentinel <- (end == max(ends) && window_length %in% c(1L, 3L, 5L)) || (end == min(ends) && window_length == 1L)
  analysis <- analyze_player_window(window, sentinel = sentinel, pilot_only = "--pilot-only" %in% args)
  if (!analysis$available) { cat("Unavailable", season, window_length, analysis$reason, "\n"); next }
  analyses[[length(analyses) + 1L]] <- analysis
  if (!"--pilot-only" %in% args) {
    diagnostics <- write_private_diagnostics(analysis)
    cat("Private diagnostics:", diagnostics$directory, "\n")
  }
  cat("Completed", season, "window", window_length, ":", nrow(window$points), "matched players\n")
  flush.console()
}
bytes <- serialize(analyses, NULL, version = 3)
analysis_path <- file.path("outputs/private", paste0("analysis-", sha256_bytes(bytes), ".rds"))
if (!file.exists(analysis_path)) write_new_bytes(bytes, analysis_path)
cat("Private analysis:", analysis_path, "\n")
if (!"--pilot-only" %in% args) {
  exports <- build_browser_exports(analyses)
  paths <- write_private_browser_exports(exports)
  cat("Private browser index:", paths$index_path, "\n")
  print(paths$view_bytes)
}
cat("Source dataset:", private_data$dataset_path, "\nNo website files written or data published.\n")
