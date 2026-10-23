#!/usr/bin/env Rscript
# Prespecified sensitivity diagnostics; no model/rule selection from these results.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %in% "--all-views")) stop("Unknown sensitivity option")
all_views <- "--all-views" %in% args
seasons <- if (all_views) sprintf("%d-%02d", 2009:2025, (2010:2026) %% 100L) else "2025-26"
metric <- acquire_lebron(seasons, offline = TRUE)
imported <- cached_earnings_import(metric)
dataset <- match_player_seasons(metric, reconcile_salary_imports(list(imported), FANSPO_SALARY_DEFINITION))
results <- run_prespecified_sensitivity(dataset)
bytes <- serialize(results, NULL, version = 3)
path <- file.path("outputs/private", paste0("sensitivity-", sha256_bytes(bytes), ".rds"))
if (!file.exists(path)) write_new_bytes(bytes, path)
cat("Private sensitivity diagnostics:", path, "\nNo thresholds or main curves were changed.\n")
