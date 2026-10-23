#!/usr/bin/env Rscript
# Run from the repository root. Raw provider data remain in ignored cache/.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
args <- commandArgs(trailingOnly = TRUE)
flags <- args[startsWith(args, "--")]
if (any(!flags %in% c("--refresh", "--offline"))) stop("Unknown option")
seasons <- args[!startsWith(args, "--")]
if (!length(seasons)) seasons <- "2025-26"
result <- acquire_lebron(seasons, refresh = "--refresh" %in% flags, offline = "--offline" %in% flags)
cat("Snapshot:", result$manifest_path, "\n")
cat("Normalization:", result$schema_version, "\n")
for (season in unique(result$players$season)) {
  rows <- result$players[result$players$season == season, ]
  cat(season, ":", nrow(rows), "player-seasons;", sum(rows$team_count > 1),
      "multi-team players;", sum(is.na(rows$lebron)), "missing LEBRON;",
      sum(is.na(rows$war)), "missing WAR\n")
}
cat("Private acquisition only. Salary matching, metric semantics, completeness and public reuse require review.\n")
