#!/usr/bin/env Rscript
# Respectful sequential public-document acquisition. Re-running reuses snapshots.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
args <- commandArgs(trailingOnly = TRUE)
value <- function(prefix, default) {
  found <- args[startsWith(args, prefix)]
  if (length(found) > 1L) stop("Repeated option: ", prefix)
  if (length(found)) substring(found, nchar(prefix) + 1L) else default
}
known <- startsWith(args, "--season=") | startsWith(args, "--max-requests=") | startsWith(args, "--delay=") | args == "--offline"
if (any(!known)) stop("Unknown acquisition option")
seasons <- strsplit(value("--season=", "2025-26"), ",", fixed = TRUE)[[1]]
limit <- as.integer(value("--max-requests=", "25"))
delay <- as.numeric(value("--delay=", "2"))
if (is.na(limit) || limit < 0L || is.na(delay) || !is.finite(delay) || delay < 1) stop("Use a nonnegative request limit and at least one second delay")
metric <- acquire_lebron(seasons, offline = "--offline" %in% args)
cohort <- metric$players[order(metric$players$season, decreasing = TRUE), ]
cohort <- cohort[!duplicated(cohort$nba_id), ]
snapshots <- list(); failures <- list(); requested <- 0L
for (i in seq_len(nrow(cohort))) {
  id <- cohort$nba_id[i]; name <- cohort$player_name[i]
  cached <- tryCatch(acquire_earnings(id, name, offline = TRUE), error = function(e) e)
  if (inherits(cached, "error") && !startsWith(conditionMessage(cached), "No cached earnings document")) stop(cached)
  if (!inherits(cached, "error")) snapshots[[length(snapshots) + 1L]] <- cached else {
    if ("--offline" %in% args || requested >= limit) next
    if (requested > 0L) Sys.sleep(delay)
    requested <- requested + 1L
    result <- tryCatch(acquire_earnings(id, name), error = function(e) e)
    if (inherits(result, "error")) {
      failures[[length(failures) + 1L]] <- list(nba_id = as.character(id), error = conditionMessage(result))
      cat("Unavailable", id, conditionMessage(result), "\n")
      if (inherits(result, c("httr2_http_403", "httr2_http_429", "httr2_http_402"))) {
        cat("Stopping on access/rate response; no bypass or continued requests.\n")
        break
      }
    } else snapshots[[length(snapshots) + 1L]] <- result
  }
  if (i %% 10L == 0L) {
    cat("Reviewed", i, "of", nrow(cohort), "IDs; cached", length(snapshots), "documents; new requests", requested, "\n")
    flush.console()
  }
}
import <- earnings_salary_import(snapshots, metric$players, seasons)
salary <- reconcile_salary_imports(list(import), FANSPO_SALARY_DEFINITION)
dataset <- match_player_seasons(metric, salary)
paths <- write_private_dataset(dataset)
cat("Documents available:", length(snapshots), "new requests:", requested, "failures:", length(failures), "\n")
print(table(import$records$source_salary_status, useNA = "ifany"))
print(dataset$coverage, width = Inf)
audit <- list(source = "fanspo_earnings", requested_seasons = as.list(seasons),
              documents = length(snapshots), new_requests = requested, failures = failures,
              source_status_counts = as.list(table(import$records$source_salary_status)), dataset = paths)
audit_path <- file.path("outputs/private", paste0("earnings-audit-", sha256_bytes(json_bytes(audit)), ".json"))
if (!file.exists(audit_path)) write_new_bytes(json_bytes(audit), audit_path)
cat("Private matched dataset:", paths$dataset_path, "\nAudit:", audit_path, "\nNo model fitted or data published.\n")
