#!/usr/bin/env Rscript
# Private matched-data audit. Optional arguments: season salary.csv metadata.json.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
args <- commandArgs(trailingOnly = TRUE)
offline <- "--offline" %in% args
args <- args[args != "--offline"]
if (!length(args)) args <- "2025-26"
if (!length(args) %in% c(1L, 3L)) stop("Expected a season, optionally followed by salary CSV and metadata paths")
metrics <- acquire_lebron(args[1], offline = offline)
if (length(args) == 3L) {
  imported <- read_salary_import(args[2], args[3])
  salaries <- reconcile_salary_imports(list(imported), imported$provenance$salary_definition_id)
} else salaries <- reconcile_salary_imports()
dataset <- match_player_seasons(metrics, salaries)
paths <- write_private_dataset(dataset)
print(dataset$coverage, width = Inf)
cat("Private dataset:", paths$dataset_path, "\n")
cat("Unmapped salary records:", nrow(dataset$unmapped_salaries), "\n")
cat("Salary-only player-seasons:", nrow(dataset$salary_only), "\n")
cat("No salary model fitted. Eligibility and publication remain unreviewed.\n")
