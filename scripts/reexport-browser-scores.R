#!/usr/bin/env Rscript
# Offline metadata-only replay of the frozen reviewed JSON; no models or source acquisition.
source("R/core.R")
source("R/browser.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply the frozen browser export directory", call. = FALSE)
base <- normalizePath(args[[1]], mustWork = TRUE)
frozen_version <- "moneyball-v1-31960f8380b3ee1b1d5c"
if (basename(base) != frozen_version) stop("Expected the pinned frozen browser version", call. = FALSE)
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
index <- read_json(file.path(base, "index.json"))
if (!identical(index$schemaVersion, "moneyball.index.v1") || !identical(index$dataVersion, frozen_version) || length(index$views) != 45L) stop("Invalid frozen index")
bundles <- lapply(index$views, function(entry) {
  if (!grepl("^20[0-9]{2}-(1|3|5)$", entry$id) || !identical(entry$url, paste0("/data/moneyball/v1/views/", entry$id, ".json"))) stop("Invalid frozen view path")
  bundle <- read_json(file.path(base, "views", paste0(entry$id, ".json")))
  if (!identical(bundle$dataVersion, frozen_version) || !identical(bundle$defaultView, entry$id) || length(bundle$views) != 1L || !identical(bundle$views[[1]]$id, entry$id)) stop("Frozen bundle identity mismatch")
  bundle
})
common_fields <- c("schemaVersion", "publication", "sources", "definitions")
common <- bundles[[1]][common_fields]
if (any(!vapply(bundles, function(b) identical(b[common_fields], common), logical(1)))) stop("Frozen common metadata differs")
views <- lapply(bundles, function(b) b$views[[1]])
frozen <- browser_export_bundle_set(common, views, "observed")
if (!identical(frozen$index, index)) stop("Frozen fingerprint or manifest differs")
updated <- lapply(views, add_browser_gap_scores)
# Removing additions must restore every old field, number and ordering exactly.
stripped <- lapply(updated, function(view) {
  view$gapSize <- NULL
  view$players <- lapply(view$players, function(player) { player$gapMultiple <- NULL; player })
  view
})
if (!identical(stripped, views)) stop("An existing frozen value changed")
exports <- browser_export_bundle_set(common, updated, "observed")
paths <- write_private_browser_exports(exports)
cat("Export:", paths$directory, "\nVersion:", exports$index$dataVersion,
    "\nPreserved:", length(views), "views;", sum(vapply(views, function(v) length(v$players), integer(1))), "player-window records; every prior value identical\n")
print(c(index_bytes = paths$index_bytes, max_view_bytes = max(paths$view_bytes)))
