#!/usr/bin/env Rscript
# Promote the pinned reviewed JSON offline; no acquisition, score calculation or refit.
source("R/core.R")
source("R/browser.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !identical(args[[2]], "--confirm-clearance")) {
  stop("Supply the frozen export directory followed by --confirm-clearance", call. = FALSE)
}
base <- normalizePath(args[[1]], mustWork = TRUE)
frozen_version <- "moneyball-v1-4592fef1aab01a76fd87"
if (basename(base) != frozen_version) stop("Expected the pinned reviewed private browser version", call. = FALSE)
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
index <- read_json(file.path(base, "index.json"))
if (!identical(index$schemaVersion, "moneyball.index.v1") || !identical(index$dataVersion, frozen_version) || length(index$views) != 45L) {
  stop("Invalid frozen index", call. = FALSE)
}
bundles <- lapply(index$views, function(entry) {
  if (!grepl("^20[0-9]{2}-(1|3|5)$", entry$id) || !identical(entry$url, paste0("/data/moneyball/v1/views/", entry$id, ".json"))) {
    stop("Invalid frozen view path", call. = FALSE)
  }
  read_json(file.path(base, "views", paste0(entry$id, ".json")))
})
common <- bundles[[1]][c("schemaVersion", "publication", "sources", "definitions")]
views <- lapply(bundles, function(bundle) bundle$views[[1]])
frozen <- browser_export_bundle_set(common, views, "observed")
if (!identical(frozen$index, index) || !identical(unname(frozen$view_bundles), bundles)) {
  stop("Frozen common metadata, manifest, fingerprint or view identity differs", call. = FALSE)
}
released <- promote_browser_publication(frozen, clearance_confirmed = TRUE)
stopifnot(identical(lapply(released$view_bundles, `[[`, "views"), lapply(frozen$view_bundles, `[[`, "views")))
paths <- write_cleared_browser_exports(released, clearance_confirmed = TRUE)
cat("Source:", base, "\nRelease:", paths$directory, "\nVersion:", released$index$dataVersion,
    "\nPreserved:", length(views), "views;", sum(vapply(views, function(view) length(view$players), integer(1))),
    "player-window records; every view value identical\n")
print(c(index_bytes = paths$index_bytes, max_view_bytes = max(paths$view_bytes)))
