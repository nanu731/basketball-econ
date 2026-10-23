# Synthetic arithmetic and metadata-only export tests; no refits or acquisition.
source("R/core.R")
source("R/browser.R")
check_error <- function(expr) stopifnot(inherits(tryCatch({ force(expr); NULL }, error = identity), "error"))
for (bad in list(.Machine$double.xmax, numeric(), c(1, NA_real_), c(1, Inf), "1", matrix(1))) check_error(gap_size(bad))
x <- c(-8, -2, 0, 2, 3)
a <- gap_size(x)
stopifnot(identical(a$metadata$method, "median-absolute-visible-gap-v1"), a$metadata$cohortN == 5,
          a$metadata$scale == 2, a$metadata$status == "available", is.null(a$metadata$reason),
          identical(unlist(a$multiples), x / 2))
stopifnot(identical(gap_size(x * 100)$multiples, a$multiples),
          identical(gap_size(rev(x))$multiples, rev(a$multiples)),
          identical(unlist(gap_size(-x)$multiples), -unlist(a$multiples)),
          identical(gap_size(c(2, 2))$multiples, list(1, 1)))
for (x in list(c(0, 0), c(0, 0, 1))) {
  z <- gap_size(x); stopifnot(z$metadata$reason == "zero-scale", is.null(z$metadata$scale), all(vapply(z$multiples, is.null, logical(1))))
}
for (x in list(c(1e-308, 1e-308, 1e308), c(1e-308, 1e308, 1e308))) {
  z <- gap_size(x); stopifnot(z$metadata$reason == "unrepresentable-ratio", all(vapply(z$multiples, is.null, logical(1))))
}
small <- gap_size(c(-1e-300, 1e-300, 2e-300)); stopifnot(small$metadata$scale > 0, max(abs(unlist(small$multiples) - c(-1, 1, 2))) < 1e-12)
subnormal <- gap_size(c(5e-324, 1e-323)); stopifnot(subnormal$metadata$scale == 1e-323, identical(unlist(subnormal$multiples), c(.5, 1)))
large <- gap_size(c(-1e308, 1e308)); stopifnot(is.finite(large$metadata$scale), identical(unlist(large$multiples), c(-1, 1)))
view <- list(id = "2026-1", players = lapply(c(-3, 0, 3), function(r) list(residual = list(lebron = r, war = 2 * r))))
updated <- add_browser_gap_scores(view)
stopifnot(updated$gapSize$war$scale == 6, updated$players[[1]]$gapMultiple$war == -1)
restored <- updated; restored$gapSize <- NULL
restored$players <- lapply(restored$players, function(p) { p$gapMultiple <- NULL; p })
stopifnot(identical(restored, view), identical(json_bytes(add_browser_gap_scores(jsonlite::fromJSON(rawToChar(json_bytes(view)), simplifyVector = FALSE))), json_bytes(updated)))
common <- list(schemaVersion = "moneyball.v1", publication = list(status = "private-preview", note = "SYNTHETIC TEST FIXTURE"))
old <- browser_export_bundle_set(common, list(view), "test_fixture")
new <- browser_export_bundle_set(common, list(updated), "test_fixture")
stopifnot(old$index$dataVersion != new$index$dataVersion, identical(new, browser_export_bundle_set(common, list(updated), "test_fixture")))
check_error(write_private_browser_exports(new, tempfile("denied-gap-export-")))
cat("PASS gap-size arithmetic, edge cases, canonical replay, preservation, version and fixture-write guards\n")
# Exercise the normal export with an explicitly synthetic saved curve, without fitting anything.
for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
source("tests/fixtures/analysis-fixtures.R")
window <- synthetic_analysis_window()
curve <- list(knots = c(0, .5), expected = c(0, 1), low = c(NA_real_, NA_real_), high = c(NA_real_, NA_real_),
  contrast = c(NA_real_, NA_real_), contrast_low = c(NA_real_, NA_real_), contrast_high = c(NA_real_, NA_real_),
  support = data.frame(supported = c(FALSE, FALSE), n_start = c(0L, 0L), n_end = c(0L, 0L), reason = c("Synthetic unsupported", "Synthetic unsupported")),
  fit = list(nba_ids = window$points$nba_id, observation_unit = "player-window", partitions = list(), selected_family = "linear"),
  bootstrap = list(valid_refits = 0L, planned_draws = 0L, seed = 1L, interval_status = "synthetic_saved_curve"), interval_method = "SYNTHETIC TEST FIXTURE")
analysis <- list(available = TRUE, pilot_only = FALSE, window = window, models = list(lebron = curve, war = curve), team_summaries = data.frame())
normal <- build_browser_exports(list(analysis), allow_test_fixture = TRUE)
encoded <- jsonlite::fromJSON(rawToChar(json_bytes(normal$view_bundles[[1]])), simplifyVector = FALSE)$views[[1]]
replayed <- add_browser_gap_scores(encoded)
stopifnot(identical(json_bytes(encoded), json_bytes(replayed)))
cat("PASS normal saved-curve pipeline and canonical JSON replay produce identical gap metadata\n")
