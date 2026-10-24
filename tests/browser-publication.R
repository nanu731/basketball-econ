# Explicitly synthetic structural fixtures; temporary files only, never release outputs.
source("R/core.R")
source("R/browser.R")
check_error <- function(expr) stopifnot(inherits(tryCatch({ force(expr); NULL }, error = identity), "error"))
common <- list(schemaVersion = "moneyball.v1", publication = list(status = "private-preview", note = "SYNTHETIC TEST FIXTURE"),
               sources = list(), definitions = list())
view <- list(id = "2026-1", endSeason = 2026L, window = 1L, players = list(list(name = "TEST_FIXTURE_PLAYER", war = -2)))
fixture <- browser_export_bundle_set(common, list(view), "test_fixture")
before <- fixture
stopifnot(identical(fixture, browser_export_bundle_set(common,
  unname(lapply(fixture$view_bundles, function(bundle) bundle$views[[1]])), "test_fixture")))
tmp <- tempfile("basketball-publication-test-")
dir.create(tmp)
check_error(promote_browser_publication(fixture))
check_error(promote_browser_publication(fixture, TRUE))
check_error(write_private_browser_exports(fixture, tmp))
check_error(write_cleared_browser_exports(fixture, tmp))
cleared <- fixture
cleared$view_bundles[[1]]$publication <- browser_release_publication()
check_error(write_cleared_browser_exports(cleared, tmp, TRUE))
forged <- fixture; forged$data_origin <- "observed"
check_error(promote_browser_publication(forged, TRUE))
check_error(write_private_browser_exports(forged, tmp))
forged <- cleared; forged$data_origin <- "observed"
check_error(write_cleared_browser_exports(forged, tmp, TRUE))
stopifnot(!length(list.files(tmp)), identical(fixture, before))
# Exercise the shared immutable byte writer only in this isolated fixture directory.
paths <- .write_browser_export_files(fixture, tmp)
files <- list.files(paths$directory, recursive = TRUE, full.names = TRUE)
hashes <- tools::sha256sum(files)
stopifnot(identical(paths, .write_browser_export_files(fixture, tmp)), identical(hashes, tools::sha256sum(files)))
writeBin(charToRaw("SYNTHETIC TEST FIXTURE tamper"), file.path(paths$directory, "views", "2026-1.json"))
check_error(.write_browser_export_files(fixture, tmp))
stopifnot(identical(readBin(file.path(paths$directory, "views", "2026-1.json"), "raw", n = 1000), charToRaw("SYNTHETIC TEST FIXTURE tamper")))
unlink(tmp, recursive = TRUE)
cat("PASS publication opt-in, synthetic/forged-origin guards, source preservation and immutable replay\n")
