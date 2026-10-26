#!/usr/bin/env Rscript
# Read-only verification of in-sample R² for the frozen browser release.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, dir.exists(args[[1]]))
files <- sort(list.files(file.path(args[[1]], "views"), pattern = "\\.json$", full.names = TRUE))
stopifnot(length(files) > 0L)
results <- lapply(files, function(file) {
  bundle <- jsonlite::fromJSON(file)
  view <- bundle$views
  players <- view$players[[1]]
  do.call(rbind, lapply(c("lebron", "war"), function(metric) {
    y <- players[[metric]]
    total <- sum((y - mean(y))^2)
    error <- sum(players$residual[[metric]]^2)
    score <- 1 - error / total
    stopifnot(length(y) == view$coverage$matched, all(is.finite(y)),
              all(is.finite(players$residual[[metric]])), total > 0,
              is.finite(total), is.finite(error), is.finite(score))
    # This independent correlation identity applies only to a straight line with intercept.
    if (view$models[[metric]]$name == "Straight line") {
      stopifnot(abs(score - cor(players$salaryCapShare, y)^2) < 1e-10)
    }
    data.frame(view = view$id, metric = metric, r_squared = score)
  }))
})
write.csv(do.call(rbind, results), stdout(), row.names = FALSE)
