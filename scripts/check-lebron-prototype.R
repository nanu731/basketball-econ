#!/usr/bin/env Rscript
# Offline geometry audit of released JSON; no fits, bootstrap, or output data files.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !dir.exists(args[1])) {
  stop("Usage: Rscript scripts/check-lebron-prototype.R <released-views-directory>", call. = FALSE)
}
files <- sort(list.files(args[1], pattern = "\\.json$", full.names = TRUE))
if (!length(files)) stop("No released view JSON files found", call. = FALSE)
check <- function(ok, message) {
  if (!isTRUE(all(ok))) stop(message, call. = FALSE)
}
close <- function(a, b) abs(a - b) <= 1e-10 * pmax(1, abs(a), abs(b))
results <- lapply(files, function(file) {
  bundle <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  check(length(bundle$views) == 1L, paste(file, "must contain exactly one view"))
  view <- bundle$views[[1]]
  label <- paste(file, view$id)
  cap <- view$referenceCapUsd
  check(length(cap) == 1L && is.finite(cap) && cap > 0, paste(label, "invalid reference cap"))
  curve <- view$models$lebron$curve
  check(length(curve) >= 2L && length(view$players) > 0L, paste(label, "missing curve or players"))
  x <- vapply(curve, `[[`, numeric(1), "x")
  y <- vapply(curve, `[[`, numeric(1), "y")
  slope <- diff(y) / diff(x)
  check(all(is.finite(c(x, y, slope))) && all(x >= 0) && all(diff(x) > 0) &&
    all(diff(y) > 0) && all(slope > 0), paste(label, "curve is not finitely, strictly increasing"))
  ids <- vapply(view$players, `[[`, character(1), "id")
  salary <- vapply(view$players, `[[`, numeric(1), "salaryCapShare")
  pay <- vapply(view$players, `[[`, numeric(1), "salaryUsd")
  impact <- vapply(view$players, `[[`, numeric(1), "lebron")
  check(!anyDuplicated(ids) && all(nzchar(ids)), paste(label, "invalid player identities"))
  check(all(is.finite(c(salary, pay, impact))) && all(salary >= 0) && all(pay >= 0),
    paste(label, "invalid player values"))
  check(close(pay, salary * cap), paste(label, "salary/cap units disagree"))
  check(close(range(salary), range(x)), paste(label, "curve/observed salary domains disagree"))

  # Reversing a strictly increasing polyline gives its unique piecewise-linear inverse.
  root <- stats::approx(y, x, xout = impact, rule = 1, ties = "ordered")$y
  inside <- impact >= min(y) & impact <= max(y)
  check(identical(is.finite(root), inside), paste(label, "inverse extrapolated or missed a crossing"))
  check(all(root[inside] >= min(x) & root[inside] <= max(x)), paste(label, "inverse escaped its domain"))
  round_trip <- stats::approx(x, y, xout = root[inside], rule = 1, ties = "ordered")$y
  check(all(is.finite(round_trip)) && all(close(round_trip, impact[inside])),
    paste(label, "inverse failed the contribution round trip"))

  # Count original full-cohort salaries, never display-filtered players or curve knots.
  supported <- vapply(root, function(at) {
    if (!is.finite(at)) return(FALSE)
    nearby <- salary[salary >= at - .02 & salary <= at + .02]
    length(nearby) >= 20L && sum(nearby < at) >= 5L && sum(nearby > at) >= 5L &&
      length(unique(nearby)) >= 5L
  }, logical(1))
  check(all(!supported | inside), paste(label, "support admitted an unavailable root"))
  reference_usd <- root[inside] * cap
  difference_usd <- reference_usd - pay[inside]
  check(all(is.finite(c(reference_usd, difference_usd))) && all(reference_usd >= 0),
    paste(label, "nonfinite dollar conversion"))
  check(close(reference_usd / cap, root[inside]) &
    close(difference_usd / cap, root[inside] - salary[inside]),
    paste(label, "reference-minus-pay dollar difference has incorrect units or sign"))
  positive_pay <- pay[inside] > 0
  percent_difference <- difference_usd[positive_pay] / pay[inside][positive_pay] * 100
  check(all(is.finite(percent_difference)) &&
    all(close(percent_difference / 100 * pay[inside][positive_pay], difference_usd[positive_pay])),
    paste(label, "invalid percentage difference"))
  # At an internal knot use the left segment; this is local slope sensitivity,
  # not a second supported endpoint or an uncertainty interval for provider LEBRON.
  segment <- findInterval(root[inside], x, all.inside = TRUE, left.open = TRUE)
  sensitivity_usd <- .1 / slope[segment] * cap
  check(all(is.finite(sensitivity_usd)) && all(sensitivity_usd > 0) &&
    all(close(sensitivity_usd / cap * slope[segment], .1)),
    paste(label, "invalid 0.1-LEBRON local salary sensitivity"))
  data.frame(id = view$id, end = view$endSeason, window = view$window,
    players = length(ids), in_domain = sum(inside), supported = sum(supported),
    sensitivity_min = min(sensitivity_usd), sensitivity_max = max(sensitivity_usd))
})
results <- do.call(rbind, results)
check(!anyDuplicated(results$id), "Duplicate released view IDs")
latest <- results[results$window == 1L & results$end == max(results$end), ]
check(nrow(latest) == 1L, "Cannot identify the default latest single-season view")
cat(sprintf("PASS: %d increasing views; %d player-window records; %d in-domain crossings; %d supported (%.2f%%).\n",
  nrow(results), sum(results$players), sum(results$in_domain), sum(results$supported),
  100 * sum(results$supported) / sum(results$players)))
cat(sprintf("Default latest single-season %s: %d/%d in-domain; %d/%d supported.\n",
  latest$id, latest$in_domain, latest$players, latest$supported, latest$players))
cat(sprintf("Hypothetical local 0.1-LEBRON sensitivity: $%.3fm–$%.3fm in view reference-cap dollars; no uncertainty interval.\n",
  min(results$sensitivity_min) / 1e6, max(results$sensitivity_max) / 1e6))
