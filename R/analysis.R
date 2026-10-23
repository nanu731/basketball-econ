cached_earnings_import <- function(metrics, require_complete_acquisition = TRUE) {
  cohort <- metrics$players[order(metrics$players$season, decreasing = TRUE), ]
  cohort <- cohort[!duplicated(cohort$nba_id), ]
  snapshots <- list(); unavailable <- character()
  for (i in seq_len(nrow(cohort))) {
    item <- tryCatch(acquire_earnings(cohort$nba_id[i], cohort$player_name[i], offline = TRUE), error = identity)
    if (inherits(item, "error")) {
      if (!startsWith(conditionMessage(item), "No cached earnings document")) stop(item)
      unavailable <- c(unavailable, as.character(cohort$nba_id[i]))
    } else snapshots[[length(snapshots) + 1L]] <- item
  }
  if (require_complete_acquisition && length(unavailable)) stop("Earnings acquisition remains incomplete for ", length(unavailable), " cohort players", call. = FALSE)
  imported <- earnings_salary_import(snapshots, metrics$players, sort(unique(metrics$players$season)))
  imported$unavailable_document_ids <- unavailable
  imported
}

analytic_curve_crosscheck <- function(curve, bootstrap) {
  model <- curve$fit$model
  if (inherits(model, "gam")) {
    design <- stats::predict(model, newdata = data.frame(x = curve$knots), type = "lpmatrix")
    covariance <- stats::vcov(model, unconditional = TRUE)
    correction <- !is.null(model$Vc)
    method <- "mgcv joint coefficient covariance; analytic sensitivity only"
  } else {
    training <- stats::model.matrix(model)
    design <- stats::model.matrix(stats::delete.response(stats::terms(model)), data.frame(x = curve$knots))
    bread <- solve(crossprod(training))
    h <- stats::hatvalues(model)
    adjusted <- stats::residuals(model) / (1 - h)
    covariance <- bread %*% crossprod(training * adjusted) %*% bread
    correction <- FALSE
    method <- "HC3 coefficient covariance; analytic sensitivity only"
  }
  if (any(!is.finite(covariance))) return(list(available = FALSE, reason = "nonfinite_analytic_covariance"))
  endpoint <- pmin(curve$support$endpoint, max(curve$knots))
  end_design <- t(visible_interpolation(curve$knots, t(design), endpoint))
  difference <- end_design - design
  curve_se <- sqrt(pmax(0, rowSums((design %*% covariance) * design)))
  contrast_se <- sqrt(pmax(0, rowSums((difference %*% covariance) * difference)))
  primary <- bootstrap_visible_intervals(curve$knots, curve$expected, bootstrap$predictions, curve$knots, endpoint)
  floor <- .01 * stats::sd(curve$fit$data$y)
  compare <- function(a, b) {
    ok <- curve$support$supported & is.finite(a) & is.finite(b)
    ratio <- pmax(1.96 * a, floor) / pmax(1.96 * b, floor)
    fraction <- if (any(ok)) mean(ratio[ok] < 2/3 | ratio[ok] > 1.5) else NA_real_
    list(material_fraction = fraction, flag = !is.na(fraction) && fraction >= .20)
  }
  list(available = TRUE, method = method, smoothing_correction_available = correction,
       curve = compare(curve_se, primary$curve_se), contrast = compare(contrast_se, primary$contrast_se))
}

apply_curve_uncertainty_gates <- function(curve) {
  check <- curve$analytic_crosscheck
  if (!isTRUE(check$available) && identical(check$reason, "nonfinite_analytic_covariance")) {
    curve$low[] <- curve$high[] <- NA_real_
    curve$contrast_low[] <- curve$contrast_high[] <- NA_real_
    curve$bootstrap$interval_status <- "nonfinite_analytic_covariance_interval_withheld"
    curve$bootstrap$withholding_reason <- paste(
      "The frozen protocol withholds intervals after a detected nonfinite covariance.",
      "This gate does not establish that empirical bootstrap draws are mathematically invalid.")
    curve$interval_method <- paste("Intervals withheld by the declared nonfinite-covariance gate.", curve$interval_method)
  }
  curve
}

analyze_player_window <- function(window, sentinel = FALSE, pilot_only = FALSE,
                                  cache_dir = "cache/bootstrap", influence = TRUE) {
  models <- list(); timing <- list()
  for (outcome in c("lebron", "war")) {
    fit <- fit_salary_relationship(window$points, outcome)
    if (!isTRUE(fit$available)) return(list(available = FALSE, window = window, reason = fit$reason))
    curve <- relationship_curve(fit, window$reference_cap_usd)
    pilot <- run_relationship_bootstrap(curve, window$end_season_year, window$window_length,
                                        draws = 25L, cache_dir = cache_dir, pilot = TRUE)
    timing[[outcome]] <- list(pilot_refits = 25L, elapsed_seconds = pilot$elapsed_seconds,
                             estimated_499_seconds = pilot$elapsed_seconds / 25 * 499)
    cat(window$end_season, "window", window$window_length, outcome, fit$selected_family,
        ": 25-refit pilot", pilot$elapsed_seconds, "seconds; estimated 499", round(timing[[outcome]]$estimated_499_seconds, 2), "seconds\n")
    flush.console()
    if (pilot_only) { models[[outcome]] <- curve; next }
    boot <- run_relationship_bootstrap(curve, window$end_season_year, window$window_length, cache_dir = cache_dir)
    curve <- attach_bootstrap_intervals(curve, boot)
    if (sentinel) {
      longer <- run_relationship_bootstrap(curve, window$end_season_year, window$window_length, draws = 999L, cache_dir = cache_dir)
      mc <- bootstrap_monte_carlo_check(curve, boot, longer)
      if (mc$curve$unstable || mc$contrast$unstable) {
        longest <- run_relationship_bootstrap(curve, window$end_season_year, window$window_length, draws = 1999L, cache_dir = cache_dir)
        again <- bootstrap_monte_carlo_check(curve, longer, longest)
        curve <- attach_bootstrap_intervals(curve, longest)
        if (again$curve$unstable) curve$low[] <- curve$high[] <- NA_real_
        if (again$contrast$unstable) curve$contrast_low[] <- curve$contrast_high[] <- NA_real_
        mc$extension <- again
        if (again$curve$unstable || again$contrast$unstable) curve$bootstrap$interval_status <- "monte_carlo_instability_unresolved"
        boot <- longest
      }
      curve$monte_carlo <- mc
    }
    curve$analytic_crosscheck <- tryCatch(analytic_curve_crosscheck(curve, boot), error = function(e) list(available = FALSE, reason = conditionMessage(e)))
    curve <- apply_curve_uncertainty_gates(curve)
    if (influence) curve$influence <- leave_player_out_influence(fit)
    if (fit$selected_family == "smooth") {
      curve$basis_diagnostic <- with_reproducible_seed(20261026L + window$end_season_year + window$window_length,
        mgcv::k.check(fit$model))
    }
    models[[outcome]] <- curve
  }
  list(available = TRUE, window = window, models = models, timing = timing, pilot_only = pilot_only,
       team_summaries = summarize_attributable_teams(window))
}

select_analysis_players <- function(analysis, team = NULL, player_ids = NULL) {
  points <- analysis$window$points
  keep <- rep(TRUE, nrow(points))
  if (!is.null(team)) keep <- keep & vapply(points$teams, function(x) team %in% x, logical(1))
  if (!is.null(player_ids)) keep <- keep & points$nba_id %in% as.character(player_ids)
  result <- analysis
  result$window$points <- points[keep, ]
  # Models, support counts and coverage describe the original eligible league.
  result
}
