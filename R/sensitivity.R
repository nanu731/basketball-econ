# Private prespecified diagnostics. These functions never choose an exposure rule,
# alter the main curve, publish a result, or acquire new data.
sensitivity_difference_summary <- function(x) {
  x <- x[is.finite(x)]
  list(n = length(x), rms = if (length(x)) sqrt(mean(x^2)) else NA_real_,
    max_absolute = if (length(x)) max(abs(x)) else NA_real_,
    range = if (length(x)) range(x) else c(NA_real_, NA_real_))
}

support_sensitivity <- function(curve) {
  if (anyDuplicated(curve$fit$nba_ids)) stop("Support requires one original observation per player", call. = FALSE)
  settings <- list(default = list(radius = .02, count = 20L),
    radius_01 = list(radius = .01, count = 20L), radius_03 = list(radius = .03, count = 20L),
    count_10 = list(radius = .02, count = 10L), count_30 = list(radius = .02, count = 30L))
  dplyr::bind_rows(lapply(names(settings), function(name) {
    config <- curve$fit$config
    config$support_radius <- settings[[name]]$radius
    config$support_count <- settings[[name]]$count
    support <- local_contrast_support(curve$fit$data$x, curve$knots, curve$reference_cap_usd, config)
    if (curve$fit$selected_family == "intercept") {
      support$supported[] <- FALSE
      support$reason[] <- "constant_model_no_resolved_salary_effect"
    }
    estimate <- rep(NA_real_, nrow(support))
    ok <- support$supported
    if (any(ok)) estimate[ok] <- visible_interpolation(curve$knots, curve$expected, support$endpoint[ok]) - curve$expected[ok]
    common <- ok & curve$support$supported
    difference <- estimate[common] - curve$contrast[common]
    details <- support
    details$estimate <- estimate
    tibble::tibble(setting = name, radius = config$support_radius, minimum_players = config$support_count,
      supported_points = sum(ok), original_grid_points = nrow(support),
      common_with_default = sum(common),
      common_contrast_max_abs_difference = if (length(difference)) max(abs(difference)) else NA_real_,
      selected_family = curve$fit$selected_family,
      note = "Support-only sensitivity; same fitted curve, no refit or bootstrap; no new interval claim",
      details = list(details))
  }))
}

compare_relationship_sensitivity <- function(main, alternative, outcomes = c("lebron", "war")) {
  if (!isTRUE(main$available) || !isTRUE(alternative$available)) {
    return(list(available = FALSE, reason = if (!isTRUE(main$available)) main$reason else alternative$reason))
  }
  comparisons <- lapply(outcomes, function(outcome) {
    a <- main$models[[outcome]]; b <- alternative$models[[outcome]]
    if (is.null(a) || is.null(b)) return(list(available = FALSE, reason = "missing_relationship_curve"))
    if (!isTRUE(all.equal(a$reference_cap_usd, b$reference_cap_usd, check.attributes = FALSE))) {
      stop("Sensitivity comparisons require the same reference cap", call. = FALSE)
    }
    if (length(a$knots) < 2L || length(b$knots) < 2L) {
      return(list(available = FALSE, reason = "fewer_than_two_visible_knots"))
    }
    x <- sort(unique(c(a$knots, b$knots)))
    x <- x[x >= max(min(a$knots), min(b$knots)) & x <= min(max(a$knots), max(b$knots))]
    ids <- intersect(a$fit$nba_ids, b$fit$nba_ids)
    residual_a <- a$fit$residual[match(ids, a$fit$nba_ids)]
    residual_b <- b$fit$residual[match(ids, b$fit$nba_ids)]
    base <- list(available = TRUE, outcome = outcome, primary_players = length(a$fit$nba_ids),
      alternative_players = length(b$fit$nba_ids), common_players = length(ids),
      primary_family = a$fit$selected_family, alternative_family = b$fit$selected_family,
      primary_cv = lapply(a$fit$partitions, `[[`, "scores"), alternative_cv = lapply(b$fit$partitions, `[[`, "scores"),
      primary_bootstrap = a$bootstrap, alternative_bootstrap = b$bootstrap,
      residual_rank_correlation_descriptive = if (length(ids) > 2L && stats::sd(residual_a) > 1e-12 && stats::sd(residual_b) > 1e-12)
        stats::cor(residual_a, residual_b, method = "spearman") else NA_real_)
    if (!length(x)) return(c(base, list(common_supported_grid_points = 0L,
      reason = "no_common_salary_domain", grid = tibble::tibble(x = double()))))
    sa <- local_contrast_support(a$fit$data$x, x, a$reference_cap_usd, a$fit$config)
    sb <- local_contrast_support(b$fit$data$x, x, b$reference_cap_usd, b$fit$config)
    if (a$fit$selected_family == "intercept") {
      sa$supported[] <- FALSE; sa$reason[] <- "constant_model_no_resolved_salary_effect"
    }
    if (b$fit$selected_family == "intercept") {
      sb$supported[] <- FALSE; sb$reason[] <- "constant_model_no_resolved_salary_effect"
    }
    common <- sa$supported & sb$supported
    ya <- visible_interpolation(a$knots, a$expected, x)
    yb <- visible_interpolation(b$knots, b$expected, x)
    da <- db <- rep(NA_real_, length(x))
    if (any(common)) {
      da[common] <- visible_interpolation(a$knots, a$expected, sa$endpoint[common]) - ya[common]
      db[common] <- visible_interpolation(b$knots, b$expected, sb$endpoint[common]) - yb[common]
    }
    band <- function(curve, field) {
      values <- curve[[field]]
      if (is.null(values) || length(values) != length(curve$knots) || any(!is.finite(values))) return(rep(NA_real_, length(x)))
      visible_interpolation(curve$knots, values, x)
    }
    grid <- tibble::tibble(x = x, primary_expected = ya, alternative_expected = yb,
      curve_difference = yb - ya, common_supported = common,
      primary_n_start = sa$n_start, primary_n_end = sa$n_end,
      alternative_n_start = sb$n_start, alternative_n_end = sb$n_end,
      primary_support_reason = sa$reason, alternative_support_reason = sb$reason,
      primary_contrast = da, alternative_contrast = db, contrast_difference = db - da,
      primary_curve_low = band(a, "low"), primary_curve_high = band(a, "high"),
      alternative_curve_low = band(b, "low"), alternative_curve_high = band(b, "high"))
    c(base, list(common_salary_grid_points = length(x), common_supported_grid_points = sum(common),
      curve_difference = sensitivity_difference_summary((yb - ya)[common]),
      curve_difference_on_common_domain = sensitivity_difference_summary(yb - ya),
      contrast_difference = sensitivity_difference_summary(db - da),
      curve_difference_range = sensitivity_difference_summary((yb - ya)[common])$range,
      contrast_difference_range = sensitivity_difference_summary(db - da)$range,
      interval_note = "Separate conditional bands retained; differences are descriptive, not a paired interval for model differences",
      grid = grid))
  })
  names(comparisons) <- outcomes
  list(available = TRUE, comparisons = comparisons, coverage = alternative$window$coverage)
}

sensitivity_window_evidence <- function(window) {
  rows <- window$active_player_seasons
  included <- as.character(rows$nba_id) %in% window$points$nba_id
  list(coverage = window$coverage, exposure = list(
    included_minutes = sum(rows$minutes[included], na.rm = TRUE),
    omitted_known_minutes = sum(rows$minutes[!included], na.rm = TRUE),
    included_possessions = sum(rows$provider_possessions[included], na.rm = TRUE),
    omitted_known_possessions = sum(rows$provider_possessions[!included], na.rm = TRUE),
    included_games = sum(rows$games[included], na.rm = TRUE),
    omitted_known_games = sum(rows$games[!included], na.rm = TRUE),
    missing_minutes = sum(!is.finite(rows$minutes)), missing_possessions = sum(!is.finite(rows$provider_possessions))),
    player_season_sets = window$points[, c("nba_id", "observed_season_ends")],
    salary_unit = window$salary_unit, signed_war_unit = "sum of source annual WAR over retained/omitted player-seasons")
}

assert_sensitivity_population <- function(expected, actual, fields = names(expected$points)) {
  a <- expected$points; b <- actual$points
  if (!all(fields %in% names(b)) || !identical(sort(a$nba_id), sort(b$nba_id)) ||
      !isTRUE(all.equal(a[order(a$nba_id), fields], b[order(b$nba_id), fields], check.attributes = FALSE))) {
    stop("Sensitivity requires the fixed main population and active-season inputs", call. = FALSE)
  }
  invisible(TRUE)
}

sensitivity_fold_maps <- function(analysis) {
  if (!isTRUE(analysis$available)) return(list())
  if (isTRUE(analysis$pilot_only)) stop("A timing pilot is not full relationship sensitivity", call. = FALSE)
  ids <- sort(as.character(analysis$window$points$nba_id))
  maps <- NULL
  for (outcome in c("lebron", "war")) {
    curve <- analysis$models[[outcome]]
    if (is.null(curve$bootstrap$planned_draws) || curve$bootstrap$planned_draws < 499L ||
        identical(curve$bootstrap$interval_status, "timing_pilot_only")) {
      stop("Full sensitivity requires at least 499 planned bootstrap refits", call. = FALSE)
    }
    fit <- curve$fit
    if (is.null(fit) || !identical(sort(fit$nba_ids), ids)) stop("Fit does not use the fixed window population", call. = FALSE)
    point_index <- match(fit$nba_ids, analysis$window$points$nba_id)
    if (!isTRUE(all.equal(fit$data$x, analysis$window$points$salary_cap_share[point_index])) ||
        !isTRUE(all.equal(fit$data$y, analysis$window$points[[outcome]][point_index]))) {
      stop("Fit does not use the fixed window inputs", call. = FALSE)
    }
    planned <- lapply(fit$config$partition_seeds, function(seed) player_fold_map(ids, seed, fit$config$folds))
    names(planned) <- as.character(fit$config$partition_seeds)
    if (!is.null(maps) && !identical(maps, planned)) stop("Outcomes require identical planned player folds", call. = FALSE)
    maps <- planned
    for (partition in fit$partitions) {
      expected <- maps[[as.character(partition$seed)]]
      if (is.null(expected) || !identical(partition$fold_map, expected)) {
        stop("Sensitivity fit changed the prescribed player fold map", call. = FALSE)
      }
    }
  }
  maps
}

run_window_sensitivity <- function(dataset, main_analysis, cache_dir = "cache/bootstrap",
                                   analysis_runner = analyze_player_window) {
  window <- main_analysis$window
  rules <- prespecified_exposure_rules()
  if (is.null(window) || !identical(window$rate_weight, "provider_possessions") ||
      !identical(window$exposure_rule, rules$main)) stop("Sensitivity requires a provider-weighted main window", call. = FALSE)
  expected <- make_player_window(dataset, window$end_season, window$window_length, rules$main)
  assert_sensitivity_population(expected, window)
  maps <- list(main = sensitivity_fold_maps(main_analysis))
  if (isTRUE(main_analysis$available) && any(vapply(main_analysis$models, function(x) is.null(x$monte_carlo), logical(1)))) {
    main_analysis <- analysis_runner(window, sentinel = TRUE, cache_dir = cache_dir)
    maps$main <- sensitivity_fold_maps(main_analysis)
  }
  analyses <- list(main = main_analysis)
  for (name in c("broad", "strict")) {
    alternative_window <- make_player_window(dataset, window$end_season, window$window_length, rules[[name]])
    analyses[[name]] <- analysis_runner(alternative_window, sentinel = TRUE, cache_dir = cache_dir)
    maps[[name]] <- sensitivity_fold_maps(analyses[[name]])
  }
  results <- list(coverage = eligibility_sensitivity(dataset, window$end_season, window$window_length, rules),
    support = if (isTRUE(main_analysis$available)) lapply(main_analysis$models, support_sensitivity) else list(),
    alternatives = lapply(analyses[c("broad", "strict")], function(x) compare_relationship_sensitivity(main_analysis, x)),
    analyses = analyses, fold_maps = maps, evidence = lapply(analyses, function(x) sensitivity_window_evidence(x$window)),
    protocol = methods_config()$protocol, publication_status = "private_diagnostics_only",
    interpretation = "Prespecified comparisons only; main exposure rule and model are not selected from these results")
  if (window$window_length %in% c(3L, 5L)) {
    minutes_window <- make_minutes_weighted_window(window)
    assert_sensitivity_population(window, minutes_window, setdiff(names(window$points), "lebron"))
    minutes <- analysis_runner(minutes_window, sentinel = TRUE, cache_dir = cache_dir)
    minute_maps <- sensitivity_fold_maps(minutes)
    if (isTRUE(main_analysis$available) && isTRUE(minutes$available)) {
      if (!identical(maps$main, minute_maps)) stop("Minutes sensitivity requires identical player folds", call. = FALSE)
      if (!identical(main_analysis$models$war$fit$data, minutes$models$war$fit$data)) stop("Minutes sensitivity changed WAR", call. = FALSE)
      # The existing analyzer handles both outcomes. Its WAR bootstrap has the
      # same cache identity; retain the exact original WAR object in this variant.
      minutes$models$war <- main_analysis$models$war
    }
    difference <- minutes_window$points$lebron - window$points$lebron
    comparison <- compare_relationship_sensitivity(main_analysis, minutes, outcomes = "lebron")
    comparison$point_outcome_difference <- sensitivity_difference_summary(difference)
    comparison$fixed_population <- TRUE
    comparison$point_rates <- tibble::tibble(nba_id = window$points$nba_id,
      provider_weighted_lebron = window$points$lebron, minutes_weighted_lebron = minutes_window$points$lebron)
    results$alternatives$minutes_weighted <- comparison
    results$analyses$minutes_weighted <- minutes
    results$fold_maps$minutes_weighted <- minute_maps
    results$evidence$minutes_weighted <- sensitivity_window_evidence(minutes_window)
  }
  results
}

sensitivity_all_window_coverage <- function(dataset) {
  seasons <- sort(unique(dataset$players$season))
  records <- list()
  for (season in seasons) for (span in c(1L, 3L, 5L)) {
    end <- as.integer(substr(season, 1, 4)) + 1L
    years <- seq.int(end - span + 1L, end)
    if (!all(sprintf("%d-%02d", years - 1L, years %% 100L) %in% seasons)) next
    for (name in names(prespecified_exposure_rules())) {
      window <- make_player_window(dataset, season, span, prespecified_exposure_rules()[[name]])
      records[[length(records) + 1L]] <- tibble::tibble(id = paste(end, span, sep = "-"), rule = name,
        end_season = season, window_length = span, eligible_players = nrow(window$points),
        evidence = list(sensitivity_window_evidence(window)))
    }
  }
  dplyr::bind_rows(records)
}

run_prespecified_sensitivity <- function(dataset, main_analyses = list(), cache_dir = "cache/bootstrap",
                                         analysis_runner = analyze_player_window) {
  plan <- sensitivity_sentinel_windows(dataset)
  if (!nrow(plan)) return(list(available = FALSE, reason = "no_shared_completed_seasons",
    windows = list(), sentinel_plan = plan, publication_status = "private_diagnostics_only"))
  windows <- list()
  for (i in seq_len(nrow(plan))) {
    id <- plan$id[i]
    if (!plan$available[i]) {
      windows[[id]] <- list(available = FALSE, reason = plan$reason[i]); next
    }
    main <- main_analyses[[id]]
    if (is.null(main)) {
      window <- make_player_window(dataset, plan$end_season[i], plan$window_length[i], prespecified_exposure_rules()$main)
      main <- analysis_runner(window, sentinel = TRUE, cache_dir = cache_dir)
    }
    if (!identical(main$window$end_season, plan$end_season[i]) ||
        !identical(as.integer(main$window$window_length), plan$window_length[i])) {
      stop("Reused main analysis does not match its sentinel window", call. = FALSE)
    }
    windows[[id]] <- run_window_sensitivity(dataset, main, cache_dir, analysis_runner)
  }
  list(available = TRUE, sentinel_plan = plan, windows = windows,
    coverage_all_windows = sensitivity_all_window_coverage(dataset),
    out_of_season = out_of_season_robustness(dataset), protocol = methods_config()$protocol,
    publication_status = "private_diagnostics_only", data_origin = dataset$data_origin)
}

# This separate model keeps raw annual rows; never fit an aggregate-window target
# and evaluate it against single-season outcomes.
fit_transport_relationship <- function(training, outcome, config = methods_config()) {
  ids <- as.character(training$nba_id)
  if (length(unique(ids)) < config$minimum_players) {
    return(list(available = FALSE, reason = "fewer_than_25_training_players"))
  }
  observation_ids <- paste(ids, training$season, sep = ":")
  ordering <- order(observation_ids)
  ids <- ids[ordering]; observation_ids <- observation_ids[ordering]
  data <- data.frame(x = training$salary_cap_share[ordering], y = training[[outcome]][ordering])
  first <- score_partition(data, ids, config$partition_seeds[1], config, observation_ids)
  partitions <- list(first)
  family <- first$selected_family
  if (family != "intercept") {
    second <- score_partition(data, ids, config$partition_seeds[2], config, observation_ids)
    partitions[[2]] <- second
    family <- config$candidate_order[min(match(c(family, second$selected_family), config$candidate_order))]
  }
  list(available = TRUE, model = fit_curve_family(data, family, config), data = data,
    selected_family = family, partitions = partitions,
    stability_disagreement = length(partitions) == 2L && first$selected_family != second$selected_family,
    observation_unit = "raw-player-season-transport", training_ids = ids,
    software = list(R = R.version.string, mgcv = as.character(utils::packageVersion("mgcv"))))
}

out_of_season_robustness <- function(dataset, config = methods_config()) {
  rows <- dataset$players
  require_columns(rows, c("season", "nba_id", "played", "minutes", "games", "provider_possessions",
    "salary_cap_share", "lebron", "war"), "Annual transport inputs")
  if (anyNA(rows$nba_id) || anyDuplicated(paste(rows$season, rows$nba_id))) {
    stop("Annual transport requires distinct identified player-seasons", call. = FALSE)
  }
  if (!nrow(rows)) return(list(available = FALSE, reason = "fewer_than_three_shared_completed_seasons",
    coverage = tibble::tibble()))
  check_seasons(sort(unique(rows$season)), as_of = Sys.Date())
  shared <- !is.na(rows$played) & rows$played &
    is.finite(rows$provider_possessions) & rows$provider_possessions > 0 &
    is.finite(rows$salary_cap_share) & rows$salary_cap_share >= 0 & is.finite(rows$lebron) & is.finite(rows$war)
  shared[is.na(shared)] <- FALSE
  # Freeze sentinel identities before applying the annual exposure rule.
  seasons <- sort(unique(rows$season[shared]))
  valid <- shared & is.finite(rows$minutes) & rows$minutes >= 200 &
    is.finite(rows$games) & rows$games >= 5
  valid[is.na(valid)] <- FALSE
  coverage <- dplyr::bind_rows(lapply(sort(unique(rows$season)), function(season) {
    at <- rows$season == season
    tibble::tibble(season = season, source_player_seasons = sum(at),
      active_player_seasons = sum(at & rows$played, na.rm = TRUE), included_player_seasons = sum(at & valid),
      shared_player_seasons = sum(at & shared),
      excluded_player_seasons = sum(at & !valid))
  }))
  rows <- rows[valid, ]
  if (length(seasons) < 3L) return(list(available = FALSE,
    reason = "fewer_than_three_shared_completed_seasons", coverage = coverage))
  held_out <- unique(seasons[c(1L, floor((length(seasons) + 1L) / 2L), length(seasons))])
  results <- list()
  for (season in held_out) for (outcome in c("lebron", "war")) {
    training <- rows[rows$season != season, ]
    test <- rows[rows$season == season, ]
    test <- test[order(as.character(test$nba_id)), ]
    key <- paste(season, outcome, sep = ":")
    base <- list(held_out_season = season, outcome = outcome,
      observation_unit = "raw player-season; distinct from standardized player-window target",
      interpretation = "Retrospective held-out-season diagnostic; later seasons may be in training; not a future forecast",
      context_note = "Interpret season length and CBA context; WAR is not rescaled to 82 games",
      training_seasons = sort(unique(training$season)), training_players = length(unique(training$nba_id)),
      training_observations = nrow(training), test_players = nrow(test),
      coverage = coverage[coverage$season == season, ])
    if (!nrow(test)) {
      results[[key]] <- c(base, list(available = FALSE, reason = "no_eligible_held_out_player_seasons")); next
    }
    fit <- tryCatch(fit_transport_relationship(training, outcome, config), error = function(e) {
      list(available = FALSE, reason = conditionMessage(e))
    })
    if (!isTRUE(fit$available)) {
      results[[key]] <- c(base, list(available = FALSE, reason = fit$reason)); next
    }
    predicted <- as.numeric(stats::predict(fit$model, newdata = data.frame(x = test$salary_cap_share)))
    if (length(predicted) != nrow(test) || any(!is.finite(predicted))) {
      results[[key]] <- c(base, list(available = FALSE, reason = "nonfinite_held_out_season_predictions")); next
    }
    observed <- test[[outcome]]
    error <- predicted - observed
    outside <- test$salary_cap_share < min(fit$data$x) | test$salary_cap_share > max(fit$data$x)
    calibration <- if (length(unique(predicted)) > 1L && stats::sd(predicted) > 1e-12 * max(1, max(abs(predicted)))) {
      coefficient <- stats::coef(stats::lm(observed ~ predicted))
      list(intercept = unname(coefficient[1]), slope = unname(coefficient[2]),
        note = "Descriptive held-out calibration only; never used for fitting or selection")
    } else list(intercept = NA_real_, slope = NA_real_, note = "Constant predictions; calibration slope unavailable")
    results[[key]] <- c(base, list(available = TRUE, selected_family = fit$selected_family,
      partitions = fit$partitions, stability_disagreement = fit$stability_disagreement, software = fit$software,
      cv_loss_unit = "equal raw player-season squared errors with whole-player folds",
      mse = mean(error^2), rmse = sqrt(mean(error^2)), mae = mean(abs(error)), mean_error = mean(error),
      observed_mean = mean(observed), predicted_mean = mean(predicted), calibration = calibration,
      training_mean_baseline_mse = mean((mean(fit$data$y) - observed)^2),
      outside_training_salary_range = sum(outside),
      in_range_mse = if (any(!outside)) mean(error[!outside]^2) else NA_real_,
      outside_range_mse = if (any(outside)) mean(error[outside]^2) else NA_real_,
      predictions = tibble::tibble(nba_id = as.character(test$nba_id), season = test$season,
        salary_cap_share = test$salary_cap_share, observed = observed, predicted = predicted,
        error = error, outside_training_salary_range = outside)))
  }
  list(available = TRUE, sentinel_seasons = held_out, coverage = coverage, results = results,
    publication_status = "private_diagnostics_only", data_origin = dataset$data_origin)
}

# Keep the main complete-season set and every non-rate point field unchanged.
make_minutes_weighted_window <- function(main_window) {
  if (!identical(main_window$rate_weight, 'provider_possessions')) {
    stop('Minutes sensitivity requires the provider-weighted main window', call. = FALSE)
  }
  result <- main_window
  rows <- main_window$active_player_seasons
  for (i in seq_len(nrow(result$points))) {
    id <- result$points$nba_id[i]
    r <- rows[as.character(rows$nba_id) == id, ]
    ends <- sort(as.integer(substr(r$season, 1, 4)) + 1L)
    if (!identical(ends, sort(result$points$observed_season_ends[[i]]))) {
      stop('Minutes sensitivity changed the fixed active-season set', call. = FALSE)
    }
    if (any(!is.finite(r$minutes)) || any(r$minutes < 0) || sum(r$minutes) <= 0 ||
        any(!is.finite(r$lebron))) stop('Invalid fixed-cohort minutes weights', call. = FALSE)
    result$points$lebron[i] <- sum(r$minutes * r$lebron) / sum(r$minutes)
  }
  result$rate_weight <- 'minutes'
  result$lebron_unit <- 'minutes_weighted_points_per_100'
  result
}

sensitivity_sentinel_windows <- function(dataset) {
  rows <- dataset$players
  require_columns(rows, c('season','played','salary_cap_share','lebron','war','provider_possessions'), 'Sentinel inputs')
  if (nrow(rows)) check_seasons(sort(unique(rows$season)), as_of = Sys.Date())
  valid <- !is.na(rows$played) & rows$played & is.finite(rows$salary_cap_share) & rows$salary_cap_share >= 0 &
    is.finite(rows$lebron) & is.finite(rows$war) & is.finite(rows$provider_possessions) & rows$provider_possessions > 0
  valid[is.na(valid)] <- FALSE
  seasons <- sort(unique(rows$season[valid]))
  if (!length(seasons)) return(tibble::tibble(id=character(),end_season=character(),
    end_year=integer(),window_length=integer(),available=logical(),reason=character()))
  check_seasons(seasons, as_of=Sys.Date())
  ends <- as.integer(substr(seasons,1,4))+1L
  latest <- max(ends); earliest <- min(ends)
  plan <- tibble::tibble(end_year=c(latest,latest,latest,earliest),window_length=c(1L,3L,5L,1L))
  plan <- plan[!duplicated(paste(plan$end_year,plan$window_length)),]
  plan$id <- paste(plan$end_year,plan$window_length,sep='-')
  plan$end_season <- sprintf('%d-%02d',plan$end_year-1L,plan$end_year%%100L)
  plan$available <- vapply(seq_len(nrow(plan)),function(i) {
    years <- seq.int(plan$end_year[i]-plan$window_length[i]+1L,plan$end_year[i])
    all(sprintf('%d-%02d',years-1L,years%%100L)%in%seasons)
  },logical(1))
  plan$reason <- ifelse(plan$available,NA_character_,'missing_contiguous_shared_seasons')
  plan[,c('id','end_season','end_year','window_length','available','reason')]
}
