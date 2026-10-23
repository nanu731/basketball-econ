# All uncertainty is conditional on provider scores, matched cohort/rules and the
# selected family. It does not estimate provider impact or post-selection error.
bootstrap_visible_intervals <- function(knots, central, draws, at = knots,
                                        endpoint, minimum_valid = 450L) {
  if (!is.matrix(draws) || ncol(draws) != length(knots)) stop("Invalid bootstrap curve matrix", call. = FALSE)
  valid <- apply(draws, 1L, function(x) all(is.finite(x)))
  draws <- draws[valid, , drop = FALSE]
  central_difference <- visible_interpolation(knots, central, endpoint) - visible_interpolation(knots, central, at)
  differences <- visible_interpolation(knots, draws, endpoint) - visible_interpolation(knots, draws, at)
  enough <- nrow(draws) >= minimum_valid
  curve_se <- if (enough) apply(draws, 2L, stats::sd) else rep(NA_real_, length(knots))
  contrast_se <- if (enough) apply(differences, 2L, stats::sd) else rep(NA_real_, length(at))
  list(valid_refits = nrow(draws), available = enough, curve_se = curve_se,
       curve_low = central - 1.96 * curve_se, curve_high = central + 1.96 * curve_se,
       contrast = central_difference, contrast_se = contrast_se,
       contrast_low = central_difference - 1.96 * contrast_se,
       contrast_high = central_difference + 1.96 * contrast_se,
       reason = if (enough) NULL else "fewer_than_450_valid_player_bootstrap_refits")
}

bootstrap_seed <- function(end_year, window_length, outcome, config = methods_config()) {
  as.integer(config$bootstrap_seed_base + 100L * end_year + 10L * window_length + match(outcome, c("lebron", "war")))
}

read_bootstrap_cache <- function(path, expected_identity, expected_draws) {
  checksum_path <- paste0(path, ".sha256")
  if (!file.exists(checksum_path)) stop("Missing bootstrap cache checksum", call. = FALSE)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  expected_hash <- rawToChar(readBin(checksum_path, "raw", n = file.info(checksum_path)$size))
  if (!identical(sha256_bytes(bytes), expected_hash)) stop("Bootstrap cache checksum mismatch", call. = FALSE)
  cached <- unserialize(bytes)
  if (!identical(cached$identity, expected_identity) || nrow(cached$predictions) != expected_draws ||
      ncol(cached$predictions) != length(expected_identity$knots)) stop("Bootstrap cache identity mismatch", call. = FALSE)
  cached
}

run_relationship_bootstrap <- function(curve, end_year, window_length, draws = 499L,
                                        cache_dir = "cache/bootstrap", pilot = FALSE) {
  fit <- curve$fit
  if (draws < 499L && !pilot) stop("Primary bootstrap requires at least 499 planned draws", call. = FALSE)
  if (length(draws) != 1L || is.na(draws) || draws < 2L || draws != floor(draws)) stop("Invalid bootstrap count", call. = FALSE)
  seed <- bootstrap_seed(end_year, window_length, fit$outcome, fit$config)
  cache_identity <- list(schema = "player-case-bootstrap-v1", data = fit$data, nba_ids = fit$nba_ids,
                   family = fit$selected_family, config = fit$config, knots = curve$knots,
                   reference_cap = curve$reference_cap_usd, seed = seed, software = fit$software)
  key <- sha256_bytes(serialize(cache_identity, NULL, version = 3))
  path <- file.path(cache_dir, paste0("bootstrap-", key, "-", draws, ".rds"))
  if (file.exists(path)) {
    return(read_bootstrap_cache(path, cache_identity, draws))
  }
  predictions <- matrix(NA_real_, nrow = draws, ncol = length(curve$knots))
  failures <- rep(NA_character_, draws); sample_min <- sample_max <- rep(NA_real_, draws)
  samples <- with_reproducible_seed(seed, replicate(draws, sample.int(nrow(fit$data), nrow(fit$data), replace = TRUE)))
  start <- 1L
  # A longer Monte Carlo check retains the actual first 499 draws, not a new run.
  prior_counts <- c(25L, 499L, 999L)
  prior_counts <- prior_counts[prior_counts < draws]
  prior_counts <- prior_counts[vapply(prior_counts, function(n) file.exists(file.path(cache_dir, paste0("bootstrap-", key, "-", n, ".rds"))), logical(1))]
  if (length(prior_counts)) {
    count <- max(prior_counts)
    prior_path <- file.path(cache_dir, paste0("bootstrap-", key, "-", count, ".rds"))
    prior <- read_bootstrap_cache(prior_path, cache_identity, count)
    predictions[seq_len(count), ] <- prior$predictions
    failures[seq_len(count)] <- prior$failures; sample_min[seq_len(count)] <- prior$sample_min; sample_max[seq_len(count)] <- prior$sample_max
    start <- count + 1L
  }
  elapsed <- system.time(for (b in seq.int(start, draws)) {
    data <- fit$data[samples[, b], , drop = FALSE]
    sample_min[b] <- min(data$x); sample_max[b] <- max(data$x)
    result <- tryCatch({
      model <- fit_curve_family(data, fit$selected_family, fit$config)
      as.numeric(stats::predict(model, newdata = data.frame(x = curve$knots)))
    }, error = identity)
    if (inherits(result, "error") || any(!is.finite(result))) failures[b] <- if (inherits(result, "error")) conditionMessage(result) else "nonfinite bootstrap predictions" else predictions[b, ] <- result
  })[["elapsed"]]
  # Resampling a precomputed one-player aggregate repeats the entire same set of
  # constituent seasons; it is algebraically identical to rebuilding that player.
  result <- list(identity = cache_identity, seed = seed, predictions = predictions, failures = failures,
                 sample_min = sample_min, sample_max = sample_max, planned_draws = draws,
                 valid_refits = sum(apply(predictions, 1L, function(x) all(is.finite(x)))),
                 elapsed_seconds = elapsed, pilot = pilot,
                 sampling_unit = "whole player, with complete matched active-season set preserved")
  bytes <- serialize(result, NULL, version = 3)
  write_new_bytes(bytes, path)
  write_new_bytes(charToRaw(sha256_bytes(bytes)), paste0(path, ".sha256"))
  result
}

bootstrap_shape_diagnostics <- function(samples, center, supported) {
  samples <- samples[apply(samples, 1L, function(x) all(is.finite(x))), , drop = FALSE]
  if (nrow(samples) < 2L || !any(supported)) return(list(flag = FALSE, evaluated = 0L, reason = "no_supported_points_or_valid_refits"))
  se <- apply(samples, 2, stats::sd)
  bias <- colMeans(samples) - center
  quantiles <- apply(samples, 2, stats::quantile, probs = c(.025, .5, .975), names = FALSE)
  ratio <- (quantiles[3, ] - quantiles[2, ]) / (quantiles[2, ] - quantiles[1, ])
  nondegenerate <- se > sqrt(.Machine$double.eps) * pmax(1, abs(center)) & supported
  flagged <- (abs(bias) > .5 * se | ratio < .5 | ratio > 2) & nondegenerate
  list(flag = any(nondegenerate) && sum(flagged, na.rm = TRUE) / sum(nondegenerate) >= .10,
       evaluated = sum(nondegenerate), flagged = sum(flagged, na.rm = TRUE),
       bias_over_se = ifelse(se > 0, bias / se, NA_real_), quantile_asymmetry = ratio)
}

attach_bootstrap_intervals <- function(curve, bootstrap) {
  valid <- apply(bootstrap$predictions, 1L, function(x) all(is.finite(x)))
  samples <- bootstrap$predictions[valid, , drop = FALSE]
  support <- curve$support$supported
  endpoint <- pmin(curve$support$endpoint, max(curve$knots))
  intervals <- bootstrap_visible_intervals(curve$knots, curve$expected, samples,
    at = curve$knots, endpoint = endpoint, minimum_valid = curve$fit$config$bootstrap_min_valid)
  differences <- visible_interpolation(curve$knots, samples, endpoint) - samples
  central_diff <- visible_interpolation(curve$knots, curve$expected, endpoint) - curve$expected
  curve_diagnostic <- bootstrap_shape_diagnostics(samples, curve$expected, support)
  contrast_diagnostic <- bootstrap_shape_diagnostics(differences, central_diff, support)
  curve$low <- intervals$curve_low; curve$high <- intervals$curve_high
  curve$contrast_low <- intervals$contrast_low; curve$contrast_high <- intervals$contrast_high
  curve$contrast_low[!support] <- NA_real_; curve$contrast_high[!support] <- NA_real_
  if (curve_diagnostic$flag || !intervals$available || bootstrap$pilot) curve$low[] <- curve$high[] <- NA_real_
  if (contrast_diagnostic$flag || !intervals$available || bootstrap$pilot) curve$contrast_low[] <- curve$contrast_high[] <- NA_real_
  curve$interval_method <- "95% pointwise normal-approximation whole-player case-bootstrap SE; conditional on fixed provider scores, cohort and selected family"
  curve$bootstrap <- list(seed = bootstrap$seed, planned_draws = bootstrap$planned_draws,
    valid_refits = bootstrap$valid_refits, elapsed_seconds = bootstrap$elapsed_seconds,
    curve_shape_diagnostic = curve_diagnostic, contrast_shape_diagnostic = contrast_diagnostic,
    boundary_omission_draws = sum(bootstrap$sample_min > min(curve$knots) | bootstrap$sample_max < max(curve$knots)),
    interval_status = if (bootstrap$pilot) "timing_pilot_only" else if (!intervals$available) intervals$reason else
      if (curve_diagnostic$flag || contrast_diagnostic$flag) "normal_approximation_review_required" else "computed_conditional_intervals")
  curve
}

bootstrap_monte_carlo_check <- function(curve, shorter, longer) {
  endpoints <- pmin(curve$support$endpoint, max(curve$knots))
  a <- bootstrap_visible_intervals(curve$knots, curve$expected, shorter$predictions, curve$knots, endpoints)
  b <- bootstrap_visible_intervals(curve$knots, curve$expected, longer$predictions, curve$knots, endpoints)
  supported <- curve$support$supported
  floor <- .01 * stats::sd(curve$fit$data$y)
  compare <- function(low_a, high_a, low_b, high_b) {
    eligible <- supported & is.finite(low_a) & is.finite(high_a) & is.finite(low_b) & is.finite(high_b)
    width_a <- (high_a - low_a) / 2; width_b <- (high_b - low_b) / 2
    changed <- abs(width_b - width_a) / pmax(width_a, floor)
    fraction <- if (any(eligible)) mean(changed[eligible] > .10) else NA_real_
    list(evaluated = sum(eligible), material_fraction = fraction,
         unstable = !is.na(fraction) && fraction >= .10)
  }
  list(curve = compare(a$curve_low, a$curve_high, b$curve_low, b$curve_high),
       contrast = compare(a$contrast_low, a$contrast_high, b$contrast_low, b$contrast_high),
       retained_first_499 = identical(shorter$predictions[1:499, , drop = FALSE], longer$predictions[1:499, , drop = FALSE]))
}
