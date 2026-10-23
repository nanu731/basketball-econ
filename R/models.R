# Prespecified METHODS v1: equal-player native-unit losses, grouped folds,
# training-only REML, one-SE simplicity, and a second fixed partition for stability.
methods_config <- function() {
  list(protocol = "moneyball-methods-v1", candidate_order = c("intercept", "linear", "smooth"),
       folds = 5L, partition_seeds = c(20261023L, 20261024L), spline_basis = 5L,
       minimum_players = 25L, bootstrap_draws = 499L, bootstrap_min_valid = 450L,
       bootstrap_seed_base = 20261025L, support_radius = .02, support_count = 20L,
       support_bracket = 5L, support_distinct = 5L, confidence = .95)
}

with_reproducible_seed <- function(seed, expression) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  previous <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(kind))
    if (had_seed) assign(".Random.seed", previous, envir = .GlobalEnv) else
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  force(expression)
}

player_fold_map <- function(ids, seed, folds = 5L) {
  ids <- sort(unique(as.character(ids)))
  if (length(ids) < folds) stop("Not enough players for the prescribed folds", call. = FALSE)
  permutation <- with_reproducible_seed(seed, sample(ids, length(ids)))
  result <- tibble::tibble(nba_id = permutation, fold = rep(seq_len(folds), length.out = length(ids)))
  result[order(result$nba_id), ]
}

fit_curve_family <- function(data, family, config = methods_config()) {
  if (!family %in% config$candidate_order) stop("Unknown curve family", call. = FALSE)
  if (any(!is.finite(data$x)) || any(!is.finite(data$y))) stop("Nonfinite relationship inputs", call. = FALSE)
  if (family == "intercept") return(stats::lm(y ~ 1, data = data))
  if (family == "linear") {
    if (length(unique(data$x)) < 2L) stop("Linear candidate has insufficient distinct salaries", call. = FALSE)
    return(stats::lm(y ~ x, data = data))
  }
  if (length(unique(data$x)) < config$spline_basis) stop("Smooth candidate has insufficient distinct training salaries", call. = FALSE)
  s <- mgcv::s
  k <- config$spline_basis
  model <- withCallingHandlers(
    mgcv::gam(y ~ s(x, bs = "cr", k = k), data = data, method = "REML", family = stats::gaussian()),
    warning = function(w) stop("Smooth candidate numerical warning: ", conditionMessage(w), call. = FALSE))
  if (!isTRUE(model$converged)) stop("Smooth candidate did not converge", call. = FALSE)
  model
}

score_partition <- function(data, ids, seed, config, observation_ids = ids) {
  map <- player_fold_map(ids, seed, config$folds)
  fold <- map$fold[match(ids, map$nba_id)]
  scores <- list(); predictions <- list(); losses <- list()
  for (family in config$candidate_order) {
    predicted <- rep(NA_real_, nrow(data)); failure <- NULL
    for (part in seq_len(config$folds)) {
      train <- data[fold != part, , drop = FALSE]
      test <- data[fold == part, , drop = FALSE]
      value <- tryCatch({
        model <- fit_curve_family(train, family, config)
        as.numeric(stats::predict(model, newdata = test))
      }, error = identity)
      if (inherits(value, "error") || length(value) != nrow(test) || any(!is.finite(value))) {
        failure <- if (inherits(value, "error")) conditionMessage(value) else "nonfinite held-out prediction"
        break
      }
      predicted[fold == part] <- value
    }
    available <- is.null(failure) && all(is.finite(predicted))
    error <- (predicted - data$y)^2
    fold_mse <- vapply(seq_len(config$folds), function(part) mean(error[fold == part]), double(1))
    scores[[family]] <- tibble::tibble(family = family, available = available,
      mse = if (available) mean(error) else NA_real_, fold_se = if (available) stats::sd(fold_mse) / sqrt(config$folds) else NA_real_,
      scored_players = if (available) length(unique(ids)) else 0L,
      scored_observations = if (available) nrow(data) else 0L, failure = if (is.null(failure)) NA_character_ else failure)
    predictions[[family]] <- tibble::tibble(nba_id = ids, observation_id = observation_ids, fold = fold, family = family,
      observed = data$y, predicted = if (available) predicted else NA_real_)
    losses[[family]] <- tibble::tibble(family = family, fold = seq_len(config$folds), mse = if (available) fold_mse else NA_real_)
  }
  scores <- dplyr::bind_rows(scores)
  valid <- which(scores$available)
  if (!length(valid)) stop("All prespecified candidates failed", call. = FALSE)
  best <- valid[which.min(scores$mse[valid])]
  threshold <- scores$mse[best] + scores$fold_se[best]
  selected <- which(scores$available & scores$mse <= threshold)[1]
  list(seed = seed, fold_map = map, scores = scores, fold_losses = dplyr::bind_rows(losses),
       predictions = dplyr::bind_rows(predictions), selected_family = scores$family[selected],
       one_se_threshold = threshold, selection_note = "Fold-SE is a simplicity heuristic, not a significance test or uncertainty interval")
}

fit_salary_relationship <- function(points, outcome, config = methods_config(),
                                     observation_unit = c("player-window", "player-season-transport")) {
  observation_unit <- match.arg(observation_unit)
  if (!outcome %in% c("lebron", "war")) stop("Only source LEBRON and WAR outcomes are permitted", call. = FALSE)
  require_columns(points, c("nba_id", "salary_cap_share", outcome), "Relationship points")
  ids <- as.character(points$nba_id)
  if (anyNA(ids) || (observation_unit == "player-window" && anyDuplicated(ids))) stop("Main relationship requires one equal-weight point per player", call. = FALSE)
  observation_ids <- ids
  if (observation_unit == "player-season-transport") {
    require_columns(points, "season", "Transport observations")
    observation_ids <- paste(ids, points$season, sep = ":")
    if (anyDuplicated(observation_ids)) stop("Duplicate player-season in transport observations", call. = FALSE)
  }
  require_numeric(points$salary_cap_share, "salary_cap_share", minimum = 0)
  require_numeric(points[[outcome]], outcome)
  if (length(unique(ids)) < config$minimum_players) return(list(available = FALSE, reason = "fewer_than_25_eligible_players", outcome = outcome))
  if (length(unique(points$salary_cap_share)) < 2L) return(list(available = FALSE,
    reason = "constant_model_no_resolved_salary_effect", outcome = outcome,
    raw_records = points, distinct_salary_values = 1L))
  order <- order(observation_ids); points <- points[order, ]; ids <- ids[order]; observation_ids <- observation_ids[order]
  data <- data.frame(x = points$salary_cap_share, y = points[[outcome]])
  first <- score_partition(data, ids, config$partition_seeds[1], config, observation_ids)
  partitions <- list(first)
  family <- first$selected_family
  if (family != "intercept") {
    second <- score_partition(data, ids, config$partition_seeds[2], config, observation_ids)
    partitions[[2]] <- second
    family <- config$candidate_order[min(match(c(first$selected_family, second$selected_family), config$candidate_order))]
  }
  model <- fit_curve_family(data, family, config)
  expected <- as.numeric(stats::predict(model, newdata = data))
  leverage <- if (inherits(model, "gam")) model$hat else stats::hatvalues(model)
  list(available = TRUE, outcome = outcome, observation_unit = observation_unit, selected_family = family, model = model,
       data = data, nba_ids = ids, partitions = partitions, config = config,
       expected = expected, residual = data$y - expected,
       stability_disagreement = length(partitions) > 1L && first$selected_family != partitions[[2]]$selected_family,
       diagnostics = list(leverage = as.numeric(leverage), residual_sd = stats::sd(data$y - expected),
         residual_salary_correlation = if (stats::sd(data$x) > 0 && stats::sd(data$y - expected) > 0) stats::cor(data$x, data$y - expected) else NA_real_),
       software = list(R = R.version.string, mgcv = as.character(utils::packageVersion("mgcv")),
                       rng = "Mersenne-Twister/Inversion/Rejection"))
}

predict_relationship <- function(fit, x) {
  if (!isTRUE(fit$available)) stop("Relationship model is unavailable", call. = FALSE)
  as.numeric(stats::predict(fit$model, newdata = data.frame(x = x)))
}

visible_interpolation <- function(knots, values, x) {
  if (length(knots) < 2L || any(diff(knots) <= 0) || any(!is.finite(knots))) stop("Invalid visible curve knots", call. = FALSE)
  matrix_input <- is.matrix(values)
  if (!matrix_input) values <- matrix(values, nrow = 1L)
  if (ncol(values) != length(knots)) stop("Curve values do not match knots", call. = FALSE)
  if (any(x < min(knots) | x > max(knots))) stop("Visible interpolation cannot extrapolate", call. = FALSE)
  index <- findInterval(x, knots, all.inside = TRUE)
  weight <- (x - knots[index]) / (knots[index + 1L] - knots[index])
  result <- sweep(values[, index, drop = FALSE], 2, 1 - weight, `*`) +
    sweep(values[, index + 1L, drop = FALSE], 2, weight, `*`)
  if (matrix_input) result else as.numeric(result)
}

local_contrast_support <- function(x, at, reference_cap, config = methods_config()) {
  require_numeric(x, "original eligible salaries", minimum = 0)
  require_numeric(reference_cap, "reference_cap", minimum = 1)
  h <- 1000000 / reference_cap
  end <- at + h
  count <- function(a) {
    nearby <- x[abs(x - a) <= config$support_radius]
    c(n = length(nearby), below = sum(nearby < a), above = sum(nearby > a), distinct = length(unique(nearby)))
  }
  start_counts <- t(vapply(at, count, numeric(4))); end_counts <- t(vapply(end, count, numeric(4)))
  reason <- vapply(seq_along(at), function(i) {
    if (at[i] < min(x) || end[i] > max(x)) return("outside_observed_range")
    if (min(start_counts[i, "n"], end_counts[i, "n"]) < config$support_count) return("insufficient_local_players")
    if (min(start_counts[i, c("below", "above")], end_counts[i, c("below", "above")]) < config$support_bracket) return("insufficient_bracketing")
    if (min(start_counts[i, "distinct"], end_counts[i, "distinct"]) < config$support_distinct) return("too_few_salary_values")
    NA_character_
  }, character(1))
  tibble::tibble(x = at, endpoint = end, supported = is.na(reason), reason = reason,
    n_start = as.integer(start_counts[, "n"]), n_end = as.integer(end_counts[, "n"]),
    below_start = as.integer(start_counts[, "below"]), above_start = as.integer(start_counts[, "above"]),
    below_end = as.integer(end_counts[, "below"]), above_end = as.integer(end_counts[, "above"]),
    distinct_start = as.integer(start_counts[, "distinct"]), distinct_end = as.integer(end_counts[, "distinct"]))
}

relationship_curve <- function(fit, reference_cap, grid_size = 101L) {
  if (!isTRUE(fit$available)) stop("Cannot construct a salary reference: ", fit$reason, call. = FALSE)
  knots <- sort(unique(c(seq(min(fit$data$x), max(fit$data$x), length.out = grid_size), fit$data$x)))
  values <- predict_relationship(fit, knots)
  support <- local_contrast_support(fit$data$x, knots, reference_cap, fit$config)
  if (fit$selected_family == "intercept") {
    support$supported <- FALSE
    support$reason <- "constant_model_no_resolved_salary_effect"
  }
  contrast <- rep(NA_real_, length(knots))
  valid <- support$supported
  if (any(valid)) contrast[valid] <- visible_interpolation(knots, values, support$endpoint[valid]) - values[valid]
  list(knots = knots, expected = values, support = support, contrast = contrast,
       reference_cap_usd = reference_cap, interpolation = "piecewise_linear",
       interval_method = "pending_bootstrap", fit = fit)
}

leave_player_out_influence <- function(fit, ids = fit$nba_ids) {
  indices <- match(as.character(ids), fit$nba_ids)
  if (anyNA(indices)) stop("Influence request contains an unknown player", call. = FALSE)
  expected <- vapply(indices, function(i) {
    if (fit$selected_family != "smooth") {
      h <- fit$diagnostics$leverage[i]
      if (1 - h <= 1e-10) return(NA_real_)
      return(fit$expected[i] - h * fit$residual[i] / (1 - h))
    }
    tryCatch({
      model <- fit_curve_family(fit$data[-i, , drop = FALSE], fit$selected_family, fit$config)
      as.numeric(stats::predict(model, newdata = fit$data[i, , drop = FALSE]))
    }, error = function(e) NA_real_)
  }, double(1))
  tibble::tibble(nba_id = fit$nba_ids[indices], full_expected = fit$expected[indices],
    leave_player_out_expected = expected, expected_change = expected - fit$expected[indices],
    status = ifelse(is.finite(expected), "computed_sensitivity_only", "refit_unavailable"))
}
