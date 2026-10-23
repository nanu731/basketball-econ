validate_exposure_rule <- function(rule) {
  require_columns(rule, c("id", "minimum_minutes", "minimum_possessions", "minimum_games"), "Exposure rule")
  if (!is.character(rule$id) || length(rule$id) != 1L || is.na(rule$id) || !nzchar(rule$id)) stop("Name the exposure rule", call. = FALSE)
  for (field in c("minimum_minutes", "minimum_possessions", "minimum_games")) {
    if (length(rule[[field]]) != 1L) stop("Exposure thresholds must be scalar", call. = FALSE)
    require_numeric(rule[[field]], field, minimum = 0, integer = field == "minimum_games")
  }
  invisible(rule)
}

prespecified_exposure_rules <- function() {
  list(broad = list(id = "broad-positive-1game", minimum_minutes = 0, minimum_games = 1, minimum_possessions = 0),
       main = list(id = "main-200minutes-5games", minimum_minutes = 200, minimum_games = 5, minimum_possessions = 0),
       strict = list(id = "strict-500minutes-10games", minimum_minutes = 500, minimum_games = 10, minimum_possessions = 0))
}

make_player_window <- function(dataset, end_season, window_length, exposure_rule,
                               rate_weight = c("provider_possessions", "minutes")) {
  rate_weight <- match.arg(rate_weight)
  end_season <- check_seasons(end_season)
  if (length(end_season) != 1L || length(window_length) != 1L || is.na(window_length) ||
      window_length < 1L || window_length != floor(window_length)) stop("Invalid window", call. = FALSE)
  validate_exposure_rule(exposure_rule)
  end <- as.integer(substr(end_season, 1, 4)) + 1L
  years <- seq.int(end - window_length + 1L, end)
  seasons <- sprintf("%d-%02d", years - 1L, years %% 100L)
  rows <- dataset$players
  require_columns(rows, c("season", "nba_id", "player_name", "played", "minutes", "games",
                          "provider_possessions", "salary_reported_usd", "salary_cap_share", "lebron", "war"), "Window inputs")
  if (!all(seasons %in% unique(rows$season))) stop("Requested window includes a season without acquired metric coverage", call. = FALSE)
  if (anyDuplicated(paste(rows$season, rows$nba_id))) stop("Duplicate player-season in window inputs", call. = FALSE)
  cap <- official_caps()
  reference_cap <- cap$cap_usd[cap$season == end_season]
  if (length(reference_cap) != 1L) stop("No official end-season reference cap", call. = FALSE)
  selected <- rows[rows$season %in% seasons, ]
  active <- selected[selected$played, ]
  groups <- split(seq_len(nrow(active)), as.character(active$nba_id))
  points <- list(); excluded <- list()
  for (indices in groups) {
    r <- active[indices, ]; r <- r[order(r$season), ]
    id <- as.character(r$nba_id[1]); reasons <- character()
    if (anyNA(r$salary_reported_usd) || anyNA(r$salary_cap_share)) reasons <- c(reasons, "missing_or_unresolved_salary")
    if (anyNA(r$lebron) || anyNA(r$war)) reasons <- c(reasons, "missing_lebron_or_war")
    if (anyNA(r$provider_possessions) || any(r$provider_possessions <= 0, na.rm = TRUE)) reasons <- c(reasons, "missing_or_nonpositive_possessions")
    if (anyNA(r$minutes) || anyNA(r$provider_possessions) || sum(r$minutes) < exposure_rule$minimum_minutes ||
        sum(r$provider_possessions) < exposure_rule$minimum_possessions ||
        sum(r$games) < exposure_rule$minimum_games) reasons <- c(reasons, "below_window_exposure_rule")
    if (length(reasons)) {
      excluded[[length(excluded) + 1L]] <- tibble::tibble(nba_id = id, name = tail(r$player_name, 1),
        observed_seasons = nrow(r), reasons = list(unique(reasons)))
      next
    }
    for (field in c("minutes", "games", "provider_possessions", "salary_reported_usd", "salary_cap_share", "lebron", "war")) {
      require_numeric(r[[field]], field, minimum = if (field %in% c("lebron", "war")) -Inf else 0)
    }
    membership <- dataset$teams[as.character(dataset$teams$nba_id) == id & dataset$teams$season %in% r$season, ]
    share <- mean(r$salary_cap_share)
    flags <- c(if (any(r$minutes < 200 | r$games < 5)) "includes_low_exposure_season",
               if (nrow(r) < window_length) "not_observed_in_every_selected_season",
               if (any(r$team_count > 1L)) "includes_unallocated_trade_season")
    points[[length(points) + 1L]] <- tibble::tibble(
      nba_id = id, name = tail(r$player_name, 1), teams = list(sort(unique(membership$team))),
      observed_seasons = nrow(r), observed_season_ends = list(as.integer(substr(r$season, 1, 4)) + 1L),
      selected_window_length = as.integer(window_length), minutes = sum(r$minutes),
      possessions = sum(r$provider_possessions), games = sum(r$games),
      salary_reported_usd = if (window_length == 1L) r$salary_reported_usd[1] else NA_real_,
      reference_salary_usd = share * reference_cap, salary_cap_share = share,
      lebron = sum(r$lebron * r[[rate_weight]]) / sum(r[[rate_weight]]),
      war = mean(r$war), flags = list(flags))
  }
  empty_points <- tibble::tibble(nba_id = character(), name = character(), teams = list(),
    observed_seasons = integer(), observed_season_ends = list(), selected_window_length = integer(),
    minutes = double(), possessions = double(), games = integer(), salary_reported_usd = double(),
    reference_salary_usd = double(), salary_cap_share = double(), lebron = double(), war = double(), flags = list())
  points <- if (length(points)) dplyr::bind_rows(points) else empty_points
  points <- points[order(points$nba_id), ]
  excluded <- if (length(excluded)) dplyr::bind_rows(excluded) else tibble::tibble(nba_id = character(), name = character(), observed_seasons = integer(), reasons = list())
  known <- !is.na(active$salary_reported_usd)
  eligible_rows <- as.character(active$nba_id) %in% points$nba_id
  known_total <- if (anyNA(active$salary_cap_share[known])) NA_real_ else sum(active$salary_cap_share[known])
  included_total <- sum(active$salary_cap_share[eligible_rows], na.rm = TRUE)
  coverage <- list(source_player_seasons = nrow(selected), active_player_seasons = nrow(active),
                   observed_active_players = length(groups), eligible_players = nrow(points),
                   excluded_players = nrow(excluded), eligible_player_seasons = sum(eligible_rows),
                   missing_salary_player_seasons = sum(!known),
                   included_known_salary_usd = included_total * reference_cap / window_length,
                   all_known_active_salary_usd = known_total * reference_cap / window_length,
                   retained_known_signed_war = sum(active$war[eligible_rows], na.rm = TRUE),
                   omitted_known_signed_war = sum(active$war[!eligible_rows], na.rm = TRUE),
                   eligible_share_of_known_cap_relative_salary = if (!is.na(known_total) && known_total > 0) included_total / known_total else NA_real_)
  list(schema_version = "player-window-v1", end_season = end_season, end_season_year = end,
       window_length = as.integer(window_length), seasons = seasons, reference_cap_usd = reference_cap,
       representation = if (window_length == 1L) "player_season" else "standardized_player_window",
       salary_unit = if (window_length == 1L) "reported_nominal_usd" else "average_annual_end_season_cap_equivalent_usd",
       lebron_unit = paste0(rate_weight, "_weighted_points_per_100"), rate_weight = rate_weight,
       war_unit = "average_WAR_per_observed_active_season",
       points = points, excluded = excluded, exposure_rule = exposure_rule, coverage = coverage,
       active_player_seasons = active, teams = dataset$teams[dataset$teams$season %in% seasons, ],
       source_provenance = dataset$source_provenance, data_origin = dataset$data_origin,
       publication_status = dataset$publication_status)
}

eligibility_sensitivity <- function(dataset, end_season, window_length, rules) {
  dplyr::bind_rows(lapply(rules, function(rule) {
    view <- make_player_window(dataset, end_season, window_length, rule)
    tibble::tibble(rule_id = rule$id, minimum_minutes = rule$minimum_minutes,
                   minimum_games = rule$minimum_games,
                   minimum_possessions = rule$minimum_possessions,
                   eligible_players = nrow(view$points), excluded_players = nrow(view$excluded),
                   eligible_player_seasons = view$coverage$eligible_player_seasons,
                   eligible_share_of_known_cap_relative_salary = view$coverage$eligible_share_of_known_cap_relative_salary)
  }))
}

summarize_attributable_teams <- function(window) {
  rows <- window$active_player_seasons
  rows$in_player_sample <- as.character(rows$nba_id) %in% window$points$nba_id
  # Season membership alone is insufficient evidence of a salary obligation.
  if (!"team_attribution_verified" %in% names(rows)) rows$team_attribution_verified <- FALSE
  if (!"attributed_team" %in% names(rows)) rows$attributed_team <- NA_character_
  supported <- rows$in_player_sample & rows$team_attribution_verified & rows$team_count == 1L &
    !is.na(rows$attributed_team) & !is.na(rows$salary_cap_share) & !is.na(rows$war)
  supported[is.na(supported)] <- FALSE
  if (any(supported)) for (i in which(supported)) {
    membership <- window$teams$team[window$teams$season == rows$season[i] &
                                    as.character(window$teams$nba_id) == as.character(rows$nba_id[i])]
    if (!rows$attributed_team[i] %in% membership) stop("Verified attribution is outside recorded team membership", call. = FALSE)
  }
  teams <- sort(unique(window$teams$team))
  dplyr::bind_rows(lapply(teams, function(team) {
    members <- unique(paste(window$teams$season[window$teams$team == team], window$teams$nba_id[window$teams$team == team]))
    membership <- paste(rows$season, rows$nba_id) %in% members
    selected <- supported & rows$attributed_team == team
    selected[is.na(selected)] <- FALSE
    available <- any(selected)
    tibble::tibble(team = team, attributable_player_seasons = sum(selected),
                   attributable_seasons = length(unique(rows$season[selected])),
                   selected_window_length = window$window_length,
                   reported_salary_average_annual_reference_usd = if (available) sum(rows$salary_cap_share[selected]) * window$reference_cap_usd / window$window_length else NA_real_,
                   war_average_annual = if (available) sum(rows$war[selected]) / window$window_length else NA_real_,
                   excluded_trade_player_seasons = sum(membership & rows$team_count > 1L),
                   excluded_ineligible_player_seasons = sum(membership & !rows$in_player_sample),
                   excluded_unknown_attribution_player_seasons = sum(membership & rows$in_player_sample & rows$team_count == 1L & !supported),
                   status = if (available) "attributable_sample_only" else "no_verified_attribution")
  }))
}
