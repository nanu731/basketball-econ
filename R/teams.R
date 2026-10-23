# Codes in the modern LEBRON period. Source spelling remains in source_team.
# Historical relocations are validated by season rather than treated as current.
canonical_team_codes <- function(team, season) {
  if (!is.character(team) || length(team) != 1L || is.na(team) ||
      !grepl("^[A-Z]{3}(/[A-Z]{3})*$", team)) stop("Invalid team membership delimiter", call. = FALSE)
  end <- as.integer(substr(check_seasons(season), 1, 4)) + 1L
  if (length(end) != 1L) stop("Team validation needs one season", call. = FALSE)
  current <- c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU",
               "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL",
               "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
  supported <- c(setdiff(current, c("BKN", "NOP")), "PHO")
  supported <- c(supported, if (end <= 2012L) "NJN" else c("BKN", "BRK"),
                 if (end <= 2013L) "NOH" else "NOP", if (end >= 2014L) "CHO")
  # The provider uses CHO already in 2013-14 for the Charlotte franchise.
  # source_team retains this spelling; it does not assert an earlier team rename.
  codes <- strsplit(team, "/", fixed = TRUE)[[1]]
  if (any(!codes %in% supported)) stop("Unsupported or aggregate team code for season", call. = FALSE)
  aliases <- c(BRK = "BKN", CHO = "CHA", PHO = "PHX")
  replace <- codes %in% names(aliases)
  codes[replace] <- unname(aliases[codes[replace]])
  if (anyDuplicated(codes)) stop("Duplicate team membership after alias normalization", call. = FALSE)
  codes
}

team_franchise_code <- function(team) {
  aliases <- c(NJN = "BKN", BRK = "BKN", NOH = "NOP", CHO = "CHA", PHO = "PHX")
  result <- as.character(team)
  replace <- result %in% names(aliases)
  result[replace] <- unname(aliases[result[replace]])
  result
}

team_name_franchise_code <- function(name) {
  # Exact normalized full names/known historical aliases only; no fuzzy matching
  # and no use of Fanspo's small integer team IDs as NBA identifiers.
  names <- c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls",
    "Cleveland Cavaliers", "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", "Golden State Warriors",
    "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies",
    "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks",
    "Oklahoma City Thunder", "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers",
    "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors", "Utah Jazz", "Washington Wizards",
    "New Jersey Nets", "New Orleans Hornets", "Charlotte Bobcats", "LA Clippers")
  codes <- c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND",
    "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR",
    "SAC", "SAS", "TOR", "UTA", "WAS", "BKN", "NOP", "CHA", "LAC")
  if (length(names) != length(codes)) stop("Team-name crosswalk length mismatch", call. = FALSE)
  normalize <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  codes[match(normalize(name), normalize(names))]
}

annotate_single_team_attribution <- function(players, salary_history) {
  players$team_attribution_verified <- FALSE
  players$attributed_team <- NA_character_
  players$attribution_evidence <- "not_verified"
  if (is.null(salary_history) || !nrow(salary_history)) return(players)
  for (i in seq_len(nrow(players))) {
    if (players$team_count[i] != 1L || is.na(players$salary_reported_usd[i])) next
    history <- salary_history[salary_history$nba_id == players$nba_id[i] & salary_history$season == players$season[i], ]
    if (nrow(history) != 1L || history$source_salary_status[1] != "reported_positive") next
    source_team <- team_name_franchise_code(history$source_team_name[1])
    metric_team <- team_franchise_code(players$teams[i])
    if (!is.na(source_team) && identical(source_team, metric_team)) {
      players$team_attribution_verified[i] <- TRUE
      players$attributed_team[i] <- players$teams[i]
      players$attribution_evidence[i] <- "single_metric_team_matches_unique_historical_salary_team"
    }
  }
  players
}
