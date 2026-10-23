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
                 if (end <= 2013L) "NOH" else "NOP", if (end >= 2015L) "CHO")
  codes <- strsplit(team, "/", fixed = TRUE)[[1]]
  if (any(!codes %in% supported)) stop("Unsupported or aggregate team code for season", call. = FALSE)
  aliases <- c(BRK = "BKN", CHO = "CHA", PHO = "PHX")
  replace <- codes %in% names(aliases)
  codes[replace] <- unname(aliases[codes[replace]])
  if (anyDuplicated(codes)) stop("Duplicate team membership after alias normalization", call. = FALSE)
  codes
}
