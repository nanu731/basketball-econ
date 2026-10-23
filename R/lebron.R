LEBRON_SOURCE_URL <- "https://fanspo.com/bbi-role-explorer/api/lebron_dashboard_data"
LEBRON_SCHEMA_VERSION <- "lebron-normalized-v1"

lebron_request <- function(seasons) {
  seasons <- check_seasons(seasons)
  list(seasons = as.list(as.character(as.integer(substr(seasons, 1, 4)) + 1L)),
       positions = list(), offensiveArchetypes = list(), defensiveRoles = list(),
       minMinutes = 0L, teams = list(), playerRoles = list(), minMpg = 0L,
       seasonView = "separate")
}

normalize_lebron <- function(bytes, seasons) {
  seasons <- check_seasons(seasons)
  if (!is.raw(bytes)) stop("LEBRON normalization requires original response bytes", call. = FALSE)
  parsed <- tryCatch(jsonlite::fromJSON(rawToChar(bytes)), error = function(e) {
    stop("Invalid LEBRON JSON response", call. = FALSE)
  })
  data <- parsed$players
  if (!is.data.frame(data) || !nrow(data)) stop("LEBRON response has no player records", call. = FALSE)
  required <- c("Season", "year", "nba_id", "bref_id", "full_nm", "team_name", "totMin",
                "net_pts_games", "tTmPoss", "LEBRON", "WAR")
  require_columns(data, required, "LEBRON response")
  require_numeric(data$nba_id, "nba_id", minimum = 1, integer = TRUE)
  require_numeric(data$year, "year", minimum = 2010, integer = TRUE)
  for (field in c("totMin", "net_pts_games", "tTmPoss")) {
    require_numeric(data[[field]], field, minimum = 0, integer = field == "net_pts_games")
  }
  for (field in c("LEBRON", "WAR")) {
    # JSON null is a missing metric. It must never become a zero.
    if (is.logical(data[[field]]) && all(is.na(data[[field]]))) data[[field]] <- as.numeric(data[[field]])
    require_numeric(data[[field]], field, missing = TRUE)
  }
  if (!is.character(data$Season) || anyNA(data$Season) ||
      !setequal(unique(data$Season), seasons) ||
      any(data$year != as.integer(substr(data$Season, 1, 4)) + 1L)) {
    stop("Response seasons do not match requested seasons", call. = FALSE)
  }
  for (field in c("full_nm", "team_name")) {
    if (!is.character(data[[field]]) || anyNA(data[[field]]) || any(!nzchar(trimws(data[[field]])))) {
      stop("Missing player identity or team membership: ", field, call. = FALSE)
    }
  }
  if (anyDuplicated(paste(data$Season, data$nba_id))) stop("Duplicate player-season in LEBRON response", call. = FALSE)
  if (any(data$nba_id > .Machine$integer.max)) stop("NBA ID exceeds supported integer range", call. = FALSE)
  split_teams <- Map(canonical_team_codes, data$team_name, data$Season)
  players <- tibble::tibble(
    season = data$Season, nba_id = as.integer(data$nba_id),
    bref_id = as.character(data$bref_id), player_name = data$full_nm,
    source_team = data$team_name,
    teams = vapply(split_teams, function(x) paste(sort(x), collapse = "/"), character(1)),
    team_count = lengths(split_teams), minutes = data$totMin,
    games = as.integer(data$net_pts_games), provider_possessions = data$tTmPoss,
    played = data$totMin > 0 & data$net_pts_games > 0,
    lebron = data$LEBRON, war = data$WAR,
    lebron_status = ifelse(is.na(data$LEBRON), "missing", "observed"),
    war_status = ifelse(is.na(data$WAR), "missing", "observed")
  )
  players <- players[order(players$season, players$nba_id), ]
  teams <- tibble::tibble(
    season = rep(data$Season, lengths(split_teams)),
    nba_id = as.integer(rep(data$nba_id, lengths(split_teams))),
    team = unlist(split_teams, use.names = FALSE),
    membership_type = "season_membership_only"
  )
  teams <- teams[order(teams$season, teams$nba_id, teams$team), ]
  list(schema_version = LEBRON_SCHEMA_VERSION, players = players, teams = teams,
       data_origin = if (!is.null(parsed$fixture_notice)) "test_fixture" else "observed")
}

fanspo_transport <- function(request) {
  req <- httr2::request(LEBRON_SOURCE_URL)
  req <- httr2::req_user_agent(req, "basketball-econ private research pipeline")
  req <- httr2::req_body_raw(req, json_bytes(request), type = "application/json")
  req <- httr2::req_timeout(req, 45)
  response <- httr2::req_perform(req)
  httr2::resp_body_raw(response)
}

read_lebron_snapshot <- function(manifest_path, expected_seasons = NULL) {
  meta <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  required <- c("snapshot_schema", "source_url", "season_type", "requested_seasons", "request",
                "request_sha256", "response_sha256", "response_file", "retrieved_utc")
  require_columns(meta, required, "Snapshot manifest")
  if (!identical(meta$snapshot_schema, "lebron-snapshot-v1") ||
      !identical(meta$source_url, LEBRON_SOURCE_URL) ||
      !identical(meta$season_type, "regular") ||
      !identical(meta$request_sha256, sha256_bytes(json_bytes(meta$request))) ||
      !identical(json_bytes(meta$request), json_bytes(lebron_request(unlist(meta$requested_seasons))))) {
    stop("Snapshot manifest source/request mismatch", call. = FALSE)
  }
  if (!is.null(expected_seasons) && !identical(check_seasons(unlist(meta$requested_seasons)), check_seasons(expected_seasons))) {
    stop("Cached snapshot seasons do not match caller request", call. = FALSE)
  }
  if (!is.character(meta$response_sha256) || length(meta$response_sha256) != 1L ||
      !grepl("^[a-f0-9]{64}$", meta$response_sha256) ||
      !identical(basename(meta$response_file), meta$response_file) ||
      !endsWith(meta$response_file, paste0(meta$response_sha256, ".json"))) {
    stop("Invalid snapshot response reference", call. = FALSE)
  }
  path <- file.path(dirname(manifest_path), meta$response_file)
  if (!file.exists(path)) stop("Snapshot response file is missing", call. = FALSE)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  if (!identical(sha256_bytes(bytes), meta$response_sha256)) stop("Snapshot response checksum mismatch", call. = FALSE)
  result <- normalize_lebron(bytes, unlist(meta$requested_seasons))
  result$provenance <- meta
  result$manifest_path <- manifest_path
  result
}

acquire_lebron <- function(seasons, cache_dir = "cache/metrics", refresh = FALSE,
                           offline = FALSE, transport = fanspo_transport, as_of = Sys.Date()) {
  seasons <- check_seasons(seasons, as_of = as_of)
  if (refresh && offline) stop("Refresh and offline modes cannot be combined", call. = FALSE)
  prefix <- paste0("lebron-", paste(seasons, collapse = "__"), "-")
  pattern <- paste0("^", prefix, "[a-f0-9]{64}\\.manifest\\.json$")
  available <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  if (!refresh && length(available)) {
    dates <- vapply(available, function(path) {
      value <- jsonlite::fromJSON(path)$retrieved_utc
      if (!is.character(value) || length(value) != 1L || is.na(value)) stop("Invalid snapshot retrieval date", call. = FALSE)
      value
    }, character(1))
    return(read_lebron_snapshot(available[order(dates, available, decreasing = TRUE)[1]], expected_seasons = seasons))
  }
  if (offline) stop("No cached LEBRON snapshot for requested seasons", call. = FALSE)
  request <- lebron_request(seasons)
  bytes <- transport(request)
  # Validate before persisting an upstream error page or incomplete season response.
  normalize_lebron(bytes, seasons)
  hash <- sha256_bytes(bytes)
  name <- paste0(prefix, hash)
  manifest_path <- file.path(cache_dir, paste0(name, ".manifest.json"))
  if (file.exists(manifest_path)) return(read_lebron_snapshot(manifest_path, expected_seasons = seasons))
  meta <- list(snapshot_schema = "lebron-snapshot-v1", source_url = LEBRON_SOURCE_URL,
               requested_seasons = as.list(seasons), season_type = "regular",
               request = request, request_sha256 = sha256_bytes(json_bytes(request)),
               response_sha256 = hash, response_file = paste0(name, ".json"),
               retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
               provider_version = NULL,
               version_basis = "exact response SHA-256; provider does not supply a release identifier",
               publication_status = "unreviewed")
  write_new_bytes(bytes, file.path(cache_dir, meta$response_file))
  write_new_bytes(json_bytes(meta), manifest_path)
  read_lebron_snapshot(manifest_path, expected_seasons = seasons)
}
