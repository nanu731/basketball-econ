FANSPO_SALARY_DEFINITION <- "fanspo_historical_reported_salary_v1"

earnings_url <- function(nba_id, name) {
  id <- as.character(nba_id)
  if (length(id) != 1L || is.na(id) || !grepl("^[1-9][0-9]*$", id)) stop("Invalid NBA player ID", call. = FALSE)
  slug <- iconv(name, to = "ASCII//TRANSLIT")
  if (length(slug) != 1L || is.na(slug)) stop("Cannot form a public player document URL", call. = FALSE)
  slug <- gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-", tolower(slug)))
  if (!nzchar(slug)) stop("Missing player name for public document URL", call. = FALSE)
  paste0("https://fanspo.com/nba/players/", slug, "/", id, "/earnings")
}

normalize_earnings <- function(bytes, nba_id) {
  if (!is.raw(bytes)) stop("Earnings parser requires original HTML bytes", call. = FALSE)
  id <- as.character(nba_id)
  if (length(id) != 1L || is.na(id) || !grepl("^[1-9][0-9]*$", id)) stop("Invalid NBA player ID", call. = FALSE)
  html <- rawToChar(bytes)
  pattern <- "(?s)<script\\b(?=[^>]*\\bid=[\"']__NEXT_DATA__[\"'])[^>]*>(.*?)</script\\s*>"
  matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))[[1]]
  if (length(matches) != 1L) stop("Public document has no unique __NEXT_DATA__ payload", call. = FALSE)
  payload <- sub(pattern, "\\1", matches, perl = TRUE)
  next_data <- tryCatch(jsonlite::fromJSON(payload, simplifyVector = FALSE), error = function(e) {
    stop("Invalid earnings document JSON", call. = FALSE)
  })
  state <- next_data$props$pageProps$apolloState
  player <- state[[paste0("nba_Player:", id)]]
  if (is.null(player) || !identical(as.character(player$id), id)) stop("Document does not identify requested NBA player", call. = FALSE)
  if (!is.character(player$fullName) || length(player$fullName) != 1L || !nzchar(player$fullName)) {
    stop("Missing earnings player name", call. = FALSE)
  }
  empty <- tibble::tibble(season = character(), nba_id = integer(), salary_record_id = character(),
                          source_player_name = character(), source_team_id = character(), source_team_name = character(),
                          from_year = integer(), to_year = integer(), source_league = character(),
                          source_salary_raw = character(), salary_reported_usd = double(), source_salary_status = character())
  refs <- vapply(player$salaries, function(x) {
    ref <- x[["__ref"]]
    if (!is.character(ref) || length(ref) != 1L || !startsWith(ref, "nba_PlayerSalary:")) stop("Invalid historical salary reference", call. = FALSE)
    ref
  }, character(1))
  if (anyDuplicated(refs)) stop("Duplicate parent salary reference", call. = FALSE)
  records <- lapply(refs, function(ref) {
    row <- state[[ref]]
    if (is.null(row) || !identical(row$`__typename`, "nba_PlayerSalary") ||
        !identical(paste0("nba_PlayerSalary:", row$id), ref)) stop("Unresolved historical salary reference", call. = FALSE)
    require_columns(row, c("id", "teamId", "teamName", "fromYear", "toYear", "league", "salary"), "Historical salary")
    from <- row$fromYear; to <- row$toYear
    if (!is.numeric(from) || length(from) != 1L || is.na(from) || from != floor(from) ||
        !is.numeric(to) || length(to) != 1L || is.na(to) || to != from + 1L) stop("Invalid historical salary year bounds", call. = FALSE)
    raw_amount <- row$salary
    amount <- if (is.null(raw_amount)) NA_real_ else suppressWarnings(as.numeric(raw_amount))
    if (length(amount) != 1L) stop("Invalid historical salary amount shape", call. = FALSE)
    league <- if (is.null(row$league)) NA_character_ else as.character(row$league)
    if (length(league) != 1L) stop("Invalid salary league", call. = FALSE)
    status <- if (is.na(league) || toupper(league) != "NBA") "non_nba_or_unknown_league" else
      if (is.null(raw_amount)) "missing_amount" else if (is.na(amount) || !is.finite(amount)) "invalid_amount" else
        if (amount < 0) "invalid_negative" else if (amount == 0) "unsupported_zero" else "reported_positive"
    tibble::tibble(season = sprintf("%d-%02d", from, to %% 100L), nba_id = as.integer(id),
                   salary_record_id = as.character(row$id), source_player_name = player$fullName,
                   source_team_id = if (is.null(row$teamId)) NA_character_ else as.character(row$teamId),
                   source_team_name = if (is.null(row$teamName)) NA_character_ else as.character(row$teamName),
                   from_year = as.integer(from), to_year = as.integer(to), source_league = league,
                   source_salary_raw = if (is.null(raw_amount)) NA_character_ else as.character(raw_amount),
                   salary_reported_usd = if (status == "reported_positive") amount else NA_real_,
                   source_salary_status = status)
  })
  records <- if (length(records)) dplyr::bind_rows(records) else empty
  records <- records[order(records$season, records$salary_record_id), ]
  list(nba_id = id, player_name = player$fullName, records = records,
       data_origin = if (!is.null(next_data$fixture_notice)) "test_fixture" else "observed")
}

fanspo_earnings_transport <- function(url) {
  # Ordinary documented public document request. No browser impersonation,
  # access-control workaround, authentication, or retry of blocking responses.
  response <- httr2::req_perform(httr2::req_timeout(httr2::request(url), 30))
  list(bytes = httr2::resp_body_raw(response), response_url = httr2::resp_url(response))
}

read_earnings_snapshot <- function(manifest_path, expected_nba_id = NULL) {
  meta <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  require_columns(meta, c("snapshot_schema", "nba_id", "source_url", "response_file", "response_sha256", "retrieved_utc"), "Earnings manifest")
  if (!identical(meta$snapshot_schema, "earnings-snapshot-v1") ||
      !is.character(meta$nba_id) || length(meta$nba_id) != 1L || !grepl("^[1-9][0-9]*$", meta$nba_id) ||
      !grepl(paste0("^https://fanspo[.]com/nba/players/[^/]+/", meta$nba_id, "(/earnings)?/?$"), meta$source_url) ||
      !grepl("^[a-f0-9]{64}$", meta$response_sha256) ||
      !identical(basename(meta$response_file), meta$response_file)) stop("Invalid earnings snapshot manifest", call. = FALSE)
  if (!is.null(expected_nba_id) && !identical(meta$nba_id, as.character(expected_nba_id))) {
    stop("Cached earnings player does not match caller request", call. = FALSE)
  }
  path <- file.path(dirname(manifest_path), meta$response_file)
  if (!file.exists(path)) stop("Missing earnings snapshot body", call. = FALSE)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  if (!identical(sha256_bytes(bytes), meta$response_sha256)) stop("Earnings snapshot checksum mismatch", call. = FALSE)
  result <- normalize_earnings(bytes, meta$nba_id)
  result$provenance <- meta
  result$manifest_path <- manifest_path
  result
}

acquire_earnings <- function(nba_id, player_name, cache_dir = "cache/earnings", offline = FALSE,
                             refresh = FALSE, transport = fanspo_earnings_transport) {
  url <- earnings_url(nba_id, player_name)
  if (offline && refresh) stop("Cannot refresh earnings offline", call. = FALSE)
  prefix <- paste0("earnings-", nba_id, "-")
  paths <- list.files(cache_dir, pattern = paste0("^", prefix, "[a-f0-9]{64}\\.manifest\\.json$"), full.names = TRUE)
  if (!refresh && length(paths)) {
    dates <- vapply(paths, function(path) jsonlite::fromJSON(path)$retrieved_utc, character(1))
    return(read_earnings_snapshot(paths[order(dates, paths, decreasing = TRUE)[1]], expected_nba_id = nba_id))
  }
  if (offline) stop("No cached earnings document for NBA ID ", nba_id, call. = FALSE)
  response <- transport(url)
  normalize_earnings(response$bytes, nba_id)
  hash <- sha256_bytes(response$bytes)
  manifest_path <- file.path(cache_dir, paste0(prefix, hash, ".manifest.json"))
  if (file.exists(manifest_path)) return(read_earnings_snapshot(manifest_path, expected_nba_id = nba_id))
  meta <- list(snapshot_schema = "earnings-snapshot-v1", nba_id = as.character(nba_id),
               source_url = response$response_url, requested_url = url,
               response_file = paste0(prefix, hash, ".html"), response_sha256 = hash,
               retrieved_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
               provider_version = NULL, publication_status = "unreviewed")
  write_new_bytes(response$bytes, file.path(cache_dir, meta$response_file))
  write_new_bytes(json_bytes(meta), manifest_path)
  read_earnings_snapshot(manifest_path, expected_nba_id = nba_id)
}

earnings_salary_import <- function(snapshots, cohort, seasons) {
  seasons <- check_seasons(seasons)
  require_columns(cohort, c("season", "nba_id"), "Metric cohort")
  cohort <- unique(cohort[cohort$season %in% seasons, c("season", "nba_id")])
  ids <- vapply(snapshots, `[[`, character(1), "nba_id")
  if (anyDuplicated(ids)) stop("Duplicate earnings snapshots for a player", call. = FALSE)
  history <- dplyr::bind_rows(lapply(snapshots, `[[`, "records"))
  rows <- lapply(seq_len(nrow(cohort)), function(i) {
    id <- cohort$nba_id[i]; season <- cohort$season[i]
    h <- if (nrow(history)) history[history$nba_id == id & history$season == season, ] else history
    status <- if (!as.character(id) %in% ids) "acquisition_unavailable" else if (!nrow(h)) "missing_historical_season" else
      if (nrow(h) > 1L) "multiple_source_records" else h$source_salary_status[1]
    tibble::tibble(season = season, nba_id = as.integer(id), source_player_id = as.character(id),
                   salary_reported_usd = if (status == "reported_positive") h$salary_reported_usd[1] else NA_real_,
                   contract_type = "unknown", identity_status = "mapped", source_id = "fanspo_earnings",
                   salary_definition_id = FANSPO_SALARY_DEFINITION, source_salary_status = status)
  })
  records <- dplyr::bind_rows(rows)
  provenance <- list(source_id = "fanspo_earnings", source_url = "https://fanspo.com/nba",
                     salary_definition_id = FANSPO_SALARY_DEFINITION,
                     salary_definition = "Parent-linked historical salary records; reported nominal USD, not independently verified cash or team obligations",
                     source_label = "Cash by season", display_label = "Fanspo-reported season salary",
                     currency = "USD", money_unit = "nominal_dollars", season_type = "regular",
                     private_use_basis = "Authorized private analysis of the owner's normal public earnings documents",
                     publication_status = "unreviewed",
                     snapshots = lapply(snapshots, `[[`, "provenance"))
  list(records = records, source_records = history, provenance = provenance,
       data_origin = if (any(vapply(snapshots, function(x) identical(x$data_origin, "test_fixture"), logical(1)))) "test_fixture" else "observed")
}
