# Import an explicitly documented annual salary file; never infer pay from metric
# value fields, cap hits, player names, or full-year rates for partial contracts.
read_salary_import <- function(csv_path, metadata_path) {
  meta <- jsonlite::fromJSON(metadata_path, simplifyVector = FALSE)
  required_meta <- c("source_id", "source_url", "salary_definition_id", "salary_definition",
                     "currency", "money_unit", "season_type", "retrieved_utc",
                     "private_use_basis", "publication_status")
  require_columns(meta, required_meta, "Salary metadata")
  for (field in required_meta) {
    if (!is.character(meta[[field]]) || length(meta[[field]]) != 1L ||
        is.na(meta[[field]]) || !nzchar(trimws(meta[[field]]))) stop("Invalid salary metadata: ", field, call. = FALSE)
  }
  if (meta$currency != "USD" || meta$money_unit != "nominal_dollars" || meta$season_type != "regular") {
    stop("Salary import must explicitly use nominal USD for regular seasons", call. = FALSE)
  }
  if (!grepl("^https://", meta$source_url) ||
      !grepl("^[A-Za-z0-9_-]+$", meta$source_id) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", meta$retrieved_utc) ||
      !meta$publication_status %in% c("unreviewed", "approved_for_release", "test_only")) {
    stop("Invalid salary source or publication metadata", call. = FALSE)
  }
  if (meta$publication_status == "approved_for_release" &&
      (is.null(meta$publication_basis) || !is.character(meta$publication_basis) ||
       length(meta$publication_basis) != 1L || is.na(meta$publication_basis) || !nzchar(meta$publication_basis))) {
    stop("Approved salary release requires a documented publication basis", call. = FALSE)
  }
  raw <- readBin(csv_path, "raw", n = file.info(csv_path)$size)
  data <- readr::read_csv(csv_path, col_types = readr::cols(.default = readr::col_character()),
                          na = c("", "NA"), show_col_types = FALSE, progress = FALSE,
                          name_repair = "minimal")
  if (nrow(readr::problems(data))) stop("Malformed salary CSV", call. = FALSE)
  if (anyDuplicated(names(data))) stop("Duplicate salary CSV column", call. = FALSE)
  require_columns(data, c("season", "nba_id", "source_player_id", "salary_reported_usd", "contract_type"), "Salary CSV")
  if (!nrow(data)) stop("Salary CSV has no records", call. = FALSE)
  check_seasons(data$season)
  if (anyNA(data$source_player_id) || any(!nzchar(data$source_player_id))) stop("Missing salary source player ID", call. = FALSE)
  if (anyDuplicated(paste(data$season, data$source_player_id))) stop("Duplicate annual salary source record", call. = FALSE)
  ids <- data$nba_id
  if (any(!is.na(ids) & !grepl("^[1-9][0-9]*$", ids))) stop("Invalid NBA ID in salary import", call. = FALSE)
  numeric_ids <- suppressWarnings(as.integer(ids))
  if (any(!is.na(ids) & is.na(numeric_ids))) stop("NBA ID exceeds supported range", call. = FALSE)
  if (anyDuplicated(paste(data$season[!is.na(ids)], ids[!is.na(ids)]))) stop("Duplicate salary player-season within source", call. = FALSE)
  amounts <- data$salary_reported_usd
  if (any(!is.na(amounts) & !grepl("^[0-9]+(\\.[0-9]{1,2})?$", amounts))) {
    stop("Salary amounts must be nonnegative plain USD amounts with at most two decimals; no unit guessing", call. = FALSE)
  }
  amounts <- as.numeric(amounts)
  require_numeric(amounts, "salary_reported_usd", missing = TRUE, minimum = 0)
  contract <- data$contract_type
  contract[is.na(contract) | !nzchar(contract)] <- "unknown"
  records <- tibble::tibble(season = data$season, nba_id = numeric_ids,
                             source_player_id = data$source_player_id,
                             salary_reported_usd = amounts, contract_type = contract,
                             identity_status = ifelse(is.na(numeric_ids), "unmapped", "mapped"),
                             source_id = meta$source_id, salary_definition_id = meta$salary_definition_id)
  records <- records[order(records$season, records$nba_id, records$source_player_id, na.last = TRUE), ]
  meta$csv_sha256 <- sha256_bytes(raw)
  meta$metadata_sha256 <- sha256_bytes(readBin(metadata_path, "raw", n = file.info(metadata_path)$size))
  list(records = records, provenance = meta)
}

reconcile_salary_imports <- function(imports = list(), definition_id = NULL) {
  empty <- tibble::tibble(season = character(), nba_id = integer(), salary_reported_usd = double(),
                          salary_status = character(), reported_sources = integer(),
                          candidate_sources = integer(), contract_types = character())
  if (!length(imports)) return(list(players = empty, records = tibble::tibble(),
                                    unmapped = tibble::tibble(), provenance = list(), definition_id = NULL))
  if (!is.character(definition_id) || length(definition_id) != 1L || is.na(definition_id) || !nzchar(definition_id)) {
    stop("Select and document the salary definition before reconciliation", call. = FALSE)
  }
  source_ids <- vapply(imports, function(x) x$provenance$source_id, character(1))
  if (anyDuplicated(source_ids)) stop("Duplicate salary source imports", call. = FALSE)
  if (any(vapply(imports, function(x) x$provenance$salary_definition_id != definition_id, logical(1)))) {
    stop("Cannot reconcile different salary definitions", call. = FALSE)
  }
  records <- dplyr::bind_rows(lapply(imports, `[[`, "records"))
  if (!"source_salary_status" %in% names(records)) {
    records$source_salary_status <- ifelse(is.na(records$salary_reported_usd), "missing_amount", "reported_amount")
  }
  unmapped <- records[is.na(records$nba_id), ]
  mapped <- records[!is.na(records$nba_id), ]
  groups <- split(seq_len(nrow(mapped)), paste(mapped$season, mapped$nba_id))
  players <- lapply(groups, function(indices) {
    rows <- mapped[indices, ]
    values <- unique(rows$salary_reported_usd[!is.na(rows$salary_reported_usd)])
    reported <- sum(!is.na(rows$salary_reported_usd))
    reasons <- sort(unique(rows$source_salary_status))
    unresolved <- c("multiple_source_records", "unsupported_zero", "invalid_negative", "invalid_amount",
                    "non_nba_or_unknown_league", "acquisition_unavailable", "missing_historical_season")
    reason <- unresolved[unresolved %in% reasons]
    status <- if (!length(values)) { if (length(reason)) reason[1] else "missing_salary" } else
      if (length(values) > 1L) "conflict" else if (reported > 1L) "agreed" else "single_source"
    tibble::tibble(season = rows$season[1], nba_id = rows$nba_id[1],
                   salary_reported_usd = if (length(values) == 1L) values else NA_real_,
                   salary_status = status, reported_sources = reported,
                   candidate_sources = nrow(rows), contract_types = paste(sort(unique(rows$contract_type)), collapse = "/"))
  })
  players <- if (length(players)) dplyr::bind_rows(players) else empty
  players <- players[order(players$season, players$nba_id), ]
  list(players = players, records = records, unmapped = unmapped,
       provenance = lapply(imports, `[[`, "provenance"), definition_id = definition_id)
}
