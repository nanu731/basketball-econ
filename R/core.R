# Shared validation and deterministic serialization. No package installation occurs.
require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(label, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
}

check_seasons <- function(seasons, as_of = NULL) {
  if (!is.character(seasons) || !length(seasons) || anyNA(seasons) ||
      any(!grepl("^20[0-9]{2}-[0-9]{2}$", seasons))) stop("Invalid season labels", call. = FALSE)
  starts <- as.integer(substr(seasons, 1, 4))
  if (any(substr(seasons, 6, 7) != sprintf("%02d", (starts + 1L) %% 100L))) {
    stop("Season ending year must follow starting year", call. = FALSE)
  }
  if (!is.null(as_of)) {
    # A conservative calendar gate, not evidence that the provider's data are complete.
    if (length(as_of) != 1L || is.na(as.Date(as_of))) stop("Invalid as_of date", call. = FALSE)
    if (any(as.Date(sprintf("%d-07-01", starts + 1L)) > as.Date(as_of))) {
      stop("Season has not passed the conservative July 1 completion gate", call. = FALSE)
    }
  }
  sort(unique(seasons))
}

require_numeric <- function(x, label, missing = FALSE, minimum = -Inf, integer = FALSE) {
  if (!is.numeric(x) || any(!is.na(x) & !is.finite(x)) ||
      (!missing && anyNA(x)) || any(x < minimum, na.rm = TRUE) ||
      (integer && any(x != floor(x), na.rm = TRUE))) stop("Invalid numeric field: ", label, call. = FALSE)
  invisible(x)
}

sha256_bytes <- function(bytes) {
  if (!is.raw(bytes)) stop("Expected raw bytes", call. = FALSE)
  if (!exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
    stop("This pipeline requires R with tools::sha256sum (available in the project's R 4.6)", call. = FALSE)
  }
  path <- tempfile("basketball-hash-")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::sha256sum(path))
}

json_bytes <- function(value) {
  charToRaw(enc2utf8(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA)))
}

write_new_bytes <- function(bytes, path) {
  if (file.exists(path)) stop("Refusing to overwrite existing file: ", path, call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(bytes, path)
  invisible(path)
}
