relationship_diagnostic_table <- function(analysis) {
  dplyr::bind_rows(lapply(names(analysis$models), function(metric) {
    model <- analysis$models[[metric]]
    fit <- model$fit
    rows <- tibble::tibble(metric = metric, nba_id = fit$nba_ids,
      salary_cap_share = fit$data$x, fitted = fit$expected, residual = fit$residual,
      absolute_residual = abs(fit$residual), leverage = fit$diagnostics$leverage)
    rows$salary_group <- pmin(5L, pmax(1L, ceiling(rank(rows$salary_cap_share, ties.method = "average") / nrow(rows) * 5)))
    rows
  }))
}

write_private_diagnostics <- function(analysis, output_dir = "outputs/private/diagnostics") {
  if (isTRUE(analysis$pilot_only)) stop("Timing pilots do not produce substantive diagnostics", call. = FALSE)
  data <- relationship_diagnostic_table(analysis)
  summary <- data |>
    dplyr::group_by(.data$metric, .data$salary_group) |>
    dplyr::summarise(players = dplyr::n(), minimum_cap_share = min(.data$salary_cap_share),
      maximum_cap_share = max(.data$salary_cap_share), mean_residual = mean(.data$residual),
      residual_variance = stats::var(.data$residual), median_absolute_residual = stats::median(.data$absolute_residual),
      maximum_leverage = max(.data$leverage), .groups = "drop")
  key <- substr(sha256_bytes(json_bytes(list(data = data, summary = summary))), 1, 20)
  directory <- file.path(output_dir, paste0(analysis$window$end_season_year, "-", analysis$window$window_length, "-", key))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  csv_path <- file.path(directory, "salary-group-diagnostics.csv")
  if (!file.exists(csv_path)) readr::write_csv(summary, csv_path)
  plot_paths <- character()
  for (metric in names(analysis$models)) {
    rows <- data[data$metric == metric, ]
    unit <- if (metric == "lebron") "LEBRON points per 100 possessions" else "LEBRON WAR"
    residual <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$fitted, y = .data$residual)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#9C3D1E") +
      ggplot2::geom_point(alpha = .55, size = 1.3, colour = "#14532D") +
      ggplot2::labs(x = paste("Fitted", unit), y = "Observed minus fitted",
        title = "Residual diagnostic", subtitle = paste(analysis$window$end_season, "window", analysis$window$window_length, metric),
        caption = "Diagnostic of the fixed eligible matched sample. Provider scores are treated as fixed; this is not player uncertainty.") +
      ggplot2::theme_minimal(base_size = 11)
    spread <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$salary_cap_share * 100, y = .data$absolute_residual)) +
      ggplot2::geom_point(alpha = .55, size = 1.3, colour = "#14532D") +
      ggplot2::labs(x = "Salary as percent of season cap", y = paste("Absolute residual in", unit),
        title = "Salary-dependent spread diagnostic", subtitle = "Inspect alongside the salary-group variance table; no extra fitted benchmark is added.") +
      ggplot2::theme_minimal(base_size = 11)
    for (name in c("residual", "spread")) {
      path <- file.path(directory, paste0(metric, "-", name, ".png"))
      if (!file.exists(path)) ggplot2::ggsave(path, if (name == "residual") residual else spread,
        device = grDevices::png, width = 7, height = 4.5, units = "in", dpi = 180)
      plot_paths <- c(plot_paths, path)
    }
  }
  list(directory = directory, summary = csv_path, plots = plot_paths)
}
