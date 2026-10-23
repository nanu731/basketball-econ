# Announced nominal NBA salary caps, not tax thresholds or forecasts.
# Values and primary references were independently checked in source research.
official_caps <- function() {
  end <- 2010:2026
  values <- c(57700000, 58044000, 58044000, 58044000, 58679000, 63065000,
              70000000, 94143000, 99093000, 101869000, 109140000, 109140000,
              112414000, 123655000, 136021000, 140588000, 154647000)
  paths <- c(
    "nba-salary-cap-for-2010-11-season-set-at-58-044-million/",
    "nba-salary-cap-for-2010-11-season-set-at-58-044-million/", "nba-cba-ratify/",
    "nba-salary-cap-2012-13-season/", "nba-salary-cap-2013-14-season/",
    "nba-salary-cap-2014-15-season/", "nba-salary-cap-2015-16-season/",
    "nba-salary-cap-2016-17-season/", "nba-salary-cap-2017-18-season/",
    "nba-salary-cap-2018-19-season/",
    "nba-salary-cap-for-2019-20-season-set-at-109-140-million/",
    "nba-nbpa-2020-21-season/",
    "nba-salary-cap-for-2021-22-season-set-at-112-414-million/",
    "nba-salary-cap-2022-23-season/",
    "nba-salary-cap-for-2023-24-season-set-at-136-021-million/",
    NA_character_, "nba-salary-cap-2025-26-season/")
  urls <- paste0("https://pr.nba.com/", paths)
  urls[16] <- "https://www.nba.com/news/nba-salary-cap-set-2024-25-season"
  tibble::tibble(season = sprintf("%d-%02d", end - 1L, end %% 100L),
                 season_end = end, cap_usd = values, cap_source_url = urls)
}
