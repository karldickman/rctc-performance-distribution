library(dplyr)
library(ggplot2)
library(janitor)
library(readr)

source("data.R")

fetch.roster <- function (cache = FALSE) {
  file.path <- "roster.csv"
  if (cache & file.exists(file.path)) {
    return(read_csv(file.path, show_col_types = FALSE))
  }
  columns <- data.frame(
    name = c("Name", "Alternate Names", "Gender", "Birthday", "Earliest BDay", "Latest BDay", "BDay Range", "Date Joined", "Date Left", "Previous Date Joined", "Previous Date Left", "Class", "Years on team"),
    type = c("c",    "c",               "c",      "D",        "D",             "D",           "d",          "D",            "D",        "D",                    "D",                   "c",    "d"            )
  )
  data <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
    "Athletes",
    col_types = paste(columns$type, collapse = "")
  ) |>
    rename(`Date joined` = `Date Joined...8`, `Date left` = `Date Left...9`)
  write.csv(data, file.path, row.names = FALSE)
  data
}

count.races <- function (performances, roster) {
  current.year <- year(Sys.Date())
  years <- tibble(year = 2017:current.year) |>
    mutate(
      start_date = as.Date(paste0(year, "-01-01")),
      end_date = as.Date(paste0(year + 1, "-01-01"))
    )
  races.by.athlete <- performances |>
    group_by(athlete, year) |>
    tally()
  end.of.next.year <- as.Date(paste0(current.year + 1, "-01-01"))
  roster |>
    mutate(date_left = coalesce(date_left, end.of.next.year)) |>
    inner_join(years, by = join_by(date_joined < end_date, date_left >= start_date)) |>
    mutate(
      from = as.Date(ifelse(date_joined < start_date, start_date, date_joined)),
      to = as.Date(ifelse(date_left >= end_date, end_date, date_left))
    ) |>
    rename(athlete = name) |>
    group_by(athlete, year) |>
    summarise(from = min(from), to = max(to), .groups = "drop") |>
    inner_join(years, by = join_by(year)) |>
    mutate(
      days = as.numeric(to - from),
      expansion_factor = as.numeric(end_date - start_date) / days
    ) |>
    filter(days > 0) |>
    left_join(races.by.athlete, by = join_by(athlete, year)) |>
    mutate(
      n = coalesce(n, 0),
      expanded_n = n * expansion_factor,
      membership_status = ifelse(
        to < end_date,
        "Former",
        "Current"
      )
    )
}

plot_races_per_year <- function (data) {
  data |>
    ggplot(aes(x = expanded_n, fill = factor(membership_status, levels = c("Former", "Current")))) +
    geom_histogram(boundary = 0) +
    facet_wrap(~ year) +
    scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
    labs(
      title = "Distribution of times raced per year",
      x = "Number of races",
      y = "Count of teammates",
      fill = "Membership status"
    ) +
    theme(legend.position = "bottom")
}

main <- function (cache = FALSE) {
  performances <- get_performance_data(cache) |>
    explode_relay_legs()
  roster <- fetch.roster(cache) |>
    clean_names()
  count.races(performances, roster) |>
    plot_races_per_year()
}
