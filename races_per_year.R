library(dplyr)
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

count.races <- function (performances, roster, year.of.interest) {
  races.by.athlete <- performances |>
    filter(
      (is.na(flag) | flag != "Relay")
      & year == year.of.interest
    ) |>
    group_by(athlete) |>
    tally()
  start.date <- as.Date(paste0(year.of.interest, "-01-01"))
  end.date <- as.Date(paste0(year.of.interest + 1, "-01-01"))
  roster |>
    filter(date_joined < end.date & (is.na(date_left) | date_left >= start.date)) |>
    transmute(
      athlete = name,
      from = as.Date(ifelse(date_joined< start.date, start.date, date_joined)),
      to = as.Date(ifelse(date_left >= end.date | is.na(date_left), end.date, date_left))
    ) |>
    group_by(athlete) |>
    summarise(from = min(from), to = max(to)) |>
    mutate(days = to - from, expansion_factor = 366 / as.numeric(to - from)) |>
    left_join(races.by.athlete, by = join_by(athlete)) |>
    mutate(n = ifelse(is.na(n), 0, n)) |>
    mutate(
      expanded_n = n * expansion_factor,
      membership_status = ifelse(
        to < end.date,
        "Former",
        "Current"
      )
    )
}

plot <- function (data, year) {
  data |>
    ggplot(aes(x = expanded_n, fill = factor(membership_status, levels = c("Former", "Current")))) +
    geom_histogram(boundary = 0) +
    scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
    labs(
      title = paste("Distribution of times raced in", year),
      x = "Number of races",
      y = "Count of teammates",
      fill = "Membership status"
    ) +
    theme(legend.position = "bottom")
}

main <- function (year = 2025, cache = FALSE) {
  performances <- get_performance_data(cache)
  roster <- fetch.roster(cache) |>
    clean_names()
  count.races(performances, roster, year) |>
    plot(year)
}
