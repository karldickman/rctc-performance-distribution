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

count.races.in.year <- function (performances, roster, year.of.interest) {
  races.by.athlete <- performances |>
    filter(year == year.of.interest) |>
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
      ),
      year = year.of.interest
    )
}

count.races <- function (data, roster) {
  count.2017 <- count.races.in.year(data, roster, 2017)
  count.2018 <- count.races.in.year(data, roster, 2018)
  count.2019 <- count.races.in.year(data, roster, 2019)
  count.2020 <- count.races.in.year(data, roster, 2020)
  count.2021 <- count.races.in.year(data, roster, 2021)
  count.2022 <- count.races.in.year(data, roster, 2022)
  count.2023 <- count.races.in.year(data, roster, 2023)
  count.2024 <- count.races.in.year(data, roster, 2024)
  count.2025 <- count.races.in.year(data, roster, 2025)
  bind_rows(
    count.2017,
    count.2018,
    count.2019,
    count.2020,
    count.2021,
    count.2022,
    count.2023,
    count.2024,
    count.2025
  )
}

plot <- function (data) {
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
    plot()
}
