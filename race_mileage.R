library(dplyr)
library(ggplot2)
library(readr)

source("data.R")

fetch_multisport_running_legs <- function (cache = FALSE) {
  cached <- "multi_sport_running_legs.csv"
  col.types <- "ccDccdcd"
  if (cache) {
    data <- read_csv(cached, col_types = col.types)
  } else {
    performances <- read_sheet(
      "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
      "Multi-sport running legs",
      col_types = col.types
    ) |>
      clean_names()
    write.csv(performances, cached, row.names = FALSE)
    performances
  }
}

main <- function (cache = FALSE) {
  # Fetch data
  performances <- get_performance_data(include_relay_legs = TRUE, cache = cache) |>
    filter(!(discipline %in% c("Duathlon", "Triathlon", "Skimo")))
  multi.sport <- fetch_multisport_running_legs(cache) |>
    mutate(year = year(date))
  performances <- bind_rows(performances, multi.sport)
  roster <- get_roster(cache)
  # Summarize
  current.year <- year(Sys.Date())
  start.of.current.year <- as.Date(paste0(current.year, "-01-01"))
  start.of.next.year <- as.Date(paste0(current.year + 1, "-01-01"))
  current.year.data <- roster |>
    mutate(date_left = coalesce(date_left, start.of.next.year), year = current.year) |>
    filter(date_joined < start.of.next.year & date_left >= start.of.current.year) |>
    rename(athlete = name) |>
    left_join(performances, by = join_by(athlete, year))
  mileage.data <- current.year.data |>
    group_by(athlete) |>
    summarise(distance_mi = sum(coalesce(distance_mi, 0))) |>
    arrange(-distance_mi)
  race.counts <- current.year.data |>
    select(athlete, race, date, distance_label, discipline) |>
    distinct() |>
    mutate(race_counter = as.numeric(!is.na(race))) |>
    group_by(athlete) |>
    summarise(races = sum(race_counter))
  data <- mileage.data |>
    left_join(race.counts, by = join_by(athlete))
  # Plot
  cat("Total miles: ", sum(data$distance_mi), "\n")
  data |>
    ggplot(aes(x = distance_mi, y = reorder(athlete, distance_mi))) +
    geom_col() +
    labs(
      x = "Cumulative race distance (mi)",
      y = "Athlete"
    )
}
