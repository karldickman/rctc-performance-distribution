library(dplyr)
library(lubridate)

source("data.R")

rank_times_as_of <- function (data, as.of.date) {
  personal.rankings <- data |>
    select(!c(personal_rank, team_rank)) |>
    filter(date <= as.of.date) |>
    arrange(minutes) |>
    group_by(athlete, distance_label, discipline) |>
    mutate(personal_rank = min_rank(minutes)) |>
    ungroup()
  personal.bests <- personal.rankings |>
    filter(personal_rank == 1)
  not.personal.bests <- personal.rankings |>
    filter(personal_rank > 1)
  team.bests <- personal.bests |>
    group_by(distance_label, discipline, gender) |>
    mutate(team_rank = min_rank(minutes)) |>
    ungroup()
  bind_rows(team.bests, not.personal.bests)
}

times_previous_top_ten_beaten <- function (data) {
  last.years.ranks <- rank_times_as_of(data, as.Date("2024-12-31"))
  last.years.top.10 <- last.years.ranks |>
    filter(team_rank == 10) |>
    select(c(athlete, race, date, distance_label, discipline, gender, use_this_time, minutes))
  data |>
    filter(year(date) == 2025) |>
    inner_join(last.years.top.10, by = join_by(distance_label, discipline, gender, minutes < minutes)) |>
    arrange(distance_mi)
}

rank_events <- function (data) {
  data |>
    group_by(distance_label, discipline) |>
    tally() |>
    arrange(-n)
}

most_improved_top_10 <- function (data) {
  last.years.ranks <- rank_times_as_of(data, as.Date("2024-12-31")) |>
    filter(team_rank == 10) |>
    select(athlete, race, date, distance_label, discipline, gender, use_this_time, minutes)
  current.years.ranks <- rank_times_as_of(data, as.Date("2025-12-31")) |>
    filter(team_rank == 10) |>
    select(athlete, race, date, distance_label, discipline, gender, use_this_time, minutes)
  last.years.ranks |>
    inner_join(current.years.ranks, by = join_by(distance_label, discipline, gender)) |>
    mutate(proportional_improvement = minutes.y / minutes.x - 1) |>
    arrange(proportional_improvement)
}

filter_times_in_top_10_doc <- function (data) {
  distances.in.top.10.doc <- read_csv("distances_in_top_10_doc.csv", show_col_types = FALSE)
  data |>
    mutate(distance_label = ifelse(distance_label == "2k steeplechase (30\")", "2k steeplechase", distance_label)) |>
    mutate(distance_label = ifelse(distance_label %in% c("Half ironman", "Half Ironman"), "Ironman 70.3", distance_label)) |>
    inner_join(distances.in.top.10.doc, by = join_by(discipline, distance_label))
}

new_people_on_top_10 <- function (data) {
  data <- data |>
    filter_times_in_top_10_doc()
  last.years.ranks <- rank_times_as_of(data, as.Date("2024-12-31")) |>
    filter(team_rank <= 10) |>
    separate_rows(athlete, sep = ",") |>
    mutate(athlete = athlete |> trimws() |> str_replace_all("[()]", ""))
  current.years.ranks <- rank_times_as_of(data, as.Date("2025-12-31")) |>
    filter(team_rank <= 10) |>
    separate_rows(athlete, sep = ",") |>
    mutate(athlete = athlete |> trimws() |> str_replace_all("[()]", "")) |>
    filter(!(athlete %in% c("Andrew Fleming", "Dennis Doyle", "Mike Murawski", "Mike Toll")))
  current.years.ranks |>
    anti_join(last.years.ranks, by = join_by(athlete)) |>
    select(athlete) |>
    distinct() |>
    arrange(athlete)
}

top_tens_by_year <- function (data) {
  data |>
    filter_times_in_top_10_doc() |>
    rank_times_as_of(as.Date("2025-12-31")) |>
    filter(team_rank <= 10) |>
    mutate(year = year(date)) |>
    group_by(year) |>
    tally() |>
    mutate(cum = cumsum(n))
}

main <- function (cache = FALSE) {
  data <- get_performance_data(include_relay_legs = FALSE, cache = cache) |>
    mutate(gender = ifelse(!is.na(flag) & flag == "Relay", "Mixed", gender))
  #times_previous_top_ten_beaten(data)
  #most_improved_top_10(data)
  #new_people_on_top_10(data)
  top_tens_by_year(data)
}
