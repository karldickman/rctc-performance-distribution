library(dplyr)
library(googlesheets4)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)

fetch_performance_data <- function (cache = FALSE) {
  cached <- "Performances.csv"
  columns <- data.frame(
    name = c("Athlete", "Race", "Date", "Distance", "Discipline", "Gun Time", "Chip Time", "Achievement", "Gender", "Flag", "Age (reported)", "Age (calculated)", "Youngest", "Oldest", "Masters", "Age (Combined)", "Earliest BDay", "Latest BDay", "Results", "Use this time", "Personal Rank", "Team Rank", "Masters Personal Rank", "Masters Team Rank", "Year", "Kilometers", "", "As Of", "a date"),
    type = c("c",       "c",    "D",    "c",        "c",          "c",        "c",         "c",           "c",      "c",    "d",              "d",                "d",        "d",      "c",       "d",              "D",             "D",           "c",       "c",             "d",             "d",         "d",                     "d",                 "d",    "d"         , "c","c",     "c")
  )
  col_types <- paste(columns$type, collapse = "")
  if (cache) {
    data <- cached |>
      read_csv(col_types = col_types, show_col_types = FALSE)
    return(data)
  }
  performances <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
    "Performances",
    col_types = col_types
  ) |>
    clean_names() |>
    select(!c(x27, as_of, x2025_12_31))
  write.csv(performances, cached, row.names = FALSE)
  performances
}

fetch_distance_relay_legs <- function (cache = FALSE) {
  cached <- "Distance relays.csv"
  columns <- tibble(
    name = c("Athlete", "Race", "Date", "Discipline", "Leg", "Distance (mi)"),
    type = c("c",       "c",    "D",    "c",          "d",   "d")
  )
  col_types <- paste(columns$type, collapse = "")
  if (cache) {
    read_csv(cached, col_types = col_types, show_col_types = FALSE)
  } else {
    data <- read_sheet(
      "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
      "Distance relay legs",
      col_types = col_types
    ) |>
      clean_names()
    write.csv(data, cached, row.names = FALSE)
    data
  }
}

fetch_roster <- function (cache = FALSE) {
  file.path <- "roster.csv"
  if (cache & file.exists(file.path)) {
    return(read_csv(file.path, show_col_types = FALSE))
  }
  columns <- data.frame(
    name = c("Name", "Alternate Names", "Gender", "Birthday", "Earliest BDay", "Latest BDay", "BDay Range", "Date Joined", "Date Left", "Previous Date Joined", "Previous Date Left", "Class", "Years on team"),
    type = c("c",    "c",               "c",      "D",        "D",             "D",           "d",          "D",            "c",        "D",                    "D",                   "c",    "d"            )
  )
  data <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
    "Athletes",
    col_types = paste(columns$type, collapse = "")
  ) |>
    rename(`Date joined` = `Date Joined...8`, `Date left` = `Date Left...9`) |>
    clean_names()
  write.csv(data, file.path, row.names = FALSE)
  data
}

parse_finish_time <- function (finish.time) {
  if (is.na(finish.time)) {
    return(NA)
  }
  if (substr(finish.time, nchar(finish.time), nchar(finish.time)) == "g") {
    finish.time <- substr(finish.time, 1, nchar(finish.time) - 1)
  }
  if (str_count(finish.time, ":") == 0) {
    finish.time <- paste0("0:00:", finish.time)
  } else if (str_count(finish.time, ":") == 1) {
    finish.time <- paste0("0:", finish.time)
  }
  as.numeric(hms(finish.time), "mins")
}

process_performance_data <- function (data) {
  data |>
    filter(
      gender != "Exclude"
      & (is.na(flag) | flag != "Exclude")
      & !(use_this_time %in% c("Exclude", "DNF", "Not found", "TBD"))
    ) |>
    mutate(
      gun_time = ifelse(gun_time == "#N/A", NA, gun_time),
      chip_time = ifelse(chip_time == "#N/A", NA, chip_time),
      finish_time = ifelse(
        str_count(distance, " hr") > 0,
        paste0(str_replace(distance, " hr", ""), ":00:00"),
        ifelse(
          distance == "Run til you drop",
          paste0(str_replace(gun_time, " laps", ""), ":00:00"),
          use_this_time
        )
      ),
      distance = str_replace(distance, " k", "k"),
      minutes = sapply(finish_time, parse_finish_time),
      kilometers = ifelse(
        str_count(distance, " hr") > 0 | distance == "Run til you drop",
        suppressWarnings(as.numeric(str_replace(use_this_time, " mi", ""))) * 1.609334,
        kilometers
      ),
      distance_mi = kilometers / 1.609334
    ) |>
    rename(distance_km = kilometers, distance_label = distance)
}

process_distance_relay_legs <- function (data) {
  data |>
    mutate(
      distance_km = distance_mi * 1.609334,
      year = year(date),
      flag = "Relay",
      distance_label = "Distance relay"
    )
}

explode_relay_legs <- function (data) {
  non.relays <- data |>
    filter(is.na(flag) | flag != "Relay")
  legs <- tibble(
    distance_label = c(
      rep("4 x 100 m relay", 4),
      rep("4 x 200 m relay", 4),
      rep("4 x 400 m relay", 4),
      rep("4 x 800 m relay", 4),
      rep("Sprint medley relay", 4),
      rep("Swedish relay", 4),
      rep("Distance medley relay", 4)
    ),
    leg = rep(1:4, 7),
    distance_km = c(
      rep(0.1, 4),
      rep(0.2, 4),
      rep(0.4, 4),
      rep(0.8, 4),
      0.1, 0.1, 0.2, 0.4,
      0.1, 0.2, 0.3, 0.4,
      1.2, 0.4, 0.8, 1.6
    )
  ) |>
    mutate(distance_mi = distance_km / 1.609334)
  relays <- data |>
    filter(
      flag == "Relay"
      & !(athlete %in% c("(members and order unknown)"))
      & tolower(distance_label) != "distance relay" # Fetch from spreadsheet instead
    ) |>
    select(c(athlete, race, date, distance_label, discipline)) |>
    mutate(team = athlete) |>
    separate_rows(athlete, sep = ",") |>
    mutate(athlete = trimws(athlete)) |>
    group_by(team, race, date, distance_label, discipline) |>
    mutate(leg = row_number()) |>
    ungroup() |>
    mutate(leg = ifelse(
      tolower(distance_label) %in% c("10 mi relay"),
      NA,
      leg
    )) |>
    filter(!(athlete %in% c("?", "Leg 1", "Leg 2", "Leg 3", "Leg 4"))) |>
    left_join(legs, by = join_by(distance_label, leg))
  order.unknown.after <- relays |>
    filter(str_starts(athlete, "\\(order\\?\\)")) |>
    select(team, race, date, distance_label, discipline, leg) |>
    inner_join(relays, by = join_by(
      team == team,
      race == race,
      date == date,
      distance_label == distance_label,
      discipline == discipline,
      leg <= leg
    )) |>
    select(!c(leg.x, leg.y, distance_mi, distance_km))
  order.unknown <- relays |>
    inner_join(order.unknown.after, by = join_by(
      team == team,
      athlete == athlete,
      race == race,
      date == date,
      distance_label == distance_label,
      discipline == discipline,
    )) |>
    mutate(athlete = athlete |> str_replace("\\(order\\?\\)", "") |> trimws()) |>
    select(!leg)
  order.known <- relays |>
    anti_join(order.unknown.after, by = join_by(
      team == team,
      athlete == athlete,
      race == race,
      date == date,
      distance_label == distance_label,
      discipline == discipline,
    ))
  relays <- bind_rows(order.known, order.unknown) |>
    mutate(flag = "Relay", year = year(date))
  bind_rows(non.relays, relays)
}

process_roster <- function (data) {
  data <- data |>
    filter(!is.na(date_joined)) |>
    mutate(date_left = as.Date(ifelse(date_left == "Current Member", NA, date_left)))
  first.joined <- data |>
    select(name, date_joined = date_joined_10, date_left = date_left_11) |>
    filter(!is.na(date_joined))
  data |>
    select(c(name, date_joined, date_left)) |>
    bind_rows(first.joined)
}

get_distance_relay_legs <- function (cache = FALSE) {
  fetch_distance_relay_legs(cache) |>
    process_distance_relay_legs()
}

get_performance_data <- function (include_relay_legs = FALSE, cache = FALSE) {
  performances <- fetch_performance_data(cache) |>
    process_performance_data()
  if (!include_relay_legs) {
    performances
  } else {
    distance_relay_legs <- get_distance_relay_legs(cache)
    performances |>
      explode_relay_legs() |>
      bind_rows(distance_relay_legs)
  }
}

get_roster <- function (cache = FALSE) {
  fetch_roster(cache) |>
    process_roster()
}
