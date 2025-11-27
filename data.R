library(dplyr)
library(googlesheets4)
library(janitor)
library(lubridate)
library(readr)
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

get_performance_data <- function (cache = FALSE) {
  fetch_performance_data(cache) |>
    process_performance_data()
}
