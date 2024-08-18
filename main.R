library(dplyr)
library(ggplot2)
library(googlesheets4)
library(lubridate)
library(stringr)

parse.chip.time <- function (chip.time) {
  if (str_count(chip.time, ":") == 1) {
    chip.time <- paste0("0:", chip.time)
  }
  as.numeric(hms(chip.time), "mins")
}

required.race.distances <- c("5 k", "10 k", "Half marathon", "Marathon")

time.standard <- tibble(
  distance = factor(required.race.distances, levels = required.race.distances),
  standard = c(25 + 1 / 60, 52 + 10 / 60, 60 + 55, 4 * 60)
)

conversions <- tibble(
  distance = c(required.race.distances, "3 mi", "Quarter marathon"),
  to = c(required.race.distances, "5 k", "10 k"),
  conversion.factor = c(rep(1, 4), 5 / (3 * 1.609334), 10 / ((26+(385*3/5280)) / 4 * 1.609334))
)

minutes.as.POSIXct <- function (minutes) {
  as.POSIXct(minutes * 60, origin = "1970-01-01", tz = "UTC")
}

plot <- function (finish.times, time.standard, year) {
  time.standard <- mutate(time.standard, standard = minutes.as.POSIXct(standard))
  finish.times %>%
    mutate(minutes = minutes.as.POSIXct(minutes)) %>%
    ggplot(aes(x = minutes)) +
    facet_wrap(vars(distance), scales = "free") +
    geom_histogram() +
    geom_vline(data = time.standard, aes(xintercept = standard)) +
    scale_x_datetime(date_labels = "%-H:%M") +
    ggtitle(paste("Race results in", year), subtitle = "Road race results for all Rose City athletes")
}

main <- function (argv = c()) {
  current.year = 2023
  columns <- data.frame(
    name = c("Athlete", "Race", "Date", "Distance", "Discipline", "Gun Time", "Chip Time", "Achievement", "Gender", "Flag", "Age (reported)", "Age (calculated)", "Youngest", "Oldest", "Masters", "Age (Combined)", "Earliest BDay", "Latest BDay", "Results", "Personal Rank", "Team Rank", "Masters Personal Rank", "Masters Team Rank", "Year", "Kilometers", "", "As Of", "a date"),
    type = c("c",       "c",    "D",    "c",        "c",          "c",        "c",         "c",           "c",      "c",    "d",              "d",                "d",        "d",      "c",       "d",              "D",             "D",           "c",       "d",             "d",         "d",                     "d",                 "d",    "d"         , "c","c",     "c")
  )
  performances <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/?usp=sharing",
    col_types = paste(columns$type, collapse = "")
  )
  finish.times <- performances %>%
    filter(Discipline == "Road") %>%
    transmute(
      athlete = Athlete,
      race = Race,
      year = year(Date),
      date = Date,
      distance = Distance,
      chip_time = ifelse(is.na(`Chip Time`), `Gun Time`, `Chip Time`)
    ) %>%
    filter(year == current.year) %>%
    filter(!is.na(chip_time)) %>%
    inner_join(conversions) %>%
    mutate(minutes = sapply(chip_time, parse.chip.time)) %>%
    mutate(
      distance = to,
      minutes = minutes * conversion.factor
    ) %>%
    mutate(
      distance = factor(distance, levels = required.race.distances)
    )
  if (!('--all' %in% argv)) {
    finish.times <- finish.times %>%
      group_by(athlete, distance) %>%
      summarise(minutes = min(minutes))
  }
  plot(finish.times, time.standard, current.year)
}
