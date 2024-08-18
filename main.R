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
  mile.paces <- performances %>%
    filter(Discipline == "Road") %>%
    transmute(
      athlete = Athlete,
      race = Race,
      year = year(Date),
      date = Date,
      miles = Kilometers / 1.609334,
      chip_time = ifelse(is.na(`Chip Time`), `Gun Time`, `Chip Time`)
    ) %>%
    filter(year == current.year) %>%
    filter(!is.na(chip_time)) %>%
    mutate(
      minutes = sapply(chip_time, parse.chip.time)
    ) %>%
    mutate(pace = minutes / miles)
  if (!('--all' %in% argv)) {
  mile.paces <- mile.paces %>%
    group_by(athlete, miles) %>%
    summarise(pace = min(pace))
  }
  required.race.distances <- c(5 / 1.609334, 10 / 1.609334, 13.1, 26.2)
  ggplot(mile.paces, aes(x = miles, y = pace)) +
    geom_point(size = 1, alpha = 0.5) +
    geom_line(data = tibble(
      miles = required.race.distances,
      pace = c(25 + 1 / 60, 52 + 10 / 60, 60 + 55, 4 * 60) / required.race.distances
    )) +
    scale_x_continuous(name = "Race distance (mi)", breaks = seq(0, 200, by = 5)) +
    scale_y_continuous(name = "Race pace (minutes/mi)") +
    ggtitle(paste("Race results in", current.year), subtitle = "Road race results for all Rose City athletes")
}
