library(dplyr)
library(ggplot2)
library(googlesheets4)
library(hms)
library(lubridate)
library(stringr)

parse.chip.time <- function (chip.time) {
  if (substr(chip.time, nchar(chip.time), nchar(chip.time)) == "g") {
    chip.time <- substr(chip.time, 1, nchar(chip.time) - 1)
  }
  if (str_count(chip.time, ":") == 0) {
    chip.time <- paste0("0:00", chip.time)
  } else if (str_count(chip.time, ":") == 1) {
    chip.time <- paste0("0:", chip.time)
  }
  as.numeric(lubridate::hms(chip.time), "mins")
}

required.race.distances <- c("5k", "10k", "Half marathon", "Marathon")

time.standard <- tibble(
  distance = factor(required.race.distances, levels = required.race.distances),
  standard = c(25 + 1 / 60, 52 + 10 / 60, 60 + 55, 4 * 60)
)

conversions <- tibble(
  distance = c(required.race.distances, "3 mi", "Quarter marathon"),
  to = c(required.race.distances, "5k", "10k"),
  conversion.factor = c(rep(1, 4), 5 / (3 * 1.609334), 10 / ((26+(385*3/5280)) / 4 * 1.609334))
)

minutes.as.POSIXct <- function (minutes) {
  as.POSIXct(minutes * 60, origin = "1970-01-01", tz = "UTC")
}

fetch.data <- function (cache = FALSE) {
  cached <- "Performances.csv"
  if (cache) {
    data <- cached |>
      read.csv(check.names = FALSE) |>
      as_tibble() |>
      mutate(Date = as.Date(Date))
    return(data)
  }
  columns <- data.frame(
    name = c("Athlete", "Race", "Date", "Distance", "Discipline", "Gun Time", "Chip Time", "Achievement", "Gender", "Flag", "Age (reported)", "Age (calculated)", "Youngest", "Oldest", "Masters", "Age (Combined)", "Earliest BDay", "Latest BDay", "Results", "Use this time", "Personal Rank", "Team Rank", "Masters Personal Rank", "Masters Team Rank", "Year", "Kilometers", "", "As Of", "a date"),
    type = c("c",       "c",    "D",    "c",        "c",          "c",        "c",         "c",           "c",      "c",    "d",              "d",                "d",        "d",      "c",       "d",              "D",             "D",           "c",       "c",             "d",             "d",         "d",                     "d",                 "d",    "d"         , "c","c",     "c")
  )
  performances <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1nnFKb2iRgadVSpTSw0zOk3gewPaLU6u4pxBb-rUY9hQ/",
    col_types = paste(columns$type, collapse = "")
  )
  write.csv(performances, cached, row.names = FALSE)
  performances
}

plot <- function (finish.times, time.standard, since) {
  time.standard <- mutate(time.standard, standard = minutes.as.POSIXct(standard))
  finish.times |>
    mutate(minutes = minutes.as.POSIXct(minutes)) |>
    ggplot(aes(x = minutes)) +
    facet_wrap(vars(distance), scales = "free") +
    geom_histogram(bins = 10) +
    geom_vline(data = time.standard, aes(xintercept = standard)) +
    scale_x_datetime(date_labels = "%-H:%M") +
    ggtitle(paste("Race results since", since), subtitle = "Road race results for all Rose City athletes") +
    xlab("Finish time (h:mm)") +
    ylab("Frequency")
}

table <- function (finish.times) {
  finish.times |>
    group_by(distance) |>
    summarise(
      median = as_hms(round(median(minutes) * 60)),
      mean = as_hms(round(mean(minutes) * 60)),
      `90%ile` = as_hms(round(quantile(minutes, 0.9) * 60)),
      max = as_hms(round(max(minutes) * 60))
    ) |>
    inner_join(mutate(time.standard, standard = as_hms(standard * 60)))
}

main <- function (argv = c()) {
  since <- Sys.Date() - 365
  year.of.interest <- argv[[1]]
  performances <- fetch.data("--cache" %in% argv)
  finish.times <- performances |>
    filter(Discipline == "Road" & !(`Use this time` %in% c("TBD", "Not found"))) |>
    transmute(
      athlete = Athlete,
      race = Race,
      year = year(Date),
      date = ymd(Date),
      distance = gsub(" k", "k", Distance),
      chip_time = ifelse(is.na(`Chip Time`), `Gun Time`, `Chip Time`)
    ) |>
    #filter(year == year.of.interest) |>
    filter(date >= since) |>
    filter(!is.na(chip_time)) |>
    inner_join(conversions) |>
    mutate(minutes = sapply(chip_time, parse.chip.time)) |>
    mutate(
      distance = to,
      minutes = minutes * conversion.factor
    ) |>
    mutate(
      distance = factor(distance, levels = required.race.distances)
    )
  if (!('--all' %in% argv)) {
    finish.times <- finish.times |>
      group_by(athlete, distance) |>
      summarise(minutes = min(minutes))
  }
  print(table(finish.times))
  plot(finish.times, time.standard, since)
}
