library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(slider)
library(tidyr)

source("../distance-matrix/attendance.R")
source("histograms.R")
source("vdot.R")

fetch.performances <- function (cache = FALSE) {
  fetch.data(cache) |>
    filter(
      (is.na(Flag) | !(Flag %in% c("Future", "Relay")))
      & !(`Use this time` %in% c("TBD", "Not found", "DNF"))
    ) |>
    mutate(
      Kilometers = ifelse(
        str_count(Distance, " hr") > 0 | Distance == "Run til you drop",
        suppressWarnings(as.numeric(gsub(" mi", "", `Use this time`))) * 1.609334,
        Kilometers
      ),
      `Use this time` = ifelse(
        str_count(Distance, " hr") > 0,
        paste0(gsub(" hr", "", Distance), ":00:00"),
        ifelse(
          Distance == "Run til you drop",
          paste0(gsub(" laps", "", `Gun Time`), ":00:00"),
          `Use this time`
        )
      )
    ) |>
    select(
      athlete = Athlete,
      gender = Gender,
      race = Race,
      date = Date,
      distance_label = Distance,
      discipline = Discipline,
      distance_km = Kilometers,
      finish_time = `Use this time`
    ) |>
    mutate(finish_time = gsub(" \\(Strava\\)", "", finish_time)) |>
    mutate(
      distance_mi = distance_km / 1.609334,
      minutes = sapply(finish_time, parse.chip.time)
    ) |>
    select(!distance_km)
}

slope <- function (x1, y1, x2, y2) {
  ifelse(
    y2 != y1,
    (y2 - y1) / (x2 - x1),
    0
  )
}

interpolate <- function (x, x1, y1, x2, y2) {
  m <- slope(x1, y1, x2, y2)
  m * (x - x1) + y1
}

interpolate.vdot <- function (performances, vdot) {
  lower.bounds <- performances |>
    select(athlete, race, date, distance_mi, discipline, pace_min_mi) |>
    fuzzy_left_join(vdot, by = c("distance_mi" = "distance_mi"), match_fun = list(`>=`)) |>
    rename(
      distance_mi = distance_mi.x,
      pace_min_mi = pace_min_mi.x,
      distance_mi.vdot = distance_mi.y,
      pace_min_mi.vdot = pace_min_mi.y
    ) |>
    group_by(distance_mi) |>
    filter(distance_mi.vdot == max(distance_mi.vdot)) |>
    group_by(athlete, race, date, distance_mi) |>
    filter(
      pace_min_mi.vdot == max(pace_min_mi.vdot[pace_min_mi.vdot <= pace_min_mi], na.rm = TRUE)
      | pace_min_mi.vdot == min(pace_min_mi.vdot[pace_min_mi.vdot >= pace_min_mi], na.rm = TRUE)
    ) |>
    summarise(
      distance_mi = first(distance_mi),
      pace_min_mi = first(pace_min_mi),
      distance_mi.vdot = first(distance_mi.vdot),
      pace_min_mi.lower = min(pace_min_mi.vdot),
      pace_min_mi.upper = max(pace_min_mi.vdot),
      vdot.lower = max(vdot),
      vdot.upper = min(vdot),
      .groups = "drop"
    )
  upper.bounds <- performances |>
    select(athlete, race, date, distance_mi, discipline, pace_min_mi) |>
    fuzzy_left_join(vdot, by = c("distance_mi" = "distance_mi"), match_fun = list(`<=`)) |>
    rename(
      distance_mi = distance_mi.x,
      pace_min_mi = pace_min_mi.x,
      distance_mi.vdot = distance_mi.y,
      pace_min_mi.vdot = pace_min_mi.y
    ) |>
    group_by(distance_mi) |>
    filter(distance_mi.vdot == min(distance_mi.vdot)) |>
    group_by(athlete, race, date, distance_mi) |>
    filter(
      pace_min_mi.vdot == max(pace_min_mi.vdot[pace_min_mi.vdot <= pace_min_mi], na.rm = TRUE)
      | pace_min_mi.vdot == min(pace_min_mi.vdot[pace_min_mi.vdot >= pace_min_mi], na.rm = TRUE)
    ) |>
    summarise(
      distance_mi = first(distance_mi),
      pace_min_mi = first(pace_min_mi),
      distance_mi.vdot = first(distance_mi.vdot),
      pace_min_mi.lower = min(pace_min_mi.vdot),
      pace_min_mi.upper = max(pace_min_mi.vdot),
      vdot.lower = max(vdot),
      vdot.upper = min(vdot),
      .groups = "drop"
    )
  interpolated.vdot <- lower.bounds |>
    left_join(select(upper.bounds, !pace_min_mi), by = join_by(athlete, race, date, distance_mi)) |>
    mutate(
      pace_min_mi.fast = interpolate(distance_mi, distance_mi.vdot.x, pace_min_mi.lower.x, distance_mi.vdot.y, pace_min_mi.lower.y),
      pace_min_mi.slow = interpolate(distance_mi, distance_mi.vdot.x, pace_min_mi.upper.x, distance_mi.vdot.y, pace_min_mi.upper.y)
    ) |>
    mutate(
      vdot.fast = interpolate(pace_min_mi.fast, pace_min_mi.lower.x, vdot.lower.x, pace_min_mi.lower.y, vdot.lower.y),
      vdot.slow = interpolate(pace_min_mi.slow, pace_min_mi.upper.x, vdot.upper.x, pace_min_mi.upper.y, vdot.upper.y)
    ) |>
    mutate(vdot = interpolate(pace_min_mi, pace_min_mi.fast, vdot.fast, pace_min_mi.slow, vdot.slow)) |>
    select(!c(distance_mi.vdot.x, pace_min_mi.lower.x, pace_min_mi.upper.x, distance_mi.vdot.y, pace_min_mi.lower.y, pace_min_mi.upper.y, vdot.lower.x, vdot.lower.y, vdot.upper.x, vdot.upper.y, pace_min_mi.fast, pace_min_mi.slow, vdot.fast, vdot.slow))
  performances |>
    left_join(select(interpolated.vdot, !c(pace_min_mi)), by = join_by(athlete, race, date, distance_mi))
}

plot.vdot.over.time <- function (data, athlete.name, lookback.days) {
  athlete.data <- data |>
    filter(athlete == athlete.name)
  min.vdot <- floor(min(athlete.data$vdot))
  max.vdot <- ceiling(max(athlete.data$vdot))
  vdot.breaks <- min.vdot:max.vdot
  athlete.data |>
    mutate(rolling_avg = slide_index_dbl(vdot, date, median, .before = days(lookback.days))) |>
    ggplot(aes(x = date, y = vdot, group = athlete)) +
    geom_line(aes(y = rolling_avg), linetype = "dashed") +
    geom_point(aes(col = discipline)) +
    scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
    scale_y_continuous(breaks = vdot.breaks) +
    labs(
      title = paste0(athlete.name, "'s VDOT since joining Rose City"),
      color = "Discipline"
    ) +
    xlab("Date") +
    ylab("VDOT") +
    theme(legend.position = "bottom")
}

training.group.assignments <- function (data, roster, lookback.days) {
  lookback.date <- Sys.Date() - lookback.days
  training.groups <- tibble(
    group = c("A", "B", "C", "D", "E", "F", "G", "H"),
    from =  c(30,  36,  42,  47,  52,  56,  61,  65),
    to =    c(36,  42,  47,  52,  56,  61,  65,  100)
  )
  last.races <- data |>
    group_by(athlete) |>
    summarise(last_race = max(date))
  relevant.data <- data |>
    left_join(last.races, by = join_by(athlete)) |>
    filter(date >= lookback.date | date == last_race) |>
    select(!last_race)
  recent.vdot <- relevant.data |>
    group_by(athlete) |>
    summarise(
      races = n(),
      last_race = max(date),
      best_vdot = max(vdot),
      median_vdot = median(vdot),
      slope = 30 * coef(lm(vdot ~ date))[[2]]
    ) |>
    mutate(days_ago = as.numeric(Sys.Date() - last_race)) |>
    select(athlete, races, last_race, days_ago, best_vdot, median_vdot, slope)
  roster |>
    left_join(recent.vdot, by = join_by(athlete)) |>
    left_join(training.groups, by = join_by(best_vdot >= from, best_vdot <= to)) |>
    select(!c(from, to)) |>
    left_join(data, by = join_by(athlete, best_vdot == vdot)) |>
    select(!c(distance_mi, minutes, pace_min_mi)) |>
    rename(best_race = race) |>
    arrange(best_vdot)
}

convert.xc.times <- function (data) {
  data |>
    mutate(pace_min_mi = ifelse(discipline == "XC", pace_min_mi - 10/60, pace_min_mi))
}

newbie.performances <- function () {
  read_csv("newbies.csv", col_types = "ccDcdcdc")
}

main <- function (argv = c()) {
  cache = "--cache" %in% argv
  lookback.days <- 90
  roster <- fetch.roster(cache) |>
    filter(Status == "Member" & is.na(To)) |>
    select(athlete = Name)
  performances <- fetch.performances(cache) |>
    filter(
      gender != "Exclude"
      & !(distance_label %in% c("2 k steeplechase", "2 k steeplechase (30\")"))
      & !(discipline %in% c("Trail", "Duathlon", "Triathlon", "Beer mile", "Skimo"))
    ) |>
    filter(distance_mi >= 1.5 / 1.609334 & distance_mi <= 26.3)
  performances <- performances |>
    bind_rows(newbie.performances()) |>
    mutate(pace_min_mi = minutes / distance_mi)
  vdot <- fetch.vdot.data(cache) |>
    prepare.vdot.data() |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    select(!minutes)
  performances |>
    convert.xc.times() |>
    interpolate.vdot(vdot) |>
    filter(discipline %in% c("Road", "Track", "XC")) |>
    training.group.assignments(roster, lookback.days)
    #plot.vdot.over.time("Karl Dickman", lookback.days)
}
