library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(tidyr)

source("histograms.R")
source("vdot.R")

fetch.performances <- function (cache = FALSE) {
  fetch.data(cache) |>
    filter(
      Gender != "Exclude"
      & (is.na(Flag) | Flag != "Relay")
      & Discipline %in% c("Road", "Track")
      & !(`Use this time` %in% c("TBD", "Not found"))
      & `Use this time` != "DNF"
    ) |>
    select(
      athlete = Athlete,
      race = Race,
      date = Date,
      distance_label = Distance,
      distance_km = Kilometers,
      finish_time = `Use this time`
    ) |>
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
    select(athlete, race, date, distance_mi, pace_min_mi) |>
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
    select(athlete, race, date, distance_mi, pace_min_mi) |>
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

plot.vdot.over.time <- function (data) {
  data |>
    ggplot(aes(x = date, y = vdot, group = athlete)) +
    geom_point() +
    scale_x_date()
}

main <- function (argv = c()) {
  cache = "--cache" %in% argv
  performances <- fetch.performances(cache) |>
    filter(distance_mi >= 1.5 / 1.609334 & distance_mi <= 26.3) |>
    mutate(pace_min_mi = minutes / distance_mi)
  vdot <- fetch.vdot.data(cache) |>
    prepare.vdot.data() |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    select(!minutes)
  interpolate.vdot(performances, vdot) |>
    plot.vdot.over.time()
}
