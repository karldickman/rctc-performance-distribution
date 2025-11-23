library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(slider)
library(tidyr)

source("data.R")
source("vdot.R")

filter_vdottable_performances <- function (data) {
  exclude <- read_csv("exclude_races.csv", show_col_types = FALSE)
  data |>
    mutate(
      discipline = ifelse(discipline == "Indoor", "Track", discipline),
      distance_mi = kilometers / 1.609334,
      pace_min_mi = minutes / distance_mi
    ) |>
    select(!kilometers) |>
    filter(
      (is.na(flag) | flag != "Relay")
      & distance_mi >= 1.5 / 1.609334 & distance_mi <= 26.3
      & !(distance_label %in% c("2 k steeplechase", "2 k steeplechase (30\")"))
      & !(discipline %in% c("Trail", "Duathlon", "Triathlon", "Beer mile", "Skimo"))
    ) |>
    anti_join(exclude, by = join_by(athlete, race, date, distance_label, discipline))
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

plot.individual.vdot.over.time <- function (data, athlete.name, lookback.days) {
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
      x = "Date",
      y = "VDOT",
      color = "Discipline"
    ) +
    theme(legend.position = "bottom")
}

plot.team.vdot.over.time <- function (data, lookback.days) {
  min.vdot <- floor(min(data$vdot))
  max.vdot <- ceiling(max(data$vdot))
  data |>
    arrange(date) |>
    mutate(rolling_avg = slide_index_dbl(vdot, date, median, .before = days(lookback.days))) |>
    ggplot(aes(x = date, y = vdot)) +
    geom_point(aes(col = discipline), size = 0.5) +
    geom_line(aes(y = rolling_avg)) +
    geom_hline(yintercept = c(37.9, 55.3, 63.2), linetype = "dashed") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "All Rose City race VDOTs",
      x = "Date",
      y = "VDOT",
      color = "Discipline"
    ) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(legend.position = "bottom")
}

convert.xc.times <- function (data) {
  data |>
    mutate(pace_min_mi = ifelse(discipline == "XC", pace_min_mi - 10/60, pace_min_mi))
}

most.improved <- function (data) {
  data |>
    filter(year(date) == year(Sys.Date())) |>
    group_by(athlete) |>
    summarise(races = n(), slope = 30 * coef(lm(vdot ~ date))[[2]]) |>
    arrange(-slope)
}

main <- function (cache = FALSE) {
  lookback.days <- 90
  performances <- get_performance_data(cache)
  vdot <- fetch.vdot.data(cache) |>
    prepare.vdot.data() |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    select(!minutes)
  performances |>
    filter_vdottable_performances() |>
    convert.xc.times() |>
    interpolate.vdot(vdot) |>
    #plot.individual.vdot.over.time("Karl Dickman", lookback.days)
    plot.team.vdot.over.time(lookback.days)
}
