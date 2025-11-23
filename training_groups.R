library(dplyr)
library(ggplot2)

source("../distance-matrix/attendance.R")
source("vdot_each_performance.R")

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

newbie.performances <- function () {
  read_csv("newbies.csv", col_types = "ccDcdcdc", show_col_types = FALSE) |>
    mutate(pace_min_mi = minutes / distance_mi)
}

main <- function (argv = c()) {
  cache = "--cache" %in% argv
  lookback.days <- 90
  roster <- fetch.roster(cache) |>
    filter(Status == "Member" & is.na(To)) |>
    select(athlete = Name)
  performances <- fetch.performances(cache) |>
    bind_rows(newbie.performances())
  vdot <- fetch.vdot.data(cache) |>
    prepare.vdot.data() |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    select(!minutes)
  performances |>
    convert.xc.times() |>
    interpolate.vdot(vdot) |>
    training.group.assignments(roster, lookback.days)
}
