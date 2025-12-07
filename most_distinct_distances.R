library(dplyr)
library(lubridate)

source("data.R")

main <- function (cache = FALSE) {
  get_performance_data(cache = cache) |>
    filter(year == year(Sys.Date())) |>
    select(athlete, distance_label, distance_km) |>
    distinct() |>
    group_by(athlete) |>
    mutate(count = n()) |>
    ungroup() |>
    arrange(-count, athlete, distance_km)
}
