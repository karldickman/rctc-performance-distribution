library(dplyr)
library(lubridate)

source("data.R")

main <- function (cache = FALSE) {
  get_performance_data(include_relay_legs = TRUE, cache = cache) |>
    filter(is.na(flag) | flag != "Team") |>
    group_by(athlete) |>
    tally()
}
