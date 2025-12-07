library(dplyr)
library(lubridate)

source("data.R")

main <- function (cache = FALSE) {
  # Fetch
  races <- get_race_data(cache)
  performances <- get_performance_data(include_relay_legs = TRUE, cache = cache)
  # Process
  current.year <- year(Sys.Date())
  races <- races |>
    filter(year == current.year)
  performances <- performances |>
    filter(year == current.year & (is.na(flag) | flag != "Team"))
  # By country
  countries <- races |>
    group_by(country) |>
    tally() |>
    arrange(-n)
  cat(nrow(countries), " countries\n")
  countries |>
    pull(country) |>
    print()
  # By state
  states <- races |>
    filter(country == "United States") |>
    group_by(state) |>
    tally() |>
    arrange(-n)
  cat(nrow(states), " states\n")
  # By person
  performances |>
    left_join(races, by = join_by(race, date, distance_label, discipline), relationship = "many-to-many") |>
    filter(country == "United States") |>
    select(athlete, state) |>
    distinct() |>
    group_by(athlete) |>
    tally() |>
    arrange(-n)
}
