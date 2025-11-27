library(dplyr)
library(ggplot2)
library(lubridate)
library(slider)

source("data.R")
source("utils.R")

main <- function (distance = "Half marathon", cache = FALSE) {
  exclude.races <- read_csv("exclude_races.csv", show_col_types = FALSE)
  # Filter to relevant data
  performances <- get_performance_data(cache) |>
    filter(distance_label == distance & discipline == "Road" & !is.na(minutes)) |>
    anti_join(exclude.races, by = join_by(athlete, race, date, distance_label, discipline))
  # Calculate rolling averages
  rolling_avgs <- performances |>
    filter(gender != "Nonbinary") |>
    arrange(date) |>
    group_by(gender) |>
    mutate(rolling_avg = slide_index_dbl(minutes, date, median, .before = days(365))) |>
    ungroup()
  # Convert to hh:mm format for easier plotting
  performances <- performances |>
    mutate(minutes = minutes.as.POSIXct(minutes))
  rolling_avgs <- rolling_avgs |>
    mutate(rolling_avg = minutes.as.POSIXct(rolling_avg))
  # Create plot
  performances |>
    ggplot(aes(x = date, y = minutes, col = gender)) +
    geom_point() +
    geom_line(data = rolling_avgs, aes(y = rolling_avg)) +
    labs(
      title = paste(distance, "performances over time"),
      x = "Date",
      y = "Minutes",
      color = "Gender"
    ) +
    theme(legend.position = "bottom")
}
