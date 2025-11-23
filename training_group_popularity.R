library(dplyr)
library(ggplot2)
library(janitor)

source("data.R")

circle.viz <- function (data) {
  data |>
    group_by(date, kilometers) |>
    tally() |>
    ggplot(aes(x = date, y = kilometers, size = n)) +
    geom_point(alpha = 0.3) +
    scale_y_log10() +
    labs(
      title = "Popularity of race distances",
      x = "Date",
      y = "Race distance (km), log scale",
      size = "Participants"
    ) +
    theme(legend.position = "bottom")
}

assign_races_to_training_groups <- function (data) {
  data |>
    mutate(
      training_group = factor(ifelse(
        discipline == "Trail" | kilometers >= 43,
        "Trail/Ultra",
        ifelse(
          kilometers < 1.5,
          "800/1500",
          ifelse(
            kilometers < 15,
            "5k/10k",
            "Marathon/Half"
            )
          )
        ), levels = c("800/1500", "5k/10k", "Marathon/Half", "Trail/Ultra"))
    )
}

bar.viz <- function (data) {
  data |>
    ggplot(aes(x = year(date), fill = training_group)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = seq(2017, 2025, by = 1)) +
    labs(
      title = "Popularity of race distances",
      x = "Year",
      y = "Number of races",
      fill = "Race distance"
    ) +
    theme(legend.position = "bottom")
}

main <- function (cache = FALSE) {
  get_performance_data(cache) |>
    filter(
      !(gender %in% c("Male team", "Female team"))
      & !(discipline %in% c("Duathlon", "Triathlon"))
      & !(distance_label %in% c("Distance relay", "Distance Relay"))
      & !is.na(kilometers)
    ) |>
    assign_races_to_training_groups() |>
    bar.viz()
}
