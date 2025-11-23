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

bar.viz <- function (data) {
  distances <- c("1 mi", "5k", "8k", "10k", "Half marathon", "Marathon")
  data |>
    #filter(eistance %in% distances) |>
    mutate(
      distance = factor(distance_label, levels = distances),
      training_group = factor(ifelse(
        kilometers < 1.5,
        "800/1500",
        ifelse(
          kilometers < 15,
          "5k/10k",
          ifelse(
            kilometers < 43,
            "Marathon/Half",
            "Ultra"
          )
        )
      ), levels = c("800/1500", "5k/10k", "Marathon/Half", "Ultra"))
    ) |>
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
    bar.viz()
}
