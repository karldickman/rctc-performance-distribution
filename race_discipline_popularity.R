library(dplyr)
library(ggplot2)
library(janitor)

source("data.R")

plot_race_discipline_popularity <- function (data) {
  data |>
    ggplot(aes(x = year(date), fill = discipline)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = seq(2017, 2025, by = 1)) +
    labs(
      title = "Popularity of race disciplines",
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
      & !(discipline %in% c("Duathlon", "Skimo", "Triathlon"))
    ) |>
    mutate(discipline = ifelse(
      discipline %in% c("Indoor", "Beer mile"),
      "Track",
      discipline
    )) |>
    plot_race_discipline_popularity()
}
