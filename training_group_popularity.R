library(dplyr)
library(ggplot2)

source("histograms.R")

options(warn = 2)

circle.viz <- function (data) {
  data |>
    group_by(Date, Kilometers) |>
    tally() |>
    ggplot(aes(x = Date, y = Kilometers, size = n)) +
    geom_point(alpha = 0.3) +
    scale_y_log10()
}

bar.viz <- function (data) {
  distances <- c("1 mi", "5k", "8k", "10k", "Half marathon", "Marathon")
  data |>
    #filter(Distance %in% distances) |>
    mutate(
      Distance = factor(Distance, levels = distances),
      training_group = factor(ifelse(
        Kilometers < 1.5,
        "Sprints",
        ifelse(
          Kilometers < 15,
          "5k/10k",
          ifelse(
            Kilometers < 43,
            "Marathon/Half",
            "Ultra"
          )
        )
      ), levels = c("Sprints", "5k/10k", "Marathon/Half", "Ultra"))
    ) |>
    ggplot(aes(x = year(Date), fill = training_group)) +
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

main <- function (argv = c()) {
  performances <- fetch.data("--cache" %in% argv)
  performances |>
    filter(
      !(Gender %in% c("Exclude", "Male team", "Female team"))
      & !(Discipline %in% c("Duathlon", "Triathlon"))
      & !(Distance %in% c("Distance relay", "Distance Relay"))
      & !is.na(Kilometers)
      & !(`Use this time` %in% c("TBD", "Not found", "DNF"))
      & !is.na(`Use this time`)
    ) |>
    mutate(
      minutes = sapply(`Use this time`, parse.chip.time),
      Distance = gsub(" k", "k", Distance)
    ) |>
    bar.viz()
}
