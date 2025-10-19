library(dplyr)
library(ggplot2)

source("training_groups.R")

plot.distance.x.pace <- function (performances) {
  performances |>
    ggplot(aes(x = distance_mi, y = minutes / distance_mi)) +
    geom_jitter(width = 0.01, size = 0.5, alpha = 0.2) +
    scale_x_continuous(transform = "log10") +
    labs(
      title = paste(nrow(performances), "races, 2017–present"),
      x = "Race distance (mi), log scale",
      y = "Race pace (min/mi)"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

plot.date.x.distance <- function (performances) {
  performances |>
    ggplot(aes(x = date, y = distance_mi)) +
    geom_jitter(width = 5, height = 0.02, size = 0.5, alpha = 0.2) +
    scale_y_continuous(transform = "log10") +
    labs(
      title = paste(nrow(performances), "races, 2017–present"),
      x = "Date",
      y = "Race distance (mi), log scale"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

plot.date.x.pace <- function (performances) {
  performances |>
    ggplot(aes(x = date, y = minutes / distance_mi)) +
    geom_jitter(width = 0, height = 0, size = 0.5, alpha = 0.2) +
    labs(
      title = paste(nrow(performances), "races, 2017–present"),
      x = "Date",
      y = "Race pace (min/mi)"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.background = element_rect(fill = "white", color = NA))
}

main <- function (argv = c()) {
  fetch.performances("--cache" %in% argv) |>
    filter(!(discipline %in% c("Duathlon", "Triathlon", "Skimo"))) |>
    #plot.distance.x.pace()
    #plot.date.x.distance()
    plot.date.x.pace()
}
