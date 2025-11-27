library(dplyr)
library(ggplot2)
library(viridis)

source("data.R")

plot.distance.x.pace <- function (performances) {
  performances |>
    ggplot(aes(x = distance_mi, y = pace)) +
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

plot.date.x.pace.x.distance <- function (performances) {
  performances |>
    ggplot(aes(x = date, y = pace, col = distance_mi)) +
    geom_point(size = 0.5) +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(transform = "log10") +
    scale_color_viridis(
      direction = -1,
      transform = "log10",
      option = "magma",
      breaks = c(0.5, 1, 3.1, 6.2, 13.1, 26.2, 100)
    ) +
    labs(
      title = paste(nrow(performances), "races, 2017–present"),
      x = "Date",
      y = "Race pace (min/mi), log scale",
      col = "Race distance (mi), log scale"
    ) +
    theme_bw(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      legend.key.width = unit(2, "cm"),
      legend.position = "bottom"
    )
}

main <- function (cache = FALSE) {
  get_performance_data(cache) |>
    filter(
      tolower(distance_label) != "distance relay"
      & !(discipline %in% c("Duathlon", "Triathlon", "Skimo"))
    ) |>
    mutate(pace = minutes / distance_mi) |>
    #plot.distance.x.pace()
    #plot.date.x.distance()
    plot.date.x.pace.x.distance()
}
