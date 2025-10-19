library(dplyr)
library(ggplot2)

source("training_groups.R")

main <- function (argv = c()) {
  performances <- fetch.performances("--cache" %in% argv) |>
    filter(!(discipline %in% c("Duathlon", "Triathlon", "Skimo")))
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
