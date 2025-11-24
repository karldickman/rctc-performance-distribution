library(dplyr)
library(ggplot2)
library(viridis)

source("training_groups.R")

compile_training_group_data = main

ea.vdot <- 55:65

get_ea_vdot_thresholds <- function () {
  female.thresholds <- tibble(gender = "Female", ea_vdot = ea.vdot) |>
    mutate(female_vdot = ea_vdot)
  nonbinary.thresholds <- female.thresholds |>
    mutate(gender = "Nonbinary")
  male.thresholds <- female.thresholds |>
    mutate(gender = "Male", ea_vdot = female_vdot * 1.13)
  bind_rows(female.thresholds, nonbinary.thresholds, male.thresholds)
}

project_program_size_by_gender <- function (data, thresholds) {
  data |>
    inner_join(thresholds, by = join_by(gender), relationship = "many-to-many") |>
    filter(best_vdot >= ea_vdot) |>
    group_by(female_vdot, gender) |>
    tally()
}

project_program_size_with_improvement <- function (data, thresholds) {
  improvement <- tibble(annual_vdot_improvement = c(0, 0.5, 1, 1.5, 2, 2.5))
  data |>
    inner_join(thresholds, by = join_by(gender), relationship = "many-to-many") |>
    cross_join(improvement) |>
    filter(best_vdot + annual_vdot_improvement >= ea_vdot) |>
    group_by(female_vdot, annual_vdot_improvement) |>
    tally()
}

plot_program_size_by_gender <- function (data) {
  data |>
    ggplot(aes(x = female_vdot, y = n, fill = gender)) +
    geom_col() +
    scale_x_continuous(breaks = ea.vdot) +
    labs(
      title = "Potential applicants to emerging athlete program",
      x = "Female VDOT",
      y = "Number of athletes who meet time standard",
      fill = "Gender"
    ) +
    theme(legend.position = "bottom")
}

plot_program_size_with_improvement <- function (data) {
  data |>
    ggplot(aes(x = female_vdot, y = n, group = annual_vdot_improvement, fill = annual_vdot_improvement)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = ea.vdot) +
    scale_fill_viridis() +
    labs(
      title = "Potential applicants to emerging athlete program",
      x = "Female VDOT",
      y = "Number of athletes who meet time standard",
      fill = "Annual VDOT improvement"
    ) +
    theme(legend.position = "bottom")
}

main <- function (cache = FALSE) {
  thresholds <- get_ea_vdot_thresholds()
  data <- compile_training_group_data(lookback.days = 730, cache = cache)
  #project_program_size_by_gender(data, thresholds) |>
  #  plot_program_size_by_gender()
  project_program_size_with_improvement(data, thresholds) |>
    plot_program_size_with_improvement()
}
