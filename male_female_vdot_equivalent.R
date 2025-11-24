library(dplyr)

source("vdot.R")
source("vdot_each_performance.R")

interpolate.vdot <- function (performances, vdot) {
  performances |>
    select(female_vdot, distance_mi, pace_min_mi) |>
    inner_join(vdot, by = join_by(distance_mi), relationship = "many-to-many") |>
    rename(
      pace_min_mi = pace_min_mi.x,
      pace_min_mi.vdot = pace_min_mi.y
    ) |>
    group_by(female_vdot, distance_mi) |>
    filter(
      length(pace_min_mi.vdot[pace_min_mi.vdot <= pace_min_mi]) > 0
      & length(pace_min_mi.vdot[pace_min_mi.vdot >= pace_min_mi]) > 0
    ) |>
    filter(
       pace_min_mi.vdot == max(pace_min_mi.vdot[pace_min_mi.vdot <= pace_min_mi], na.rm = TRUE)
      | pace_min_mi.vdot == min(pace_min_mi.vdot[pace_min_mi.vdot >= pace_min_mi], na.rm = TRUE)
    ) |>
    summarise(
      pace_min_mi = first(pace_min_mi),
      pace_min_mi.lower = min(pace_min_mi.vdot),
      pace_min_mi.upper = max(pace_min_mi.vdot),
      vdot.lower = max(vdot),
      vdot.upper = min(vdot),
      .groups = "drop"
    ) |>
    mutate(
      male_vdot = interpolate(pace_min_mi, pace_min_mi.lower, vdot.lower, pace_min_mi.upper, vdot.upper)
    )
}

get_male_female_equivalent_vdot <- function (cache) {
  vdot <- fetch.vdot.data(cache) |>
    prepare.vdot.data()
  vdot |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    mutate(velocity_mi_per_mn = 1 / pace_min_mi) |>
    rename(female_vdot = vdot) |>
    mutate(
      velocity_mi_per_mn = velocity_mi_per_mn * 1.12,
      pace_min_mi = 1 / velocity_mi_per_mn,
      minutes = distance_mi * pace_min_mi
    ) |>
    interpolate.vdot(vdot)
}

plot_male_female_equivalent_vdot <- function (data) {
  data |>
    ggplot(aes(x = female_vdot, y = male_vdot)) +
    geom_point(size = 0.5) +
    labs(
      title = "Relationship between female and male VDOT",
      subtitle = "Equivalent male VDOT is 13% higher than female VDOT",
      x = "Female VDOT",
      y = "Male VDOT"
    )
}

main <- function (cache = FALSE) {
  data <- get_male_female_equivalent_vdot(cache)
  linear.fit <- lm(male_vdot ~ female_vdot, data = data)
  coefficients <- coef(linear.fit)
  intercept <- coefficients["(Intercept)"]
  slope <- coefficients["female_vdot"]
  cat(slope, "* x +", intercept, "\n")
  data |>
    plot_male_female_equivalent_vdot()
}
