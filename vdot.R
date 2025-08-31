library(dplyr)
library(ggplot2)
library(googlesheets4)
library(viridis)

fetch.vdot.data <- function (cache = FALSE) {
  cached <- "VDOT.csv"
  if (cache) {
    return(read.csv(cached, check.names = FALSE))
  }
  vdot <- read_sheet(
    "https://docs.google.com/spreadsheets/d/153Sc67IzEqo6bNfHlSmr-ECfaA2oi1u2obvzgNmVA3E/",
    "Equivalent Paces",
    "B1:J361"
  )
  write.csv(vdot, cached, row.names = FALSE)
  vdot
}

prepare.vdot.data <- function (data) {
  data |>
    select(
      distance_mi = `Distance (mi)`,
      vdot = VDOT,
      minutes = `Decimal minutes`,
      pace_min_mi = `Pace (min/mi)`
    )
}

plot.vdot.data <- function (data) {
  data |>
    ggplot(aes(x = distance_mi, y = pace_min_mi, col = vdot)) +
    geom_point() +
    scale_x_log10() +
    scale_color_viridis(name = "VDOT") +
    labs(title = "VDOT race paces", x = "Race distance (mi)", y = "Race pace (min/mi)")
}

main <- function () {
  fetch.vdot.data() |>
    prepare.vdot.data() |>
    plot.vdot.data()
}
