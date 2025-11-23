library(dplyr)
library(ggplot2)

source("data.R")

required.race.distances <- c("5k", "10k", "Half marathon", "Marathon")

time.standard <- tibble(
  distance = factor(required.race.distances, levels = required.race.distances),
  standard = c(25 + 1 / 60, 52 + 10 / 60, 60 + 55, 4 * 60)
)

conversions <- tibble(
  distance_label = c(required.race.distances, "3 mi", "Quarter marathon"),
  to_distance = c(required.race.distances, "5k", "10k"),
  conversion_factor = c(rep(1, 4), 5 / (3 * 1.609334), 10 / ((26+(385*3/5280)) / 4 * 1.609334))
)

minutes.as.POSIXct <- function (minutes) {
  as.POSIXct(minutes * 60, origin = "1970-01-01", tz = "UTC")
}

plot <- function (finish.times, time.standard, time.period) {
  time.standard <- mutate(time.standard, standard = minutes.as.POSIXct(standard))
  finish.times |>
    mutate(minutes = minutes.as.POSIXct(minutes)) |>
    ggplot(aes(x = minutes)) +
    facet_wrap(vars(distance), scales = "free") +
    geom_histogram(bins = 10) +
    geom_vline(data = time.standard, aes(xintercept = standard)) +
    scale_x_datetime(date_labels = "%-H:%M") +
    labs(
      title = paste("Race results", time.period),
      subtitle = "Road race results for all Rose City athletes",
      x = "Finish time (h:mm)",
      y = "Frequency"
    )
}

table <- function (finish.times) {
  finish.times |>
    group_by(distance) |>
    summarise(
      median = as_hms(round(median(minutes) * 60)),
      mean = as_hms(round(mean(minutes) * 60)),
      `90%ile` = as_hms(round(quantile(minutes, 0.9) * 60)),
      max = as_hms(round(max(minutes) * 60))
    ) |>
    inner_join(mutate(time.standard, standard = as_hms(standard * 60)))
}

main <- function (year = NA, cache = FALSE, show.all.times = FALSE) {
  # Parse arguments
  since <- ifelse(is.na(year), Sys.Date() - 365, as.Date(paste0(year, "-01-01"))) |>
    as.Date()
  until <- ifelse(is.na(year), Sys.Date() + 1, as.Date(paste0(year + 1, "-01-01"))) |>
    as.Date()
  # Fetch data
  performances <- get_performance_data(cache)
  finish.times <- performances |>
    filter(discipline == "Road" & !is.na(minutes)) |>
    filter(date >= since & date < until) |>
    inner_join(conversions, by = join_by(distance_label)) |>
    mutate(
      distance_label = to_distance,
      minutes = minutes * conversion_factor
    ) |>
    mutate(
      distance = factor(distance_label, levels = required.race.distances)
    )
  if (!show.all.times) {
    finish.times <- finish.times |>
      group_by(athlete, distance) |>
      summarise(minutes = min(minutes), .groups = "drop")
  }
  finish.times |>
    table() |>
    print()
  finish.times |>
    plot(time.standard, ifelse(
      is.na(year),
      paste("since", since),
      paste("in", year)
    ))
}
