library(dplyr)
library(ggplot2)
library(janitor)
library(viridis)

source("../distance-matrix/attendance.R")
source("data.R")

current.year <- year(Sys.Date())

distance.axis.breaks <- c(0.1, 0.2, 0.4, 0.8, 1.6, 3, 5, 10, 21.1, 42.2, 100, 160.9)

circle.viz <- function (data) {
  data |>
    group_by(date, distance_km) |>
    tally() |>
    ggplot(aes(x = date, y = distance_km, size = n)) +
    geom_point(alpha = 0.3) +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    scale_y_log10(breaks = distance.axis.breaks) +
    labs(
      title = "Popularity of race distances",
      x = "Date",
      y = "Race distance (km), log scale",
      size = "Participants"
    ) +
    theme(legend.position = "bottom")
}

hex.viz <- function (data) {
  data |>
    group_by(date, distance_km) |>
    ggplot(aes(x = date, y = distance_km)) +
    geom_hex() +
    scale_x_date(breaks = "1 year", date_labels = "%Y") +
    scale_y_log10(breaks = distance.axis.breaks) +
    scale_fill_viridis() +
    labs(
      title = "Popularity of race distances",
      x = "Date",
      y = "Race distance (km), log scale",
      fill = "Participants"
    ) +
    theme(legend.position = "bottom")
}

training.groups <- c("800/1500", "5k/10k", "Marathon/Half", "Trail/Ultra")

assign_races_to_training_groups <- function (data) {
  data |>
    mutate(
      training_group = factor(ifelse(
        discipline == "Trail" | distance_km >= 43,
        "Trail/Ultra",
        ifelse(
          distance_km < 1.5,
          "800/1500",
          ifelse(
            distance_km < 15,
            "5k/10k",
            "Marathon/Half"
            )
          )
        ), levels = training.groups)
    )
}

assign_people_to_training_groups <- function (data) {
  person_data <- data |>
    filter(
      (is.na(flag) | !(flag %in% c("Relay", "Team")))
      & athlete != "Greg Mitchell"
    )
  fallbacks <- read_csv("fallback_training_groups.csv", show_col_types = FALSE)
  training.groups.overall <- person_data |>
    group_by(athlete, training_group) |>
    summarise(races = n(), .groups = "drop") |>
    group_by(athlete) |>
    filter(races == max(races)) |>
    filter(n() == 1) |>
    ungroup() |>
    bind_rows(fallbacks)
  training.groups.by.year <- person_data |>
    group_by(athlete, year, training_group) |>
    summarise(races = n(), .groups = "drop") |>
    group_by(athlete, year) |>
    filter(races == max(races))
  unambiguous.assignments <- training.groups.by.year |>
    filter(n() == 1) |>
    ungroup()
  ambiguous.assignments <- training.groups.by.year |>
    filter(n() > 1) |>
    ungroup()
  ambiguous.assignments |>
    left_join(training.groups.overall, by = join_by(athlete)) |>
    select(athlete, year, training_group = training_group.y, races = races.y) |>
    distinct() |>
    bind_rows(unambiguous.assignments) |>
    rename(person_training_group = training_group) |>
    select(!races) |>
    mutate(person_training_group = factor(person_training_group, levels = training.groups))
}

# Source - https://stackoverflow.com/a
# Posted by John Colby, modified by community. See post 'Timeline' for change history
# Retrieved 2025-11-23, License - CC BY-SA 3.0
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

plot_person_training_group_popularity <- function (data) {
  custom_colors = c(gg_color_hue(4), "#888")
  data |>
    ggplot(aes(x = year, fill = person_training_group)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = seq(2017, current.year, by = 1)) +
    scale_fill_manual(values = custom_colors) +
    labs(
      title = "Popularity of training groups",
      x = "Year",
      y = "Number of races",
      fill = "Race distance"
    ) +
    theme(legend.position = "bottom")
}

plot_race_training_group_popularity <- function (data) {
  data |>
    ggplot(aes(x = year(date), fill = training_group)) +
    geom_bar(position = "dodge") +
    scale_x_continuous(breaks = seq(2017, current.year, by = 1)) +
    labs(
      title = "Popularity of race distances",
      x = "Year",
      y = "Number of races",
      fill = "Race distance"
    ) +
    theme(legend.position = "bottom")
}

main <- function (training.group.source = "Race", cache = FALSE) {
  performances <- get_performance_data(cache) |>
    explode_relay_legs() |>
    filter(
      !(is.na(flag) | !(flag %in% c("Team", "Pacer")))
      & !(discipline %in% c("Duathlon", "Triathlon"))
      & tolower(distance_label) != "distance relay"
      & !is.na(distance_km)
    ) |>
    assign_races_to_training_groups()
  if (training.group.source == "Race") {
    performances |>
      plot_race_training_group_popularity()
  } else if (training.group.source == "Person") {
    roster <- fetch.roster(cache) |>
      clean_names()
    years <- tibble(year = 2017:current.year)
    roster.by.year <- roster |>
      select(name, status, from, to) |>
      mutate(from = year(from), to = year(coalesce(to, Sys.Date()))) |>
      inner_join(years, by = join_by(from <= year, to >= year)) |>
      select(name, year) |>
      distinct()
    assignments <- performances |>
      assign_people_to_training_groups()
    roster.by.year |>
      left_join(assignments, by = join_by(name == athlete, year == year)) |>
      mutate(
        person_training_group = person_training_group |>
          coalesce("Did not race") |>
          factor(levels = c(training.groups, "Did not race"))
      ) |>
      plot_person_training_group_popularity()
  } else {
    stop(paste("Unknown training group source", training.group.source))
  }
}
