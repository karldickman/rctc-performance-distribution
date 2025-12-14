library(mapproj)

main <- function (cache = FALSE) {
  # Fetch
  races <- get_race_data(cache)
  performances <- get_performance_data(include_relay_legs = TRUE, cache = cache)
  # Process
  races.by.state <- performances |>
    filter(is.na(flag) | flag != "Team") |>
    left_join(races, by = join_by(race, date, distance_label, discipline), relationship = "many-to-many") |>
    filter(country == "United States") |>
    group_by(state) |>
    tally() |>
    arrange(-n)
  # Map
  state.boundaries <- map_data("state", projection = "albers", parameters = c(39, 45)) |>
    inner_join(tibble(
      state = state.abb,
      region = tolower(state.name)
    ), by = join_by(region))
  choropleth <- state.boundaries |>
    left_join(races.by.state, by = join_by(state))
  choropleth |>
    ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = n), colour = alpha("white", 1 / 2), linewidth = 0.2) +
    geom_polygon(data = state.boundaries, colour = "white", fill = NA) +
    coord_fixed() +
    scale_fill_viridis(option = "magma", direction = -1, transform = "log10") +
    labs(
      title = "Number of races by U.S. state, 2017–present",
      fill = "Races (log scale)"
    ) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom"
    )
}
