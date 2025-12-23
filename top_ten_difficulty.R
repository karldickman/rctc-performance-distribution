library(dplyr)
library(lubridate)
library(stringr)

source("top_tens.R")
source("vdot_each_performance.R")

main <- function (cache = FALSE) {
  end.of.current.year <- Sys.Date() |>
    year() |>
    paste0("-12-31") |>
    as.Date()
  # Fetch
  distances.in.top.10.doc <- read_csv("distances_in_top_10_doc.csv", show_col_types = FALSE)
  performances <- get_performance_data(cache = cache)
  vdot <- get_vdot_data(cache)
  # Apply VDOT calculations
  vdot <- vdot |>
    filter(abs(1.6 / 1.609334 - distance_mi) > 0.00000001) |>
    select(!minutes)
  performances <- performances |>
    filter_vdottable_performances() |>
    convert.xc.times() |>
    interpolate.vdot(vdot) |>
    # Rank performances
    rank_times_as_of(end.of.current.year)
  # Apply gender and age categories
  categories <- c("Female", "Masters Female", "Male", "Masters Male", "Nonbinary")
  all.ages <- performances |>
    mutate(category = factor(gender, levels = categories))
  masters <- performances |>
    filter(age >= 40 & gender != "Nonbinary") |>
    select(!c(personal_rank, team_rank)) |>
    mutate(
      category = factor(paste("Masters", gender), levels = categories),
      personal_rank = masters_personal_rank,
      team_rank = masters_team_rank
    )
  performances <- bind_rows(all.ages, masters) |>
    select(!c(masters_personal_rank, masters_team_rank))
  # Count performances by category
  performance.counts <- performances |>
    filter(personal_rank == 1) |>
    group_by(category, distance_label, discipline) |>
    tally() |>
    rename(list_length = n)
  # Get current top 10s
  tenth.time <- performances |>
    filter(team_rank == 10)
  # Produce report for all distances in top 10 doc
  distances.in.top.10.doc |>
    filter(discipline != "Beer mile" & !str_detect(distance_label, "relay")) |>
    cross_join(tibble(category = factor(categories, levels = categories))) |>
    left_join(performance.counts, by = join_by(category, distance_label, discipline)) |>
    mutate(list_length = coalesce(list_length, 0)) |>
    left_join(tenth.time, by = join_by(category, distance_label, discipline)) |>
    select(
      category, distance = distance_label, discipline, list_length, race, date,
      finish_time = use_this_time, vdot
    ) |>
    arrange(category, coalesce(vdot, 0), list_length)
}
