source("histograms.R")

main <- function (argv = c()) {
  performances <- fetch.data("--cache" %in% argv)
  performances |>
    filter(Distance == "Half marathon" & Discipline == "Road" & !(`Gun Time` %in% c("TBD", "Not found"))) |>
    transmute(
      athlete = Athlete,
      race = Race,
      gender = Gender,
      year = year(Date),
      date = ymd(Date),
      distance = gsub(" k", "k", Distance),
      chip_time = ifelse(is.na(`Chip Time`), `Gun Time`, `Chip Time`)
    ) |>
    filter(!is.na(chip_time)) |>
    filter(gender != "Nonbinary") |>
    mutate(minutes = sapply(chip_time, parse.chip.time)) |>
    filter(minutes < 150) |>
    mutate(minutes = minutes.as.POSIXct(minutes)) |>
    ggplot(aes(x = date, y = minutes, col = gender)) +
    geom_point() +
    geom_smooth()
}
