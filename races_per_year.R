source("histograms.R")

fetch.roster <- function (cache = FALSE) {
  file.path <- "roster.csv"
  if (cache & file.exists(file.path)) {
    return(read_csv(file.path))
  }
  columns <- data.frame(
    name = c("Name", "Slack status", "Status", "Required?", "Date joined", "Date left", "From", "To", "Days", "Required", "Attended", "RSVPed", "Total days", "Events/day", "Events/week", "Events/month", "Deficit/Surplus", "Last event", "Last reach-out", "Monday Strength", "Wednesday Strength", "Strength Average", "Duniwednesday", "Open Gym", "Foodie Friday", "Team Race", "Social", "Long Run", "Recorded from", "Days not recorded"),
    type = c("c",    "c",            "c",       "l",        "D",           "D",         "D",    "D",  "d",    "d",        "d",        "d",      "d",          "d",          "d",           "d",            "d",               "D",          "D",              "d",               "d",                  "d",                "d",             "d",        "d",             "d",         "d",      "d",        "D",             "d")
  )
  data <- read_sheet(
    "https://docs.google.com/spreadsheets/d/18VXvuxgnlPdGizA4prGbejZdAbWws7DwK_CE-u_qdzA/",
    "Roster",
    col_types = paste(columns$type, collapse = "")
  ) |>
    select(!`Last event`)
  write.csv(data, file.path, row.names = FALSE)
  data
}

count.races <- function (performances, roster) {
  races.by.athlete <- performances |>
    filter(is.na(Flag) | Flag != "Relay") |>
    transmute(
      athlete = Athlete,
      race = Race,
      gender = Gender,
      year = year(Date),
      date = ymd(Date),
      distance = gsub(" k", "k", Distance),
      chip_time = ifelse(is.na(`Chip Time`), `Gun Time`, `Chip Time`)
    ) |>
    filter(year == 2024 & !(gender %in% c("Female team", "Male team", "Male Team", "Female Team"))) |>
    group_by(athlete) |>
    tally()
  start.date <- as.Date("2024-01-01")
  end.date <- as.Date("2025-01-01")
  roster |>
    filter(`Date joined` < end.date & (is.na(`Date left`) | `Date left` >= start.date)) |>
    transmute(
      athlete = Name,
      from = as.Date(ifelse(`Date joined` < start.date, start.date, `Date joined`)),
      to = as.Date(ifelse(`Date left` >= end.date | is.na(`Date left`), end.date, `Date left`))
    ) |>
    group_by(athlete) |>
    summarise(from = min(from), to = max(to)) |>
    mutate(days = to - from, expansion_factor = 366 / as.numeric(to - from)) |>
    left_join(races.by.athlete) |>
    mutate(n = ifelse(is.na(n), 0, n)) |>
    mutate(expanded_n = n * expansion_factor)
}

plot <- function (data) {
  ggplot(data, aes(x = expanded_n)) +
    geom_histogram(boundary = 0) +
    ggtitle("Distribution of times raced in 2024") +
    xlab("Number of races") +
    ylab("Count of teammates")
}

main <- function (argv = c()) {
  performances <- fetch.data("--cache" %in% argv)
  roster <- fetch.roster("--cache" %in% argv)
  count.races(performances, roster) |>
    plot()
}
