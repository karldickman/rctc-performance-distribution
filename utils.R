minutes.as.POSIXct <- function (minutes) {
  as.POSIXct(minutes * 60, origin = "1970-01-01", tz = "UTC")
}
