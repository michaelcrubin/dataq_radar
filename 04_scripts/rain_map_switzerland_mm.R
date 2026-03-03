# Minimal Meteomatics rain-layer fetch for Switzerland
# Goal: set a date and retrieve one-day rain map grid data.

# Required env vars:
#   mm_user
#   mm_password
#
# Example:
#   Sys.setenv(mm_user = "your_user", mm_password = "your_password")
#   source("04_scripts/rain_map_switzerland_mm.R")
#   rain <- fetch_switzerland_rain_map("2025-06-15")
#   head(rain)

# Hard-coded defaults (minimal and explicit)
MM_BASE_URL <- "https://api.meteomatics.com"
SWITZERLAND_BBOX <- list(lat_n = 47.9, lon_w = 5.9, lat_s = 45.8, lon_e = 10.6)
GRID_RESOLUTION <- list(lat = 0.04, lon = 0.04)
RAIN_PARAMETER <- "precip_24h:mm"
MODEL_QUERY <- "model=mix"
TIME_RESOLUTION <- "P1D"

as_mm_timestamp <- function(date_string) {
  date <- as.Date(date_string)
  if (is.na(date)) stop("date_string must be a valid date in YYYY-MM-DD format")
  paste0(format(date, "%Y-%m-%d"), "T00:00:00.000+00:00")
}

build_grid_string <- function(bbox = SWITZERLAND_BBOX, resolution = GRID_RESOLUTION) {
  paste0(
    bbox$lat_n, ",", bbox$lon_w, "_",
    bbox$lat_s, ",", bbox$lon_e, ":",
    resolution$lat, ",", resolution$lon
  )
}

build_rain_map_url <- function(date_string,
                               parameter = RAIN_PARAMETER,
                               model_query = MODEL_QUERY,
                               timeres = TIME_RESOLUTION,
                               bbox = SWITZERLAND_BBOX,
                               resolution = GRID_RESOLUTION) {
  ts <- as_mm_timestamp(date_string)
  grid <- build_grid_string(bbox = bbox, resolution = resolution)

  paste0(
    MM_BASE_URL, "/",
    ts, "--", ts, ":", timeres, "/",
    parameter, "/", grid, "/csv?", model_query
  )
}

fetch_switzerland_rain_map <- function(date_string,
                                       user = Sys.getenv("mm_user"),
                                       password = Sys.getenv("mm_password")) {
  if (!nzchar(user) || !nzchar(password)) {
    stop("Missing credentials. Set environment variables mm_user and mm_password.")
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install it with install.packages('httr').")
  }

  url <- build_rain_map_url(date_string)

  response <- httr::GET(url, httr::authenticate(user = user, password = password))
  status <- httr::status_code(response)
  if (status >= 300) {
    stop(paste0("Meteomatics request failed with HTTP ", status, ". URL: ", url))
  }

  raw_csv <- httr::content(response, type = "text", encoding = "UTF-8")

  out <- utils::read.csv(
    text = raw_csv,
    sep = ";",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out
}

save_rain_map_csv <- function(date_string, path = NULL) {
  data <- fetch_switzerland_rain_map(date_string)

  if (is.null(path)) {
    path <- paste0("rain_map_switzerland_", gsub("-", "", date_string), ".csv")
  }

  utils::write.csv(data, file = path, row.names = FALSE)
  path
}

# Optional CLI mode:
#   Rscript 04_scripts/rain_map_switzerland_mm.R 2025-06-15
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1) {
  target_date <- args[[1]]
  file_path <- save_rain_map_csv(target_date)
  message("Saved rain map to: ", file_path)
}
