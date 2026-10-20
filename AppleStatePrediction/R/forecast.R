# ============================================================
# WEATHER FORECAST
# ============================================================


# weather_api.R

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)


get_weather_forecast <- function(district_id) {
  
  url <- paste0(
    "https://api-weather.services.siag.it/api/v2/district/",
    district_id,
    "/bulletin?format=json&lang=de"
  )
  
  response <- httr::RETRY(
    "GET",
    url,
    times = 3,
    pause_min = 1,
    pause_cap = 4,
    httr::timeout(20)
  )
  
  httr::stop_for_status(response)
  
  jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    ),
    simplifyVector = TRUE
  )
}



prepare_forecast_data <- function(district_id) {
  
  raw <- get_weather_forecast(district_id)
  
  # Inspect the actual API structure if needed:
  # str(raw, max.level = 3)
  
  forecast <- tibble::as_tibble(raw$forecasts)
  
  forecast %>%
    transmute(
      date = as.Date(date),
      tmin = as.numeric(temperatureMin),
      tmax = as.numeric(temperatureMax)
    ) %>%
    arrange(date)
}