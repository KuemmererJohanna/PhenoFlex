
# R/bloomPrediction.R

# Predict early and late full-bloom dates for Golden Delicious
# using measured temperatures, the weather forecast, historical
# temperature scenarios, and the fitted PhenoFlex parameters.



run_bloom_phenoflex <- function(hourly_data, parameters) {
  
  required_cols <- c("DATE", "Hour", "Temp")
  missing_cols <- setdiff(required_cols, names(hourly_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Hourly data is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  hourly_data <- hourly_data %>%
    dplyr::mutate(DATE = as.Date(DATE)) %>%
    dplyr::filter(is.finite(Temp)) %>%
    dplyr::arrange(DATE, Hour)
  
  if (nrow(hourly_data) == 0) {
    stop("No valid hourly temperatures available for PhenoFlex.")
  }
  
  if (!all(c(
    "yc", "zc", "s1", "Tu", "E0", "E1",
    "A0", "A1", "Tf", "Tc", "Tb", "slope"
  ) %in% names(parameters))) {
    stop("The PhenoFlex parameter vector has missing or incorrect names.")
  }
  
  result <- chillR::PhenoFlex(
    temp = hourly_data$Temp,
    times = seq_along(hourly_data$Temp),
    A0 = parameters[["A0"]],
    A1 = parameters[["A1"]],
    E0 = parameters[["E0"]],
    E1 = parameters[["E1"]],
    slope = parameters[["slope"]],
    Tf = parameters[["Tf"]],
    s1 = parameters[["s1"]],
    Tu = parameters[["Tu"]],
    Tb = parameters[["Tb"]],
    Tc = parameters[["Tc"]],
    zc = parameters[["zc"]],
    stopatzc = TRUE,
    yc = parameters[["yc"]],
    basic_output = FALSE
  )
  
  bloom_index <- result$bloomindex
  
  if (is.null(bloom_index) ||
      length(bloom_index) != 1 ||
      is.na(bloom_index) ||
      !is.finite(bloom_index) ||
      bloom_index < 1 ||
      bloom_index > nrow(hourly_data)) {
    
    message("PhenoFlex result structure:")
    print(str(result))
    
    message("Bloom index:")
    print(bloom_index)
    
    message("Hourly input row count: ", nrow(hourly_data))
    message("Hourly input date range:")
    print(range(hourly_data$DATE, na.rm = TRUE))
    
    stop("PhenoFlex returned no valid bloom index; see diagnostic output.")
  }
  
  as.Date(hourly_data$DATE[as.integer(bloom_index)])
}




addDailyYearColumns <- function(temp_data) {
  
  temp_data <- temp_data %>%
    dplyr::mutate(
      date = as.Date(date),
      tmin = as.numeric(tmin),
      tmax = as.numeric(tmax)
    ) %>%
    dplyr::transmute(
      DATE = date,
      Tmin = tmin,
      Tmax = tmax,
      YEARMODA = as.integer(format(date, "%Y%m%d")),
      Year = as.integer(format(date, "%Y")),
      Month = as.integer(format(date, "%m")),
      Day = as.integer(format(date, "%d"))
    )
  
  temp_data <- chillR::make_JDay(temp_data)
  
  temp_data
}




# ------------------------------------------------------------
# 1. Load the previously fitted PhenoFlex parameters
# ------------------------------------------------------------

load_phenoflex_parameters <- function(
    path
) {
  if (!file.exists(path)) {
    stop("PhenoFlex parameter file not found: ", path)
  }
  
  parameters <- chillR::read_tab(path)[, 2]
  parameters <- as.numeric(parameters)
  
  if (
    length(parameters) < 12 ||
    any(!is.finite(parameters[1:12]))
  ) {
    stop("The parameter file must contain 12 valid parameters.")
  }
  
  names(parameters) <- c(
    "yc", "zc", "s1", "Tu", "E0", "E1",
    "A0", "A1", "Tf", "Tc", "Tb", "slope"
  )
  
  parameters
}


# ------------------------------------------------------------
# 2. Convert daily minimum/maximum temperatures into hourly
#    temperatures using chillR
# ------------------------------------------------------------


bloom_daily_to_hourly <- function(
    daily_data,
    season_start,
    latitude = 46.3825
) {
  
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    stop("No daily temperature data available.")
  }
  
  required_cols <- c("date", "tmin", "tmax")
  missing_cols <- setdiff(required_cols, names(daily_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Use forecast temperatures if the corresponding observed
  # temperatures are unavailable.
  if (!"forecast_tmin" %in% names(daily_data)) {
    daily_data$forecast_tmin <- NA_real_
  }
  
  if (!"forecast_tmax" %in% names(daily_data)) {
    daily_data$forecast_tmax <- NA_real_
  }
  
  clean_daily <- daily_data |>
    dplyr::transmute(
      DATE = as.Date(date),
      Tmin = dplyr::coalesce(
        as.numeric(tmin),
        as.numeric(forecast_tmin)
      ),
      Tmax = dplyr::coalesce(
        as.numeric(tmax),
        as.numeric(forecast_tmax)
      )
    ) |>
    dplyr::filter(
      DATE >= as.Date(season_start),
      is.finite(Tmin),
      is.finite(Tmax),
      Tmin <= Tmax
    ) |>
    dplyr::arrange(DATE) |>
    dplyr::distinct(DATE, .keep_all = TRUE) |>
    dplyr::mutate(
      YEARMODA = as.integer(format(DATE, "%Y%m%d")),
      Year = as.integer(format(DATE, "%Y")),
      Month = as.integer(format(DATE, "%m")),
      Day = as.integer(format(DATE, "%d"))
    )
  
  if (nrow(clean_daily) == 0) {
    stop("No valid daily temperatures remain after filtering.")
  }
  
  clean_daily <- chillR::make_JDay(clean_daily)
  clean_daily <- as.data.frame(clean_daily)
  
  hourly_result <- chillR::stack_hourly_temps(
    weather = clean_daily,
    latitude = latitude
  )
  
  hourly_data <- hourly_result$hourtemps |>
    dplyr::filter(
      !is.na(DATE),
      is.finite(Temp)
    ) |>
    dplyr::arrange(DATE, Hour)
  
  if (nrow(hourly_data) == 0) {
    stop("Hourly temperature conversion returned no data.")
  }
  
  hourly_data
}




# ------------------------------------------------------------
# 3. Main function: predict early and late full-bloom dates
# ------------------------------------------------------------



predict_bloom_dates <- function(
    current_data,
    percentile_data,
    reference_date,
    season_start,
    latitude = 46.3825,
    parameter_path = "param/GoldenDeliciousFullBloomLaimburg.csv",
    historical_hourly_data = NULL
) {
  if (is.null(current_data) || nrow(current_data) == 0) {
    stop("No current-year temperature data available.")
  }
  
  if (is.null(percentile_data) || nrow(percentile_data) == 0) {
    stop("No historical temperature percentiles available.")
  }
  
  reference_date <- as.Date(reference_date)
  season_start <- as.Date(season_start)
  
  parameters <- load_phenoflex_parameters(parameter_path)
  
  # Daily temperatures for the current year, before the reference date.
  measured <- current_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date < reference_date) |>
    dplyr::transmute(
      date,
      tmin = as.numeric(tmin),
      tmax = as.numeric(tmax)
    ) |>
    dplyr::filter(is.finite(tmin), is.finite(tmax))
  
  # Actual forecast from the reference date onward.
  forecast <- current_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(
      date >= reference_date,
      is.finite(forecast_tmin),
      is.finite(forecast_tmax)
    ) |>
    dplyr::transmute(
      date,
      tmin = as.numeric(forecast_tmin),
      tmax = as.numeric(forecast_tmax)
    )
  
  forecast_end <- if (nrow(forecast) > 0) {
    max(forecast$date)
  } else {
    reference_date - 1
  }
  
  # Warm scenario for early bloom.
  warm_future <- percentile_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date > forecast_end) |>
    dplyr::transmute(
      date,
      tmin = as.numeric(tmin_warm),
      tmax = as.numeric(tmax_warm)
    )
  
  # Cold scenario for late bloom.
  cold_future <- percentile_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date > forecast_end) |>
    dplyr::transmute(
      date,
      tmin = as.numeric(tmin_cold),
      tmax = as.numeric(tmax_cold)
    )
  
  # Build the daily scenario inputs.
  assemble_scenario <- function(future_data) {
    dplyr::bind_rows(
      measured,
      forecast,
      future_data
    ) |>
      dplyr::filter(date >= season_start) |>
      dplyr::arrange(date) |>
      dplyr::distinct(date, .keep_all = TRUE)
  }
  
  early_daily <- assemble_scenario(warm_future)
  late_daily <- assemble_scenario(cold_future)
  
  # Convert daily temperatures into hourly estimates for now.
  early_hourly <- bloom_daily_to_hourly(
    daily_data = early_daily,
    season_start = season_start,
    latitude = latitude
  )
  
  late_hourly <- bloom_daily_to_hourly(
    daily_data = late_daily,
    season_start = season_start,
    latitude = latitude
  )
  
  # Actual historical hourly measurements will be integrated here
  # once their temperature column and format are confirmed.
  
  early_date <- run_bloom_phenoflex(
    hourly_data = early_hourly,
    parameters = parameters
  )
  
  late_date <- run_bloom_phenoflex(
    hourly_data = late_hourly,
    parameters = parameters
  )
  
  tibble::tibble(
    Scenario = c("Early", "Late"),
    PhenologicalStage = c("Full Bloom", "Full Bloom"),
    PredictedDate = as.Date(c(early_date, late_date))
  )
}
