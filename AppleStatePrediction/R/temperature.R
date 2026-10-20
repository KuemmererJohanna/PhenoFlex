# ============================================================
# R/temperature.R
# Temperature data preparation and calculations
# ============================================================

library(dplyr)
library(lubridate)


# prepare hourly temperature for usage

prepare_hourly_temperature <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      datetime = ymd_hms(
        date,
        quiet = TRUE
      ),
      
      year = year(datetime),
      month = month(datetime),
      day = day(datetime),
      hour = hour(datetime),
      
      date_only = as.Date(datetime)
    )
}


# prepare average temperature for usage

prepare_average_temperature <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      datetime = ymd_hms(
        date,
        quiet = TRUE
      ),
      
      month = month(datetime),
      day = day(datetime),
      hour = hour(datetime)
    )
}


# filter temperature data by a certain year

filter_temperature_year <- function(
    data,
    year
) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    filter(
      .data$year == as.integer(year)
    )
}


# map the historical average to a certain year

map_average_to_year <- function(
    average_data,
    year
) {
  
  if (is.null(average_data) || nrow(average_data) == 0) {
    return(data.frame())
  }
  
  average_data %>%
    mutate(
      date = make_datetime(
        year = as.integer(year),
        month = month,
        day = day,
        hour = hour
      )
    )
}


# create daily temperature from given data set 


calculate_daily_temperature <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      date_only = as.Date(datetime)
    ) %>%
    group_by(date_only) %>%
    summarise(
      tmin = if (all(is.na(tmin))) {
        NA_real_
      } else {
        min(tmin, na.rm = TRUE)
      },
      tmax = if (all(is.na(tmax))) {
        NA_real_
      } else {
        max(tmax, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    rename(date = date_only)
}



#
# REST PROBABLY UNNECESSARY
#

# prepare measured data for temperature diagram

prepare_current_year_temperature <- function(
    hourly_data,
    selected_year
) {
  
  hourly_data %>%
    filter(
      year == as.integer(selected_year)
    ) %>%
    calculate_daily_temperature()
}


# prepare historical average temperature for the diagram

prepare_average_temperature_for_year <- function(
    average_data,
    year
) {
  
  average_data %>%
    map_average_to_year(year) %>%
    calculate_daily_temperature()
}


# probably unnecessary function
# get time range of data set

get_temperature_range <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(c(NA_real_, NA_real_))
  }
  
  c(
    min(data$tmin, na.rm = TRUE),
    max(data$tmax, na.rm = TRUE)
  )
}

# Retain historical observations inside the percentile range

filter_percentile_range <- function(
    x,
    lower_percentile,
    upper_percentile
) {
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(numeric(0))
  }
  
  if (lower_percentile == 0 && upper_percentile == 100) {
    return(x)
  }
  
  bounds <- quantile(
    x,
    probs = c(
      lower_percentile / 100,
      upper_percentile / 100
    ),
    names = FALSE,
    na.rm = TRUE,
    type = 7
  )
  
  x[x >= bounds[1] & x <= bounds[2]]
}



# Calculate cold and warm temperature scenarios for each
# calendar day across all historical years.

prepare_temperature_percentiles <- function(
    data,
    lower_percentile,
    upper_percentile
) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    mutate(
      date = as.Date(date),
      month = lubridate::month(date),
      day = lubridate::day(date)
    ) %>%
    group_by(month, day) %>%
    summarise(
      
      # Cold scenario: lower selected percentile
      tmin_cold = {
        x <- tmin[is.finite(tmin)]
        if (length(x)) {
          as.numeric(quantile(
            x,
            probs = lower_percentile / 100,
            na.rm = TRUE,
            type = 7
          ))
        } else {
          NA_real_
        }
      },
      
      tmax_cold = {
        x <- tmax[is.finite(tmax)]
        if (length(x)) {
          as.numeric(quantile(
            x,
            probs = lower_percentile / 100,
            na.rm = TRUE,
            type = 7
          ))
        } else {
          NA_real_
        }
      },
      
      # Warm scenario: upper selected percentile
      tmin_warm = {
        x <- tmin[is.finite(tmin)]
        if (length(x)) {
          as.numeric(quantile(
            x,
            probs = upper_percentile / 100,
            na.rm = TRUE,
            type = 7
          ))
        } else {
          NA_real_
        }
      },
      
      tmax_warm = {
        x <- tmax[is.finite(tmax)]
        if (length(x)) {
          as.numeric(quantile(
            x,
            probs = upper_percentile / 100,
            na.rm = TRUE,
            type = 7
          ))
        } else {
          NA_real_
        }
      },
      
      .groups = "drop"
    )
}
