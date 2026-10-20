# ============================================================
# R/phenology.R
# Phenological data and calculations
# ============================================================

library(dplyr)
library(tidyr)


# ============================================================
# PHENOLOGY COLUMN NAMES
# ============================================================

phenology_states <- c(
  "bud_break",
  "mouse_ear_stage",
  "green_bud_stage",
  "pink_bud_stage",
  "flowering_begin",
  "full_flowering",
  "flowering_end",
  "t_stage",
  "harvest_start"
)


# ============================================================
# GET PHENOLOGY FOR YEAR
# ============================================================

get_phenology_for_year <- function(year) {
  
  data <- get_phenology_year(year)
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  data %>%
    pivot_longer(
      cols = any_of(phenology_states),
      names_to = "state",
      values_to = "date"
    ) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    filter(
      !is.na(date)
    )
}


# ============================================================
# CALCULATE DIFFERENCE BETWEEN TWO DATES
# ============================================================

get_difference_days <- function(
    date_1,
    date_2
) {
  
  if (
    is.na(date_1) ||
    is.na(date_2)
  ) {
    return(NA_integer_)
  }
  
  as.integer(
    date_2 - date_1
  )
}


# ============================================================
# CALCULATE DIFFERENCES TO CURRENT DATE
# ============================================================

calculate_phenology_differences <- function(
    phenology_data,
    current_date
) {
  
  if (
    is.null(phenology_data) ||
    nrow(phenology_data) == 0
  ) {
    return(data.frame())
  }
  
  phenology_data %>%
    mutate(
      difference_days = as.integer(
        date - current_date
      )
    )
}