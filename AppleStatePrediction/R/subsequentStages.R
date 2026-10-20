




# R/subsequentStages.R

# Predict subsequent phenological stages from a predicted budBreak date.
#
# hourly_data:
#   Hourly temperature data compatible with chillR::GDH_asymcur().
#   Must contain DATE and JDay columns, as in the supplied example.
#
# budbreak_date:
#   The predicted budBreak date supplied by PhenoFlex.
#
# Returns:
#   A one-row data frame containing the starting date and the predicted
#   dates of all subsequent phenological stages.


predict_subsequent_stages <- function(
    hourly_data,
    budbreak_date,
    Tb = 1.5,
    Topt = 34.402,
    Tcrit = 34.412,
    stage_thresholds = c(
      mausohr        = 1333,
      gruenknospen   = 1100,
      rotknospen     = 820,
      bluetenbeginn  = 1548,
      vollbluete     = 989,
      bluetenende    = 2883,
      t_stadium      = 11437,
      erntebeginn    = 61012
    )
) {
  
  # 1. Validate inputs
  if (is.null(hourly_data) || nrow(hourly_data) == 0) {
    stop("No hourly temperature data available.")
  }
  
  budbreak_date <- as.Date(budbreak_date)
  
  if (length(budbreak_date) != 1L || is.na(budbreak_date)) {
    stop("A valid predicted bud-break date is required.")
  }
  
  data <- as.data.frame(hourly_data)
  
  # 2. Normalize the date column
  if ("DATE" %in% names(data)) {
    data$Date <- as.Date(data$DATE)
  } else if ("Date" %in% names(data)) {
    data$Date <- as.Date(data$Date)
  } else {
    stop("Hourly data must contain DATE or Date.")
  }
  
  # 3. Validate required hourly columns
  if (!"Temp" %in% names(data)) {
    stop("Hourly data must contain Temp.")
  }
  
  if (!"Hour" %in% names(data)) {
    stop("Hourly data must contain Hour.")
  }
  
  # 4. Create the calendar fields required by fruclimadapt
  data <- data |>
    dplyr::mutate(
      Year  = as.integer(format(Date, "%Y")),
      Month = as.integer(format(Date, "%m")),
      Day   = as.integer(format(Date, "%d")),
      DOY   = as.integer(format(Date, "%j")),
      Hour  = as.numeric(Hour),
      Temp  = as.numeric(Temp)
    ) |>
    dplyr::filter(
      Date >= budbreak_date,
      is.finite(Temp),
      is.finite(Hour)
    ) |>
    dplyr::arrange(Date, Hour)
  
  if (nrow(data) == 0) {
    stop("No hourly temperature data on or after bud break.")
  }
  
  if (!any(data$Date == budbreak_date)) {
    stop(
      "The predicted bud-break date ",
      format(budbreak_date, "%d.%m.%Y"),
      " is not present in the hourly temperature data."
    )
  }
  
  # 5. Pass only the required columns to GDH_asymcur.
  # Do not pass Date, DATE, or other date-valued columns.
  GDH_input <- data |>
    dplyr::select(
      Year, Month, Day, DOY, Hour, Temp
    ) |>
    as.data.frame()
  
  GDH_daily <- fruclimadapt::GDH_asymcur(
    GDH_input,
    Tb = Tb,
    Topt = Topt,
    Tcrit = Tcrit
  )
  
  GDH_daily <- as.data.frame(GDH_daily)
  
  if (!"GDH" %in% names(GDH_daily)) {
    stop("GDH_asymcur() did not return a GDH column.")
  }
  
  # 6. Normalize the daily date column returned by GDH_asymcur.
  if ("Date" %in% names(GDH_daily)) {
    GDH_daily$Date <- as.Date(GDH_daily$Date)
  } else if (all(c("Year", "Month", "Day") %in% names(GDH_daily))) {
    GDH_daily$Date <- as.Date(
      sprintf(
        "%04d-%02d-%02d",
        GDH_daily$Year,
        GDH_daily$Month,
        GDH_daily$Day
      )
    )
  } else {
    stop(
      "GDH_asymcur() must return Date or Year, Month, and Day columns."
    )
  }
  
  GDH_daily <- GDH_daily |>
    dplyr::filter(
      !is.na(Date),
      Date >= budbreak_date,
      is.finite(GDH)
    ) |>
    dplyr::arrange(Date) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      GDH = sum(GDH),
      .groups = "drop"
    ) |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      sumGDH = cumsum(GDH)
    )
  
  if (nrow(GDH_daily) == 0) {
    stop("No valid daily GDH values were returned.")
  }
  
  # 7. Calculate cumulative stage targets.
  cumulative_targets <- cumsum(stage_thresholds)
  
  get_stage_date <- function(target) {
    
    reached <- GDH_daily |>
      dplyr::filter(sumGDH >= target) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(Date)
    
    if (length(reached) == 0L || is.na(reached[1])) {
      return(as.Date(NA))
    }
    
    as.Date(reached[1])
  }
  
  predicted_dates <- lapply(
    cumulative_targets,
    get_stage_date
  )
  
  names(predicted_dates) <- names(stage_thresholds)
  
  # 8. Return one row containing all predicted stage dates.
  result <- data.frame(
    knospenaufbruch = budbreak_date
  )
  
  for (stage in names(predicted_dates)) {
    result[[stage]] <- predicted_dates[[stage]]
  }
  
  result
}







build_subsequent_stages_table <- function(
    early_predictions,
    late_predictions,
    reference_date
) {
  if (is.null(early_predictions) ||
      is.null(late_predictions) ||
      nrow(early_predictions) == 0 ||
      nrow(late_predictions) == 0) {
    return(NULL)
  }
  
  reference_date <- as.Date(reference_date)
  
  stages <- c(
    "knospenaufbruch",
    "mausohr",
    "gruenknospen",
    "rotknospen",
    "bluetenbeginn",
    "vollbluete",
    "bluetenende",
    "t_stadium",
    "erntebeginn"
  )
  
  # Ensure both inputs contain the expected stage columns.
  missing_early <- setdiff(stages, names(early_predictions))
  missing_late  <- setdiff(stages, names(late_predictions))
  
  if (length(missing_early) > 0) {
    stop(
      "Missing columns in early_predictions: ",
      paste(missing_early, collapse = ", ")
    )
  }
  
  if (length(missing_late) > 0) {
    stop(
      "Missing columns in late_predictions: ",
      paste(missing_late, collapse = ", ")
    )
  }
  
  early_dates <- as.Date(
    unlist(early_predictions[1, stages], use.names = FALSE),
    origin = "1970-01-01"
  )
  
  late_dates <- as.Date(
    unlist(late_predictions[1, stages], use.names = FALSE),
    origin = "1970-01-01"
  )
  
  stage_labels <- c(
    knospenaufbruch = "Knospenaufbruch",
    mausohr = "Mausohr",
    gruenknospen = "Grünknospen",
    rotknospen = "Rotknospen",
    bluetenbeginn = "Blütenbeginn",
    vollbluete = "Vollblüte",
    bluetenende = "Blütenende",
    t_stadium = "T-Stadium",
    erntebeginn = "Erntebeginn"
  )
  
  midpoint_dates <- as.Date(
    rep(NA_real_, length(stages)),
    origin = "1970-01-01"
  )
  
  for (i in seq_along(stages)) {
    if (!is.na(early_dates[i]) && !is.na(late_dates[i])) {
      midpoint_dates[i] <- as.Date(
        round(
          mean(as.numeric(c(early_dates[i], late_dates[i])))
        ),
        origin = "1970-01-01"
      )
    }
  }
  
  days_until <- as.integer(midpoint_dates - reference_date)
  
  data.frame(
    `Phenological stage` = unname(stage_labels[stages]),
    Early = early_dates,
    Late = late_dates,
    `Days until` = days_until,
    check.names = FALSE
  )
}

