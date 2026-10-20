library(shiny)


source("R/database.R")
source("R/temperature.R")
source("R/phenology.R")
source("R/forecast.R")
source("R/plots.R")
source("R/import_api_temperature.R")
source("R/bloomPrediction.R")
source("R/subsequentStages.R")



server <- function(input, output, session) {
  
  # Trigger for reloading data and redrawing plots
  data_refresh <- reactiveVal(0)
  
  observeEvent(input$btn_settings, {
    
    showModal(
      modalDialog(
        title = "Settings",
        
        selectInput(
          "setting_location",
          "Temperature location",
          choices = c("Laimburg", "Eyrs", "Fragsburg", "Dietenheim", "Marienberg", "Meran Gratsch", "Naturns", "Gargazon", "Brixen Vahrn", "Bozen", "Auer", "Kaltern Oberplanitzing")
        ),
        
        selectInput(
          "setting_variety",
          "Apple variety",
          choices = c("Golden Delicious", "Cripps Pink", "Gala Schnitzer")
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            "btn_save_settings",
            "Save",
            class = "btn-primary"
          )
        ),
        
        easyClose = TRUE
      )
    )
  })
  
  
  
  
  # Refresh air-temperature data when the button is clicked
  observeEvent(input$btn_previous, {
    
    showNotification(
      "Refreshing air-temperature data...",
      type = "message",
      duration = NULL,
      id = "temperature_refresh"
    )
    
    tryCatch({
      
      import_api_temperature()
      
      removeNotification("temperature_refresh")
      
      showNotification(
        "Air-temperature data refresh completed.",
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      
      removeNotification("temperature_refresh")
      
      showNotification(
        paste("Data refresh failed:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )
      
    })
    
  }, ignoreInit = TRUE)
  
  
  
  # Initial saved settings
  saved_location <- reactiveVal("Laimburg")
  saved_variety <- reactiveVal("Golden Delicious")
  
  
  # Save button
  observeEvent(input$btn_save_settings, {
    
    saved_location(input$setting_location)
    saved_variety(input$setting_variety)
    
    removeModal()
    
  })
  
  output$app_title <- renderText({
    
    paste(
      "PHENOLOGY of",
      saved_variety(),
      "in",
      saved_location()
    )
    
  })
  
  # date of current prediction
  
  current_date <- reactive({
    
    req(input$input_date)
    
    result <- as.Date(input$input_date)
    
    result
  })
  
  
  
  # Session-level cache for the local weather forecast.
  # The API is called only when the cache is empty.
  
  forecast_cache <- shiny::reactiveVal(NULL)
  
  get_cached_weather_forecast <- function(district_id = 1) {
    
    cached <- shiny::isolate(forecast_cache())
    
    if (!is.null(cached)) {
      return(cached)
    }
    
    forecast <- prepare_forecast_data(
      district_id = district_id
    )
    
    forecast_cache(forecast)
    
    forecast
  }
  
  
  
  
  current_year_temperature <- reactive({
    
    temperature_data <- prepare_current_year_temperature(
      prepared_hourly_temperature(),
      current_year()
    ) |>
      dplyr::mutate(date = as.Date(date))
    
    website_date <- as.Date(current_date())
    actual_date <- Sys.Date()
    
    # Fetch the forecast only when the selected date is today.
    if (identical(website_date, actual_date)) {
      
      forecast_data <- get_cached_weather_forecast(
        district_id = 1
      ) |>
        dplyr::mutate(date = as.Date(date)) |>
        dplyr::filter(
          date >= actual_date,
          date <= actual_date + 6
        ) |>
        dplyr::transmute(
          date,
          forecast_tmin = tmin,
          forecast_tmax = tmax
        )
      
      temperature_data <- dplyr::full_join(
        temperature_data,
        forecast_data,
        by = "date"
      ) |>
        dplyr::arrange(date)
      
    } else {
      
      temperature_data <- temperature_data |>
        dplyr::mutate(
          forecast_tmin = NA_real_,
          forecast_tmax = NA_real_
        )
    }
    
    temperature_data
  })
  
  
  effective_current_date <- reactive({
    
    data <- prepared_hourly_temperature()
    
    req(!is.null(data), nrow(data) > 0)
    
    available_dates <- data$date_only[
      !is.na(data$date_only)
    ]
    
    req(length(available_dates) > 0)
    
    min(
      as.Date(current_date()),
      max(available_dates)
    )
  })
  
  
  output$output_effective_date <- renderText({
    paste(
      "Prediction reference date:",
      format(effective_current_date(), "%d %B %Y")
    )
  })
  
  
  
  # current year
  
  current_year <- reactive({
    
    as.integer(
      format(
        current_date(),
        "%Y"
      )
    )
  })
  
  
  # get hourly temperature
  
  hourly_temperature <- reactive({
    
    get_hourly_temperature()
    
  })
  
  
  # prepare hourly temperature
  
  prepared_hourly_temperature <- reactive({
    
    prepare_hourly_temperature(
      hourly_temperature()
    )
    
  })
  
  
  bloom_season_hourly <- reactive({
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    prepared_hourly_temperature() %>%
      filter(
        !is.na(datetime),
        date_only >= season_start,
        datetime <= as.POSIXct(reference_date + 1, tz = "UTC")
      ) %>%
      arrange(datetime)
  })
  
  
  
  
  # prepare average hourly temperature
  
  average_temperature <- reactive({
    
    data <- get_average_hourly_temperature()
    
    prepare_average_temperature(
      data
    )
    
  })
  
  
  
  # prepare average temperature for current year
  
  average_temperature_current_year <- reactive({
    
    prepare_average_temperature_for_year(
      average_temperature(),
      current_year()
    )
    
  })
  
  
  
  # Historical temperature percentiles based on daily extremes
  
  
  temperature_percentiles <- reactive({
    raw_data <- get_raw_temperature_data()
    
    req(!is.null(raw_data))
    req(nrow(raw_data) > 0)
    req(all(c("ts", "value") %in% names(raw_data)))
    req(length(input$input_range) == 2)
    
    lower <- input$input_range[1]
    upper <- input$input_range[2]
    
    if (
      length(lower) != 1 ||
      length(upper) != 1 ||
      !is.finite(lower) ||
      !is.finite(upper) ||
      lower < 0 ||
      upper > 100 ||
      lower > upper
    ) {
      validate(
        need(FALSE, "Select a valid percentile range.")
      )
    }
    
    daily_data <- raw_data %>%
      mutate(date = as.Date(ts)) %>%
      filter(!is.na(date)) %>%
      group_by(date) %>%
      summarise(
        tmin = {
          x <- value[is.finite(value)]
          if (length(x)) min(x) else NA_real_
        },
        tmax = {
          x <- value[is.finite(value)]
          if (length(x)) max(x) else NA_real_
        },
        .groups = "drop"
      )
    
    prepare_temperature_percentiles(
      data = daily_data,
      lower_percentile = lower,
      upper_percentile = upper
    )
  })
  
  
  
 
  
  # Expand historical percentile scenarios to every date
  # in the selected year
  
  temperature_percentiles_current_year <- reactive({
    
    data <- temperature_percentiles()
    
    req(nrow(data) > 0)
    
    year <- current_year()
    
    all_dates <- data.frame(
      date = seq.Date(
        from = as.Date(sprintf("%d-01-01", year)),
        to = as.Date(sprintf("%d-12-31", year)),
        by = "day"
      )
    )
    
    all_dates %>%
      mutate(
        month = lubridate::month(date),
        day = lubridate::day(date)
      ) %>%
      left_join(
        data,
        by = c("month", "day")
      ) %>%
      select(
        date,
        tmin_cold,
        tmax_cold,
        tmin_warm,
        tmax_warm
      )
  })
  
  
  # get phenology for current year
  
  phenology_current_year <- reactive({
    
    if (
      !isTRUE(input$setting_show_phenology)
    ) {
      
      return(data.frame())
    }
    
    get_phenology_for_year(
      current_year()
    )
    
  })
  
  
  
  bloom_predictions <- reactive({
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    predict_bloom_dates(
      current_data = bloom_temperature_data(),
      percentile_data = bloom_temperature_percentiles(),
      reference_date = reference_date,
      season_start = season_start
    )
  })
  
  
  output$bloom_early_date <- renderText({
    predictions <- bloom_predictions()
    
    early_date <- predictions$PredictedDate[
      predictions$Scenario == "Early"
    ]
    
    if (length(early_date) == 0 || is.na(early_date[1])) {
      return("Unavailable")
    }
    
    format(as.Date(early_date[1]), "%d.%m.%Y")
  })
  
  
  output$bloom_late_date <- renderText({
    predictions <- bloom_predictions()
    
    late_date <- predictions$PredictedDate[
      predictions$Scenario == "Late"
    ]
    
    if (length(late_date) == 0 || is.na(late_date[1])) {
      return("Unavailable")
    }
    
    format(as.Date(late_date[1]), "%d.%m.%Y")
  })
  
  
  budBreak_predictions <- reactive({
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    predict_bloom_dates(
      current_data = bloom_temperature_data(),
      percentile_data = bloom_temperature_percentiles(),
      reference_date = reference_date,
      season_start = season_start,
      parameter_path = "param/GoldenDeliciousBudBreakLaimburg.csv",
    )
  })
  
  
  output$budBreak_early_date <- renderText({
    predictions <- budBreak_predictions()
    
    early_date <- predictions$PredictedDate[
      predictions$Scenario == "Early"
    ]
    
    if (length(early_date) == 0 || is.na(early_date[1])) {
      return("Unavailable")
    }
    
    format(as.Date(early_date[1]), "%d.%m.%Y")
  })
  
  
  output$budBreak_late_date <- renderText({
    predictions <- budBreak_predictions()
    
    late_date <- predictions$PredictedDate[
      predictions$Scenario == "Late"
    ]
    
    if (length(late_date) == 0 || is.na(late_date[1])) {
      return("Unavailable")
    }
    
    format(as.Date(late_date[1]), "%d.%m.%Y")
  })
  
  
  
  # Convert the existing daily temperature data only once.
  subsequent_hourly_data <- reactive({
    daily_data <- bloom_temperature_data()
    
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    bloom_daily_to_hourly(
      daily_data = daily_data,
      season_start = season_start
    )
  })
  
  # Predict subsequent stages from the Early bud-break date.
  subsequent_early_predictions <- reactive({
    budbreak_date <- budBreak_predictions() |>
      dplyr::filter(Scenario == "Early") |>
      dplyr::pull(PredictedDate)
    
    if (length(budbreak_date) == 0 || is.na(budbreak_date[1])) {
      return(NULL)
    }
    
    predict_subsequent_stages(
      hourly_data = subsequent_hourly_data(),
      budbreak_date = budbreak_date[1]
    )
  })
  
  # Predict subsequent stages from the Late bud-break date.
  subsequent_late_predictions <- reactive({
    budbreak_date <- budBreak_predictions() |>
      dplyr::filter(Scenario == "Late") |>
      dplyr::pull(PredictedDate)
    
    if (length(budbreak_date) == 0 || is.na(budbreak_date[1])) {
      return(NULL)
    }
    
    predict_subsequent_stages(
      hourly_data = subsequent_hourly_data(),
      budbreak_date = budbreak_date[1]
    )
  })
  
  # Combine all stages into a single table.
  
  subsequent_stages_table <- reactive({
    
    early_stages <- subsequent_early_predictions()
    late_stages  <- subsequent_late_predictions()
    
    reference_date <- as.Date(effective_current_date())
    
    # Diagnostic output appears in the R console.
    message("---- Subsequent stages diagnostic ----")
    
    if (is.null(early_stages) || nrow(early_stages) == 0) {
      message("Early predictions are empty.")
      return(NULL)
    }
    
    if (is.null(late_stages) || nrow(late_stages) == 0) {
      message("Late predictions are empty.")
      return(NULL)
    }
    
    message("Early prediction columns:")
    print(names(early_stages))
    print(early_stages)
    
    message("Late prediction columns:")
    print(names(late_stages))
    print(late_stages)
    
    # Expected stage columns in the prediction results.
    stage_cols <- c(
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
    
    # Check that the prediction functions returned all stages.
    missing_early <- setdiff(stage_cols, names(early_stages))
    missing_late  <- setdiff(stage_cols, names(late_stages))
    
    if (length(missing_early) > 0) {
      stop(
        "Early predictions are missing columns: ",
        paste(missing_early, collapse = ", ")
      )
    }
    
    if (length(missing_late) > 0) {
      stop(
        "Late predictions are missing columns: ",
        paste(missing_late, collapse = ", ")
      )
    }
    
    # Extract predicted dates without requiring a Scenario column.
    extract_dates <- function(data) {
      values <- vapply(
        stage_cols,
        function(column) {
          as.numeric(as.Date(data[[column]][1]))
        },
        numeric(1)
      )
      
      as.Date(values, origin = "1970-01-01")
    }
    
    early_dates <- extract_dates(early_stages)
    late_dates  <- extract_dates(late_stages)
    
    # Calculate the midpoint between Early and Late predictions.
    midpoint_dates <- as.Date(
      rep(NA_real_, length(stage_cols)),
      origin = "1970-01-01"
    )
    
    for (i in seq_along(stage_cols)) {
      if (!is.na(early_dates[i]) && !is.na(late_dates[i])) {
        midpoint_dates[i] <- as.Date(
          round(mean(as.numeric(c(
            early_dates[i],
            late_dates[i]
          )))),
          origin = "1970-01-01"
        )
      }
    }
    
    
    # Calculate days from the reference date.
    days_until <- as.integer(midpoint_dates - reference_date)
    
    # Format past and future dates as readable text.
    days_label <- vapply(days_until, function(days) {
      if (is.na(days)) {
        return("Not available")
      } else if (days < 0) {
        paste(abs(days), "days ago")
      } else if (days == 0) {
        "Today"
      } else {
        paste("in", days, "days")
      }
    }, character(1))
    
    stage_labels <- c(
      "Knospenaufbruch",
      "Mausohr",
      "Grünknospen",
      "Rotknospen",
      "Blütenbeginn",
      "Vollblüte",
      "Blütenende",
      "T-Stadium",
      "Erntebeginn"
    )
    
    # Explicitly format dates as calendar dates.
    early_display <- ifelse(
      is.na(early_dates),
      "Not available",
      format(early_dates, "%d.%m.%Y")
    )
    
    late_display <- ifelse(
      is.na(late_dates),
      "Not available",
      format(late_dates, "%d.%m.%Y")
    )
    
    result <- data.frame(
      `Phenological stage` = stage_labels,
      Early = early_display,
      Late = late_display,
      `Days until` = days_label,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    message("Subsequent stages table created successfully.")
    
    result
    
  })
  
  
  
  
  
  output$subsequent_stages_table <- renderTable({
    subsequent_stages_table()
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  
  output$early_bluetenbeginn_date <- renderText({
    
    predictions <- subsequent_early_predictions()
    
    if (is.null(predictions)) {
      return("Unavailable")
    }
    
    predicted_date <- predictions$bluetenbeginn[1]
    
    if (is.na(predicted_date)) {
      return("Unavailable")
    }
    
    format(as.Date(predicted_date), "%d.%m.%Y")
  })
  
  
  bloom_temperature_percentiles <- reactive({
    data <- temperature_percentiles()
    req(nrow(data) > 0)
    
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    end_date <- as.Date(sprintf(
      "%d-12-31",
      as.integer(format(reference_date, "%Y"))
    ))
    
    all_dates <- data.frame(
      date = seq.Date(
        from = season_start,
        to = end_date,
        by = "day"
      )
    )
    
    all_dates %>%
      mutate(
        month = lubridate::month(date),
        day = lubridate::day(date)
      ) %>%
      left_join(data, by = c("month", "day")) %>%
      select(
        date,
        tmin_cold,
        tmax_cold,
        tmin_warm,
        tmax_warm
      )
  })
  
  
  output$bloom_prediction_table <- renderTable({
    bloom_predictions() |>
      dplyr::mutate(
        PredictedDate = ifelse(
          is.na(PredictedDate),
          "Unavailable",
          format(PredictedDate, "%d.%m.%Y")
        )
      )
  })
  
  
  
  bloom_historical_temperature <- reactive({
    reference_date <- effective_current_date()
    
    season_start <- as.Date(sprintf(
      "%d-10-01",
      as.integer(format(reference_date, "%Y")) - 1L
    ))
    
    current_year_start <- as.Date(sprintf(
      "%d-01-01",
      as.integer(format(reference_date, "%Y"))
    ))
    
    prepared_hourly_temperature() %>%
      filter(
        !is.na(datetime),
        date_only >= season_start,
        date_only < current_year_start
      ) %>%
      calculate_daily_temperature() %>%
      mutate(date = as.Date(date))
  })
  
  
  bloom_temperature_data <- reactive({
    dplyr::bind_rows(
      bloom_historical_temperature(),
      current_year_temperature()
    ) %>%
      arrange(date)
  })
  
   # create temperature plot for ui
  
  output$temperature_plot <- renderPlot({
    
    req(current_year_temperature())
    
    create_temperature_plot(
      current_data = current_year_temperature(),
      
      average_data = average_temperature_current_year(),
      
      percentile_data = temperature_percentiles_current_year(),
      
      current_date = effective_current_date(),
      
      phenology_data = phenology_current_year()
    )
    
  })
  
  
  # create text for conficence interval 
  output$output_confidence_range <- renderText({
    
    min <- input$input_range[1]
    max <- input$input_range[2]
    
    
    # Temporary placeholder calculation.
    # We will later connect this to the actual slider
    # and diagram range.
    
    percentage <- max - min
    
    paste0(
      percentage,
      " %"
    )
    
  })
  
  # predicted bloom start for confidence interval
  output$output_start_date <- renderText({
    
    req(input$input_range)
    
    current_year <- current_year()
    
    # Temporary placeholder calculation.
    # We will later connect this to the actual slider
    # and diagram range.
    
    paste0(
      current_year,
      "-01-01"
    )
    
  })
  
  
  # predicted end date for conficence date
  output$output_end_date <- renderText({
    
    req(input$input_range)
    
    current_year <- current_year()
    
    paste0(
      current_year,
      "-12-31"
    )
    
  })
  
  
  output$temperature_data_debug <- renderPrint({
    dat <- bloom_temperature_data()
    
    cat("Column names:\n")
    print(names(dat))
    
    cat("\nData structure:\n")
    str(dat)
    
    cat("\nFirst six rows:\n")
    print(utils::head(dat))
  })
  
  
 
  
  
}
