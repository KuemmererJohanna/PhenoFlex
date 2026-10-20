# ============================================================
# R/plots.R
# Plot functions
# ============================================================

library(ggplot2)


create_temperature_plot <- function(
    current_data,
    average_data = NULL,
    percentile_data = NULL,
    current_date = NULL,
    phenology_data = NULL
) {
  
  if (is.null(current_data) || nrow(current_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  current_data <- current_data %>%
    mutate(date = as.Date(date))
  
  current_date <- as.Date(current_date)
  
  plot_year <- lubridate::year(
    min(current_data$date, na.rm = TRUE)
  )
  
  plot_start <- as.Date(paste0(plot_year, "-01-01"))
  plot_end <- as.Date(paste0(plot_year, "-12-31"))
  
  
  # Historical average (preserved for compatibility)
  if (!is.null(average_data) && nrow(average_data) > 0) {
    
    average_data <- average_data %>%
      mutate(
        date = as.Date(date),
        date = as.Date(
          paste0(plot_year, "-", format(date, "%m-%d"))
        )
      ) %>%
      filter(date >= current_date)
  }
  
  
  # Observed data end before the selected date
  observed_data <- current_data %>%
    filter(date < current_date)
  
  
  # Initialize plot
  p <- ggplot()
  
  
  # Detect whether local weather forecast is available
  has_forecast_columns <- all(
    c("forecast_tmin", "forecast_tmax") %in%
      names(current_data)
  )
  
  has_local_forecast <- FALSE
  
  if (has_forecast_columns) {
    has_local_forecast <- any(
      is.finite(current_data$forecast_tmin) |
        is.finite(current_data$forecast_tmax),
      na.rm = TRUE
    )
  }
  
  
  # Historical percentile scenarios
  if (!is.null(percentile_data) && nrow(percentile_data) > 0) {
    
    percentile_data <- percentile_data %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= current_date)
    
    # Hide historical scenarios during the local forecast period
    if (has_local_forecast) {
      percentile_data <- percentile_data %>%
        filter(date > current_date + 6)
    }
    
    # Cold scenario: lower selected percentile
    p <- p +
      geom_line(
        data = percentile_data,
        aes(
          x = date,
          y = tmin_cold,
          color = "Cold scenario - minimum"
        ),
        linetype = "dotted",
        linewidth = 0.7,
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_line(
        data = percentile_data,
        aes(
          x = date,
          y = tmax_cold,
          color = "Cold scenario - maximum"
        ),
        linetype = "dotted",
        linewidth = 0.7,
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      
      # Warm scenario: upper selected percentile
      geom_line(
        data = percentile_data,
        aes(
          x = date,
          y = tmin_warm,
          color = "Warm scenario - minimum"
        ),
        linetype = "dotted",
        linewidth = 0.7,
        na.rm = TRUE,
        inherit.aes = FALSE
      ) +
      geom_line(
        data = percentile_data,
        aes(
          x = date,
          y = tmax_warm,
          color = "Warm scenario - maximum"
        ),
        linetype = "dotted",
        linewidth = 0.7,
        na.rm = TRUE,
        inherit.aes = FALSE
      )
  }
  
  
  # Observed temperatures connected to local forecast
  if (has_local_forecast) {
    
    line_data <- current_data %>%
      mutate(
        plot_tmin = ifelse(
          date >= current_date,
          forecast_tmin,
          tmin
        ),
        plot_tmax = ifelse(
          date >= current_date,
          forecast_tmax,
          tmax
        )
      ) %>%
      filter(date <= current_date + 6)
    
    p <- p +
      geom_line(
        data = line_data,
        aes(
          x = date,
          y = plot_tmin,
          color = "Min Temperature"
        ),
        linewidth = 0.7,
        na.rm = TRUE
      ) +
      geom_line(
        data = line_data,
        aes(
          x = date,
          y = plot_tmax,
          color = "Max Temperature"
        ),
        linewidth = 0.7,
        na.rm = TRUE
      )
    
  } else if (nrow(observed_data) > 0) {
    
    # Historical-date view: measured temperatures only
    p <- p +
      geom_line(
        data = observed_data,
        aes(
          x = date,
          y = tmin,
          color = "Min Temperature"
        ),
        linewidth = 0.7,
        na.rm = TRUE
      ) +
      geom_line(
        data = observed_data,
        aes(
          x = date,
          y = tmax,
          color = "Max Temperature"
        ),
        linewidth = 0.7,
        na.rm = TRUE
      )
  }
  
  
  # Grey area: dates before the selected date
  p <- p +
    annotate(
      "rect",
      xmin = plot_start,
      xmax = current_date,
      ymin = -Inf,
      ymax = Inf,
      fill = "grey90",
      alpha = 0.25
    )
  
  
  # Blue area: only when local forecast values exist
  if (has_local_forecast) {
    
    p <- p +
      annotate(
        "rect",
        xmin = current_date,
        xmax = min(current_date + 6, plot_end),
        ymin = -Inf,
        ymax = Inf,
        fill = "lightblue",
        alpha = 0.25
      )
  }
  
  
  # Selected date marker
  p <- p +
    geom_vline(
      xintercept = current_date,
      linetype = "dashed",
      linewidth = 0.5
    )
  
  
  # Labels and appearance
  p +
    labs(
      x = "Month",
      y = "Temperature (°C)",
      title = "Temperature Scenarios and Forecast",
      color = "Temperature scenario"
    ) +
    scale_color_manual(
      values = c(
        "Min Temperature" = "red",
        "Max Temperature" = "blue",
        "Cold scenario - minimum" = "firebrick",
        "Cold scenario - maximum" = "navy",
        "Warm scenario - minimum" = "orange",
        "Warm scenario - maximum" = "darkgreen"
      )
    ) +
    scale_x_date(
      limits = c(plot_start, plot_end),
      date_breaks = "1 month",
      date_labels = "%b",
      expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
}
