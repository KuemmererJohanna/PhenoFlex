#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)



ui <- page_fillable(
  includeCSS("www/styles.css"),
  div(
    class = "app-header",
    
    div(
      class = "app-title",
      textOutput("app_title")
    ),
    
    div(
      class = "header-actions",
      
      # Settings button
      actionButton(
        inputId = "btn_settings",
        label = NULL,
        icon = icon("gear"),
        class = "btn btn-outline-secondary"
      )
    )
  ),
  
  
  # prediction phenological stages handling
  div(
    class = "app-content",
    
    # table of predictions
    div(
      class = "left-panel",
      
      # date of prediction 
      div(
        class = "date-controls",
        
        div(
          class = "date-input-container",
          
          dateInput(
            inputId = "input_date",
            label = "Prediction starts from:",
            value = Sys.Date(),
            format = "dd.mm.yyyy"
          )
        ),
        textOutput("output_effective_date"),
        
        div(
          class = "icon-buttons",
          
          # Refresh data
          actionButton(
            inputId = "btn_previous",
            label = NULL,
            icon = icon("arrow-rotate-left"),
            class = "btn btn-outline-secondary",
            title = "Refresh data"
          ),
          
          # Interpolate data set 
          actionButton(
            inputId = "btn_next",
            label = NULL,
            icon = icon("star-half-stroke"),
            class = "btn btn-outline-secondary",
            title = "Next"
          )
        )
      ),
      
      
      
      
      div(
        class = "table-container",
        
        h4("Data"),
        
        # Prediction of phenological dates
        div(
          class = "table-placeholder",
          tableOutput("subsequent_stages_table")
          
        )
      )
    ),
    
    
    # temperature data handling
    div(
      class = "right-panel",
      
      div(
        class = "chart-container",
        
        h4("Temperature / Phenology"),
        
        div(
          class = "chart-box",
          # temperature plot
          plotOutput(
            outputId = "temperature_plot",
            width = "100%",
            height = "100%"
          )
        )
      ),
      
      
      # confidence interval 
      div(
        class = "slider-container",
        
        sliderInput(
          inputId = "input_range",
          label = "Prediction interval (percentiles)",
          min = 0,
          max = 100,
          value = c(25,75),
          step = 1,
          width = "100%"
        )
      ),
      
      
      # bloom date prediction interval
      div(
        class = "date-range-container",
        div(
          class = "range-date",
          span(
            class = "range-date-label",
            "Confidence Interval"
          ),
          textOutput("output_confidence_range")
        ),
        
        div(
          class = "range-date",
          
          span(
            class = "range-date-label",
            "Between"
          ),
          textOutput("bloom_early_date", inline = TRUE)
        ),
        
        div(
          class = "range-date",
          
          span(
            class = "range-date-label",
            "And"
          ),
          textOutput("bloom_late_date", inline = TRUE)
        )
      )
    )
  )
)