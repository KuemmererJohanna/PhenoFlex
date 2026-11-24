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

source("translations/helpers.R")
source("translations/explanations.R")
source("translations/interface.R")



ui <- page_fillable(
  includeCSS("www/styles.css"),
  
  div(
    class = "app-header",
    
    div(
      class = "app-title",
      
      div(
        style = "display: flex; align-items: center; gap: 6px;",
        
        textOutput("app_title"),
        info_button("info_general")
      )
    ),
    
    div(
      class = "header-actions",
      
      style = "display: flex; align-items: center; gap: 6px;",
      
      radioButtons(
        inputId = "setting_language",
        label = NULL,
        choices = c(
          "DE" = "de",
          "EN" = "en",
          "IT" = "it"
        ),
        selected = "de",
        inline = TRUE
      ),
      
        
        
      # Settings button
        actionButton(
          inputId = "btn_settings",
          label = NULL,
          icon = icon("gear"),
          class = "btn btn-outline-secondary"
        ),
      info_button("info_settings")
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
            label = textOutput("date_input_label", inline = TRUE),
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
        
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          
          h4(textOutput("data_header", inline = TRUE)),
          info_button("info_phenological_stages")
        ),
        
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
        
        div(
          style = "display: flex; align-items: center; gap: 6px;",
          
          h4(textOutput("temperature_header", inline = TRUE)),
          info_button("info_temperature_plot")
        ),
        
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
        
        div(
          
          style = "display: flex; align-items: center; gap: 6px;",
        
          h4(textOutput("percentiles_header", inline = TRUE)),
          
          info_button("info_percentiles")
          
        ),
        
        sliderInput(
          inputId = "input_range",
          label = NULL,
          min = 0,
          max = 100,
          value = c(25, 75),
          step = 1,
          width = "100%"
        ),
        
        # bloom date prediction interval
        div(
          class = "date-range-container",
          
          div(
            class = "range-date",
            span(
              class = "range-date-label",
              textOutput("confidence_interval_label", inline = TRUE)
            ),
            textOutput("output_confidence_range")
          ),
          
          div(
            class = "range-date",
            
            span(
              class = "range-date-label",
              textOutput("confidence_interval_between", inline = TRUE)
            ),
            textOutput("bloom_early_date", inline = TRUE)
          ),
          
          div(
            class = "range-date",
            
            span(
              class = "range-date-label",
              textOutput("confidence_interval_and", inline = TRUE)
            ),
            textOutput("bloom_late_date", inline = TRUE)
          )
        )
      ),
    )
  )
)




