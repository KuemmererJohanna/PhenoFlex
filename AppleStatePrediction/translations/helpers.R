

register_info_button <- function(
    id, key, input, output, current_language, session
) {
  
  # Generate icon and tooltip
  output_id <- paste0(id, "_ui")
  
  output[[output_id]] <- renderUI({
    
    lang <- current_language()
    info <- explanations[[key]][[lang]]
    
    actionLink(
      inputId = id,
      label = NULL,
      icon = icon("info-circle"),
      title = info$tooltip,
      style = "
        color: #2583C5;
        font-size: 16px;
        margin-left: 5px;
        text-decoration: none;
      "
    )
  })
  
  # Open translated information window
  observeEvent(input[[id]], {
    
    lang <- current_language()
    info <- explanations[[key]][[lang]]
    
    showModal(
      modalDialog(
        
        title = info$title,
        
        tagList(
          lapply(info$paragraphs, tags$p)
        ),
        
        easyClose = TRUE,
        footer = modalButton(info$close)
      ),
      
      session = session
    )
    
  }, ignoreInit = TRUE)
}





get_text <- function(key, language) {
  
  text <- interface_text[[key]][[language]]
  
  if (is.null(text)) {
    stop(
      paste(
        "Missing translation:",
        key,
        "for language:",
        language
      )
    )
  }
  
  text
}





info_button <- function(id) {
  
  uiOutput(
    outputId = paste0(id, "_ui"),
    inline = TRUE
  )
}
