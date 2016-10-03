YearInput <- function(id){
  ns <- NS(id)
  uiOutput(ns("ui_year"))
}

Year <- function(input, output, session, ...) {
  
  output$ui_year <- renderUI({
    ns <- session$ns
    
    # for dynamic 'year' selection based on sheet names
    list_years <- list_sheets %>% str_extract('201\\d') %>% unique %>% subset(!is.na(.)) %>% as.numeric
    
    radioButtons(
      ns('year'),
      'Select year of interest:',
      list_years,
      selected = max(list_years)
    )
    
  })
  
  reactive(input$year)
  
}
