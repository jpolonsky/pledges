FundedInput <- function(id){
  ns <- NS(id)
  uiOutput(ns("ui_funded"))
}

Funded <- function(input, output, session, ...) {
  
  output$ui_funded <- renderUI({
    ns <- session$ns
    
    sliderInput(
      ns("slide_funded"), 
      "Proportion funded", 
      min = 0, 
      max = max(list_df()[[1]]$`Funded (%)`),
      # value = range(list_df()[[1]]$`Funded (%)`),
      value = c(0, max(list_df()[[1]]$`Funded (%)`)),
      step = 10
    )
    
  })
  
  reactive(input$slide_funded)
  
}
