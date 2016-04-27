TypeInput <- function(id){
  ns <- NS(id)
  
  checkboxGroupInput(
    ns('type'), 
    'Crisis type:', 
    choices = c('L3', 'L2', 'Priority', 'Other'), 
    selected = c('L3', 'L2', 'Priority', 'Other')
  )
  
}

Type <- function(input, output, session, ...) reactive(input$type)
