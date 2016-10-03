TypeInput <- function(id){
  ns <- NS(id)
  
  checkboxGroupInput(
    ns('type'), 
    'Crisis type:', 
    # choices = c('L3', 'L2', 'Priority', 'Other'), 
    # selected = c('L3', 'L2', 'Priority', 'Other')
    choices = c('G3', 'G2', 'WHO appeals', 'Joint appeals', 'Priority countries'), 
    selected = c('G3', 'G2', 'WHO appeals', 'Joint appeals', 'Priority countries')
  )
  
}

Type <- function(input, output, session, ...) reactive(input$type)
