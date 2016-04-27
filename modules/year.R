YearInput <- function(id){
  ns <- NS(id)
  
  radioButtons(
    ns('year'), 
    'Select year of interest:',
    c('2014', '2015', '2016'), 
    selected = '2016'
  )
  
}

Year <- function(input, output, session, ...) reactive(input$year)
