DFTableOutput <- function(id){
  ns <- NS(id)
  
  tabPanel(
    'Data tables', icon = icon("table"),
    tags$style(type = "text/css", ".row {margin-top: 15px;}"),
    
    fluidRow(
      downloadButton('dl_table', label = "Download table"),
      tags$style(type='text/css', "#dl_table {background-color:LightGrey; float:right; margin-bottom: 15px;}")
    ),
    
    DT::dataTableOutput(ns('table')),
    style = 'width: 95%'
  )
  
}

DFTable <- function(input, output, session, ...){
  
  output$table <- DT::renderDataTable(data_filtered()[[1]], options = list(paging = FALSE))
  output$dl_table <- downloadHandler(
    filename <- 'table.csv',
    content <- function(file) write.csv(data_filtered()[[1]], file)
  )
  
}
