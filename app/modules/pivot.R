library(rpivotTable)

PivotTableOutput <- function(id){
  ns <- NS(id)
  
  tabPanel(
    'Pivot table', icon = icon("random"),
    rpivotTableOutput(ns('pivot')),
    style = 'width: 95%'
  )
  
}

PivotTable <- function(input, output, session, ...){
  
  output$pivot <- 
    renderRpivotTable(
      rpivotTable(data_filtered()[[2]],
                  rows = c('status', 'appeal'),
                  cols = c('donor', ''),
                  aggregatorName = 'Sum',
                  vals = 'prop_funded',
                  rendererName = 'Heatmap'
      )
    )
  
}

