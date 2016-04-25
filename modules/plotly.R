library(plotly)
# Sys.setenv("plotly_username" = "jpolonsky")
# Sys.setenv("plotly_api_key" = "l363g0n5u9")

PlotlyBarOutput <- function(id){
  ns <- NS(id)
  
  tabPanel(
    'Plotly', icon = icon("bar-chart"),
    ColourPickerInput('tmp'),
    plotlyOutput(ns('plotly'), height = "600px"),
    style = 'width: 95%'
  )
  
}

PlotlyBar <- function(input, output, session, ...){
  
  output$plotly <- renderPlotly({
    
    PlotBar(data = data_filtered()[[1]],
            xvar = 'appeal', 
            yvar = 'Funded (%)', 
            yaxis = 'Funded (%)',
            colscheme = input$colours
            # NB this namespace (input$colours) relates to colours.R, not to call in PlotlyBarOutput above
    ) %>% 
      ggplotly
    
  })
  
}

