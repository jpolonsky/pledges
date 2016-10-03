library(highcharter)

HighChartBarOutput <- function(id){
  ns <- NS(id)
  
  tabPanel(
    'Highchart', icon = icon("bar-chart"),
    highchartOutput(ns('highchart'), height = "600px"),
    style = 'width: 95%'
  )
  
}

HighChartBar <- function(input, output, session, ...){
  
  output$highchart <- renderHighchart({
    df_hc <- data_filtered()[[1]] %>% arrange(appeal)
    
    highchart() %>% 
      hc_add_series(name = 'Funded (%)', data = df_hc$`Funded (%)`, 
                    dataLabels = list(align = "top", enabled = TRUE)) %>% 
      hc_xAxis(categories = df_hc$appeal) %>% 
      hc_yAxis(max = 100, tickInterval = 10, title = NULL, gridLineColor = 'transparent') %>% 
      hc_chart(type = 'bar') %>% 
      hc_exporting(enabled = TRUE)
    
  })
  
}
