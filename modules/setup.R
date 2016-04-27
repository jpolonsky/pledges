# load modules
## sidebar
source('modules/year.R')
source('modules/funded.R')
source('modules/type.R')
source('modules/report.R')
## main page
source('modules/datatable.R')
source('modules/pivot.R')
source('modules/colours.R')
source('modules/plotly.R')
source('modules/highchart.R')

SetUp <- function(input, output, session, ...){

  panel_side <-
    list(
      YearInput('id_year'),
      FundedInput('id_funded'),
      TypeInput('id_type'),
      ReportInput('id_report'),
      hr(),
      a(href = "https://primewho.org", img(src = "logo_prime.jpg", width = "100%")),
      tags$a(href="mailto:prime_support@who.int", "Comments & suggestions")
    )

  panel_main <-
    tabsetPanel(
      DFTableOutput('tmp'),
      PivotTableOutput('tmp'),
      PlotlyBarOutput('tmp')
      # HighChartBarOutput('tmp')
    )

  list(panel_side, panel_main) %>% setNames(c('side', 'main'))

}

# SetUpOutput <- function(id){
#   ns <- NS(id)
#   
#   panel_side <- 
#     list(        
#       YearInput('id_year'),
#       FundedInput('id_funded'),
#       TypeInput('id_type'),
#       ReportInput('id_report'),
#       hr(),
#       a(href = "https://primewho.org", img(src = "logo_prime.jpg", width = "100%")),
#       tags$a(href="mailto:prime_support@who.int", "Comments & suggestions")
#     )
#   
#   panel_main <- 
#     tabsetPanel(
#       DFTableOutput('tmp'),
#       PivotTableOutput('tmp'),
#       PlotlyBarOutput('tmp')
#       # HighChartBarOutput('tmp')
#     ) 
#   
#   list(panel_side, panel_main) %>% setNames(c('side', 'main'))
#   
# }
# 
# SetUp <- function(input, output, session, ...){
#   
#   output$panel_side <- renderUI({
#     data_uploaded() %>% need(message = FALSE) %>% validate
#     ui_setup[['side']]
#   })
#   
#   output$panel_main <- renderUI({
#     if (data_uploaded() %>% is.null) callModule(Welcome, 'tmp')
#     else ui_setup[['main']] 
#   })
#   
#   
# }
