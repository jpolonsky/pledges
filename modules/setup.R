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
      YearInput('year'),
      FundedInput('funded'),
      TypeInput('type'),
      ReportInput('report_button'),
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
