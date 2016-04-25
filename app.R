library(shiny)
library(dplyr)
# library(magrittr)
library(readxl)

# load functions
source('functions.R')
# load modules
source('modules/setup.R')
source('modules/upload.R')
source('modules/welcome.R')

## app -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('Emergency Risk Management financial reporting tool'), fluidRow(
    sidebarLayout(
      sidebarPanel(
        width = 2, 
        tags$style(type = "text/css", ".well { background-color: white; margin-left: 15px; margin-top: 40px;}"),
        UploadInput('tmp'),
        uiOutput('panel_side')
      ),
      mainPanel(
        width = 10, 
        tags$style(type = "text/css", ".row { margin-top: 15px;}"),
        uiOutput('panel_main')
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ui setup
  ui_setup <- callModule(SetUp, 'tmp')
  
  output$panel_side <- renderUI({
    input$data_upload %>% need(message = FALSE) %>% validate
    ui_setup[['side']]
  })
  
  output$panel_main <- renderUI({
    if (input$data_upload %>% is.null) callModule(Welcome, 'tmp')
    else ui_setup[['main']] 
  })
  
  
  # call modules
  callModule(DFTable, 'tmp')
  callModule(PivotTable, 'tmp')
  callModule(HighChartBar, 'tmp')
  callModule(PlotlyBar, 'tmp')
  callModule(Report, 'report_button') # name is important here - links to setup.R
  
  
  # upload & wrangle data
  list_df <<- reactive({
    
    inFile <- input$data_upload
    if (is.null(inFile)) return(NULL)
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath, paste(inFile$datapath, ext, sep = '.'))
    # list_sheets <- excel_sheets(inFile$datapath, ext, sep = '.')) # excel_sheets not yet working in shiny
    
    df_raw <- read_excel(paste(inFile$datapath, ext, sep = '.'), sheet = 'Contribution Data', skip = 1) %>% ExcludeEmpty
    
    df_extra <-
      read_excel(paste(inFile$datapath, ext, sep = '.'),
                 sheet = paste0('Soft pledges-other ctrbns ', input$year),
                 skip = 1) %>%
      ExcludeEmpty
    
    df_filter <-
      read_excel(paste(inFile$datapath, ext, sep = '.'),
                 sheet = paste0('SRP ', input$year, ' funds requested'),
                 skip = 2) %>%
      ExcludeEmpty
    
    PrepareData(df_raw, df_extra, df_filter)
    
  })

  
  # filter data based on user inputs  
  data_filtered <<- reactive({
    
    if (is.null(input$data_upload)) return()
    
    tmp1 <-
      list_df()[[1]] %>%
      filter(`Funded (%)` >= input$slide_funded[[1]] &
               `Funded (%)` <= input$slide_funded[[2]] &
               `Crisis type` %in% input$type
      )
    
    tmp2 <-
      list_df()[[2]] %>%
      filter(prop_funded >= input$slide_funded[[1]] &
               prop_funded <= input$slide_funded[[2]] &
               status %in% input$type #& donor %in% input$donor
      )
    
    list(tmp1, tmp2)
    
  })
  
}

shinyApp(ui = ui, server = server)

