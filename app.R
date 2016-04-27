library(shiny)
# load functions
source('functions.R')
# load modules
source('modules/setup.R')
source('modules/upload.R')
source('modules/welcome.R')
# source('modules/wrangle.R')

## app -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel('Emergency Risk Management financial reporting tool'), fluidRow(
    sidebarLayout(
      sidebarPanel(
        width = 2, 
        tags$style(type = "text/css", ".well {background-color: white; margin-left: 15px; margin-top: 40px;}"),
        UploadInput('tmp'),
        uiOutput('panel_side')
      ),
      mainPanel(
        width = 10, 
        tags$style(type = "text/css", ".row {margin-top: 15px;}"),
        uiOutput('panel_main')
      )
    )
  )
)

server <- function(input, output, session) {
  
  # initial setup
  data_uploaded <- callModule(Upload, 'tmp')
  
  # ui setup
  ui_setup <- callModule(SetUp, 'tmp')
  
  output$panel_side <- renderUI({
    data_uploaded() %>% need(message = FALSE) %>% validate
    ui_setup[['side']]
  })
  
  output$panel_main <- renderUI({
    if (data_uploaded() %>% is.null) callModule(Welcome, 'tmp')
    else ui_setup[['main']]
  })
  
  
  # call ui modules
  callModule(DFTable, 'tmp')
  callModule(PivotTable, 'tmp')
  callModule(PlotlyBar, 'tmp')
  callModule(HighChartBar, 'tmp')
  callModule(Report, 'id_report') # name is important here - links to setup.R
  
  # call ui modules & store reactive inputs - namespaces links 
  year_selected <- callModule(Year, 'id_year')
  type_selected <- callModule(Type, 'id_type')
  funded_selected <- callModule(Funded, 'id_funded') # NB. a renderUI module!
  # list_df <- callModule(WrangleData, 'tmp', data_uploaded) # doesn't work as expected
  
  # upload & wrangle data
  list_df <<- reactive({

    in_file <- data_uploaded()
    if (is.null(in_file)) return()
    ext <- tools::file_ext(in_file$name)
    file.rename(in_file$datapath, paste(in_file$datapath, ext, sep = '.'))
    # list_sheets <- paste(in_file$datapath, ext, sep = '.') %>% excel_sheets # excel_sheets not yet working in shiny

    df_raw <- read_excel(paste(in_file$datapath, ext, sep = '.'), sheet = 'Contribution Data', skip = 1) %>% ExcludeEmpty

    df_extra <-
      read_excel(paste(in_file$datapath, ext, sep = '.'),
                 sheet = paste0('Soft pledges-other ctrbns ', year_selected()),
                 skip = 1) %>%
      ExcludeEmpty

    df_filter <-
      read_excel(paste(in_file$datapath, ext, sep = '.'),
                 sheet = paste0('SRP ', year_selected(), ' funds requested'),
                 skip = 2) %>%
      ExcludeEmpty

    PrepareData(df_raw, df_extra, df_filter)

  })
  
  # filter data based on user inputs  
  data_filtered <<- reactive({
    
    tmp1 <-
      list_df()[[1]] %>%
      filter(
        `Funded (%)` >= funded_selected()[[1]] &
          `Funded (%)` <= funded_selected()[[2]] &
          `Crisis type` %in% type_selected()
      )
    
    tmp2 <-
      list_df()[[2]] %>%
      filter(
        prop_funded >= funded_selected()[[1]] &
          prop_funded <= funded_selected()[[2]] &
          status %in% type_selected()
      )
    
    list(tmp1, tmp2)
    
  })
  
}

shinyApp(ui = ui, server = server)

