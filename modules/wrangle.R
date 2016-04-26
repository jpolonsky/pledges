library(rpivotTable)

WrangleData <- function(input, output, session, ...){
  
  list_df <<- reactive({

    # inFile <- input$data_upload
    inFile <- data_uploaded()
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
  

  
}


