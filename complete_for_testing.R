source('app/functions.R')

file_name <- '/Users/jonathanpolonsky/OneDrive/who/2014_2015_2016_pledges_and_contributions.xlsx'

df_raw <- read_excel(file_name, sheet = 'Contribution Data', skip = 1) %>% ExcludeEmpty

df_extra <-
  read_excel(file_name,
             sheet = paste0('Soft pledges-other ctrbns 2016'),
             skip = 1) %>%
  ExcludeEmpty

df_filter <-
  read_excel(file_name,
             sheet = paste0('SRP 2016 funds requested'),
             skip = 2) %>%
  ExcludeEmpty

tmp <- PrepareData(df_raw, df_extra, df_filter)

tmp2 <- 
  tmp[[1]] %>% 
  bind_rows(
    summarise(tmp[[1]],
    appeal = 'Total', `Crisis type` = '-',
    `Amount requested` = sum(`Amount requested`),
    `Amount received` = sum(`Amount received`),
    `Funded (%)` = `Amount received`/`Amount requested`*100
  )
  )

tmp2$`Crisis type` <- factor(tmp2$`Crisis type`, levels = c(levels(tmp[[1]]$`Crisis type`), '-'))

PlotBar(data = tmp2,
        xvar = 'appeal', 
        yvar = 'Funded (%)', 
        yaxis = 'Funded (%)',
        colscheme = 'Blues'
        # NB this namespace (input$colours) relates to colours.R, not to call in PlotlyBarOutput above
) 
