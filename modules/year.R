YearInput <- function(id){
  ns <- NS(id)
  radioButtons('year', 'Select year of interest:', 
               c('2014', '2015', '2016'), selected = '2016'
               # list_years, selected = max(list_years)
  )
}
