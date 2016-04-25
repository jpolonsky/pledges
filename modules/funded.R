FundedInput <- function(id){
  ns <- NS(id)
  
  tagList(
    sliderInput(
      "slide_funded", "Proportion funded", 
      min = 0, 
      #       max = max(data()[[1]]$`Funded (%)`), 
      #       # value = range(data()[[1]]$`Funded (%)`),
      #       value = c(0, max(data()[[1]]$`Funded (%)`)),
      max = 100, 
      value = c(0, 100),
      step = 10
    )
  )
  
}
