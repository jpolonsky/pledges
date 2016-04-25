TypeInput <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxGroupInput('type', 'Crisis type:', 
                       choices = c('L3', 'L2', 'Priority', 'Other'), 
                       selected = c('L3', 'L2', 'Priority', 'Other')
    )
  )
  
}
