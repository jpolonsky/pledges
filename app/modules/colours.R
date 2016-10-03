list_colours <- 
  RColorBrewer::brewer.pal.info %>% 
  subset(category %in% c("seq", "div")) %>% 
  rownames(.)

list_colours <- c( 'Blues', 'Spectral', 'YlOrRd')

ColourPickerInput <- function(id){
  ns <- NS(id)
  selectInput(ns('colours'), 'Colour Scheme', list_colours, selected = 'Blues')
}
