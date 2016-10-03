Welcome <- function(input, output, session){
  
  paste("Hey Paula's team!", 
        "Please upload your excel file using the Upload Data button", 
        img(src = "arrow.png", width = "5%"), 
        sep = '<br/>') %>% 
    HTML
  
}
