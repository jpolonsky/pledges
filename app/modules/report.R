ReportInput <- function(id){
  ns <- NS(id)
  downloadButton(ns('report'), 'Download report')
}

Report <- function(input, output, session){
  
  output$report <- downloadHandler(
    
    filename = function() paste0('ERM_financial_report_', Sys.Date(), '.pdf'),
    
    content = function(file) {
      #       src <- normalizePath('report.Rmd')
      #       tex <- normalizePath('titlepage.tex')
      #       sty <- normalizePath('styles.sty')
      #       func <- normalizePath('functions.R')
      #       who <- normalizePath('./figure/logo_who.pdf')
      #       prime <- normalizePath('./figure/logo_prime.pdf')
      #       
      #       owd <- setwd(tempdir())
      #       on.exit(setwd(owd))
      #       file.copy(src, 'report.Rmd')
      #       file.copy(tex, 'titlepage.tex')
      #       file.copy(sty, 'mystyles.sty')
      #       file.copy(func, 'functions.R')
      #       dir.create('figure')
      #       file.copy(who, './figure/logo_who.pdf')
      #       file.copy(prime, './figure/logo_prime.pdf')
      
      file_rmd <- httr::GET("https://raw.githubusercontent.com/jpolonsky/erm/master/report.Rmd")
      bin <- httr::content(file_rmd, 'raw')
      writeBin(bin, "tmp.Rmd")

      styles <- httr::GET("https://raw.githubusercontent.com/jpolonsky/erm/master/styles.sty")
      bin <- httr::content(styles, 'raw')
      writeBin(bin, "tmp.sty")

      titlepage <- httr::GET("https://raw.githubusercontent.com/jpolonsky/erm/master/titlepage.tex")
      bin <- httr::content(titlepage, 'raw')
      writeBin(bin, "tmp.tex")
      
      out <- 
        # rmarkdown::render(
        #   'Report.Rmd',
        rmarkdown::render(
          'tmp.Rmd',
          #   switch(input$format, PDF = pdf_document(), HTML = html_document(), Word = word_document())
          tufte::tufte_handout(
            # pandoc_args = c('--include-in-header=styles.sty', '--include-before-body=titlepage.tex')
            pandoc_args = c('--include-in-header=tmp.sty', '--include-before-body=tmp.tex')
          )
        )
      file.rename(out, file)
    }
    
)}
