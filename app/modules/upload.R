UploadInput <- function(id) {
  ns <- NS(id)
  # fileInput('data_upload', 'Upload data', accept = '.xlsx')
  fileInput(ns('data_upload'), 'Upload data', accept = '.xlsx')
}

Upload <- function(input, output, session, ...) reactive(input$data_upload)
