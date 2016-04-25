UploadInput <- function(id) {
  ns <- NS(id)
  fileInput('data_upload', 'Upload data', accept = '.xlsx')
}
