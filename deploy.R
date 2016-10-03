files <- c(dir("app", recursive = TRUE))
# files <- files[!(files %in% c(paste0("data/cache/", dir("app/data/cache"))))]
# files <- c(files, dir("data", recursive = TRUE))
rsconnect::deployApp(
  appDir = "app", appName = "pledges", appFiles = files, 
  account = "prime", launch.browser = TRUE, contentCategory = "application"
)
