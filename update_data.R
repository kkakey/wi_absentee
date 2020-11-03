library(rsconnect)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data_processing.R")
rsconnect::deployApp(launch.browser = F, forceUpdate = T)
