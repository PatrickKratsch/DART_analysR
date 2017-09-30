DART_activity_overall <- function(tidy.clean, cell_length){
  
  library(tidyr)
  library(data.table)
  library(dplyr)
  
  experiment_length <- cell_length * dim(tidy.clean)[1] / 60
  
  activity_per_minute_overall <- tidy.clean[, .(total_activity = sum(activity) / ), by = fly]
  
}