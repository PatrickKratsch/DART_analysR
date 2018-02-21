DART_tidy <- function(all_movement_fit, constant){
  
  # all_movement_fit          = tidy = FALSE-output from DART_transform_batch
  #                           = column name not to collapse (in quotes, e.g. "time")
  
  # Load library
  library("tidyr")
  
  # Tidy all_movement_fit and return
  all_movement_fit_tidy <- gather(all_movement_fit, fly, mm, -constant)
  all_movement_fit_tidy
}