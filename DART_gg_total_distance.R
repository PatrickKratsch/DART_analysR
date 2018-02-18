DART_gg_total_distance <- function(all_movement_fit_tidy, base_names){
  
  # all_movement_fit_tidy                   = tidy (= T) output from DART_transform_batch
  # base_names                              = the base name for each genotype - this is
  #                                           needed to average over individual flies
  
  # Load libraries
  library(data.table)
  library(ggplot2)
  
  # Calculate a data.table with total distance for each fly for whole experiment
  setDT(all_movement_fit_tidy)
  total_distance <- all_movement_fit_tidy[, .(total_distance = sum(speed)), by = fly]
  
  # Convert fly column to factor and order data.table by values of activity_per_minute
  total_distance$fly <- factor(total_distance$fly, levels = 
                                      total_distance$fly[order(total_distance$total_distance)])
  
  grep("loxp", total_distance$fly, perl = TRUE, value = TRUE)
  
  # Plot
  ggplot(data = total_distance, aes(x = fly, y = total_distance)) + 
    geom_point() + 
    coord_flip() +
    ylab("Total Distance / mm") + xlab("Individual Flies") # Note that I used coord_flip
}
