DART_gg_median_speed <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Load libraries
  library("data.table")
  
  # Subset data to get activity only rows = delete all
  # stationary periods of fly, because this would cause
  # the median of the speed to be 0 in most cases
  all_movement_speed <- all_movement_fit_tidy[mm > 0]
  
  # Get the median speed / (mm / 5 s) for each fly
  median_speed <- all_movement_speed[, .(median(mm)), by = fly]
  colnames(median_speed)[2] <- "median_speed"
  
  # Add genotype column to initiation
  median_speed[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names
  geno_length <- min(nchar(median_speed$genotype))
  median_speed[, genotype := substr(fly, 1, geno_length)]
  
  # Plot the number of movement initiations
  ggplot(median_speed, aes(x = genotype, y = median_speed, colour = genotype)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 20, 1)) +
    ylab("Median Speed / (mm / 5 s)") +
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
}
