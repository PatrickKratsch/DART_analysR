DART_gg_max_speed <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Load libraries
  library("data.table")
  
  # Subset data to get activity only rows = delete all
  # stationary periods of fly, because this would cause
  # the median of the speed to be 0 in most cases
  all_movement_speed <- all_movement_fit_tidy[mm > 0]
  
  # Get the max speed / (mm / 5 s) for each fly
  max_speed <- all_movement_speed[, .(max(mm)), by = fly]
  colnames(max_speed)[2] <- "max_speed"
  
  # Add genotype column to initiation
  max_speed[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names
  geno_length <- min(nchar(max_speed$genotype))
  max_speed[, genotype := substr(fly, 1, geno_length)]
  
  # Plot the number of movement initiations
  ggplot(max_speed, aes(x = genotype, y = max_speed, colour = genotype)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 60, 2)) +
    ylab("Maximum Speed / (mm / 5 s)") +
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
}
