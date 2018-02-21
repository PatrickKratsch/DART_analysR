DART_movement_initiation <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Load libraries
  library("data.table")
  
  # Add fifth column of movement initiation - pad with FALSE (fill argument)
  all_movement_fit_tidy[, initiation := shift(mm, fill = 1) == 0 & mm != 0, by = fly]
  
  # Get the movement initiaton ratio per fly as a new data.table
  initiation <- all_movement_fit_tidy[, .(sum(initiation)), by = fly]
  colnames(initiation)[2] <- "initiation_number"
  
  # Add genotype column to initiation
  initiation[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names
  geno_length <- min(nchar(initiation$genotype))
  initiation[, genotype := substr(fly, 1, geno_length)]
  
  # Plot the number of movement initiations
  ggplot(initiation, aes(x = genotype, y = initiation_number, colour = genotype)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 1000, 25)) +
    ylab("Number of Movement Initiations") +
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
}
