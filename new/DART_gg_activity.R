DART_gg_activity <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  library("ggplot2")
  library("ggthemes")
  library("data.table")
  
  # Calculate a data.table with average activity per minute 
  # throughout experiment for each fly - convert distance to m
  setDT(all_movement_fit_tidy)
  
  # Add third column to total_distance, defining genotype/ID for groups of flies
  all_movement_fit_tidy[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  
  # If more than 9 flies were used, need to correct for genotype names - 
  geno_length <- min(nchar(all_movement_fit_tidy$genotype))
  all_movement_fit_tidy[, genotype := substr(fly, 1, geno_length)]
  
  # Alternatively, can draw boxplot - use notch = T once you have high enough n - see:
  # http://ggplot2.tidyverse.org/reference/geom_boxplot.html
  ggplot(data = all_movement_fit_tidy, aes(x = time, y = mm, colour = genotype)) + 
    geom_smooth() +
    theme_bw() +
    xlab("Time / s") +
    ylab("Distance / mm") +
    theme(axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank())
}
