DART_gg_speed_distribution <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Load packages
  library("ggplot2")
  library("data.table")
  
  # Add third column, defining genotype/ID for groups of flies
  setDT(all_movement_fit_tidy)
  all_movement_fit_tidy[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names - 
  geno_length <- min(nchar(all_movement_fit_tidy$genotype))
  all_movement_fit_tidy[, genotype := substr(fly, 1, geno_length)]
  
  # Get non-zero activity (mm) columns and plot histogram of
  # non-zero values - note that you should have the same number of
  # data points for each genotype when you plot counts on the y-axis
  activity <- all_movement_fit_tidy[mm > 0, ]
  ggplot(activity, aes(x = mm, colour = genotype)) +
    stat_ecdf(geom = "step") +
    theme_bw() +
    theme(axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
}
