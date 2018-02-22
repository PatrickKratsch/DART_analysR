DART_gg_boxplot <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  library("ggplot2")
  library("ggthemes")
  library("data.table")

  # Calculate a data.table with average activity per minute 
  # throughout experiment for each fly - convert distance to m
  setDT(all_movement_fit_tidy)
  total_distance <- all_movement_fit_tidy[, .(total_distance = sum(mm) / 1000), by = fly]
  
  # Add third column to total_distance, defining genotype/ID for groups of flies
  total_distance[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  
  # If more than 9 flies were used, need to correct for genotype names - 
  geno_length <- min(nchar(total_distance$genotype))
  total_distance[, genotype := substr(fly, 1, geno_length)]
  
  # Alternatively, can draw boxplot - use notch = T once you have high enough n - see:
  # http://ggplot2.tidyverse.org/reference/geom_boxplot.html
  ggplot(data = total_distance, aes(x = genotype, y = total_distance, colour = genotype)) + 
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 70, 2)) +
    ylab("Total Distance / m") +
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
}
