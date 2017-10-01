DART_gg_activity_overall <- function(tidy.clean, cell_length, geno_names, ...){
  
  # NB: geno_names is a character vector with the two genotype names
  # NB: ... is a list of two vectors, each one specifying, in order, 
  # the fly numbers corresponding to geno_names[1] and geno_names[2]
  
  library(data.table)
  library(ggplot2)
  
  # Extract genotypes from ... (requires vectors for what flies are genotype 1, genotype 2, genotype 3, etc.)
  genotypes <- list(...)
  
  # Calculate the length of the experiemnt in minutes
  experiment_length <- cell_length * dim(tidy.clean)[1] / 60
  
  # Calculate an average data.table with average activity per minute throughout experiment for each fly
  activity_per_minute <- tidy.clean[, .(activity_per_minute = sum(activity) / experiment_length), by = fly]
  # Convert fly column to factor and order data.table by values of activity_per_minute
  activity_per_minute$fly <- factor(activity_per_minute$fly, levels = 
                                      activity_per_minute$fly[order(activity_per_minute$activity_per_minute)])
  
  # Add genotype column
  i <- 1
  for(genotype in genotypes){
    
    activity_per_minute[(genotype), geno := geno_names[i]]
    
    i <- i + 1
  }
  
  # Simple plot
  ggplot(data = activity_per_minute) + 
    geom_point(mapping = aes(x = fly, y = activity_per_minute, colour = geno)) + 
    coord_flip()
  
}
