DART_gg_total_distance <- function(tidy.clean, geno_names, ...){
  
  # NB: geno_names is a character vector with the two genotype names
  # NB: ... are two vectors, each one specifying, in order, 
  # the fly numbers corresponding to geno_names[1] and geno_names[2]
  
  library(data.table)
  library(ggplot2)
  
  # Extract genotypes from ... (requires vectors for what flies are genotype 1, genotype 2, genotype 3, etc.)
  genotypes <- list(...)
  
  # Calculate a data.table with average activity per minute throughout experiment for each fly
  total_distance <- tidy.clean[, .(total_distance = sum(activity) / 10), by = fly]
  
  # Convert fly column to factor and order data.table by values of activity_per_minute
  total_distance$fly <- factor(total_distance$fly, levels = 
                                      total_distance$fly[order(total_distance$total_distance)])
  
  # Add genotype column
  i <- 1
  for(geno in genotypes){
    
    total_distance[(geno), genotype := geno_names[i]]
    
    i <- i + 1
  }
  
  # Simple plot
  ggplot(data = total_distance, aes(x = fly, y = total_distance, colour = genotype)) + 
    geom_point() + 
    coord_flip() +
    ylab("Total Distance / cm") + xlab("Individual Flies") # Note that I used coord_flip
}