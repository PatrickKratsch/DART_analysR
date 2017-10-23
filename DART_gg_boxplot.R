# NB: This function only works for 2 genotypes
DART_gg_boxplot <- function(tidy.clean, geno_names, genotypes){
  
  library(ggplot2)
  library(data.table)

  # Calculate a data.table with average activity per minute throughout experiment for each fly
  total_distance <- tidy.clean[, .(total_distance = sum(activity) / 10), by = fly]

  # Add genotype column
  i <- 1
  for(geno in genotypes){
    
    total_distance[(geno), genotype := geno_names[i]]
    
    i <- i + 1
  }
  
  ggplot(data = total_distance, aes(x = genotype, y = total_distance, colour = genotype)) + 
    geom_boxplot() +
    geom_jitter() + 
    xlab("Genotype") + ylab("Total Distance / cm")
}
