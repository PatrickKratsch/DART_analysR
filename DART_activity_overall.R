DART_activity_overall <- function(tidy.clean, geno_names, ...){
  
  library(tidyr)
  library(data.table)
  library(dplyr)
  
  genotypes <- list(...)
  
  i <- 1
  for(genotype in genotypes){
      
    geno.data <- tidy.clean[, genotype := ifelse(fly %in% genotype, geno_names[i], "undefined")]
    
    i <- i + 1
  }
}