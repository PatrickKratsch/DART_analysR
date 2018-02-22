DART_gg_time_moving <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Get the fraction of time active per fly
  fraction_active <- all_movement_fit_tidy[, .(sum(mm != 0) / .SD[, .N]), by = fly]
  colnames(fraction_active)[2] <- "active"
  
  # Add genotype column to fraction_active
  fraction_active[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names
  geno_length <- min(nchar(fraction_active$genotype))
  fraction_active[, genotype := substr(fly, 1, geno_length)]
  
  # Plot active fractions
  ggplot(fraction_active, aes(x = genotype, y = active, colour = genotype)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
    ylab("Activity Fraction") +
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
  
}