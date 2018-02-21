DART_speed_distribution <- function(all_movement_fit_tidy){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  
  # Load packages
  library("ggplot2")
  library("data.table")
  library("gridExtra")
  
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
  ggplot(activity, aes(x = mm, fill = genotype)) +
    geom_histogram(binwidth = 1, boundary = 0, closed = "left", alpha = 0.5, position = "identity") +
    scale_x_continuous(breaks = seq(3, 49, 2)) +
    scale_y_continuous(breaks = seq(0, 5000, 100)) +
    xlab("mm / s") +
    theme_bw() +
    theme(axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
  
  # Add to plot the ratio of non-zero activity to zero activity
  # for each genotype - add genotype as third column (as above)
  activity_ratio <- all_movement_fit_tidy[, .(sum(mm > 0) / length(mm)), by = fly]
  colnames(activity_ratio)[2] <- "activity_ratio"
  activity_ratio[, genotype := substr(fly, 1, (nchar(fly) - 2))]
  # If more than 9 flies were used, need to correct for genotype names - 
  geno_length <- min(nchar(activity_ratio$genotype))
  activity_ratio[, genotype := substr(fly, 1, geno_length)]
  
  # Plot the two plots side-by-side
  histo <- ggplot(activity, aes(x = mm, fill = genotype)) +
    geom_histogram(binwidth = 1, boundary = 0, closed = "left", alpha = 0.5, position = "identity") +
    scale_x_continuous(breaks = seq(3, 49, 2)) +
    scale_y_continuous(breaks = seq(0, 5000, 100)) +
    xlab("mm / s") +
    theme_bw() +
    theme(axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
  ratio <- ggplot(activity_ratio, aes(x = genotype, y = activity_ratio, colour = genotype)) +
    geom_boxplot() +
    geom_jitter(height = 0, width = 0.1) + 
    scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
    theme_bw() +
    theme(axis.line = element_line(colour="black"),
          panel.grid.major = element_line(colour="#f0f0f0"), 
          panel.grid.minor = element_blank())
  
  grid.arrange(histo, ratio, ncol = 2)
  
}
