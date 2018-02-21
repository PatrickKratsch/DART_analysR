DART_range <- function(all_movement_fit_tidy, parameter = "max"){
  
  # all_movement_fit_tidy          = tidy output from DART_tidy()
  # parameter                      = default max, defines what parameter to plot
  
  # Load packages
  library("ggplot2")
  library("data.table")
  
  if(parameter == "max"){
    
    # Calculate maxima
    maxima <- all_movement_fit_tidy[, .(max(mm)), by = fly]
    colnames(maxima) <- c("fly", "maximum")
    
    # Add third column, defining genotype/ID for groups of flies
    maxima[, genotype := substr(fly, 1, (nchar(fly) - 2))]
    
    # If more than 9 flies were used, need to correct for genotype names
    geno_length <- min(nchar(maxima$genotype))
    maxima[, genotype := substr(fly, 1, geno_length)]
    
    # Violin plot
    ggplot(data = maxima, aes(x = genotype, y = maximum, colour = genotype)) + 
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(height = 0, width = 0.1) + 
      theme_bw() +
      ylab("Maximum Speed / (mm/s)") +
      theme(axis.title.x = element_blank(), 
            axis.line = element_line(colour="black"),
            panel.grid.major = element_line(colour="#f0f0f0"), 
            panel.grid.minor = element_blank())
  }
  else if(parameter == "min"){
    
    # Calculate minima
    minima <- all_movement_fit_tidy[, .(min(mm)), by = fly]
    colnames(minima) <- c("fly", "minimum")
    
    # Add third column, defining genotype/ID for groups of flies
    minima[, genotype := substr(fly, 1, (nchar(fly) - 2))]
    
    # If more than 9 flies were used, need to correct for genotype names
    geno_length <- min(nchar(minima$genotype))
    minima[, genotype := substr(fly, 1, geno_length)]
    
    # Violin plot
    ggplot(data = minima, aes(x = genotype, y = minimum, colour = genotype)) + 
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(height = 0, width = 0.1) + 
      theme_bw() +
      ylab("Minimum Speed / (mm/s)") +
      theme(axis.title.x = element_blank(), 
            axis.line = element_line(colour="black"),
            panel.grid.major = element_line(colour="#f0f0f0"), 
            panel.grid.minor = element_blank())
  }
  else if(parameter == "mean"){
    
    # Calculate mean
    means <- all_movement_fit_tidy[, .(mean(mm)), by = fly]
    colnames(means) <- c("fly", "mean")
    
    # Add third column, defining genotype/ID for groups of flies
    means[, genotype := substr(fly, 1, (nchar(fly) - 2))]
    
    # If more than 9 flies were used, need to correct for genotype names
    geno_length <- min(nchar(means$genotype))
    means[, genotype := substr(fly, 1, geno_length)]
    
    # Violin plot
    ggplot(data = means, aes(x = genotype, y = mean, colour = genotype)) + 
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(height = 0, width = 0.1) + 
      theme_bw() +
      ylab("Mean Speed / (mm/s)") +
      theme(axis.title.x = element_blank(), 
            axis.line = element_line(colour="black"),
            panel.grid.major = element_line(colour="#f0f0f0"), 
            panel.grid.minor = element_blank())
  }
  else if(parameter == "median"){
    
    # Calculate mean
    medians <- all_movement_fit_tidy[, .(median(mm)), by = fly]
    colnames(medians) <- c("fly", "median")
    
    # Add third column, defining genotype/ID for groups of flies
    medians[, genotype := substr(fly, 1, (nchar(fly) - 2))]
    
    # If more than 9 flies were used, need to correct for genotype names
    geno_length <- min(nchar(medians$genotype))
    medians[, genotype := substr(fly, 1, geno_length)]
    
    # Violin plot
    ggplot(data = medians, aes(x = genotype, y = median, colour = genotype)) + 
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      geom_jitter(height = 0, width = 0.1) + 
      theme_bw() +
      ylab("Mean Speed / (mm/s)") +
      theme(axis.title.x = element_blank(), 
            axis.line = element_line(colour="black"),
            panel.grid.major = element_line(colour="#f0f0f0"), 
            panel.grid.minor = element_blank())
  }
  else{
    
    print("Parameter not applicable.")
  }
  
}
