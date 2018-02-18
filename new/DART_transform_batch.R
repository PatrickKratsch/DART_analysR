DART_transform_batch <- function(dir_path, sample_name_path, start = 1, end = 0, tidy = F){
  
  # dir_path          = path to directory containing appartus spreadsheets
  # sample_name_path  = path to sample_names csv file, INCLUDING FILE NAME
  #                     Note that you need to account for dead flies (This
  #                     can also be done 'on the fly' when checking 
  #                     'str(movement_list))' -
  #                     IMPORTANT: Naming convention: <genotypeID>_1, 
  #                     <genotypeID>_2, <genotypeID>_3, etc., where 
  #                     the underscore-number indicates each individual fly -
  #                     this is required for later plotting
  # start, end        = start and end are the time in seconds you want to
  #                     start and end the analysis: e.g. you may want to
  #                     start at second 24 and end 59 seconds before the
  #                     end of the last movie, because your light regime
  #                     is not second-exact. By default, the whole experiment
  #                     is analysed
  # tidy              = if set to TRUE, outputs the data in tidy format - 
  #                     this is required for plotting in R - for sleep analysis
  #                     with DART_analysR, however, a non-tidy dataset is needed
  
  # Load libraries
  library("rlist")
  library("tidyr")
  
  # Source required function
  source("DART_transform.R")
  
  # Store file names
  file_names <- list.files(dir_path)
  
  # Read in list of sample names - this needs 
  # to be prepared in advance, e.g. in Excel
  sample_names <- read.csv(sample_name_path, header = F)
  sample_names <- as.character(sample_names$V1)
  
  # Generate list, which will carry each
  # analysed movement data per apparatus
  # as a separate list entry
  movement_list <- list()
  # Create variables to keep track of start
  # and end of sample names
  sample_start <- 1
  sample_end <- 0
  for(i in 1:length(file_names)){
    
    # We need to go through each apparatus in order,
    # which cannot be done with sort() if more than 9
    # apparati are used - hence, we need to grep
    # regex based on i
    reg_ex <- sprintf("#%s\\)", i)
    
    # grep the current file name
    apparatus <- grep(reg_ex, file_names, perl = TRUE, value = TRUE)
    print(sprintf("Processing apparatus: %s", apparatus))
    apparatus <- read.csv(paste0(dir_path, apparatus), header = T)
    
    # Calculate movement data
    movement_list[[i]] <- DART_transform(apparatus)
    
    # Rename columns
    sample_end <- sample_end + ncol(movement_list[[i]]) - 1
    colnames(movement_list[[i]]) <- c("time", sample_names[sample_start:sample_end])
    sample_start <- sample_start + ncol(movement_list[[i]]) - 1
    
    # Round time decimals to whole seconds for first apparatus
    if(i == 1){
      
      movement_list[[i]]$time <- round(movement_list[[i]]$time)
    }
    
    # Remove time column for all but the first apparatus
    if(i > 1){
      
      movement_list[[i]] <- movement_list[[i]][, 2:ncol(movement_list[[i]])]
    }
    
  }
  
  # Bind all data.tables together
  all_movement <- list.cbind(movement_list)
  
  # Remove rows from start and/or end (e.g. when analysing day or night only)
  all_movement_fit <- all_movement[start:(nrow(all_movement) - end), ]
  
  # Create tidy data.table for plotting OR return non-tidy data.table for sleep analysis
  if(tidy == T){
    
    all_movement_fit_tidy <- gather(all_movement_fit, fly, speed, -time)
    all_movement_fit_tidy
  }
  else{
    
    all_movement_fit
  }
}