DART_transform_batch <- function(dir_path, sample_name_file, start = 1, end = 0, bin = 5, threshold = 3, dead = 1440, csv = FALSE){
  
  # dir_path          = path to directory containing appartus spreadsheets
  # sample_name_file  = path to sample_names csv file, INCLUDING FILE NAME
  #                     Note that you need to account for dead flies (This
  #                     can also be done 'on the fly' when checking 
  #                     'str(movement_list))' -
  #                     IMPORTANT: Naming convention: <genotypeID>_1, 
  #                     <genotypeID>_2, <genotypeID>_3, etc., where 
  #                     the underscore-number indicates each individual fly -
  #                     this is required for later plotting
  #                     IMPORTANT: All sample names have to be of equal length
  # start, end        = start and end are the time in seconds you want to
  #                     start and end the analysis: e.g. you may want to
  #                     start at second 24 and end 59 seconds before the
  #                     end of the last movie, because your light regime
  #                     is not second-exact. By default, the whole experiment
  #                     is analysed - check videos for start and end times
  # bin               = rows to bin by - default is 5 s bins
  # threshold         = the amount of movement per bin (as defined above)
  #                     that should be converted to 0 - this is done tue to
  #                     limitations of camera resolution etc. - default = 3mm / 5s -
  #                     NB: thresholding is done after binning
  # dead              = looks through the last 'dead' minutes of the experiment to
  #                     determine whether a fly may have been dead - deafault = 1440 (2 h
  #                     after binning)
  # excel             = If TRUE, instead of stdout, the output data.table will be written
  #                     to an excel file. Make sure JAVA is up to date for this option.
  
  # Load libraries
  library("rlist")
  library("tidyr")
  library("data.table")
  
  # Source required function
  source("DART_transform.R")
  
  # Store file names
  file_names <- list.files(dir_path)
  file_names <- grep(".csv$", file_names, perl=TRUE, value=TRUE)
  
  # Read in list of sample names - this needs 
  # to be prepared in advance, e.g. in Excel -
  # use fread() because it's much faster
  sample_names <- fread(sample_name_file, header = T, sep = ",")
  sample_names <- sample_names$ID
  
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
    apparatus <- fread(paste0(dir_path, apparatus), header = T, sep = ",")
    
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
  
  # Remove rows from start and/or end (e.g. when analysing day or night only,
  # or when wanting to remove some light artifacts during light transitions)
  # If end == 0, i.e. default, the end of the analysis is the end
  # of the actual experiment.
  if(end == 0){
    
    end <- all_movement[, .N]
  }
  all_movement_fit <- all_movement[start:end, ]
  
  # Re-define time axis - this might not be necessary,
  # as data gets tidied up below anyway
  all_movement_fit$time <- 1:all_movement_fit[, .N]
  
  # Bin by bin argument to function (supplied in seconds)
  # Note that the last bin might not be of exact length == bin,
  # as nrow(data) may not be an integer-multiple of bin
  print(sprintf("Binning rows of data by %s s...", bin))
  row_num <- all_movement_fit[, .N]
  all_movement_fit <- all_movement_fit[, as.list(colSums(.SD)), by = gl(ceiling(row_num / bin), bin, row_num)]
  # Save new row_num for dead fly calculation below
  row_num <- all_movement_fit[, .N]
  
  # Redefine time column and remove gl column
  all_movement_fit <- all_movement_fit[, 2:ncol(all_movement_fit)]
  all_movement_fit[, time := bin*(1:all_movement_fit[, .N])]
  
  # Tidy datatable - need to setDT again --> Figure out why
  # but leave this line for now
  print("Tidying data...")
  all_movement_fit_tidy <- gather(all_movement_fit, fly, mm, -time)
  setDT(all_movement_fit_tidy)
  
  # Convert all values below threshold (as defined 
  # as input to this function) to 0
  print(sprintf("Converting all observations below %s mm / %s s to 0...", threshold, bin))
  all_movement_fit_tidy[mm <= threshold, mm := 0]
  
  # Test for dead flies
  dead_flies <- all_movement_fit_tidy[, .(sum(.SD[(row_num - dead):row_num, mm])), by = fly]
  colnames(dead_flies)[2] <- "mm"
  dead_flies <- dead_flies[mm == 0]$fly
  for(dead in dead_flies){
    
    dead <- dead
    print(sprintf("This fly seems to be dead: %s", dead))
  }

  # Return all_movement_fit_tidy
  if(csv){
    
    all_movement_fit_untidy <- spread(all_movement_fit_tidy, fly, mm)
    print("Writing results to .csv file...")
    write.csv(all_movement_fit_untidy, paste0(dir_path, "results.csv"))
  }
  else{
    
    all_movement_fit_tidy
  }
}