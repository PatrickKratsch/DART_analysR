DART_analysR <- function(data, sleep_threshold, cell_length = 1, transition = NULL, before_after = NULL){
  
  ## Parameters ## 
  # data            = DART output - if more flies in one csv file, subset
  #                   relevant columns, e.g. data <- myexp[, 11:25] -
  #                   don't forget to always include column 1 (time)
  # sleep_threshold = velocity of flies (per cell_length) must be 
  #                   below this value to be considered sleeping
  # cell_length     = length of one sampling window in seconds -
  #                   by default, this is set to 1 s
  # transition      = time point in seconds a light transition occurred
  # before_after    = if transition is given, this lets the user
  #                   specify whether DART_analysR should analyse
  #                   the data before or after the transition
  
  library(dplyr)
  library(data.table)
  
  # Source auxilliary functions (need to be in working directory)
  source("DART_sleep_define.R")
  source("DART_sleep_start.R")
  source("DART_sleep_end.R")
  
  # If ld_transition is given manually, analyse relevant part,
  # i.e. day or night. If before_after is "before",
  # this programme will analyse all the data of clean
  # up to and including "before", whereas if 
  # before_after is specified as "after", the programme
  # will analyse all the data of clean from
  # transition to the last row of clean.
  if(!is.null(transition)){
    
    if(!is.null(before_after)){
      
      if(before_after == "before"){
        
        data <- data[0:transition, ]
      }
      else if(before_after == "after"){
        
        data <- data[transition:nrow(data), ]
      }
    }
  }
  
  # Bind an extra row of 'non-sleep' to first position of clean
  # This extra row is important for downstream analysis, 
  # but does not represent anything biological
  col_num <- ncol(data)
  data2 <- as.list(rep.int((sleep_threshold + 1), col_num)) %>% rbind(data)
  
  # Calculate analogue_to_binary df, where 1 
  # indicates sleep, while 0 indicates movement
  analogue_to_binary <- data2
  cols <- ncol(analogue_to_binary)
  for(i in 2:cols){
    
    analogue_to_binary[[i]] <- sapply(analogue_to_binary[[i]], 
                                                  function(foo){if(foo < sleep_threshold){foo <- 1} 
                                                  else{foo <- 0}})
  }
  
  # Create new dataframe five_min_bouts, 
  # which has 1's at sleep onset times, 
  # i.e. at the beginning
  # of time bouts of 300 seconds/5 min of inactivity, 
  # based on analogue_to_binary
  five_min_bouts <- analogue_to_binary
  for(i in 2:cols){
    
    five_min_bouts[[i]] <- DART_sleep_define(five_min_bouts[[i]], cell_length)
  }
  
  # Now slide down every column of five_min_bouts 
  # to find sleep onset and sleep offset
  # These are stored in two separate lists
  sleep_start_list <- list()
  sleep_end_list <- list()
  for(i in 2:cols){
    
    sleep_start_column <- DART_sleep_start(five_min_bouts[[i]])
    start_index <- length(sleep_start_list) + 1
    sleep_start_list[[start_index]] <- sleep_start_column
    
    sleep_end_column <- DART_sleep_end(five_min_bouts[[i]], cell_length)
    end_index <- length(sleep_end_list) + 1
    sleep_end_list[[end_index]] <- sleep_end_column
  }
  
  # Subtract corresponding columns from sleep_end_list 
  # from sleep_start_list, in oder to generate
  # a new list of vectors representing sleep lengths 
  # for each sleep bout for each fly.
  sleep_bout_length <- list()
  for(i in 2:cols){
    
    bout_index <- length(sleep_bout_length) + 1
    sleep_bout_length[[bout_index]] <- sleep_end_list[[bout_index]] - 
                                       sleep_start_list[[bout_index]] + 1
  }
  
  # Transform relevant vectors to minutes
  for(i in 1:length(sleep_start_list)){
    
    sleep_start_list[[i]] <- cell_length / 60 * sleep_start_list[[i]]
  }
  
  for(i in 1:length(sleep_end_list)){
    
    sleep_end_list[[i]] <- cell_length / 60 * sleep_end_list[[i]]
  }
  
  for(i in 1:length(sleep_bout_length)){
    
    sleep_bout_length[[i]] <- cell_length / 60 * sleep_bout_length[[i]]
  }
  
  output <- list(transition = transition, clean = clean, 
                 analogue_to_binary = analogue_to_binary, 
                 five_min_bouts = five_min_bouts, sleep_start_list = sleep_start_list, 
                 sleep_end_list = sleep_end_list, sleep_bout_length = sleep_bout_length)
  
  # Return list output
  output
}
