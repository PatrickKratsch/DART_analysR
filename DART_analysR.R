DART_analysR <- function(clean, sleep_threshold, cell_length, ld_transition){
  
  ## Parameters ## 
  # clean = clean.R output
  # sleep_threshold = velocity of flies must be below this value to be considered non-moving
  # cell_length = length of one sampling window in seconds
  # ld_transition = time point of light-to-dark transition in seconds
  
  # Source auxilliary functions (need to be in working directory)
  source("sleep_define.R")
  source("sleep_start.R")
  source("sleep_end.R")
  
  # Define experiment length
  exp_length <- dim(clean)[1]
  
  # Bind an extra row of 1's to first position of clean
  # This extra row is important for downstream analysis, but does not represent anything physiological
  col_num <- dim(clean)[2]
  ghost_row <- rep.int(1, col_num - 1)
  ghost_row2 <- c(0, ghost_row)
  clean2 <- rbind(ghost_row2, clean)
  
  # Calculate analogue_to_binary df, where 1 indicates no movement, while 0 indicates movement
  analogue_to_binary <- clean2
  cols <- ncol(analogue_to_binary)
  for(i in 2:cols){
    analogue_to_binary[, i] <- sapply(analogue_to_binary[, i], function(foo){
      if(foo < sleep_threshold){foo <- 1} else{foo <- 0}})
  }
  
  # Create new dataframe five_min_bouts, which has 1's at sleep onset times, i.e. at the beginning
  # of time bouts of 5 min of inactivity, based on analogue_to_binary.
  five_min_bouts <- analogue_to_binary
  for(i in 2:cols){
    five_min_bouts[, i] <- sleep_define(five_min_bouts[, i], cell_length)
  }
  
  # Now we slide down every column of five_min_bouts to find sleep onset and sleep offset.
  # The latter are stored in two separate lists.
  sleep_start_list <- list()
  sleep_end_list <- list()
  for(i in 2:cols){
    sleep_start_column <- sleep_start(five_min_bouts[, i])
    start_index <- length(sleep_start_list) + 1
    sleep_start_list[[start_index]] <- sleep_start_column
    
    sleep_end_column <- sleep_end(five_min_bouts[, i])
    end_index <- length(sleep_end_list) + 1
    sleep_end_list[[end_index]] <- sleep_end_column
  }
  
  # Subtract corresponding columns from sleep_end_list from sleep_start_list, in oder to generate
  # a new list of vectors representing sleep lengths for each sleep bout for each fly.
  sleep_bout_length <- list()
  for(i in 2:cols){
    bout_index <- length(sleep_bout_length) + 1
    sleep_bout_length[[bout_index]] <- sleep_end_list[[bout_index]] - sleep_start_list[[bout_index]] + 1
  }
  
  output <- list(ld_transition = ld_transition, clean = clean, analogue_to_binary = analogue_to_binary, 
                 five_min_bouts = five_min_bouts, sleep_start_list = sleep_start_list, 
                 sleep_end_list = sleep_end_list, sleep_bout_length = sleep_bout_length)
  
  # Return list output
  output
}
