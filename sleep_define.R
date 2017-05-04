sleep_define <- function(vec, cell_length){
    
  # cell_num stores the number of consecutive inactivity cells to define a sleep bout 
  len <- length(vec)
  cell_num <- 300 / cell_length
  
  # The for-loop looking for sleep bouts can run intil adjusted_len, akin to EOF  
  adjusted_len <- len - (300 - cell_length)
  
  # Run for loop to identify sleep bouts (= cell_num times inactivity cells)
  for(i in 2:adjusted_len){
    window_sum <- sum(vec[i:(i + cell_num - 1)])
        
    if(window_sum == cell_num){
      vec[i] <- 1
    }
    else{
      vec[i] <- 0
    }
  }
    
    # Add an extra condition for the last 4
    # bouts in vec (analogue_to_binary). These
    # cannot be one, and so must all be set to zero.
    vec[(len - 300 - (2 * cell_length)):len] <- 0
    
    vec
}
