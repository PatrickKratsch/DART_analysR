sleep_define <- function(vec, cell_length){
    
  # cell_num stores the number of consecutive inactivity cells needed to define a sleep bout 
  len <- length(vec)
  cell_num <- 300 / cell_length
  
  # The for-loop looking for sleep bouts can run intil adjusted_len, akin to EOF  
  adjusted_len <- len - (cell_num - 1)
  
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
    
    # Add an extra condition for the last 
    # bouts in vec (analogue_to_binary). These
    # cannot be one, and so must all be set to zero.
    vec[(len - (cell_num - 2)):len] <- 0
    
    vec
}
