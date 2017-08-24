sleep_start <- function(vec){
    
    adjusted_length <- length(vec) - 1
    
    sleep_start_vec <- vector(mode = "numeric")
    
    for(i in 1:adjusted_length){
      
        if(vec[i] == 0 && vec[i + 1] == 1){
          
            sleep_start_vec <- c(sleep_start_vec, i + 1)
        }
    }
        
    sleep_start_vec
}