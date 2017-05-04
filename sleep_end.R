sleep_end <- function(vec){
    
    adjusted_length <- length(vec) - 1
    
    sleep_end_vec <- vector(mode = "numeric")
    
    for(i in 1:adjusted_length){
        if(vec[i] == 1 && vec[i + 1] == 0){
            
            sleep_end_vec <- c(sleep_end_vec, (i + 3))
        }
    }
    
    sleep_end_vec
}