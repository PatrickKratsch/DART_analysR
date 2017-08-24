clean <- function(raw, ...){
  
  flies_app <- list(...)
  
  library(dplyr)
  library(tidyr)
  library(data.table)
  
  # Replace all missing values and N/A's with NA and convert to DT
  raw <- read.csv(raw, na.strings = c("", "N/A", "NA")) %>% setDT

  # Remove first unneccessary columns
  data <- select(raw, -c(1:(8 + (2 * (length(flies_app) - 1)))))[-(1:3), ]
  
  # Save time column
  time <- select(data, 1)
  
  # Select relevant columns and rows based on app_num (input)
  flies <- time
  colnames(flies) <- "time"
  
  j <- 1
  for(fly_num in flies_app){
    
    # Select flies from current monitor
    flies_app <- select(data, 3:(fly_num + 2))
    
    # Name columns
    for(i in 1:dim(flies_app)[2]){
      
      colnames(flies_app)[i] <- sprintf("fly %i", j)
      j <- j + 1
    }
    
    # Bind data.table together
    flies <- cbind(flies, flies_app)
    
    # Remove columns already added to `flies`
    data <- data[, -(1:(fly_num * 2 + 3))]
  }
  
  # Factor-to-numeric of time column
  flies$time <- flies$time %>% as.character %>% as.numeric
  
  # Factor-to-numeric of fly columns
  for(i in 1:(dim(flies)[2] - 1)){
    
    current_fly <- sprintf("fly %i", i)
    flies[[current_fly]] <- flies[[current_fly]] %>% as.character %>% as.numeric
  }
  
  # Find and correct missing values
  # Split NA's in flies by NA intervals
  flies_NA <- which(is.na(flies[[1]]))
  flies_NA_split <- split(flies_NA, cumsum(c(TRUE, diff(flies_NA) != 1)))
  NA_length <- length(flies_NA_split)
  
  # Fill in NA's in time column
  for(i in 1:NA_length){
    
    NA_chunk <- c(flies_NA_split[[i]][1] - 1, flies_NA_split[[i]])
    chunk_interval1 <- flies[[1]][NA_chunk[1] - 1]
    chunk_interval2 <- flies[[1]][NA_chunk[length(NA_chunk)] + 1]
    interval_filler <- (chunk_interval2 - chunk_interval1) / (length(NA_chunk) + 1)
    
    j <- 1
    for(cell in NA_chunk){
      
      flies[[1]][cell] <- flies[[1]][NA_chunk[j] - 1] %>% as.character %>%
                         as.numeric %>% + interval_filler
      j <- j + 1
    }
  }
  
  # Fill in NA's for fly velocity
  for(i in 2:(dim(flies)[2])){
    
    for(j in 1:NA_length){
      
      NA_chunk <- c(flies_NA_split[[j]][1] - 1, flies_NA_split[[j]])
      chunk_interval1 <- flies[[i]][NA_chunk[1] - 1]
      chunk_interval2 <- flies[[i]][NA_chunk[length(NA_chunk)] + 1]
      interval_filler <- (chunk_interval2 + chunk_interval1) / 2
      
      for(cell in NA_chunk){
        
        flies[[i]][cell] <- interval_filler
      }
    }
  }
  
  flies
}
