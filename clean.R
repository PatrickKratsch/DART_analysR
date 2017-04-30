clean <- function(raw, app_num, sleep_thresh){
  
  library(dplyr)
  library(tidyr)
  
  # Replace all missing values and N/A's with NA
  raw <- read.csv(raw, na.strings = c("", "N/A", "NA"))
  raw_tbl <- tbl_df(raw)
  # Remove first 10 columns from raw data
  data <- select(raw, -(X:X.9))
  
  # Select relevant columns and rows based on app_num (input)
  time <- select(data, X.10)
  time <- time[-(1:3), ]
  if(app_num == 1 ){
    flies_app_1 <- select(data, X.12:X.31)
    flies_app_1 <- flies_app_1[-(1:3), ]
    flies_app_1 <- cbind(time, flies_app_1)
    for(i in 1:20){
      names(flies_app_1)[i + 1] <- sprintf("fly %i", i)
    }
    flies <- flies_app_1
  }
  else if(app_num == 2){
    flies_app_1 <- select(data, X.12:X.31)
    flies_app_1 <- flies_app_1[-(1:3), ]
    flies_app_1 <- cbind(time, flies_app_1)
    for(i in 1:20){
      names(flies_app_1)[i + 1] <- sprintf("fly %i", i)
    }
    
    flies_app_2 <- select(data, X.55:X.74)
    flies_app_2 <- flies_app_2[-(1:3), ]
    for(i in 1:20){
      names(flies_app_2)[i] <- sprintf("fly %i", i + 20)
    }
    flies <- cbind(flies_app_1, flies_app_2)
  }
  
  # Factor-to-numeric of time column
  flies$time <- as.numeric(as.character(flies$time))
  
  ## Find and correct missing values
  # Split NA's in flies by consecutive sums
  flies_NA <- which(is.na(flies[[1]]))
  flies_NA_split <- split(flies_NA, cumsum(c(TRUE, diff(flies_NA) != 1)))
  
  # Fill in NA's in time column
  NA_length <- length(flies_NA_split)
  for(i in 1:NA_length){
    NA_chunk <- flies_NA_split[[i]]
    chunk_interval1 <- as.numeric(as.character(flies[[1]][NA_chunk[1] - 1]))
    chunk_interval2 <- as.numeric(as.character(flies[[1]][NA_chunk[length(NA_chunk)] + 1]))
    interval_filler <- (chunk_interval2 - chunk_interval1) / (length(NA_chunk) + 1)
    
    j <- 1
    for(cell in NA_chunk){
      flies[[1]][cell] <- as.numeric(as.character(flies[[1]][NA_chunk[j] - 1])) + interval_filler
      j <- j + 1
    }
  }
  
  flies
}
