DART_transform <- function(data, sample_names = NA){
  
  # Load relevant packages
  library("data.table")
  
  # Read in data
  data <- read.csv(data)
  
  # Transform data into data.table
  setDT(data)
  
  # Generate an offset table to prepare
  # displacement analysis
  data_offset <- rbind(data[2:nrow(data), ], data[nrow(data), ])
  data_displacement <- cbind(data_offset[, 1], data_offset[, 2:ncol(data_offset)] - data[, 2:ncol(data)])
  
  # Save time in vector to bin it back to data.table
  # once movement is generated
  time <- data_displacement[, 1]
  
  # Now, get displacement data.table without time,
  # and calculate movement - bind time back to movement
  data_displacement2 <- data_displacement[, 2:ncol(data_displacement)]
  movement <- data_displacement2[, lapply(1:(ncol(.SD)/2), function(x) sqrt((.SD[[2*x-1]])^2 + (.SD[[2*x]])^2))]
  movement <- cbind(time, movement)
  
  # Kill last row, because there is no movement for the last second
  movement <- movement[-(nrow(movement)), ]
  
  # Rename samples if relevant
  if(!is.na(sample_names[1])){
    
    colnames(movement) <- c("time", sample_names)
  }
  
  movement
}
