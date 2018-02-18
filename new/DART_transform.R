DART_transform <- function(data){
  
  # data                    = input to this function, as called by DART_transform_batch -
  #                           once .csv file at a time from a directory of output .csv
  #                           files, as defined by dir_path in DART_transform_batch
  
  # Load relevant packages
  library("data.table")
  
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
  
  movement
}
