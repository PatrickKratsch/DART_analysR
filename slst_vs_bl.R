slst_vs_bl <- function(DART_analysR_output, xlsx_file_name, channels, transition = NULL){
    
    # NB: the xlsx package is built in JAVA, so you need JAVA installed to run this script
    library("xlsx")
    library("plyr")
    
    # Create data frames for each fly of sleep start times vs bout lengths
    # Store these data frames in a list
    sleepstart_vs_boutlength_dfs <- list()
    i <- 1
    for(channel in channels){
        channel_length <- length(DART_analysR_output$sleep_start_list[[channel]])
        sleepstart_vs_boutlength_dfs[[i]] <- data.frame(DART_analysR_output$sleep_start_list[[channel]],
                                                        DART_analysR_output$sleep_bout_length[[channel]],
                                                        rep(channel, channel_length))
        names(sleepstart_vs_boutlength_dfs[[i]]) <- c("sleep_start", "bout_length", "channel")
        i <- i + 1
    }
    
    # Bind all data frames into one big data frame
    single_df <- ldply(sleepstart_vs_boutlength_dfs, data.frame)
    
    # If transition is specified, add light indicator column
    # Regardless of transition type, a '1' will be added
    # before and at transition, and '0' after transition
    if(!is.null(transition)){
      
      for(i in 1:dim(single_df)[1]){
        
        
      } 
    }
    
    # Write that data frame into xlsx file
    write.xlsx(single_df, xlsx_file_name)
}