import_data <- function(exp_info){

  experiment_data <- list()
  
  # Loop through data types for each experiment
  for (data in exp_info$data_types) {
    
    if (length(exp_info$data_types) == 0) stop("data_types has length 0. Check that the experiment info file was sourced correctly.")
    
    # List csv files
    path <- file.path("data", "raw_data", exp_info$experiment_name, data)
    files <- here::here(path, list.files(path = path, pattern = "*.csv"))
    
    # For the EARS data in exp_partial_feedback_future, "exp_information_or_reward", and exp_sampling, read_csv was parsing the questions column with the wrong type specifications
    # Set manually for that column, otherwise use the default specifications
    if (exp_info$experiment_name %in% c("exp_partial_feedback_future", "exp_information_or_reward", "exp_free_sampling") & data == "ears") {
      col_types <- cols(questions = "c", participant = "c")
    } else {
      col_types <- cols(participant = "c")
    }
    
    # Read and combine
    temp_data <- files %>%
      map(read_csv, col_types = col_types) %>% 
      bind_rows()
    
    # Check data and assign to output list with name of data type
    if (length(temp_data) == 0) stop(paste("temp_data for", data,"has length 0. Check that the data exists and that import_data() is working correctly."))
    experiment_data[[data]] <- temp_data
    
  }
  
  return(experiment_data)
    
}