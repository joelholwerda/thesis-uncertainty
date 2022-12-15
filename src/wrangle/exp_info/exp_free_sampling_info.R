exp_free_sampling_info <- list(
  
  # Experiment name (also used as folder for save and load location)
  experiment_name = "exp_free_sampling",
  
  # Specify which data was produced by the experiment
  data_types = c("demographics", "sampling", "choices", "ears"),
  manipulated_distribution = FALSE,
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    sampling = NA,
    choices = 6,
    ears = 6
  )
  
)
