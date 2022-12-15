exp_partial_feedback_past_info <- list(
  
  # Experiment name (also used as folder for save and load location)
  experiment_name = "exp_partial_feedback_past",
  
  # Specify which data was produced by the experiment
  data_types = c("demographics", "choices", "ears"),
  manipulated_distribution = TRUE,
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    choices = 110,
    ears = 4
  )
  
)