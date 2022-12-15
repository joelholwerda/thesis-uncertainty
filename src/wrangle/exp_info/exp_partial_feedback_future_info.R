exp_partial_feedback_future_info <- list(
  
  # Experiment name (also used as folder for save and load location)
  experiment_name = "exp_partial_feedback_future",
  
  # Specify which data was produced by the experiment
  data_types = c("demographics", "choices", "ears", "ellsberg"),
  manipulated_distribution = FALSE,
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 2,
    choices = 110,
    ears = 2,
    ellsberg = 1
  )
  
)