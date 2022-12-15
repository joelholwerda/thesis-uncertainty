exp_information_or_reward_info <- list(
  
  # Experiment name (also used as folder for save and load location)
  experiment_name = "exp_information_or_reward",
  
  # Specify which data was produced by the experiment
  data_types = c("demographics", "sampling", "ears", "ellsberg"),
  manipulated_distribution = FALSE,
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 3,
    sampling = 100,
    ears = 2,
    ellsberg = 1
  )
  
)