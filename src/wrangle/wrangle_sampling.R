wrangle_sampling <- function(raw_data, exp_info){
  
  # Test that the data imported correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(raw_data, exp_info, "sampling")
  
  # Wrangle sampling data ----
  
  # exp_information_or_reward and exp_free_sampling have different structures and are wrangled separately
  if (exp_info$experiment_name == "exp_free_sampling") {
    tidy_data <- raw_data %>% 
      mutate(
        # Change condition name from referring to the level of variability in the images to whether they were unique or constant
        condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images")
      ) %>% 
      select(
        "participant",
        "randomID",
        "condition",
        "sample_number",
        "block" = round_number,
        "sampling_type",
        "nSamples",
        "mean",
        "feedback",
        "stimulus", 
        "stimulus_colour", 
        "rt"
      )
    
  } else if (exp_info$experiment_name == "exp_information_or_reward") {
  
  # Ordered columns to select (include distribution if applicable)
  columns <- c(
    "participant", 
    "condition", 
    "trial",
    "block",
    "choice",
    "action",
    "risky",
    "claim",
    "feedback",
    "left_stimulus", 
    "right_stimulus", 
    "left_option", 
    "right_option", 
    "rt"
  )
  
  if (exp_info$manipulated_distribution == TRUE) {columns <- append(columns, "distribution", after = 2)}
  
  # Create risky and blocks columns
  tidy_data <- raw_data %>%
    mutate(
      # Change condition name from referring to the level of variability in the images to whether they were unique or constant
      condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"),
      # Label as risky boolean
      risky = case_when(
        choice == "safe" ~ 0,
        choice == "risky" ~ 1
      ),
      # Label as claim boolean
      claim = case_when(
        action == "observe" ~ 0,
        action == "claim" ~ 1
      ),
      # Split into five blocks
      block = cut_interval(trial, 5, labels = FALSE)
    ) %>% 
    select(all_of(columns))
  
  }
  
  return(tidy_data)
  
}
