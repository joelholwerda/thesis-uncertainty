wrangle_choices <- function(raw_data, exp_info){
  
  # Test that the data imported correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(raw_data, exp_info, "choices")
  
  # Wrangle choices data ----
  if (exp_info$experiment_name == "exp_free_sampling") {
    tidy_data <- raw_data %>% 
      mutate(
        choice = case_when(choice == 1 ~ "safe", choice == 2 ~ "risky")
      ) %>% 
      select(
        "participant",
        "randomID",
        "condition", 
        "block" = round_number,
        "sampling_type",
        "choice",
        "mean",
        "safe_value",
        "stimulus", 
        "stimulus_colour", 
        "rt"
      )
    
  } else {
    
    # Ordered columns to select (include distribution if applicable)
    columns <- c(
      "participant", 
      "condition", 
      "trial",
      "block",
      "choice",
      "feedback",
      "left_stimulus", 
      "right_stimulus", 
      "left_option", 
      "right_option", 
      "rt"
    )
    
    if (exp_info$manipulated_distribution == TRUE) {columns <- append(columns, "distribution", after = 2)}
    
    # Split into blocks and remove junk columns
    tidy_data <- raw_data %>% 
      mutate(
        # Split into five blocks
        block = cut_interval(trial, 5, labels = FALSE)
      ) %>% 
      select(all_of(columns))
  }
  
  # Format conditions and convert choice to numerical for calculating the proportion of risky choices
  tidy_data <- tidy_data %>%
    mutate(
      # Change condition name from referring to the level of variability in the images to whether they were unique or constant
      condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"),
      # Label as risky boolean
      risky = case_when(
        choice == "safe" ~ 0,
        choice == "risky" ~ 1
      )
    )
  
  return(tidy_data)

}
