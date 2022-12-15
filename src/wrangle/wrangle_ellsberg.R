wrangle_ellsberg <- function(raw_data, exp_info){

  # Test that the data imported correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(raw_data, exp_info, "ellsberg")
  
  tidy_data <- raw_data %>% 
    mutate(
      # Change condition name from referring to the level of variability in the images to whether they were unique or constant
      condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"),
      # Record the response from the first choice (equivalent to standard ellesberg task)
      # Select second outcome displayed based on position within the string and use this to determine first choice
      first_prob_win = probWin %>% 
        str_sub(start = 5, end = 6) %>% 
        as.numeric(),
      first_choice = case_when(
        first_prob_win == 30 ~ "Known",
        first_prob_win == 70 ~ "Unknown"
      ),
      
      # Record the final Ellsberg score (+/-2.5 percentage points from the final displayed outcome depending on final choice)
      # Select final outcome displayed and final choice based on their position within the string
      final_prob_win = probWin %>% 
        str_sub(start = -3, end = -2) %>% 
        as.numeric(),
      final_choice = choice %>% 
        str_sub(start = -9, end = -3) %>% 
        str_replace_all(",|\"", ""),
      ellsberg_score = case_when(
        final_choice == "known" ~ final_prob_win - 2.5,
        final_choice == "unknown" ~ final_prob_win + 2.5
      )
    ) %>% 
    select(
      participant,
      condition,
      first_choice,
      ellsberg_score
    )
  
  return(tidy_data)
  
}