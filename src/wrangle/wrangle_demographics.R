wrangle_demographics <- function(raw_data, exp_info){
  
  # Test that the data imported correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(raw_data, exp_info, "demographics")
  
  # Wrangle demographics data ----
  age_gender <- raw_data %>%
    # For each participant, take the row containing age and gender data
    group_by(participant) %>% 
    slice(1) %>%
    # any_of() is required because not all experiments have a randomID column
    select(
      any_of(
        c(
          "participant",
          "randomID",
          "condition", 
          "responses"
        )
      )
    ) %>%
    # Convert from JSON format
    mutate(responses = map(responses, fromJSON)) %>% 
    unnest(responses) %>% 
    mutate(
      responses = unlist(responses),
      type = c("age", "gender")
    ) %>% 
    # Spread into age and gender columns
    spread(key = type, value = responses) %>%
    mutate(
      # Recode gender columns (this works for the current dataset because all responses were female or male)
      gender = ifelse(gender %>% str_to_lower() %>% str_detect("f"), "Female", "Male")
    ) %>% 
    ungroup()

  # Wrangle comments
  # exp_partial_feedback_past and exp_free_sampling didn't record comments from participants
  if (!exp_info$experiment_name %in% c("exp_partial_feedback_past", "exp_free_sampling")) {
  
    comments <- raw_data %>%
      # For each participant, take the row containing the comments data
      group_by(participant) %>% 
      slice(n()) %>%
      select(
        participant,
        condition, 
        responses
      ) %>% 
      # Convert from JSON format
      mutate(responses = map(responses, fromJSON)) %>% 
      mutate(
        comments = unlist(responses)
      ) %>%
      select(-responses) %>% 
      ungroup()
    
    tidy_data <- left_join(age_gender, comments, by = c("condition", "participant")) %>%
      # Change condition name from referring to the level of variability in the images to whether they were unique or constant
      mutate(condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"))
    
    return(tidy_data)
    # Wrangle demographics from experiments without comments
  } else {
    
    tidy_data <- age_gender %>%
      # Change condition name from referring to the level of variability in the images to whether they were unique or constant
      mutate(condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"))
    
    return(tidy_data)
  }
  
}
