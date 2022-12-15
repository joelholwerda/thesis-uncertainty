summarise_demographics <- function(demographics_data) {
  
  require(rlang)
  
  # Count number of participants. If there are multiple experiments, first unite the experiment and participant columns
  if ("experiment" %in% names(demographics_data)) {
    n_participants <- unite(demographics_data, "participant", participant, experiment) %>% 
      .$participant %>% 
      unique() %>% 
      length()
  } else {
    n_participants <- demographics_data %>% 
      .$participant %>% 
      unique() %>% 
      length()
  }
  
  # Place demographics data into tibble
  demographics_summary <- tibble(
    n_participants = n_participants,
    n_per_condition = demographics_data$condition %>% table() %>% table_to_string(),
    age_mean = demographics_data$age %>% as.numeric() %>% mean(na.rm = TRUE),
    age_sd = demographics_data$age %>% as.numeric() %>% sd(na.rm = TRUE),
    gender = demographics_data$gender %>% table() %>% table_to_string(),
    n_female = sum(demographics_data$gender == "Female"),
    n_male = sum(demographics_data$gender == "Male")
  )
  
  return(demographics_summary)
  
}
