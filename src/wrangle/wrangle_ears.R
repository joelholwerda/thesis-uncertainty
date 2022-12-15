wrangle_ears <- function(raw_data, exp_info){
  
  # Test that the data imported correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(raw_data, exp_info, "ears")
  
  # The format of the data from exp_partial_feedback_past was different than the others ----
  # Reformat each experiment to ensure consistency
  
  # Reformat exp_partial_feedback_past ----
  if (exp_info$experiment_name == "exp_partial_feedback_past") {
    
    raw_data <- raw_data %>%
      mutate(order = "0,1,2,3,4,5,6,7,8,9")
    
    shared_columns <- c(
      "participant",
      "condition",
      "risky",
      "colour",
      "order"
    )

    set_A <- raw_data %>%
      filter(set == "A") %>% 
      select(
        all_of(shared_columns),
        question_1 = Q0,
        question_2 = Q1,
        question_3 = Q2,
        question_4 = Q3,
        question_5 = Q4
      )
    
    set_B <- raw_data %>%
      filter(set == "B") %>% 
      select(
        all_of(shared_columns),
        question_6 = Q0,
        question_7 = Q1,
        question_8 = Q2,
        question_9 = Q3,
        question_10 = Q4
      )
    
    formatted_data <- left_join(set_A, set_B, by = shared_columns)
    
  # Reformat exp_partial_feedback_future, exp_information_or_reward, and exp_sampling ----
  } else if (exp_info$experiment_name %in% c("exp_partial_feedback_future", "exp_information_or_reward", "exp_free_sampling")) {
    
    # Reorder EARS responses based on question number not presentation order
    
    # Select just the responses to each question
    responses <- raw_data %>% select(contains("Q"), -questions)
    
    # Create matrix to hold ordered responses
    n_rows <- dim(responses)[1]
    n_cols <- dim(responses)[2]
    question_matrix <- matrix(nrow = n_rows, ncol = n_cols)
    
    # Separate out question order from single string
    order <- str_split(raw_data$questions, ",", simplify = TRUE)
    
    # Label columns with question numbers
    col_names <- array(n_cols)
    for (col in 1:n_cols) { col_names[col] <- paste("question", col, sep = "_") }
    colnames(question_matrix) <- col_names
    
    # Loop through each cell of the question order matrix
    # Reorder responses for each row in an array and then transfer to matrix with all responses
    for (row in 1:n_rows) {
      
      row_array <- array(dim = n_cols)
      
      for (col in 1:n_cols) {
        question_number <- order[row, col]
        row_array[as.numeric(question_number) + 1] <- responses[row, col][[1]]
        row_array
      }
      
      question_matrix[row, ] <- row_array
    }
    
    # Convert ordered responses to a tibble
    ordered_responses <- question_matrix %>% as_tibble()
    
    # Clean up unused columns
    formatted_data <- raw_data %>%
      select(
        any_of(
          c(
            "participant",
            "randomID",
            "condition",
            "sampling_type",
            "risky",
            "colour", 
            "side",
            "stimulus",
            "order" = "questions"
          )
        )
      ) %>% 
      # Combine with response data
      bind_cols(ordered_responses)
  }
  
  # Calculate an average score for the aleatory and epistemic questions (1 to 7)
  # Calculate an average score combining the epistemic questions and reverse scored aleatory questions
  # exp_free_sampling used a 4-item version of the EARS scale 
  if (exp_info$experiment_name == "exp_free_sampling") {
    tidy_data <- formatted_data %>%
      mutate(
        # Change condition name from referring to the level of variability in the images to whether they were unique or constant
        condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"),
        aleatory = (question_1 + question_2) / 2 + 1,
        epistemic = (question_3 + question_4) / 2 + 1,
        combined_epistemic = ((8 - aleatory) + epistemic) / 2
      )
    # the other experiments used a 10-item version of the EARS scale 
  } else {
    tidy_data <- formatted_data %>%
      mutate(
        # Change condition name from referring to the level of variability in the images to whether they were unique or constant
        condition = case_when(condition == "low" ~ "Constant images", condition == "high" ~ "Unique images"),
        aleatory = (question_1 + question_2 + question_3 + question_4) / 4 + 1,
        epistemic = (question_5 + question_6 + question_7 + question_8 + question_9 + question_10) / 6 + 1,
        combined_epistemic = ((8 - aleatory) * 4 + epistemic * 6) / 10
      )
  }
  
  return(tidy_data)
  
}