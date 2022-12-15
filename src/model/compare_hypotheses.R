compare_hypotheses <- function(hypotheses, n_samples) {
  
  # Select linear hypotheses
  linear_hypotheses_list <- hypotheses[
    str_detect(names(hypotheses), "ears") & 
    str_detect(names(hypotheses), "linear")
  ]
  
  # Select ordinal hypotheses
  ordinal_hypotheses_list <- hypotheses[
    str_detect(names(hypotheses), "ears") & 
    !str_detect(names(hypotheses), "linear")
  ]
  
  # Collect estimates
  linear_hypotheses <- linear_hypotheses_list %>% map_df(~ .$summary)
  ordinal_hypotheses <- ordinal_hypotheses_list %>% map_df(~ .$summary)
  
  # Add hypothesis names
  linear_hypotheses$hypothesis <- names(linear_hypotheses_list)
  ordinal_hypotheses$hypothesis <- paste0(names(ordinal_hypotheses_list), "_ordinal")
  
  # Ensure hypotheses are comparable
  linear_hypotheses <- conform_hypotheses(linear_hypotheses, n_samples)
  ordinal_hypotheses <- conform_hypotheses(ordinal_hypotheses, n_samples) 
  
  # Throw error if linear and ordinal hypotheses don't match
  comparable <- str_remove(linear_hypotheses$hypothesis, "linear") == str_remove(ordinal_hypotheses$hypothesis, "ordinal")
  if (any(!comparable)) {error("Check that each ordinal hypothesis corresponds to a linear hypothesis.")}
  
  # Calculate differences
  differences <- tibble(
    id = 1:(dim(linear_hypotheses)[1]),
    hypothesis = str_remove(linear_hypotheses$hypothesis, "linear") %>% paste0("difference"),
    estimate = linear_hypotheses$estimate - ordinal_hypotheses$estimate,
    ci_lower = linear_hypotheses$ci_lower - ordinal_hypotheses$ci_lower,
    ci_upper = linear_hypotheses$ci_upper - ordinal_hypotheses$ci_upper,
    evidence_ratio = linear_hypotheses$evidence_ratio / ordinal_hypotheses$evidence_ratio,
    posterior_probability = linear_hypotheses$posterior_probability - ordinal_hypotheses$posterior_probability
  )
  
  all_hypotheses <- bind_rows(differences, linear_hypotheses, ordinal_hypotheses) %>% 
    arrange(id, hypothesis) %>% 
    select(-id, -width, -ci_type)
    
  return(all_hypotheses)
  
}

conform_hypotheses <- function(hypotheses, n_samples) {
  
  hypotheses %>% 
  arrange(hypothesis, n_samples) %>% 
    mutate(
      # Ensure all hypotheses are in the same direction and not infinite
      evidence_ratio = case_when(
        is.infinite(evidence_ratio) ~ n_samples,
        evidence_ratio < 1 ~ 1 / evidence_ratio,
        TRUE ~ evidence_ratio
      ),
      posterior_probability = case_when(
        posterior_probability < .5 ~ 1 - posterior_probability,
        TRUE ~ posterior_probability
      )
    )
}
