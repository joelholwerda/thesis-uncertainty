# Experiments
# 7a. `exp_partial_feedback_past`: Examined the influence of stimulus variability on choices with the EARS referring to past choices
# 7b. `exp_partial_feedback_future`: Examined the influence of stimulus variability on choices with the EARS referring to a specific future choice
# 8. `exp_information_or_reward`: Examined the influence of stimulus variability on whether participants chose to observe or claim the outcome of their choices
# 9. exp_free_sampling: Examined the influence of stimulus variability the number of outcomes participants chose to observe before making a consequential choice

# Data wrangling
# The experiment info files allow the functions to import and wrangle the data automatically. Changing their naming structure might break the wrangling functions. Additional experiments would require creating additional info files.

# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(lubridate)
library(forcats)
library(jsonlite)
library(standardize)
library(rlang)
library(here)

# Source experiment info files and wrangling functions
here::here("src", "wrangle", "exp_info", "exp_partial_feedback_past_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_partial_feedback_future_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_information_or_reward_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_free_sampling_info.R") %>% source()
here::here("src", "wrangle", "import_data.R") %>% source()
here::here("src", "wrangle", "wrangle_all.R") %>% source()
here::here("src", "wrangle", "table_to_string.R") %>% source()
here::here("src", "wrangle", "format_conditions.R") %>% source()
here::here("src", "wrangle", "summarise_demographics.R") %>% source()
here::here("src", "utilities", "save_tidy_data.R") %>% source()

temp <- list()

## 7. Partial-feedback ---------------------------------------------------------

### Import data ----

# Import data and do initial wrangling with generic wrangling functions
exp_partial_feedback_past <- import_data(exp_partial_feedback_past_info) %>% 
  wrangle_all(exp_partial_feedback_past_info)

exp_partial_feedback_future <- import_data(exp_partial_feedback_future_info) %>% 
  wrangle_all(exp_partial_feedback_future_info)

# Combine exp_partial_feedback_past and exp_partial_feedback_future
combine_experiments <- function(data_type) {
  bind_rows(
    "Experiment a" = exp_partial_feedback_past[[data_type]],
    "Experiment b" = exp_partial_feedback_future[[data_type]],
    .id = "experiment"
  ) %>% 
    mutate(
      participant = ifelse(
        experiment == "Experiment a",
        paste0(participant, "a"),
        paste0(participant, "b")
      )
    )
}

exp_partial_feedback <- list(
  demographics = combine_experiments("demographics"),
  choices = combine_experiments("choices"),
  ears = combine_experiments("ears")
)

### Wrangle demographics ----

exp_partial_feedback$demographics_summary <- summarise_demographics(exp_partial_feedback$demographics)
exp_partial_feedback_past$demographics_summary <- summarise_demographics(exp_partial_feedback_past$demographics)
exp_partial_feedback_future$demographics_summary <- summarise_demographics(exp_partial_feedback_future$demographics)

### Wrangle EARS ----

# Summary used for graphing
exp_partial_feedback$ears_summary <- exp_partial_feedback$ears %>% 
  select(
    experiment,
    participant,
    condition,
    risky,
    epistemic,
    aleatory
  ) %>% 
  pivot_longer(
    cols = c("epistemic", "aleatory"),
    names_to = "uncertainty_type",
    values_to = "mean_response"
  )

# Long data used for modelling
exp_partial_feedback$ears_long <- exp_partial_feedback$ears %>% 
  select(
    experiment,
    participant,
    condition,
    risky,
    starts_with("question")
  ) %>% 
  pivot_longer(
    cols = starts_with("question"),
    names_to = "question",
    values_to = "response"
  ) %>% 
  mutate(
    uncertainty_type = case_when(
      question %in% paste0("question_", 1:4) ~ "Aleatory",
      question %in% paste0("question_", 5:10) ~ "Epistemic"
    ),
    response = (response + 1) %>% factor(levels = 1:7, ordered = TRUE)
  )

### Wrangle choices ----

exp_partial_feedback$choices_summary <- exp_partial_feedback$choices %>% 
  group_by(experiment, participant, condition, distribution) %>% 
  summarise(
    prop_risky = mean(risky),
    risky_count = sum(risky),
    n_choices = n()
  ) %>% 
  ungroup() %>% 
  mutate(distribution = as.character(distribution))

### Combine choices with EARS responses (risky) ----

exp_partial_feedback$choices_ears <- exp_partial_feedback$ears_summary %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_lower()) %>%
  pivot_wider(names_from = uncertainty_type, values_from = mean_response) %>%
  left_join(exp_partial_feedback$choices_summary)

exp_partial_feedback$choices_ears_long <- exp_partial_feedback$ears_long %>% 
  left_join(exp_partial_feedback$choices_summary)

### Wrangle Ellsberg ----

exp_partial_feedback$ellsberg <- exp_partial_feedback_future$ellsberg %>% mutate(participant = paste0(participant, "b"))

# Combine Ellsberg responses with choices data
exp_partial_feedback$ellsberg_choices <- exp_partial_feedback$choices_ears %>% 
  filter(experiment == "Experiment b") %>% 
  select(-distribution) %>% 
  left_join(exp_partial_feedback$ellsberg) %>% 
  select(-experiment)

### Format categorical variables ----

# Column names of categorical variables
temp$exp_partial_feedback_factors <- c("experiment", "condition", "risky", "first_choice", "uncertainty_type")

# Format and append contrasts for each categorical variable in each tibble
exp_partial_feedback <- exp_partial_feedback %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(temp$exp_partial_feedback_factors)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

### Save tidy data ----

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_partial_feedback)

## 8. Information or reward ----------------------------------------------------

### Import data ----

# Import data and do initial wrangling with generic wrangling functions
exp_information_or_reward <- import_data(exp_information_or_reward_info) %>% 
  wrangle_all(exp_information_or_reward_info)

### Wrangle demographics ----

exp_information_or_reward$demographics_summary <- summarise_demographics(exp_information_or_reward$demographics)

### Wrangle EARS ----

# Wrangle EARS responses ----

# Summary used for graphing
exp_information_or_reward$ears_summary <- exp_information_or_reward$ears %>% 
  select(
    participant,
    condition,
    risky,
    epistemic,
    aleatory
  ) %>% 
  pivot_longer(
    cols = c("epistemic", "aleatory"),
    names_to = "uncertainty_type",
    values_to = "mean_response"
  )

# Long data used for modelling
exp_information_or_reward$ears_long <- exp_information_or_reward$ears %>% 
  select(
    participant,
    condition,
    risky,
    starts_with("question")
  ) %>% 
  pivot_longer(
    cols = starts_with("question"),
    names_to = "question",
    values_to = "response"
  ) %>% 
  mutate(
    uncertainty_type = case_when(
      question %in% paste0("question_", 1:4) ~ "Aleatory",
      question %in% paste0("question_", 5:10) ~ "Epistemic"
    ),
    response = (response + 1) %>% factor(levels = 1:7, ordered = TRUE)
  )

# Pivot ears responses for risky and safe so that epistemic and aleatory are different columns
ears_risky_wide <- exp_information_or_reward$ears_summary %>% 
  filter(risky == "risky") %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_lower()) %>%
  pivot_wider(names_from = uncertainty_type, values_from = mean_response)

ears_safe_wide <- exp_information_or_reward$ears_summary %>% 
  filter(risky == "safe") %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_lower()) %>%
  pivot_wider(names_from = uncertainty_type, values_from = mean_response)

exp_information_or_reward$ears_wide <- bind_rows(ears_risky_wide, ears_safe_wide)

### Wrangle sampling/choices ----

exp_information_or_reward$sampling_summary <- exp_information_or_reward$sampling %>% 
  mutate(
    safe_explore = ifelse(risky == 0 & claim == 0, 1, 0),
    safe_claim = ifelse(risky == 0 & claim == 1, 1, 0),
    risky_explore = ifelse(risky == 1 & claim == 0, 1, 0),
    risky_claim = ifelse(risky == 1 & claim == 1, 1, 0),
    safe_feedback =  feedback * safe_explore %>% na_if(0),
    risky_feedback = feedback * risky_explore %>% na_if(0)
  ) %>%
  group_by(condition, participant) %>% 
  summarise(
    prop_safe_explore = sum(safe_explore) / 100,
    prop_safe_claim = sum(safe_claim) / 100,
    prop_risky_explore = sum(risky_explore) / 100,
    prop_risky_claim = sum(risky_claim) / 100,
    mean_risky_feedback = mean(risky_feedback, na.rm = TRUE),
    sd_risky_feedback = sd(risky_feedback, na.rm = TRUE)
  ) %>%
  gather(
    key = "response", 
    value = "proportion", 
    prop_safe_explore, 
    prop_safe_claim, 
    prop_risky_explore, 
    prop_risky_claim
  ) %>%
  mutate(
    risky = ifelse(response %>% str_detect("risky"), "Risky", "Safe"),
    claim = ifelse(response %>% str_detect("claim"), "Claim", "Observe")
  ) %>% 
  ungroup() %>% 
  arrange(participant)

### Wrangle sampling ----

# Does whether participants observe or claim depend on the condition?
exp_information_or_reward$observe_or_claim_a <- exp_information_or_reward$sampling_summary %>% 
  filter(claim == "Observe") %>% 
  group_by(condition, participant, risky, mean_risky_feedback, sd_risky_feedback) %>% 
  summarise(
    prop_observe = sum(proportion),
    observe_count = as.integer(prop_observe * 100)
  ) %>% 
  ungroup()

exp_information_or_reward$observe_or_claim_b <- exp_information_or_reward$sampling_summary %>% 
  group_by(condition, participant, risky, mean_risky_feedback, sd_risky_feedback) %>% 
  summarise(
    proportion = sum(proportion),
    n_choices = as.integer(proportion * 100)
  ) %>% 
  ungroup()

exp_information_or_reward$observe_or_claim <- left_join(exp_information_or_reward$observe_or_claim_a, exp_information_or_reward$observe_or_claim_b) %>% 
  group_by(condition, participant, risky, mean_risky_feedback, sd_risky_feedback) %>% 
  mutate(
    prop_observe = sum(prop_observe / proportion),
    risky = risky %>% tolower()
  ) %>% 
  ungroup() %>% 
  select(-proportion)

### Wrangle choices ----

# When participants select observe / claim, what is the proportion of risky choices?
exp_information_or_reward$safe_or_risky_a <- exp_information_or_reward$sampling_summary %>% 
  filter(risky == "Risky") %>% 
  group_by(condition, participant, claim, mean_risky_feedback, sd_risky_feedback) %>% 
  summarise(
    prop_risky = sum(proportion),
    risky_count = as.integer(prop_risky * 100)
  ) %>% 
  ungroup()

exp_information_or_reward$safe_or_risky_b <- exp_information_or_reward$sampling_summary %>% 
  group_by(condition, participant, claim, mean_risky_feedback, sd_risky_feedback) %>% 
  summarise(
    proportion = sum(proportion),
    n_choices = as.integer(proportion * 100)
  ) %>% 
  ungroup()

exp_information_or_reward$safe_or_risky <- left_join(exp_information_or_reward$safe_or_risky_a, exp_information_or_reward$safe_or_risky_b) %>% 
  group_by(condition, participant, claim, mean_risky_feedback, sd_risky_feedback) %>% 
  mutate(prop_risky = sum(prop_risky / proportion)) %>% 
  ungroup() %>% 
  filter(n_choices > 1) %>% 
  select(-proportion)

### Combine sampling with EARS responses (risky) ----

# Combine sampling data with pivoted EARS responses - the EARS responses correspond to the option in the column "risky"
exp_information_or_reward$observe_or_claim_ears <- left_join( 
  exp_information_or_reward$ears_wide, 
  exp_information_or_reward$observe_or_claim
)

### Combine choices with EARS responses ----

# Combine choices data with pivoted EARS responses - the EARS responses correspond to the option in the column "risky"
exp_information_or_reward$safe_or_risky_ears <- left_join( 
  exp_information_or_reward$ears_wide, 
  exp_information_or_reward$safe_or_risky
)

### Wrangle Ellsberg ----

# Combine Ellsberg responses with observe_or_claim data
exp_information_or_reward$ellsberg_safe_or_risky <- exp_information_or_reward$safe_or_risky_ears %>% 
  left_join(exp_information_or_reward$ellsberg)

### Format categorical variables ----

# Column names of categorical variables
temp$exp_information_or_reward_factors <- c("condition", "risky", "claim", "first_choice", "uncertainty_type")

# Format and append contrasts for each categorical variable in each tibble
exp_information_or_reward <- exp_information_or_reward %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(temp$exp_information_or_reward_factors)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

### Save tidy data ----

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_information_or_reward)

## 9. Free sampling ------------------------------------------------------------

### Import data ----

# Import data and do initial wrangling with generic wrangling functions
exp_free_sampling <- import_data(exp_free_sampling_info) %>% 
  wrangle_all(exp_free_sampling_info)

### Wrangle demographics ----

exp_free_sampling$demographics_summary <- summarise_demographics(exp_free_sampling$demographics)

### Wrangle EARS ----

# Rename questions for compatibility for their number in the 10-item EARS
exp_free_sampling$ears <- exp_free_sampling$ears %>% 
  rename(question_3 = question_2, question_7 = question_3, question_8 = question_4)

# Summary used for graphing
exp_free_sampling$ears_summary <- exp_free_sampling$ears %>%
  pivot_longer(
    cols = c("epistemic", "aleatory"),
    names_to = "uncertainty_type",
    values_to = "response"
  ) %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_title) %>% 
  group_by(
    participant,
    randomID,
    condition,
    uncertainty_type
  ) %>% 
  summarise(mean_response = mean(response)) %>% 
  ungroup()

# Summary used for modelling by block
exp_free_sampling$ears_summary_block <- exp_free_sampling$ears %>%
  pivot_longer(
    cols = c("epistemic", "aleatory"),
    names_to = "uncertainty_type",
    values_to = "response"
  ) %>% 
  group_by(participant) %>% 
  mutate(
    uncertainty_type = uncertainty_type %>% str_to_title,
    block = rep(1:6, each = 2)
  ) %>% 
  group_by(
    participant,
    randomID,
    condition,
    uncertainty_type,
    block
  ) %>% 
  summarise(mean_response = mean(response)) %>% 
  ungroup()

# Long data used for modelling
exp_free_sampling$ears_long <- exp_free_sampling$ears %>% 
  select(
    participant,
    randomID,
    condition,
    sampling_type,
    starts_with("question")
  ) %>% 
  pivot_longer(
    cols = starts_with("question"),
    names_to = "question",
    values_to = "response"
  ) %>% 
  mutate_at(vars(question, participant), format_conditions) %>% 
  group_by(randomID) %>% 
  mutate(
    block = rep(1:6, each = 4) %>% factor(levels = 1:6, ordered = TRUE),
    response = (response + 1) %>% factor(levels = 1:7, ordered = TRUE),
    uncertainty_type = case_when(
      question %in% paste("Question", 1:4) ~ "Aleatory",
      question %in% paste("Question", 5:10) ~ "Epistemic"
    )
  ) %>%
  ungroup()

### Wrangle sampling ----

exp_free_sampling$sampling_summary <- exp_free_sampling$sampling %>%
  mutate(block = block %>% factor(levels = 1:6, ordered = TRUE)) %>% 
  group_by(participant, randomID, condition, block, sampling_type) %>% 
  summarise(
    n_samples = n(),
    mean_risky_distribution = first(mean),
    mean_risky_feedback = mean(feedback),
    sd_risky_feedback = sd(feedback),
  ) %>% 
  ungroup()

### Wrangle choices ----

exp_free_sampling$choices <- exp_free_sampling$choices %>% 
  mutate(
    block = block %>% factor(levels = 1:6, ordered = TRUE), 
    comparison = case_when(
      mean > safe_value ~ "Risky better",
      mean < safe_value ~ "Safe better",
      TRUE ~ "Equal"
    ) %>% 
      factor(levels = c("Risky better", "Equal", "Safe better"), ordered = TRUE)
  )

### Combine sampling with EARS responses ----

exp_free_sampling$sampling_ears <- exp_free_sampling$ears_long %>% 
  group_by(participant, randomID, condition, sampling_type, block, uncertainty_type) %>%
  summarise(mean_response = mean(as.numeric(response))) %>% 
  ungroup() %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_lower()) %>%
  pivot_wider(names_from = uncertainty_type, values_from = mean_response) %>%   
  left_join(exp_free_sampling$sampling_summary)

### Combine choices with EARS responses ----

exp_free_sampling$choices_ears <- exp_free_sampling$ears_long %>% 
  group_by(participant, randomID, condition, sampling_type, block, uncertainty_type) %>%
  summarise(mean_response = mean(as.numeric(response))) %>% 
  ungroup() %>% 
  mutate(uncertainty_type = uncertainty_type %>% str_to_lower()) %>%
  pivot_wider(names_from = uncertainty_type, values_from = mean_response) %>%   
  left_join(exp_free_sampling$choices)

### Format categorical variables ----

# Column names of categorical variables
temp$exp_free_sampling_factors <- c("condition", "uncertainty_type", "sampling_type", "choice")

# Format and append contrasts for each categorical variable in each tibble
exp_free_sampling <- exp_free_sampling %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(temp$exp_free_sampling_factors)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

### Save tidy data ----

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_free_sampling)

## Combine experiments ----

temp <- list()

# Split partial feedback experiments into part a and b
temp$partial_feedback_past_ears <- exp_partial_feedback$ears_long %>% filter(experiment == "Experiment a") %>% select(-experiment)

temp$partial_feedback_future_ears <- exp_partial_feedback$ears_long %>% filter(experiment == "Experiment a") %>% select(-experiment)

# Recode participants for consistency with other experiments 
temp$information_or_reward_ears <- exp_information_or_reward$ears_long %>% arrange(participant) %>% mutate(participant = rep(1:137, each = 20) %>% as.character() %>% str_c("c"))

# Add column to indicate that EARS responses correspond to the risky option and recode participants
temp$free_sampling_ears <- exp_free_sampling$ears_long %>% mutate(risky = "Risky" %>% factor(levels = c("Safe", "Risky"))) %>% mutate(participant = participant %>% as.character() %>% str_c("d")) %>% select(-randomID, -sampling_type)

exp_combined <- list()

exp_combined$ears_long <- bind_rows(
  temp$partial_feedback_past_ears,
  temp$partial_feedback_future_ears,
  temp$information_or_reward_ears,
  temp$free_sampling_ears,
  .id = "experiment"
)

### Format categorical variables ----

# Column names of categorical variables
temp$exp_combined_factors <- c("experiment", "condition", "risky", "uncertainty_type", "question")

# Format and append contrasts for each categorical variable in each tibble
exp_combined <- exp_combined %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(temp$exp_combined_factors)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

### Save tidy data ----

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_combined)

## Summarise demographics ----

experiment_names <-  c(
  "exp_partial_feedback", 
  "exp_partial_feedback_past",
  "exp_partial_feedback_future",
  "exp_information_or_reward",
  "exp_free_sampling"
)

demographic_summaries <- NULL

# Get the demographics summaries for each experiment (in a list)
demographic_summaries$experiments <- mget(experiment_names) %>% 
  map(~ .$demographics_summary) %>% 
  # Bind together the demographics summaries for each experiment (in a tibble)
  bind_rows(.id = "experiment")

# Get demographics data for each experiment (in a list)
combined_demographics_data <- mget(experiment_names) %>% 
  map(~ .$demographics) %>% 
  # Bind together the demographics data (in a tibble)
  bind_rows(.id = "experiment")

# Create overall summary for all experiments combined
demographic_summaries$overall <-  combined_demographics_data %>% 
  summarise_demographics() %>% 
  select(-n_per_condition)

save_tidy_data(demographic_summaries)
