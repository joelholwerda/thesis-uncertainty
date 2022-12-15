library(tidyverse)

# Filter participants that completed the experiment (at least once)
completed <- exp_sampling$demographics %>% filter(!is.na(prize))

# Filter participants that have the wrong number of rows for demographics
wrong_demographics_length <- exp_sampling$demographics %>% 
  count(participant) %>% 
  filter(n != 3)

# Filter participants that have the wrong number of rows for sampling
wrong_sampling_length <- exp_sampling$sampling %>% 
  count(participant) %>% 
  filter(n != 100)

# Participant that have too many demographics rows but completed and only 
# attempted sampling once
valid_data <- wrong_demographics_length %>% 
  filter(
    participant %in% completed$participant,
    !participant %in% wrong_sampling_length$participant
  )

# Participants that had the wrong number of rows of demographics data and that
# either did not complete the experiment or attempted sampling more than once
invalid_data <- wrong_demographics_length %>% 
  filter(!participant %in% valid_data$participant)

also_invalid_data <- exp_sampling$demographics %>% 
  filter(
    !participant %in% completed$participant,
    !participant %in% invalid_data$participant
  )
