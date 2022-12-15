
library(tidyverse)
library(here)

# List csv files in paypal folder
path <- file.path("data", "exp_sampling", "data_raw", "paypal")
files <- here(path, list.files(path = path, pattern = "*.csv"))

# Read and combine .csv files
paypal_data <- files %>%
  map(read_csv, col_types = cols()) %>% 
  bind_rows()

# Check for duplicates
paypal_data %>% count(paypal) %>% arrange(desc(n))

# Filter for valid email addresses
email_is_valid <- grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", paypal_data$paypal, ignore.case=TRUE)
valid_emails <- paypal_data %>% filter(email_is_valid)

# Check prize amounts
valid_emails %>% count(prize)

# Check total payment amount
valid_emails$prize %>% sum()
