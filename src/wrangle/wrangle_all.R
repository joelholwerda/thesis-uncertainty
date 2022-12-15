
# Load custom helper functions
here::here("src", "wrangle", "wrangle_demographics.R") %>% source()
here::here("src", "wrangle", "wrangle_choices.R") %>% source()
here::here("src", "wrangle", "wrangle_sampling.R") %>% source()
here::here("src", "wrangle", "wrangle_ears.R") %>% source()
here::here("src", "wrangle", "wrangle_ellsberg.R") %>% source()
here::here("src", "wrangle", "test_correct_dimensions.R") %>% source()
here::here("src", "wrangle", "format_conditions.R") %>% source()

wrangle_all <- function(raw_data, exp_info){
  
  if (length(raw_data) == 0) stop(paste("raw_data for ", exp_info$experiment_name, " has length 0. Check that the data imported properly."))
  
  # Run wrangling functions and add to data list
  data <- list()
  
  if ("demographics" %in% exp_info$data_types) {data[["demographics"]] <- wrangle_demographics(raw_data$demographics, exp_info)}
  if ("choices" %in% exp_info$data_types) {data[["choices"]] <- wrangle_choices(raw_data$choices, exp_info)}
  if ("sampling" %in% exp_info$data_types) {data[["sampling"]] <- wrangle_sampling(raw_data$sampling, exp_info)}
  if ("ears" %in% exp_info$data_types) {data[["ears"]] <- wrangle_ears(raw_data$ears, exp_info)}
  if ("ellsberg" %in% exp_info$data_types) {data[["ellsberg"]] <- wrangle_ellsberg(raw_data$ellsberg, exp_info)}
  
  return(data)
}