save_model <- function(data_type, experiment, quick_version = getOption("quick_version"), overwrite_saved_models = getOption("overwrite_saved_models")) {
  
  # Location to save fitted models
  parent_folder <- here::here("output", "fitted_models")
  save_location <- file.path(parent_folder, experiment)
  
  # Create the tidy data folders if they do not exist
  if (!dir.exists(parent_folder)) {dir.create(parent_folder)}
  if (!dir.exists(save_location)) {dir.create(save_location)}
  
  # Location of the files created when quick_models = FALSE
  slow_model_path <- file.path(save_location, paste0(experiment, "_", data_type))
  slow_model_path_rds <- paste0(slow_model_path, ".rds") # brms automatically adds ".rds"
  # Check whether a saved model exists
  slow_model_exists <- slow_model_path_rds %>% file.exists()
  
  # Location of the files created when quick_models = FALSE
  quick_model_path <- file.path(save_location, paste0(experiment, "_", data_type, "_quick")) 
  quick_model_path_rds <- paste0(quick_model_path, ".rds")
  # Check whether a saved model exists
  quick_model_exists <- quick_model_path_rds %>% file.exists()
  
  # Warn the user if a saved model is used
  throw_warning <- function(file_location, data_type, experiment) {
    file_info <- file.info(file_location)
    modified <- file_info$mtime
    
    warning_string <- paste0(
      "An existing fitted model from ", file_location, 
      " was loaded instead of running the Stan model again. If you have made any changes to the ", 
      data_type, " model for ", experiment, " since ", modified, 
      ", you will need to delete the existing .rds file or set the overwrite_saved_models parameter of save_model() to TRUE."
    )
    
    warning(warning_string, call. = FALSE)
  }
  
  # If overwrite_saved_models is TRUE, remove the quick/slow file if it exists and return the quick/slow location
  if (quick_version & overwrite_saved_models) {
    if (quick_model_exists) { file.remove(quick_model_path_rds) }
    return(quick_model_path)
  } else if (!quick_version & overwrite_saved_models) {
    if (slow_model_exists) { file.remove(slow_model_path_rds) }
    return(slow_model_path)
  # If the slow (then quick) model exists, return its location with a warning that an existing fit was used
  } else if (slow_model_exists) {
    throw_warning(slow_model_path_rds, data_type, experiment)
    return(slow_model_path)
  } else if (!quick_version) {
    # Replace quick model with slow model
    if (quick_model_exists) { file.remove(quick_model_path_rds) }
    return(slow_model_path)
  } else if (quick_model_exists) {
    throw_warning(quick_model_path_rds, data_type, experiment)
    return(quick_model_path)
  } else {
    return(quick_model_path)
  }
}
