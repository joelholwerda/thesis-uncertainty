save_tidy_data <- function(data, parent_folder = NA, data_name = NA, save_csv = TRUE) {

  # Extract name of variable passed in as the data argument for naming folders/files
  if (is_na(data_name)) {data_name <- rlang::enexpr(data) %>% rlang::expr_text()}
  
  # Use the tidy_data folder as a default save location
  if (is_na(parent_folder)) {parent_folder <- here::here("data", "tidy_data")}
  # If folder name argument is supplied, use it to name the folder that will contain
  # the .csv files, otherwise use the name of the variable used as the data argument
  save_location <- file.path(parent_folder, data_name)
  
  # Create the tidy data folders if they do not exist
  if (!dir.exists(parent_folder)) {dir.create(parent_folder)}
  if (!dir.exists(save_location)) {dir.create(save_location)}

  # Save data as an .rds file in the save_location folder named based on the data name
  write_rds(data, file = file.path(save_location, paste0(data_name, ".rds")))
  
  if (save_csv) {
    # csv files are saved in a separate folder
    save_location_csv <- file.path(save_location, "csv_files")
    if (!dir.exists(save_location_csv)) {dir.create(save_location_csv)}
    
    elements_to_write_csv <- !names(data) %in% c("exclusions", "graphs", "models")
    
    # If data is a list of data elements to be saved in separate .csv files, save
    # each element in the save_location folder named based on their element names
    if (is.list(data) & !is.data.frame(data)) {
      file_names <- file.path(save_location_csv, paste0(names(data), ".csv"))
      walk2(data[elements_to_write_csv], file_names[elements_to_write_csv], ~ write_csv(.x, file = .y))
    } else {
      write_csv(
        data[elements_to_write_csv], file = file.path(save_location_csv, paste0(data_name, ".csv")))
    }
  }
}

