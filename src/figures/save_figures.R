save_figures <- function(data, width = 80, height = 80, image_format = "pdf", data_name = NA, parent_folder = NA) {
  
  # Extract name of variable passed in as the data argument for naming folders/files
  if (is_na(data_name)) {data_name <- rlang::ensym(data) %>% rlang::expr_text(.)}
  
  # Set default save location
  if (is_na(parent_folder)) {parent_folder <- here::here("output", "figures")}
  # If folder name argument is supplied, use it to name the folder that will contain
  # the figures, otherwise use the name of the variable used as the data argument
  save_location <- file.path(parent_folder, data_name)
  
  # Images are saved in a separate folder
  save_location_images <- file.path(save_location, paste0(image_format, "_files"))
  
  # Create the tidy data folders if they do not exist
  if (!dir.exists(parent_folder)) {dir.create(parent_folder)}
  if (!dir.exists(save_location)) {dir.create(save_location)}
  if (!dir.exists(save_location_images)) {dir.create(save_location_images)}
  
  # Save data as an .rds file in the save_location folder named based on the data name
  write_rds(data$figure, file = file.path(save_location, paste0(data_name, ".rds")))

  # If data is a list of data elements to be saved in separate image files, save
  # each element in the save_location folder named based on their element names
  if (is.list(data) & !is.ggplot(data)) {
    file_names <- file.path(save_location_images, paste0(names(data$figures), ".", image_format))
    # If a single width or height is given, repeat it for compatibility with inputting arrays
    if (length(width) == 1) {width <- rep(width, length(data$figures))}
    if (length(height) == 1) {height <- rep(height, length(data$figures))}
    # Save each figure based on the file name and dimensions
    pwalk(.l = list(..1 = data$figures, ..2 = file_names, ..3 = width, ..4 = height), .f = ~ ggsave(filename = ..2, plot = ..1, width = ..3, height = ..4, units = "mm"))
  } else {
    file_name <- file.path(save_location_images, paste0(data_name, ".", image_format))
    ggsave(data, file = file_name, width = width, height = height, units = "mm")
  }
}

