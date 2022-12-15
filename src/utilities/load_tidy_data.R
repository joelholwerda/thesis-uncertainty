load_tidy_data <- function(data_names, data_type = NA, path = NA) {
    
    # Set default location to load data
    if (is_na(path)) {path <- here("data", "tidy_data")}
    # List files based on data_names and check whether they exist
    files <- file.path(path, data_names, paste0(data_names, ".rds"))
    file_exists <- file.exists(files)
    
    walk2(
        data_names[file_exists], 
        files[file_exists], 
        ~ {
            if(!exists(.x)){assign(.x, list(), envir = .GlobalEnv)}
            parse(
            text = paste0(
                .x, 
                ifelse(is.na(data_type), "", paste0("$", data_type)),
                " <- ", "read_rds(\"", .y, "\")")
            ) %>% eval(envir = .GlobalEnv)
        }
    )
    
    # Warn the user if some data_names do not correspond to existing files
    if (!all(file_exists)) {
        warning(
            paste0(
                "An .rds file was not found for the following variables: ",
                knitr::combine_words(data_names[!file_exists]), 
                ". Have you run 01_wrangle_data.Rmd to generate demographic summaries, 
                02_model_data.Rmd to generate model summaries, or 03_create_figures.Rmd
                to generate figures."
            )
        )
    }
} 
