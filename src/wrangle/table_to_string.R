# Function that takes the output of table() and results in a string with each 
# name followed by its value
table_to_string <- function(table) {
  
  if (length(table) == 0) {
    return(NA)
  } else {
  
  string <-  ""
  
  for (i in 1:length(table)) {
    # For each element, add the name and value
    string <- paste0(string, table %>% names %>% .[i], ": ", table %>% .[i])
    # Add a comma and space unless the final element
    if (i < length(table)) {
      string <- paste0(string, ", ")
    }
  }
  
  return(string)
  
  }
}
