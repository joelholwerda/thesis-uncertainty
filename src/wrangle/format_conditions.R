# Function that converts the conditions to an ordered factor (low, high) in title case
format_conditions <- function(string) {
  
  # Change from camel or snake case to title case
  title_string <- string %>% 
    # Matches upper case letters not preceded by a space, an upper case letter, or the start of the string and adds a space
    str_replace_all("(?<!([:space:]|[:upper:]|^))[:upper:]", function(match){return(paste("", match))}) %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence()
  
  # If Low or High appears in title_string convert to factor with Low and High as first levels
  low_or_high <- title_string %>% str_count("Low|High") %>% sum()
  
  if (!is.na(low_or_high) & low_or_high > 0) {
    levels <- unique(title_string)
    low_index <- str_which(levels, "Low")
    high_index <- str_which(levels, "High")
    all_indexes <- 1:length(title_string)
    other_indexes <- all_indexes[c(-low_index, -high_index)]
    levels_ordered = c(levels[low_index], levels[high_index], levels[other_indexes])
    factor = factor(title_string, levels_ordered)
  
  # Otherwise convert to factor with default levels
  } else {
    
    factor = factor(title_string)
    
  }
  
  return(factor)
}

