# Function that makes a tibble with the contrasts associated with each level
get_contrasts <- function(x) {
    contrasts <- x %>% 
        contrasts() %>% 
        as_tibble(rownames = NA) %>% 
        rownames_to_column()
    
    names(contrasts)[1] <- "level"
    
    if (dim(contrasts) %>% .[2] == 2) {
        names(contrasts)[2] <- "contrasts"
    }
    
    return(contrasts)
}
