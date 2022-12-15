# Functions to standardize and unstandardise variables -- borrowed from McElreath's rethinking package (not on CRAN)
standardise <- function(x) {
    x <- scale(x)
    z <- as.numeric(x)
    attr(z,"scaled:center") <- attr(x,"scaled:center")
    attr(z,"scaled:scale") <- attr(x,"scaled:scale")
    return(z)
}

unstandardise <- function(x) {
    scale <- attr(x,"scaled:scale")
    center <- attr(x,"scaled:center")
    z <- x*scale + center
    return( as.numeric(z) )
}