generate_simo_priors <- function(prior, model) {
  
  prior_string <- prior$prior
  
  # Check prior argument
  if (!str_detect(prior_string, "dirichlet\\([:digit:]\\)")) {
    stop("prior must be a brmsprior object that specifies a dirichlet prior. For example, prior(dirichlet(1), class = simo) would produce a uniform prior over all simplexes.")
  } 
  
  # Check model argument
  if (!(is.list(model) & "formula" %in% names(model) & "data" %in% names(model))) {
    stop("model must be a list containing two elements: formula and data.")
  }
  
  # Retrieve simo parameters using the formula and data specified in the model argument (list)
  simo_priors <- get_prior(
    model$formula,
    model$data
  ) %>% 
    filter(class == "simo") %>% 
    mutate(prior = prior_string)
  
  return(simo_priors)
  
}
