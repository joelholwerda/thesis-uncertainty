pp_check_percent <- function(brms_object, responses, between_condition, within_condition = "none", n_samples = 50) {
  
  # Quote the arguments
  responses_quoted <- ensym(responses)
  between_quoted <- ensym(between_condition)
  within_quoted <- ensym(within_condition)
  
  # Facet the graph depending whether there is a within subjects condition
  if (expr_text(within_quoted) == "none") {
    facet <- facet_wrap(vars(!!between_quoted))
  } else {
    facet <- facet_grid(rows = vars(!!within_quoted), cols = vars(!!between_quoted))
  }
  
  plot <- fitted_draws(
    newdata = brms_object$data, 
    model = brms_object,
    n = ifelse(expr_text(within_quoted) == "none", n_samples, n_samples * 2), 
    dpar = TRUE
  ) %>% 
    mutate(
      # Sample form normal distribution based on fitted draws
      .prediction = map2_dbl(mu, sigma, ~ rnorm(1, .x, .y)),
      simulation = rep(1:n_samples, each = n() / n_samples)
    ) %>%
    # When sampling the prior there were a small number of large values that created issues with graphing densities
    filter(.prediction > -1000, .prediction < 1000) %>% 
    # Create density plots
    ggplot(aes(x = .prediction, group = simulation)) + 
    geom_density(colour = "gray70") + 
    # Reference value drawn from a normal distribution with SD = 25
    geom_density(aes(!!responses_quoted), inherit.aes = FALSE, size = 1, colour = "navy") +
    # Split by condition
    facet +
    # Restrict plotting range
    coord_cartesian(xlim = c(-100, 100)) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(plot)
  
}
