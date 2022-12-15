run_mcmc_diagnostics <- function(brms_object, type, categorical = "none", continuous = "none", n_samples = 50) {
  
  # Check type argument
  if (!type %in% c("density", "bars")) {stop("type must be either 'density' or 'bars'.")}
  
  diagnostics <- list()

  # Rank histograms
  diagnostics$rank_histogram <- brms_object %>% mcmc_rank_hist(regex_pars = "b_|sd_|cor_") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0)) +
    labs(title = "Rank histograms (chain convergence)")

  # Posterior density plots
  diagnostics$density <- brms_object %>% mcmc_dens_overlay(regex_pars = "b_|sd_|cor_") +
    labs(title = "Posterior density plots")

  # Pairs plots
  diagnostics$pairs <- brms_object %>% mcmc_pairs(regex_pars = "b_")
  
  # Posterior predictive checks
  if (type == "density") {
    diagnostics$posterior_predictive <- brms_object %>%
      brms::pp_check(type = "dens_overlay", ndraws = 50)
  } else if (type == "bars") {
    diagnostics$posterior_predictive <- brms_object %>%
      brms::pp_check(type = "bars", ndraws = 50)
  }

  diagnostics$posterior_predictive <- diagnostics$posterior_predictive +
    labs(title = "Posterior predictive distribution")

  # Posterior predictive checks by group (if categorical variables provided)
  if (!identical(categorical, "none")) {
    # pp_check() only allows one grouping variable. Combine categorical variables if applicable
    if (length(categorical) > 1) {
      brms_object$data <- brms_object$data %>% unite("combined_condition", all_of(categorical), remove = FALSE)
      grouping_variables <- expr(combined_condition)
    } else{
      grouping_variables <- categorical
    }

    # run pp_check() using grouping_variables
    if (type == "density") {
      diagnostics$posterior_predictive_grouped <- brms_object %>%
        brms::pp_check(type = "dens_overlay_grouped", group = grouping_variables, ndraws = 50)
    } else if (type == "bars") {
      diagnostics$posterior_predictive_grouped <- brms_object %>%
        brms::pp_check(type = "bars_grouped", group = grouping_variables, ndraws = 50)
    }
    
    diagnostics$posterior_predictive_grouped <- diagnostics$posterior_predictive_grouped +
      labs(title = "Posterior predictive distribution (by condition)")
  }

  # Error scatterplot
  diagnostics$error <- brms_object %>%
    pp_check(type = "error_scatter", ndraws = 12, alpha = ifelse(type == "density", 0.2, 0.01)) +
    geom_vline(xintercept = 0) +
    coord_flip() +
    labs(title = "Error scatterplot")

  # Error scatterplots by continuous variables (if applicable)
  if (!identical(continuous, "none")) {
    # Create graph for each continuous variable if more than one provided
    if (length(continuous) > 1) {
      diagnostics$error_continuous <- map(
        continuous, 
        ~ pp_check(brms_object, type = "error_scatter_avg_vs_x", x = ., alpha = 0.4) + 
          labs(title = paste0("Average error by predictor variable (", ., ")"), x = str_to_title(.))+
          geom_hline(yintercept = 0)
      )
      
      # Name list elements by the continuous variable
      names(diagnostics$error_continuous) <- continuous
    } else {
      diagnostics$error_continuous <- pp_check(brms_object, type = "error_scatter_avg_vs_x", x = continuous, alpha = 0.4) +
        labs(title = paste0("Average error by predictor variable (", continuous, ")"), x = str_to_title(continuous)) +
        geom_hline(yintercept = 0)
    }
  }
  
  return(diagnostics)
  
}
