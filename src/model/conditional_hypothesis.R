# `conditional_hypothesis()` is a helper function that uses the `hypothesis()` function from the brms package to test hypotheses regarding one or more (effect) variables whilst specifying the values of other (conditional) variables

# `brms_object` needs to be a model fitted using brms (brmsfit object)
# `hypothesis` must be a character string that includes levels of factor effect_variables or names of numeric effect_variables. The two sides of the hypothesis must be separated by > or <. For example, 'x1 > x2'. Interactions can be specified by placing : between valid factor levels or numeric variables. For example, 'x1:y1 > x1:y2'
# `effect_variables` must be a character string (or vector) that corresponds to population level variables in brms_object.
# `conditional_variables` must be a character string (or vector) that corresponds to a population level variable in brms_object.
# `conditional_levels` must be a character string (or vector) that corresponds to one level from (each of) conditional_variables.

conditional_hypothesis <- function(brms_object = NULL, hypothesis = NULL, effect_variables = NULL, conditional_variables = NULL, conditional_levels = NULL, outcome_level = NULL, estimate_type = "median", ci_width = 0.95, ci_type = "hdi") {
  
  # Check that brms_object is a brmsfit object
  if (!is.brmsfit(brms_object)) {
    stop("brms_object needs to be a model fitted using brms (brmsfit object).")
  }
  
  # Parse hypothesis direction (< or >)
  hypothesis_direction <- parse_hypothesis_direction(hypothesis)
  
  # Get population level parameters from brms_object$data
  parameters <- get_parameters(brms_object, outcome_level)
  
  # Get names of valid effect and conditional variables
  valid_variables <- get_valid_variables(brms_object, parameters)
  
  # Check that effect_variable is valid (provide assistance if false)
  check_effect_variables(effect_variables, valid_variables)
  
  # Check that conditional_variable is valid (provide assistance if false)
  check_conditional_variables(effect_variables, conditional_variables, valid_variables)
  
  # Check that conditional_levels is valid (provide assistance if false)
  check_conditional_levels(conditional_variables, conditional_levels, brms_object)
  
  # Check that variables are centred
  check_centred_variables(valid_variables, brms_object)
  
  # Get the levels of effect_variables
  effect <- parse_effect(effect_variables, valid_variables, brms_object)
  
  # Parse hypothesis sides
  hypothesis_sides <- parse_hypothesis_sides(hypothesis)
  
  # Check the hypothesis sides are valid (provide assistance if false)
  check_hypothesis_sides(hypothesis_sides, valid_variables, effect_variables, effect)
  
  # Select model parameters that correspond to the effect and conditional variables
  relevant_parameters <- select_relevant_parameters(parameters, effect_variables, conditional_variables)
  
  # Calculate contrasts for each effect
  effect_contrasts <- calculate_effect_contrasts(hypothesis_sides, effect, effect_variables, brms_object)
  
  # Skip calculating contrasts for the conditional variables if none provided
  if (!(is.null(conditional_variables) || any(conditional_variables == "none"))) {
    
    # Calculate contrasts for each effect
    conditional_contrasts <- calculate_conditional_contrasts(conditional_variables, conditional_levels, relevant_parameters, brms_object)
    
  } else {conditional_contrasts <- NULL}
  
  # Combine effect and conditional contrasts for each parameter
  contrasts <- combine_contrasts(hypothesis_sides, relevant_parameters, conditional_contrasts, effect_contrasts)
  
  # Compose hypothesis phrase from the hypothesis sides and direction 
  hypothesis_phrase <- compose_hypothesis_phrase(hypothesis_sides, hypothesis_direction, relevant_parameters, contrasts)
  
  # Evaluate hypothesis phrase using brms::hypothesis
  hypothesis_evaluation <- evaluate_hypothesis(hypothesis_phrase, brms_object)
  
  # Summarise brms::hypothesis output using ggdist::point_interval 
  hypothesis_summary <- summarise_hypothesis_output(hypothesis_evaluation, estimate_type, ci_width, ci_type)
  
  # Return summary, hypothesis, and samples
  output_list <- list(
    summary = hypothesis_summary, 
    hypothesis = hypothesis, 
    conditional_variables = conditional_variables,
    conditional_levels = conditional_levels,
    hypothesis_phrase = hypothesis_phrase, 
    model_formula = brms_object$formula,
    samples = hypothesis_evaluation$samples
  )
  
  # Change class of output to control print() behaviour
  output <- structure(output_list, class = "conditionalhypothesis")
  
  return(output)
  
}

# Parse hypothesis direction ---------------------------------------------------------------------------------------

parse_hypothesis_direction <- function(hypothesis) {
  
  # Check that hypothesis is provided
  if (!is.character(hypothesis)) {
    stop(
      paste(
        "hypothesis must be a character string that includes levels of factor effect_variables or names of numeric or ordinal effect_variables.",
        "The two sides of the hypothesis must be separated by > or <. For example, 'x1 > x2'.",
        "Interactions can be specified by placing : between valid factor levels or numeric variables. For example, 'x1:y1 > x1:y2'"
      )
    )
  }
  
  hypothesis_direction <- hypothesis %>% str_extract("[<>]")
  
  # Check that hypothesis included a > or <
  if (!hypothesis_direction %in% c(">", "<")) {
    stop("Levels in hypothesis need to be separated by > or <. For example, 'x1 > x2'.")
  }
  
  return(hypothesis_direction)
  
}

# Get population level parameters from brms_object$data -------------------------------------------------------

get_parameters <- function(brms_object, outcome_level) {
  
  parameters <- list()
  
  # Get the name of each population-level effect parameter
  parameters$names_long <- names(brms_object$fit) %>% 
    str_subset("^b_|^bsp_") %>% 
    .[!str_detect(., "sigma")]
  
  # If outcome_level is specified, filter parameters for that level (for categorical models) 
  if (!is.null(outcome_level)) {
    
    outcome_level_parameters <- parameters$names_long %>% 
      str_subset(outcome_level)
    
    # Check that outcome_levels is valid
    if (length(outcome_level_parameters) == 0) {
      
      valid_outcome_levels <- parameters$names_long %>% 
        str_extract("mu[:alpha:]+") %>% 
        str_remove("mu") %>% 
        unique()
      
      stop(paste0("outcome_levels must be a charecter string that corresponds to a level of the outcome variable in a categorical or multinomial family model. The valid outcome_levels are: ", paste0(valid_outcome_levels, collapse = ", ")))
      
    } else {
      parameters$names_long <- outcome_level_parameters
    }
  }
  
  # Remove class prefixes
  parameters$names_short <- parameters$names_long %>%
    str_remove("^b_mu[:alpha:]*_|^bsp_mu[:alpha:]*_mo|^b_|^bsp_mo")
  
  return(parameters)
  
}

# Get names of valid effect and conditional variables ---------------------------------------------------------

get_valid_variables <- function(brms_object, parameters) {
  
  valid_variables <- list()
  
  # Retrieve valid variable names
  variable_in_parameters <- names(brms_object$data) %>% 
    map_lgl(~ str_detect(parameters$names_short, .) %>% any())
  
  valid_variables$all <- names(brms_object$data) %>% .[variable_in_parameters] 
  
  valid_variables$effects_phrase <- valid_variables$all %>% paste0(collapse = ", ")
  
  valid_variables$is_factor <- map_lgl(brms_object$data[valid_variables$all], ~ is.factor(.))
  
  valid_variables$is_numeric <- map_lgl(brms_object$data[valid_variables$all], ~ is.numeric(.))
  
  valid_variables$is_ordered <- map_lgl(brms_object$data[valid_variables$all], ~ is.ordered(.))
  
  valid_variables$conditional_phrase <- valid_variables$all[valid_variables$is_factor] %>% paste0(collapse = ", ")
  
  return(valid_variables)
  
}

# Check that effect_variable is valid (provide assistance if false) ----------------------------------------------------

check_effect_variables <- function(effect_variables, valid_variables) {
  
  # Check that effect_variables is provided
  if (!is.character(effect_variables)) {
    stop(
      paste0(
        "effect_variables must be a character string (or vector) that corresponds to population level variables in brms_object. ",
        "The valid effect_variables are: ", valid_variables$effects_phrase, "."
      )
    )
  }
  
  # Check that effect_variables correspond to at least one parameter
  effect_is_valid <- map_lgl(effect_variables, ~ . %in% valid_variables$all)
  
  invalid_effects <- effect_variables[!effect_is_valid] %>% 
    paste0(collapse = ", ")
  
  if (!all(effect_is_valid)) {
    stop(
      paste0(
        "At least one of effect_variables is invalid: ", invalid_effects,
        ". effect_variables must be a character string (or vector) that corresponds to population level variables in brms_object. ",
        "The valid effect_variables are: ", valid_variables$effects_phrase, "."
      )
    )
  }
}

# Check that conditional_variable is valid (provide assistance if false) ---------------------------------------------

check_conditional_variables <- function(effect_variables, conditional_variables, valid_variables) {
  
  # Check that conditional_variables is provided
  if (is.null(conditional_variables)) {
    warning(
      paste0(
        "conditional_variables was not specified. To evaluate a hypothesis conditional on one or more levels of a predictor variable, ",
        "you must specify conditional_variables as a character string (or vector) that correspond to population level variables in brms_object. ",
        "The valid conditional_variables are: ", valid_variables$conditional_phrase, 
        ". If you are trying to evaluate a hypothesis that is not conditional, set conditonal_variables = 'none' to silence this warning."
      )
    )
  } else if (!is.character(conditional_variables)) {
    stop(
      paste0(
        "conditional_variables must be a character string (or vector) that corresponds to a population level variable in brms_object. ",
        "The valid conditional_variables are: ", valid_variables$conditional_phrase, "."
      )
    )
  }
  
  # Skip checking conditional variables if none provided
  if (!(is.null(conditional_variables) || any(conditional_variables == "none"))) {
    
    # Check that conditional_variables correspond to at least one parameter
    conditional_is_valid <- map_lgl(conditional_variables, ~ . %in% valid_variables$all[valid_variables$is_factor])
    
    invalid_conditional <- conditional_variables[!conditional_is_valid] %>% 
      paste0(collapse = ", ")
    
    if (!all(conditional_is_valid)) {
      stop(
        paste0(
          "At least one of conditional_variables is invalid: ", invalid_conditional,
          ". conditional_variables must be a character string (or vector) that corresponds to a population level variable in brms_object. ",
          "The valid variable names are: ", valid_variables$conditional_phrase, ".",
          " This error can occur if conditional_variables are not factors."
        )
      )
    }
    
    # Check that effect_variables are not also conditional_variables
    is_effect_and_conditional <- map_lgl(effect_variables, ~ . %in% conditional_variables %>% any()) 
    effect_and_conditional_phrase <- effect_variables[is_effect_and_conditional] %>% 
      paste0(collapse = ", ")
    
    if (any(is_effect_and_conditional)) {
      stop(
        paste0(
          effect_and_conditional_phrase, " is included in effect_variables and conditional_variables. ",
          "This would not produce sensible results. Change one of them to: ", valid_variables$effects_phrase, "."
        )
      )
    }
  }
}

# Check that conditional_levels is valid (provide assistance if false) ---------------------------------------------

check_conditional_levels <- function(conditional_variables, conditional_levels, brms_object) {
  
  # Skip checking conditional_levels if no conditional_variables provided
  if (!(is.null(conditional_variables) || any(conditional_variables == "none"))) {
    
    # Make phrase with valid levels of conditional_variables
    valid_conditional_levels <- map(
      conditional_variables, 
      ~ levels(brms_object$data[[.]])) %>%
      flatten_chr() %>% 
      paste0(collapse = ", ")
    
    # Check that conditional_levels is provided
    if (!is.character(conditional_levels)) {
      stop(
        paste0(
          "conditional_levels must be a character string (or vector) that corresponds to one level from (each of) conditional_variables. ",
          "The valid conditional_variables are: ", valid_conditional_levels, "."
        )
      )
    }
    
    # Check that conditional_levels exist for each of conditional_variables
    conditional_levels_are_valid <- map2_lgl(
      .x = conditional_levels, 
      .y = conditional_variables, 
      ~ .x %in% levels(brms_object$data[[.y]])
    )
    
    invalid_conditional_levels <- conditional_levels[!conditional_levels_are_valid] %>% 
      paste0(collapse = ", ")
    
    if (!all(conditional_levels_are_valid)) {
      stop(
        paste0(
          "At least one of conditional_levels is not a valid level of the conditional_variables: ", invalid_conditional_levels,
          ". conditional_levels must be a character string (or vector) that corresponds to one level from (each of) conditional_variables. ",
          "The valid level names are: ", valid_conditional_levels, "."
        )
      )
    }
  }
}

# Check that variables are centred -------------------------------------------------------------------------------

check_centred_variables <- function(valid_variables, brms_object) {
  
  # Calculate means for factor contrasts and numeric variables
  factor_means <- map_dbl(valid_variables$all[valid_variables$is_factor], ~ brms_object$data[[.]] %>% contrasts() %>% mean())
  continuous_means <- map_dbl(valid_variables$all[valid_variables$is_numeric], ~ brms_object$data[[.]] %>% mean())
  
  # Check if variables are centred around 0 (allowing a little bit of noise for floating points)
  variable_is_centred <- (c(factor_means, continuous_means) > -0.001 & c(factor_means, continuous_means) < 0.001) %>% all()
  uncentred_variables <- valid_variables[!variable_is_centred] %>% 
    paste0(collapse = ", ")
  
  if (!all(variable_is_centred)) {
    warning(
      paste0(
        "At least one of effect_variables or conditional_variables is not centred: ", uncentred_variables,
        ". The interpretation of non-centred variables changes when interactions are included in the model. ",
        "Continuous variables can be centred by subtracting the mean. By default, R uses dummy coding for factors. ",
        "This coding scheme is not centred. An alternative is to use sum coding by running options(contrasts = c('contr.sum', 'contr.poly'))"
      )
    )
  }
}

# Parse the effect_variables into type (factor/numeric) and get  -------------------------------------------------------------------------

parse_effect <- function(effect_variables, valid_variables, brms_object) {
  
  effect <- list()
  
  # Check whether effect_variables are categorical, ordered, or numeric
  effect$is_factor <- map_lgl(effect_variables, ~ . %in% valid_variables$all[valid_variables$is_factor])
  effect$is_numeric <- map_lgl(effect_variables, ~ . %in% valid_variables$all[valid_variables$is_numeric])
  effect$is_ordered <- map_lgl(effect_variables, ~ . %in% valid_variables$all[valid_variables$is_ordered])
  
  if (any(effect$is_factor & !effect$is_ordered)) {
    
    # Retrieve the level names associated with factor effect_variables 
    effect$levels <- map(effect_variables[effect$is_factor & !effect$is_ordered], ~ levels(brms_object$data[[.]]))
    names(effect$levels) <- effect_variables[effect$is_factor & !effect$is_ordered]
    
    # Check whether there are variables and levels that share the same name
    is_variable_and_level <- map_lgl(effect_variables, ~ . %in% unlist(effect$levels) %>% any()) 
    variable_and_level_phrase <- effect_variables[is_variable_and_level] %>% 
      paste0(collapse = ", ")
    
    if (any(is_variable_and_level)) {
      stop(
        paste0(
          "At least one of effect_variables names is shared with an effect_variables factor level name: ", variable_and_level_phrase,
          "The way hypothesis is parsed makes this ambiguous. Change the shared variable or level name and refit the model."
        )
      )
    }
  }
  
  return(effect)
  
}

# Parse hypothesis sides ---------------------------------------------------------------------------------------

parse_hypothesis_sides <- function(hypothesis) {
  
  hypothesis_sides <- list()
  
  ## Parse hypothesis into sides containing variable names / levels (a vector for interactions)
  hypothesis_sides$effects <- hypothesis %>% 
    str_split("[<>]") %>% 
    map(
      ~ str_split(., ":") %>% 
        map(~ str_trim(., side = "both"))
    ) %>%
    squash()
  
  names(hypothesis_sides$effects) <- c("left", "right")
  
  # Check whether either side of the hypothesis is 0
  hypothesis_sides$is_zero <- map_lgl(hypothesis_sides$effects, ~ identical(., "0"))
  
  return(hypothesis_sides)
  
}

# Check the hypothesis sides are valid (provide assistance if false) ---------------------------------------------

check_hypothesis_sides <- function(hypothesis_sides, valid_variables, effect_variables, effect) {
  
  # Variables corresponding to c(left, right). Will change to TRUE in for loop if side is 0 or valid factor or numeric variable
  valid_factor <- c(FALSE, FALSE)
  valid_numeric <- c(FALSE, FALSE)
  valid_ordered <- c(FALSE, FALSE)
  valid_hypothesis <- c(FALSE, FALSE)
  
  for (side in 1:length(hypothesis_sides$effects)) {
    
    # Check that side of hypothesis is valid (unless it equals 0)
    if (!hypothesis_sides$is_zero[side]) {
      # Check that factor effect_variable levels in the hypothesis correspond to levels of the effect_variables
      if (any(effect$is_factor & !effect$is_ordered)) {
        valid_factor[side] <- map_lgl(
          hypothesis_sides$effects[[side]], 
          ~ . %in% unlist(effect$levels)
        ) %>% sum()
      }
      
      # Check that numeric variables in the hypothesis correspond to numeric effect_variables
      if (any(effect$is_numeric)) {
        valid_numeric[side] <- map_lgl(
          hypothesis_sides$effects[[side]], 
          ~ . %in% effect_variables[effect$is_numeric] & . %in% effect_variables
        ) %>% sum()
      }
      
      # Check that ordered variables in the hypothesis correspond to ordered effect_variables
      if (any(effect$is_ordered)) {
        valid_ordered[side] <- map_lgl(
          hypothesis_sides$effects[[side]], 
          ~ . %in% effect_variables[effect$is_ordered] & . %in% effect_variables
        ) %>% sum()
      }
    }
    
    # Check that the number of effects equals the number of valid elements
    all_valid <- length(hypothesis_sides$effects[[side]]) == valid_factor[side] + valid_numeric[side] + valid_ordered[side]
    
    valid_hypothesis[side] <- any(c(hypothesis_sides$is_zero[side], all_valid))
  }
  
  # Phrase if factor variables in valid_variables
  if (length(valid_variables$all[valid_variables$is_factor]) == 0) {
    factor_variables_phrase <- ""
  } else {
    factor_variables_phrase <- paste0(
      "Factor variables should be included using a level of the corresponding effect_variable. ",
      "Valid levels are: ", paste0(unlist(effect$levels), collapse = ", "), ". "
    )
  }
  
  # Phrase if numeric variables in valid_variables
  if (length(valid_variables$all[valid_variables$is_numeric]) == 0) {
    numeric_variables_phrase <- ""
  } else {
    numeric_variables_phrase <- paste0(
      "Numeric variables should be included using the name of the corresponding effect_variable. ",
      "Valid numeric effect_variables are: ", paste0(effect_variables[!effect$is_factor], collapse = ", "), ". "
    )
  }
  
  # Error if hypothesis is incorrectly specified
  if (any(!valid_hypothesis)) {
    stop(
      paste0(
        "At least one of the effect_variables is incorrectly specified in hypothesis. ", 
        factor_variables_phrase, numeric_variables_phrase,
        "Interactions can be specified by placing : between valid effect variables or levels. For example, 'x1:y1 > x1:y2'"
      )
    )
  }
  
  # Check that the left and right side of the hypothesis have the same interaction level
  n_hypothesis_levels <- map_dbl(hypothesis_sides$effects, length) %>% .[!hypothesis_sides$is_zero] %>% unique()
  
  if (length(n_hypothesis_levels) > 1) {
    stop("If an interaction is specified using : on one side of the hypothesis, an interaction of the same level must be specified on the other side.")
  }
  
  # Check length of effect_variables matches the number of variables in hypothesis
  if (n_hypothesis_levels != length(effect_variables)) {
    stop(
      paste(
        "The number of variables supplied to effect_variables must match the number of variables that interact in the hypothesis.",
        "For example, if hypothesis is 'x1 > x2', effect_variables must include one variable.", 
        "If hypothesis is 'x1:y1 > x1:y2', effect_variables must include two variables."
      )
    )
  }
}

# Select model parameters that correspond to the effect and conditional variables -----------------------------------------------------------                                 
select_relevant_parameters <- function(parameters, effect_variables, conditional_variables) {
  
  effect_parameters <- parameters$names_short
  
  # Subset parameters that include all effect variables
  for (variable in effect_variables) {
    effect_parameters <- effect_parameters %>% str_subset(variable)
  }
  
  # How many variables interact in each parameter?
  n_variables <- effect_parameters %>% str_count(":") + 1
  
  # Skip selecting parameters with conditional variables if none provided
  if (!(is.null(conditional_variables) || any(conditional_variables == "none"))) {
    
    # Does the parameter include the conditional variable?
    includes_conditional <- map(
      conditional_variables, 
      ~ str_detect(effect_parameters, .x)
    )
    names(includes_conditional) <- conditional_variables
    
    # How many conditional parameters are included in each effect parameter?
    n_conditional <- pmap_int(includes_conditional, sum)
    
    # Are there any additional variables except the effect and conditional variables?
    includes_additional <- n_conditional + length(effect_variables) < n_variables
    
    # Which conditional variables are included in each parameter?
    includes_conditional <- map(includes_conditional, ~ .[!includes_additional])
    names(includes_conditional) <- conditional_variables
    
    # List parameters that include the effect and at least one additional parameters
    relevant_parameters <- data.frame(
      parameter = effect_parameters[!includes_additional],
      # Retrieve parameter name including prefix
      name = parameters$names_long[parameters$names_short %in% effect_parameters[!includes_additional]]
    ) %>% 
      # Does the parameter include the conditional variable?
      bind_cols(includes_conditional)
    
  } else {
    
    # Are there any additional variables except the effect variables?
    includes_additional <- length(effect_variables) < n_variables
    
    # List parameters that include the effect and at least one additional parameters
    relevant_parameters <- data.frame(
      parameter = effect_parameters[!includes_additional],
      # Retrieve parameter name including prefix
      name = parameters$names_long[parameters$names_short %in% effect_parameters[!includes_additional]]
    )
    
  }
  
  return(relevant_parameters)
  
}

# Calculate contrasts for each effect ---------------------------------------------------------------------------------------

calculate_effect_contrasts <- function(hypothesis_sides, effect, effect_variables, brms_object) {
  
  effect_contrasts <- list()
  
  if (any(effect$is_factor & !effect$is_ordered)) {
    
    # Get the contrasts associated with each level of the effect_variables factors
    effect_contrasts$all <- map(effect_variables[effect$is_factor & !effect$is_ordered], ~ contrasts(brms_object$data[[.]]))
    names(effect_contrasts$all) <- effect_variables[effect$is_factor & !effect$is_ordered]
    
    # Find the product of levels specified on the left and right side of hypothesis (multiplication only relevant to interactions)
    if (!hypothesis_sides$is_zero["left"]) {
      effect_contrasts$left <- map_dbl(effect_contrasts$all, ~ {
        contrast_level <- rownames(.) %in% hypothesis_sides$effects$left
        .[contrast_level]
      }
      ) %>% 
        prod()
    }
    
    if (!hypothesis_sides$is_zero["right"]) {
      effect_contrasts$right <- map_dbl(effect_contrasts$all, ~ {
        contrast_level <- rownames(.) %in% hypothesis_sides$effects$right
        .[contrast_level]
      }
      ) %>% 
        prod()
    }
  } else {
    # If none of effect_variables are factors, set effect contrast to 1
    effect_contrasts$left <- 1
    effect_contrasts$right <- 1
  }
  
  return(effect_contrasts)
  
}

# Calculate contrasts for each effect ---------------------------------------------------------------------------------------

calculate_conditional_contrasts <- function(conditional_variables, conditional_levels, relevant_parameters, brms_object) {
  
  conditional_contrasts <- list()
  
  # For each conditional_variable get the contrasts associated with the indicated conditional_level
  conditional_contrasts$by_levels <- map2_dbl(
    .x = conditional_variables, 
    .y = conditional_levels, 
    ~ contrasts(brms_object$data[[.x]])[.y, ]
  )
  names(conditional_contrasts$by_levels) <- paste0(conditional_variables, " (", conditional_levels, ")")
  
  # For each conditional_variable, if it included in the parameter, output the contrast, else output 1
  conditional_by_parameter <- map2(
    .x = conditional_variables, 
    .y = conditional_contrasts$by_levels, 
    ~ relevant_parameters[[.x]] %>% ifelse(.y, 1)
  )
  names(conditional_by_parameter) <- conditional_variables
  
  # List contrasts by parameter for each conditional variable
  conditional_contrasts$by_parameter <- data.frame(parameter = relevant_parameters$parameter) %>% 
    bind_cols(conditional_by_parameter)
  
  return(conditional_contrasts)
  
}

# Combine effect and conditional contrasts for each parameter --------------------------------------------

combine_contrasts <- function(hypothesis_sides, relevant_parameters, conditional_contrasts, effect_contrasts) {
  
  # Make list with parameter names for combined contrasts
  contrasts <- data.frame(parameter = relevant_parameters$parameter)
  
  # Skip combining contrasts if conditional_variables not provided
  if (!(is.null(conditional_contrasts))) {
    
    # Calculate the combined contrast for each parameter by multiplying the effect and conditional contrasts
    combined_conditional_contrasts <- rep(0, dim(conditional_contrasts$by_parameter)[1])
    
    for (row in 1:dim(conditional_contrasts$by_parameter)[1]) {
      combined_conditional_contrasts[row] <- prod(conditional_contrasts$by_parameter[-1][row, ])
    }
    
    if (!hypothesis_sides$is_zero["left"]) {contrasts$left <- combined_conditional_contrasts * effect_contrasts$left}
    
    if (!hypothesis_sides$is_zero["right"]) {contrasts$right <- combined_conditional_contrasts * effect_contrasts$right}
    
  } else {
    # Use effect contrasts if no conditional_variables
    contrasts$left <- effect_contrasts$left
    contrasts$right <- effect_contrasts$right
  }
  
  return(contrasts)
  
}

# Compose hypothesis phrase from the hypothesis sides and direction  ----------------------------------------------

compose_hypothesis_phrase <- function(hypothesis_sides, hypothesis_direction, relevant_parameters, contrasts) {
  
  if (!hypothesis_sides$is_zero["left"]) {
    phrase_left <- map2(.x = relevant_parameters$name, .y = contrasts$left, ~ paste0(.x, " * ", .y)) %>% 
      flatten_chr() %>% 
      paste0(collapse = " + ")
  } else {
    phrase_left <- "0"
  }
  
  if (!hypothesis_sides$is_zero["right"]) {
    phrase_right <- map2(.x = relevant_parameters$name, .y = contrasts$right, ~ paste0(.x, " * ", .y)) %>% 
      flatten_chr() %>% 
      paste0(collapse = " + ")
  } else {
    phrase_right <- "0"
  }
  
  hypothesis_phrase <- paste(phrase_left, hypothesis_direction, phrase_right)
  
  return(hypothesis_phrase)
  
}

# Evaluate hypothesis phrase using brms::hypothesis ---------------------------------------------------------------------------------------

evaluate_hypothesis <- function(hypothesis_phrase, brms_object) {
  
  # run brms::hypothesis
  hypothesis_evaluation <- hypothesis(brms_object, hypothesis_phrase, class = NULL)
  
  return(hypothesis_evaluation)
  
}

# Summarise brms::hypothesis output using bayestestR ---------------------------------------------------------------------------

summarise_hypothesis_output <- function(hypothesis_evaluation, estimate_type, ci_width, ci_type) {
  
  # Calculate the point estimate depending on the estimate_type
  if (estimate_type == "mean") {
    estimate <- mean(hypothesis_evaluation$samples[[1]])
  } else if (estimate_type == "median") {
    estimate <- median(hypothesis_evaluation$samples[[1]])
  } else if (estimate_type == "mode") {
    estimate <- mode(hypothesis_evaluation$samples[[1]])
  } else {
    stop("A valid point estimate type (estimate_type) was not provided. Options are: 'mean', 'median', or 'mode")
  }
  
  # Check that a valid ci_width was provided
  if (!(is.numeric(ci_width) & ci_width > 0 & ci_width < 1)) {
    stop("A valid credible interval width (ci_width) was not provided. ci_width must be a number between 0 and 1.")
  }
  
  # Calculate the credible interval depending on the ci_type and ci_width
  if (ci_type == "hdi") {  
    hypothesis_ci <- bayestestR::hdi(hypothesis_evaluation$samples[[1]], ci = ci_width)
  } else if (ci_type == "eti") {
    hypothesis_ci <- bayestestR::eti(hypothesis_evaluation$samples[[1]], ci = ci_width)
  } else {
    stop("A valid credible interval type (ci_type) was not provided. Options are: 'hdi' for highest density interval or 'eti' for equal-tailed interval")
  }
  
  # Make tibble to hold hypothesis summary
  hypothesis_summary <- tibble(
    estimate = estimate,
    ci_lower = hypothesis_ci$CI_low,
    ci_upper = hypothesis_ci$CI_high,
    width = ci_width,
    ci_type = ci_type,
    evidence_ratio = hypothesis_evaluation$hypothesis$Evid.Ratio,
    posterior_probability = hypothesis_evaluation$hypothesis$Post.Prob
  )
  
  return(hypothesis_summary)
  
}

# Print summary rather than summary and samples
print.conditionalhypothesis <- function(x, digits = 3) {
  
  if (!(is.null(x$conditional_variables) || any(x$conditional_variables == "none"))) {
    
    # List the conditional variables and their levels
    conditional_phrase <- paste0(
      " conditional on ", 
      paste0(
        map2(
          .x = x$conditional_variables, 
          .y = x$conditional_levels, 
          ~ paste0("level ", .y, " of the variable ", .x)
        ), 
        collapse = ", "
      )
    )
    
  } else {
    conditional_phrase <- ""
  }
  
  formula_phrase <- paste(x$model_formula$formula[2], x$model_formula$formula[1], x$model_formula$formula[3])
  
  cat(paste0("Test of the hypothesis: '", x$hypothesis, "'", conditional_phrase, ". Based on the model: ", formula_phrase, ". \n\n", sep = ""))
  print(as.data.frame(x$summary), digits = digits, row.names = FALSE)
  invisible(x)
  
}
