# Experiments ------------------------------------------------------------------

# 7a. `exp_partial_feedback_past`: Examined the influence of stimulus variability on choices with the EARS referring to past choices
# 7b. `exp_partial_feedback_future`: Examined the influence of stimulus variability on choices with the EARS referring to a specific future choice
# 8. `exp_information_or_reward`: Examined the influence of stimulus variability on whether participants chose to observe or claim the outcome of their choices
# 9. exp_free_sampling: Examined the influence of stimulus variability the number of outcomes participants chose to observe before making a consequential choice

# Setup ------------------------------------------------------------------------

set.seed(1234)

# Load packages
library(tidyverse)
library(parallel)
library(standardize)
library(rlang)
library(psych)
library(lavaan)
library(semPlot)
library(MBESS)
library(brms)
library(bayestestR)
library(tidybayes)
library(bayesplot)
library(loo)
library(here)

# Source custom functions
here::here("src", "utilities", "save_tidy_data.R") %>% source()
here::here("src", "utilities", "load_tidy_data.R") %>% source()
here::here("src", "model", "standardise.R") %>% source()
here::here("src", "model", "run_mcmc_diagnostics.R") %>% source()
here::here("src", "model", "conditional_hypothesis.R") %>% source()
here::here("src", "model", "save_model.R") %>% source()

## Load data ----

data_names <-  c(
  "exp_partial_feedback", 
  "exp_information_or_reward",
  "exp_free_sampling",
  "exp_combined"
)

load_tidy_data(data_names)

## Options ----

# Setting quick_version to TRUE allows faster but less precise parameter estimation by reducing the number of samples taken in the brms models. Set to FALSE to reproduce published values.
# Setting overwrite_saved_models to TRUE ensures that the Stan models are run every time instead of loading a cached version. In order to save time, this should be set to FALSE unless changes have been made to the model. 
# A few Boost and Rcpp warning messages might be generated when the C++ code for the Stan model is compiling, but these are nothing to worry about (see [here](https://discourse.mc-stan.org/t/boost-and-rcppeigen-warnings-for-r-package-using-stan/3478)).
# The number of cores used for the Stan models will be the number of available cores minus the value of the reserved_cores variable (or one if the number of reserved cores is greater than or equal to the number of available cores).

options(quick_version = TRUE)
options(overwrite_saved_models = FALSE)
reserved_cores = 2

# Convert reserved cores into used cores
cores <- max(c(detectCores() - reserved_cores, 1))

# Run as many chains as there are available cores
chains <- cores

warmup <- ifelse(getOption("quick_version"), 1000, 2000)

# Set default contrast type to -1, 1 for factors
options(contrasts = c("contr.sum", "contr.poly"))

## Set priors ----

priors <- NULL

priors$varying_intercepts <- c(
  prior(student_t(7, 0, 0.5), class = Intercept),
  prior(student_t(7, 0, 0.5), class = b),
  prior(student_t(7, 0, 0.5), class = sd)
)

priors$varying_slopes <- prior(lkj(4), class = cor)

priors$simo <- c(
  prior(dirichlet(3), class = simo, coef = moblock1),
  prior(dirichlet(3), class = simo, coef = moblock:conditionConstantImages1),
  prior(dirichlet(3), class = simo, coef = moblock:uncertainty_typeAleatory1),
  prior(dirichlet(3), class = simo, coef = moblock:conditionConstantImages:uncertainty_typeAleatory1),
  prior(dirichlet(3), class = simo, coef = moblock:epistemic1),
  prior(dirichlet(3), class = simo, coef = mocomparison1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock2),
  prior(dirichlet(3), class = simo, coef = mocomparison:conditionConstantImages1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock:conditionConstantImages1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock:conditionConstantImages2),
  prior(dirichlet(3), class = simo, coef = mocomparison:epistemic1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock:epistemic1),
  prior(dirichlet(3), class = simo, coef = mocomparison:moblock:epistemic2)
)

priors$mo <- prior(student_t(7, 0, 0.25), class = b, coef = moblock)

priors$negative_binomial <- prior(student_t(7, 0, 0.5), class = disp)

alternate_priors <- NULL

# Priors for EARS, sampling, and choices models with varying intercepts
alternate_priors$normal$varying_intercepts <- c(
  prior(normal(0, 0.5), class = Intercept),
  prior(normal(0, 0.5), class = b),
  prior(normal(0, 0.5), class = sd)
)

alternate_priors$normal$varying_slopes <- prior(lkj(4), class = cor)
alternate_priors$normal$mo <- prior(normal(0, 0.25), class = b, coef = moblock)

alternate_priors$student$varying_intercepts <- c(
  prior(student_t(3, 0, 1), class = Intercept),
  prior(student_t(3, 0, 1), class = b),
  prior(student_t(3, 0, 1), class = sd)
)

alternate_priors$student$varying_slopes <- prior(lkj(1), class = cor)
alternate_priors$student$mo <- prior(student_t(3, 0, 0.5), class = b, coef = moblock)

# Fit models to the experimental data ------------------------------------------

## 1. Partial-feedback ---------------------------------------------------------

### EARS questionnaire ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_partial_feedback$alternate_models$ears[[prior_type]] <- brm(
    response ~ experiment * condition * risky * uncertainty_type + 
      (1 + risky * uncertainty_type | gr(participant, by = condition)) + 
      (1 + experiment * risky * condition | gr(question, by = uncertainty_type)), 
    data = exp_partial_feedback$ears_long,  
    family = cumulative("probit"),
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]]
    ), 
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(30000 / chains) + 2000),
    file = save_model(paste0("ears_", prior_type), "exp_partial_feedback"),
    control = list(adapt_delta = 0.99),
    inits = 0,
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

# EARS questionnaire - simplified varying slopes for participants
exp_partial_feedback$alternate_models$ears$varying_slopes_simple <- brm(
  response ~ experiment * condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | participant) + 
    (1 | question), 
  data = exp_partial_feedback$ears_long,  
  family = cumulative("probit"),
  prior = c(priors$varying_intercepts, priors$varying_slopes),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(30000 / chains) + 2000),
  file = save_model("ears_varying_slopes_simple", "exp_partial_feedback"),
  control = list(adapt_delta = 0.95),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

# EARS questionnaire - varying intercepts only
exp_partial_feedback$alternate_models$ears$varying_intercepts <- brm(
  response ~ experiment * condition * risky * uncertainty_type + 
    (1 | participant) + 
    (1 | question), 
  data = exp_partial_feedback$ears_long,  
  family = cumulative("probit"),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(30000 / chains) + 2000),
  file = save_model("ears_varying_intercepts", "exp_partial_feedback"),
  control = list(adapt_delta = 0.95),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

# EARS questionnaire - metric
exp_partial_feedback$alternate_models$ears$metric <- brm(
  mean_response ~ experiment * condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | gr(participant, by = condition)), 
  data = exp_partial_feedback$ears_summary %>% 
    mutate(epistemic = standardise(mean_response)), 
  prior = c(priors$varying_intercepts, priors$varying_slopes),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(30000 / chains) + 2000),
  file = save_model("ears_metric", "exp_partial_feedback"),
  control = list(adapt_delta = 0.99),
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_partial_feedback$alternate_models$ears)) {
  
  model_type <- names(exp_partial_feedback$alternate_models$ears[model])
  
  exp_partial_feedback$alternate_hypotheses$ears_risky[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "risky", 
    conditional_levels = "Risky"
  )
  
  exp_partial_feedback$alternate_hypotheses$ears_risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Risky", "Epistemic")
  )
  
  exp_partial_feedback$alternate_hypotheses$ears_risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Risky", "Aleatory")
  )
  
  exp_partial_feedback$alternate_hypotheses$ears_safe[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images:Epistemic < Constant images:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "risky", 
    conditional_levels = "Safe"
  )
  
  exp_partial_feedback$alternate_hypotheses$ears_safe_epistemic[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Safe", "Epistemic")
  )
  
  exp_partial_feedback$alternate_hypotheses$ears_safe_aleatory[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Safe", "Aleatory")
  )
  
  exp_partial_feedback$alternate_hypotheses$risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Safe > Risky", 
    effect_variables = "risky", 
    conditional_variables = c("uncertainty_type"), 
    conditional_levels = c("Epistemic")
  )
  
  exp_partial_feedback$alternate_hypotheses$risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$ears[[model]], 
    hypothesis = "Safe < Risky", 
    effect_variables = "risky", 
    conditional_variables = c("uncertainty_type"), 
    conditional_levels = c("Aleatory")
  )
  
}

### Choices ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_partial_feedback$alternate_models$choices_condition[[prior_type]] <- brm(
    risky_count | trials(n_choices) ~ 1 + experiment * condition + (1 | participant),
    data = exp_partial_feedback$choices_summary,
    family = binomial,
    prior = alternate_priors[[prior]][["varying_intercepts"]],
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model(paste0("choices_condition_", prior_type), "exp_partial_feedback"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Choices (condition) - metric
exp_partial_feedback$alternate_models$choices_condition$metric <- brm(
  prop_risky ~ 1 + experiment * condition,
  data = exp_partial_feedback$choices_summary %>% 
    mutate(prop_risky = standardise(prop_risky)),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_condition_metric", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_partial_feedback$alternate_models$choices_condition)) {
  
  model_type <- names(exp_partial_feedback$alternate_models$choices_condition[model])
  
  exp_partial_feedback$alternate_hypotheses$choices_condition[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$choices_condition[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = c("condition"), 
    conditional_variables = "none"
  )
}

### Choices by responses to the epistemic questions for the risky option ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_partial_feedback$alternate_models$choices_epistemic_risky[[prior_type]] <- brm(
    risky_count | trials(n_choices) ~ 1 + experiment * epistemic + (1 | participant),
    data = exp_partial_feedback$choices_ears %>% 
      filter(risky == "Risky") %>% 
      mutate(epistemic = standardise(epistemic)),
    family = binomial,
    prior = alternate_priors[[prior]][["varying_intercepts"]],
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model(paste0("choices_epistemic_risky_", prior_type), "exp_partial_feedback"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Choices (epistemic, risky) - metric
exp_partial_feedback$alternate_models$choices_epistemic_risky$metric <- brm(
  prop_risky ~ 1 + experiment * epistemic,
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Risky") %>% 
    mutate(
      epistemic = standardise(epistemic),
      prop_risky = standardise(prop_risky)
    ),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_epistemic_risky_metric", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_partial_feedback$alternate_models$choices_epistemic_risky)) {
  
  model_type <- names(exp_partial_feedback$alternate_models$choices_epistemic_risky[model])
  
  exp_partial_feedback$alternate_hypotheses$choices_epistemic_risky[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$choices_epistemic_risky[[model]], 
    hypothesis = "epistemic > 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "none"
  )
}

### Choices by responses to the aleatory questions for the safe option ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_partial_feedback$alternate_models$choices_aleatory_safe[[prior_type]] <- brm(
    risky_count | trials(n_choices) ~ 1 + experiment * aleatory + (1 | participant),
    data = exp_partial_feedback$choices_ears %>% 
      filter(risky == "Safe") %>% 
      mutate(aleatory = standardise(aleatory)),
    family = binomial,
    prior = alternate_priors[[prior]][["varying_intercepts"]],
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model(paste0("choices_aleatory_safe_", prior_type), "exp_partial_feedback"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Choices (aleatory, safe) - metric
exp_partial_feedback$alternate_models$choices_aleatory_safe$metric <- brm(
  prop_risky ~ 1 + experiment * aleatory,
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Safe") %>% 
    mutate(
      aleatory = standardise(aleatory),
      prop_risky = standardise(prop_risky)
    ),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_aleatory_safe_metric", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_partial_feedback$alternate_models$choices_aleatory_safe)) {
  
  model_type <- names(exp_partial_feedback$alternate_models$choices_aleatory_safe[model])
  
  exp_partial_feedback$alternate_hypotheses$choices_aleatory_safe[[model_type]] <- conditional_hypothesis(
    exp_partial_feedback$alternate_models$choices_aleatory_safe[[model]], 
    hypothesis = "aleatory > 0", 
    effect_variables = c("aleatory"), 
    conditional_variables = "none"
  )
}

### Save hypothesis summaries ----

save_tidy_data(
  exp_partial_feedback$alternate_hypotheses,
  parent_folder = here("output", "alternate_hypotheses"),
  data_name = "exp_partial_feedback",
  save_csv = FALSE
)

## 2. Information or reward ----------------------------------------------------

### EARS questionnaire ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_information_or_reward$alternate_models$ears[[prior_type]] <- brm(
    response ~ condition * risky * uncertainty_type + 
      (1 + risky * uncertainty_type | gr(participant, by = condition)) + 
      (1 + risky * condition | gr(question, by = uncertainty_type)), 
    data = exp_information_or_reward$ears_long,  
    family = cumulative("probit"),
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(35000 / chains) + 2000),
    file = save_model(paste0("ears_", prior_type), "exp_information_or_reward"),
    control = list(adapt_delta = 0.99),
    inits = 0,
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

# EARS questionnaire - simplified varying slopes for participants
exp_information_or_reward$alternate_models$ears$varying_slopes_simple <- brm(
  response ~ condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | participant) + 
    (1 | question), 
  data = exp_information_or_reward$ears_long,  
  family = cumulative("probit"),
  prior = c(priors$varying_intercepts, priors$varying_slopes),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(35000 / chains) + 2000),
  file = save_model("varying_slopes_simple", "exp_information_or_reward"),
  control = list(adapt_delta = 0.95),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

# EARS questionnaire - varying intercepts only
exp_information_or_reward$alternate_models$ears$varying_intercepts <- brm(
  response ~ condition * risky * uncertainty_type + 
    (1 | participant) + 
    (1 | question), 
  data = exp_information_or_reward$ears_long,  
  family = cumulative("probit"),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(35000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_information_or_reward"),
  control = list(adapt_delta = 0.95),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

# EARS questionnaire - metric
exp_information_or_reward$alternate_models$ears$metric <- brm(
  mean_response ~ condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | gr(participant, by = condition)),
  data = exp_information_or_reward$ears_summary %>% 
    mutate(epistemic = standardise(mean_response)),
  prior = c(priors$varying_intercepts, priors$varying_slopes), 
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(35000 / chains) + 2000),
  file = save_model("ears_metric", "exp_information_or_reward"),
  control = list(adapt_delta = 0.995, max_treedepth = 15),
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_information_or_reward$alternate_models$ears)) {
  
  model_type <- names(exp_information_or_reward$alternate_models$ears[model])
  
  exp_information_or_reward$alternate_hypotheses$ears_risky[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "risky", 
    conditional_levels = "Risky"
  )
  
  exp_information_or_reward$alternate_hypotheses$ears_risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Risky", "Epistemic")
  )
  
  exp_information_or_reward$alternate_hypotheses$ears_risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Risky", "Aleatory")
  )
  
  exp_information_or_reward$alternate_hypotheses$ears_safe[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images:Epistemic < Constant images:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "risky", 
    conditional_levels = "Safe"
  )
  
  exp_information_or_reward$alternate_hypotheses$ears_safe_epistemic[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Safe", "Epistemic")
  )
  
  exp_information_or_reward$alternate_hypotheses$ears_safe_aleatory[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = c("risky", "uncertainty_type"), 
    conditional_levels = c("Safe", "Aleatory")
  )
  
  exp_information_or_reward$alternate_hypotheses$risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Safe > Risky", 
    effect_variables = "risky", 
    conditional_variables = c("uncertainty_type"), 
    conditional_levels = c("Epistemic")
  )
  
  exp_information_or_reward$alternate_hypotheses$risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$ears[[model]], 
    hypothesis = "Safe < Risky", 
    effect_variables = "risky", 
    conditional_variables = c("uncertainty_type"), 
    conditional_levels = c("Aleatory")
  )
}

### Sampling ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_information_or_reward$alternate_models$observe_or_claim_condition[[prior_type]] <- brm(
    observe_count | trials(n_choices) ~ 1 + condition * risky + (risky | participant),
    data = exp_information_or_reward$observe_or_claim,
    family = binomial,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
    file = save_model(paste0("observe_or_claim_condition_", prior_type), "exp_information_or_reward"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Observe or claim (condition) - varying intercepts only
exp_information_or_reward$alternate_models$observe_or_claim_condition$varying_intercepts <- brm(
  observe_count | trials(n_choices) ~ 1 + condition * risky + (1 | participant),
  data = exp_information_or_reward$observe_or_claim,
  family = binomial,
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_condition_varying_intercepts", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

# Observe or claim (condition) - metric
exp_information_or_reward$alternate_models$observe_or_claim_condition$metric <- brm(
  prop_observe ~ 1 + condition * risky + 
    (risky | participant),
  data = exp_information_or_reward$observe_or_claim %>% 
    mutate(prop_observe = standardise(prop_observe)),
  prior = c(priors$varying_intercepts, priors$varying_slopes),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_condition_metric", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_information_or_reward$alternate_models$observe_or_claim_condition)) {
  
  model_type <- names(exp_information_or_reward$alternate_models$observe_or_claim_condition[model])
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_condition[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_condition[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = c("condition"), 
    conditional_variables = "none"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_condition_interaction[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_condition[[model]], 
    hypothesis = "Unique images:Risky > Constant images:Risky", 
    effect_variables = c("condition", "risky"), 
    conditional_variables = "none"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_condition_risky[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_condition[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = "risky", 
    conditional_levels = "Risky"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_condition_safe[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_condition[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = "risky", 
    conditional_levels = "Safe"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_risky[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_condition[[model]], 
    hypothesis = "Risky > Safe", 
    effect_variables = c("risky"), 
    conditional_variables = "none"
  )
}

# Observe or claim (EARS)
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_information_or_reward$alternate_models$observe_or_claim_epistemic[[prior_type]] <- brm(
    observe_count | trials(n_choices) ~ 1 + epistemic * risky + (risky | participant),
    data = exp_information_or_reward$observe_or_claim_ears %>% 
      mutate(epistemic = standardise(epistemic)),
    family = binomial,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
    file = save_model(paste0("observe_or_claim_epistemic_", prior_type), "exp_information_or_reward"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Observe or claim (EARS) - varying intercepts only
exp_information_or_reward$alternate_models$observe_or_claim_epistemic$varying_intercepts <- brm(
  prop_observe ~ 1 + epistemic * risky + (1 | participant),
  data = exp_information_or_reward$observe_or_claim_ears %>% 
    mutate(
      epistemic = standardise(epistemic),
      prop_observe = standardise(prop_observe)
    ),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_epistemic_varying_intercepts", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

# Observe or claim (EARS) - metric
exp_information_or_reward$alternate_models$observe_or_claim_epistemic$metric <- brm(
  prop_observe ~ 1 + epistemic * risky + (risky | participant),
  data = exp_information_or_reward$observe_or_claim_ears %>% 
    mutate(
      epistemic = standardise(epistemic),
      prop_observe = standardise(prop_observe)
    ),
  prior = c(priors$varying_intercepts, priors$varying_slopes),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_epistemic_metric", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_information_or_reward$alternate_models$observe_or_claim_epistemic)) {
  
  model_type <- names(exp_information_or_reward$alternate_models$observe_or_claim_epistemic[model])
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_epistemic[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_epistemic[[model]], 
    hypothesis = "epistemic > 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "none"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_epistemic_risky[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_epistemic[[model]], 
    hypothesis = "epistemic > 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "risky", 
    conditional_levels = "Risky"
  )
  
  exp_information_or_reward$alternate_hypotheses$observe_or_claim_epistemic_safe[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$observe_or_claim_epistemic[[model]], 
    hypothesis = "epistemic > 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "risky", 
    conditional_levels = "Safe"
  )
}

### Choices ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_information_or_reward$alternate_models$safe_or_risky_condition[[prior_type]] <- brm(
    risky_count | trials(n_choices) ~ 1 + condition + (1 | participant),
    data = exp_information_or_reward$safe_or_risky %>% filter(claim == "Claim"),
    family = binomial,
    prior = alternate_priors[[prior]][["varying_intercepts"]],
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
    file = save_model(paste0("safe_or_risky_condition_", prior_type), "exp_information_or_reward"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Safe or risky (condition) - metric
exp_information_or_reward$alternate_models$safe_or_risky_condition$metric <- brm(
  prop_risky ~ 1 + condition,
  data = exp_information_or_reward$safe_or_risky %>% 
    filter(claim == "Claim") %>% 
    mutate(standardise(prop_risky)),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
  file = save_model("safe_or_risky_condition_metric", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_information_or_reward$alternate_models$safe_or_risky_condition)) {
  
  model_type <- names(exp_information_or_reward$alternate_models$safe_or_risky_condition[model])
  
  exp_information_or_reward$alternate_hypotheses$safe_or_risky_condition[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$safe_or_risky_condition[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = c("condition"), 
    conditional_variables = "none"
  )
}

# Safe or risky (epistemic)
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_information_or_reward$alternate_models$safe_or_risky_epistemic[[prior_type]] <- brm(
    risky_count | trials(n_choices) ~ 1 + epistemic + (1 | participant),
    data = exp_information_or_reward$safe_or_risky_ears %>% 
      filter(claim == "Claim") %>% 
      mutate(epistemic = standardise(epistemic)),
    family = binomial,
    prior = alternate_priors[[prior]][["varying_intercepts"]],
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
    file = save_model(paste0("safe_or_risky_epistemic_", prior_type), "exp_information_or_reward"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Safe or risky (epistemic) - metric
exp_information_or_reward$alternate_models$safe_or_risky_epistemic$metric <- brm(
  prop_risky ~ 1 + epistemic,
  data = exp_information_or_reward$safe_or_risky_ears %>% 
    filter(claim == "Claim") %>% 
    mutate(
      epistemic = standardise(epistemic),
      prop_risky = standardise(prop_risky)
    ),
  prior = priors$varying_intercepts,
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
  file = save_model("safe_or_risky_epistemic_metric", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_information_or_reward$alternate_models$safe_or_risky_epistemic)) {
  
  model_type <- names(exp_information_or_reward$alternate_models$safe_or_risky_epistemic[model])
  
  exp_information_or_reward$alternate_hypotheses$safe_or_risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_information_or_reward$alternate_models$safe_or_risky_epistemic[[model]], 
    hypothesis = "epistemic < 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "none"
  )
}

### Save hypothesis summaries ----

save_tidy_data(
  exp_information_or_reward$alternate_hypotheses,
  parent_folder = here("output", "alternate_hypotheses"),
  data_name = "exp_information_or_reward",
  save_csv = FALSE
)

## 3. Free sampling ------------------------------------------------------------

### EARS questionnaire ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_free_sampling$alternate_models$ears[[prior_type]] <- brm(
    response ~ condition * uncertainty_type * mo(block) + 
      (uncertainty_type | gr(participant, by = condition)) + 
      (1 + condition | gr(question, by = uncertainty_type)), 
    data = exp_free_sampling$ears_long %>% 
      mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
    family = cumulative("probit"),
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]],
      alternate_priors[[prior]][["mo"]],
      priors$simo[1:4,]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
    file = save_model(paste0("ears_", prior_type), "exp_free_sampling"),
    control = list(adapt_delta = 0.99),
    inits = 0,
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )   
}

# EARS questionnaire - simplified varying slopes for participants
exp_free_sampling$alternate_models$ears$simplified_varying_slopes <- brm(
  response ~ condition * uncertainty_type * mo(block) + 
    (uncertainty_type | participant) + 
    (1 | question), 
  data = exp_free_sampling$ears_long %>% 
    mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
  family = cumulative("probit"),
  prior = c(
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$simo[1:4,]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
  file = save_model("simplified_varying_slopes", "exp_free_sampling"),
  control = list(adapt_delta = 0.99),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)   

# EARS questionnaire - varying intercepts only
exp_free_sampling$alternate_models$ears$varying_intercepts <- brm(
  response ~ condition * uncertainty_type * mo(block) + 
    (1 | participant) + 
    (1 | question), 
  data = exp_free_sampling$ears_long %>% 
    mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
  family = cumulative("probit"),
  prior = c(
    priors$varying_intercepts,
    priors$mo,
    priors$simo[1:4,]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_free_sampling"),
  control = list(adapt_delta = 0.99),
  inits = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)   

# EARS questionnaire - metric
exp_free_sampling$alternate_models$ears$metric <- brm(
  mean_response ~ condition * uncertainty_type * mo(block) + 
    (uncertainty_type | gr(participant, by = condition)), 
  data = exp_free_sampling$ears_summary_block %>% 
    mutate(
      epistemic = standardise(mean_response),
      condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE) # brms didn't like the space in the condition names when the model included monotonic predictors
    ),
  prior = c(
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$simo[1:4,]
  ), 
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
  file = save_model("ears_metric", "exp_free_sampling"),
  control = list(adapt_delta = 0.99),
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_free_sampling$alternate_models$ears)) {
  
  model_type <- names(exp_free_sampling$alternate_models$ears[model])
  
  exp_free_sampling$alternate_hypotheses$ears_risky[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$ears[[model]], 
    hypothesis = "UniqueImages:Epistemic > ConstantImages:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "none"
  )
  
  exp_free_sampling$alternate_hypotheses$ears_risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$ears[[model]], 
    hypothesis = "UniqueImages > ConstantImages", 
    effect_variables = "condition", 
    conditional_variables = "uncertainty_type", 
    conditional_levels = "Epistemic"
  )
  
  exp_free_sampling$alternate_hypotheses$ears_risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$ears[[model]], 
    hypothesis = "UniqueImages < ConstantImages", 
    effect_variables = "condition", 
    conditional_variables = "uncertainty_type", 
    conditional_levels = "Aleatory"
  )
}

### Sampling ----

# Negative binomial regression corresponds to poisson regression when the dispersion/shape/phi parameter is infinite. Therefore, the Stan authors recommend setting the prior on the negative square root of this parameter (https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations). This is achieved using a custom faily in brms.
negbinomial_inverse_prior <- custom_family(
  "negbinomial_inverse_prior",
  dpars = c("mu", "disp"),
  links = c("log", "log"),
  lb = c(0, 0),
  ub = c(NA, NA),
  type = "int"
)

# Some functions passed to Stan
stan_funs <- "
  real negbinomial_inverse_prior_lpmf(int y, real mu, real disp) {
    real phi = 1/disp^2;
    return neg_binomial_2_lpmf(y | mu, phi);
  }
  int negbinomial_inverse_prior_rng(int y, real mu, real disp) {
    real phi = 1/disp^2;
    return neg_binomial_2_rng(mu, phi);
  }
"

stanvars <- stanvar(scode = stan_funs, block = "functions")

# Sampling (condition) - alternate priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_free_sampling$alternate_models$sampling_condition[[prior_type]] <- brm(
    n_samples ~ condition * mo(block) + (1 + mo(block) | participant), 
    data = exp_free_sampling$sampling_summary %>% 
      filter(sampling_type == "Free") %>% 
      mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
    family = negbinomial_inverse_prior,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]],
      alternate_priors[[prior]][["mo"]],
      priors$negative_binomial,
      priors$simo[1:2,]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
    file = save_model(paste0("sampling_condition_", prior_type), "exp_free_sampling"),
    cores = cores, 
    chains = chains,
    stanvars = stanvars,
    backend = "cmdstanr"
  )
}

# Sampling (condition) - varying intercepts only
exp_free_sampling$alternate_models$sampling_condition$varying_intercepts <- brm(
  n_samples ~ condition * mo(block) + (1 | participant), 
  data = exp_free_sampling$sampling_summary %>% 
    filter(sampling_type == "Free") %>% 
    mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
  family = negbinomial_inverse_prior,
  prior = c(
    priors$varying_intercepts,
    priors$mo,
    priors$negative_binomial,
    priors$simo[1:2,]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_free_sampling"),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

# Sampling (condition) - poisson
exp_free_sampling$alternate_models$sampling_condition$poisson <- brm(
  n_samples ~ condition * mo(block) + (1 + mo(block) | participant), 
  data = exp_free_sampling$sampling_summary %>% 
    filter(sampling_type == "Free") %>% 
    mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)), # brms didn't like the space in the condition names when the model included monotonic predictors
  family = poisson,
  prior = c(
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$simo[1:2,]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("sampling_condition$poisson", "exp_free_sampling"),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_free_sampling$alternate_models$sampling_condition)) {
  
  model_type <- names(exp_free_sampling$alternate_models$sampling_condition[model])
  
  exp_free_sampling$alternate_hypotheses$sampling_condition[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$sampling_condition[[model]], 
    hypothesis = "UniqueImages > ConstantImages", 
    effect_variables = c("condition"), 
    conditional_variables = "none"
  )
}

# Sampling (EARS) - alternate priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_free_sampling$alternate_models$sampling_epistemic[[prior_type]] <- brm(
    n_samples ~ epistemic * mo(block) + (1 + mo(block) | participant), 
    data = exp_free_sampling$sampling_ears %>% 
      filter(sampling_type == "Free") %>% 
      mutate(
        epistemic = standardise(epistemic),
        # brms didn't like the space in the condition names when the model included monotonic predictors
        condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)
      ),   
    family = negbinomial_inverse_prior,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]],
      alternate_priors[[prior]][["mo"]],
      priors$negative_binomial,
      priors$simo[c(1, 5),]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
    file = save_model(paste0("sampling_epistemic_", prior_type), "exp_free_sampling"),
    cores = cores, 
    chains = chains,
    stanvars = stanvars,
    backend = "cmdstanr"
  )
}

# Sampling (EARS) - intercepts only
exp_free_sampling$alternate_models$sampling_epistemic$varying_intercepts <- brm(
  n_samples ~ epistemic * mo(block) + (1 + mo(block) | participant), 
  data = exp_free_sampling$sampling_ears %>% 
    filter(sampling_type == "Free") %>% 
    mutate(
      epistemic = standardise(epistemic),
      # brms didn't like the space in the condition names when the model included monotonic predictors
      condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)
    ),   
  family = negbinomial_inverse_prior,
  prior = c(
    priors$varying_intercepts,
    priors$mo,
    priors$negative_binomial,
    priors$simo[c(1, 5),]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_free_sampling"),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

# Sampling (EARS) - poisson
exp_free_sampling$alternate_models$sampling_epistemic$poisson <- brm(
  n_samples ~ epistemic * mo(block) + (1 + mo(block) | participant), 
  data = exp_free_sampling$sampling_ears %>% 
    filter(sampling_type == "Free") %>% 
    mutate(
      epistemic = standardise(epistemic),
      # brms didn't like the space in the condition names when the model included monotonic predictors
      condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE)
    ),   
  family = poisson,
  prior = c(
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$simo[c(1, 5),]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("sampling_epistemic_poisson", "exp_free_sampling"),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_free_sampling$alternate_models$sampling_epistemic)) {
  
  model_type <- names(exp_free_sampling$alternate_models$sampling_epistemic[model])
  
  exp_free_sampling$alternate_hypotheses$sampling_epistemic[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$sampling_epistemic[[model]], 
    hypothesis = "epistemic > 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "none"
  )
}

### Choices ----

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_free_sampling$alternate_models$choices_condition[[prior_type]] <- brm(
    risky ~ condition * mo(comparison) * mo(block) + 
      (1 + mo(comparison) * mo(block) | participant),
    data = exp_free_sampling$choices %>% 
      mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE) # brms didn't like the space in the condition names when the model included monotonic predictors
      ),
    family = bernoulli,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]],
      alternate_priors[[prior]][["mo"]],
      priors$simo[c(1:2, 6:11),]
    ),  
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
    file = save_model(paste0("choices_condition_", prior_type), "exp_free_sampling"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Choices (condition) - varying intercepts only
exp_free_sampling$alternate_models$choices_condition$varying_intercepts <- brm(
  risky ~ condition * mo(comparison) * mo(block) + 
    (1 | participant),
  data = exp_free_sampling$choices %>% 
    mutate(condition = condition %>% str_remove(" ") %>% factor() %>% named_contr_sum(., return_contr = FALSE) # brms didn't like the space in the condition names when the model included monotonic predictors
    ),
  family = bernoulli,
  prior = c(
    priors$varying_intercepts,
    priors$mo,
    priors$simo[c(1:2, 6:11),]
  ),  
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_free_sampling"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_free_sampling$alternate_models$choices_condition)) {
  
  model_type <- names(exp_free_sampling$alternate_models$choices_condition[model])
  
  exp_free_sampling$alternate_hypotheses$choices_condition[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$choices_condition[[model]], 
    hypothesis = "UniqueImages < ConstantImages", 
    effect_variables = c("condition"), 
    conditional_variables = "none"
  )
}

# Choices (epistemic) - alternate priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_free_sampling$alternate_models$choices_epistemic[[prior_type]] <- brm(
    risky ~ epistemic * mo(comparison) * mo(block) + 
      (1 + mo(comparison) * mo(block) | participant),
    data = exp_free_sampling$choices_ears %>% 
      mutate(epistemic = standardise(epistemic)),
    family = bernoulli,
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]],
      alternate_priors[[prior]][["mo"]],
      priors$simo[c(1, 5:8, 12:14),]
    ),
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
    file = save_model(paste0("choices_epistemic_", prior_type), "exp_free_sampling"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
}

# Choices (epistemic) - varying intercepts only
exp_free_sampling$alternate_models$choices_epistemic$varying_intercepts <- brm(
  risky ~ epistemic * mo(comparison) * mo(block) + 
    (1 | participant),
  data = exp_free_sampling$choices_ears %>% 
    mutate(epistemic = standardise(epistemic)),
  family = bernoulli,
  prior = c(
    priors$varying_intercepts,
    priors$mo,
    priors$simo[c(1, 5:8, 12:14),]
  ),
  warmup = warmup,
  iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
  file = save_model("varying_intercepts", "exp_free_sampling"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
for (model in 1:length(exp_free_sampling$alternate_models$choices_epistemic)) {
  
  model_type <- names(exp_free_sampling$alternate_models$choices_epistemic[model])
  
  exp_free_sampling$alternate_hypotheses$choices_epistemic[[model_type]] <- conditional_hypothesis(
    exp_free_sampling$alternate_models$choices_epistemic[[model]], 
    hypothesis = "epistemic < 0", 
    effect_variables = c("epistemic"), 
    conditional_variables = "none"
  )
}

### Save hypothesis summaries ----

save_tidy_data(
  exp_free_sampling$alternate_hypotheses,
  parent_folder = here("output", "alternate_hypotheses"),
  data_name = "exp_free_sampling",
  save_csv = FALSE
)

# Fit other reported statistics ------------------------------------------------

## Combined analysis of EARS responses ----

exp_combined <- list()

# Alternative priors
for (prior in 1:length(alternate_priors)) {
  
  prior_type <- names(alternate_priors[prior])
  
  exp_combined$alternate_models$ears[[prior_type]] <- brm(
    response ~ 1 + experiment * condition * uncertainty_type + 
      (1 + uncertainty_type | gr(participant, by = condition)) +
      (1 + condition | gr(question, by = uncertainty_type)),
    data = exp_combined$ears_long %>% filter(risky == "Risky"), 
    family = cumulative("probit"),
    prior = c(
      alternate_priors[[prior]][["varying_intercepts"]], 
      alternate_priors[[prior]][["varying_slopes"]]
    ), 
    warmup = warmup,
    iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
    file = save_model(paste0("ears_", prior_type), "exp_combined"),
    control = list(adapt_delta = 0.99),
    inits = 0,
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

## Test hypotheses based on model
for (model in 1:length(exp_combined$alternate_models$ears)) {
  
  model_type <- names(exp_combined$alternate_models$ears[model])
  
  exp_combined$alternate_hypotheses$ears_risky[[model_type]] <- conditional_hypothesis(
    exp_combined$alternate_models$ears[[model]], 
    hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
    effect_variables = c("condition", "uncertainty_type"), 
    conditional_variables = "none"
  )
  
  exp_combined$alternate_hypotheses$ears_risky_epistemic[[model_type]] <- conditional_hypothesis(
    exp_combined$alternate_models$ears[[model]], 
    hypothesis = "Unique images > Constant images", 
    effect_variables = "condition", 
    conditional_variables = "uncertainty_type", 
    conditional_levels = "Epistemic"
  )
  
  exp_combined$alternate_hypotheses$ears_risky_aleatory[[model_type]] <- conditional_hypothesis(
    exp_combined$alternate_models$ears[[model]], 
    hypothesis = "Unique images < Constant images", 
    effect_variables = "condition", 
    conditional_variables = "uncertainty_type", 
    conditional_levels = "Aleatory"
  )
}

### Save hypothesis summaries ----

save_tidy_data(
  exp_combined$alternate_hypotheses,
  parent_folder = here("output", "alternate_hypotheses"),
  data_name = "exp_combined",
  save_csv = FALSE
)
