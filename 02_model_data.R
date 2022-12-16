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
library(GPArotation)
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
here::here("src", "model", "generate_simo_priors.R") %>% source()
here::here("src", "model", "run_mcmc_diagnostics.R") %>% source()
here::here("src", "model", "conditional_hypothesis.R") %>% source()
here::here("src", "model", "save_model.R") %>% source()

temp <- list()

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
# Set run_diagnostics to TRUE to create additional diagnostic plots for the brms models (e.g., rank histograms, posterior predictive checks). Even if FALSE, the default warnings (e.g., divergences, rhat) will still be displayed if applicable.
# The number of cores used for the Stan models will be the number of available cores minus the value of the reserved_cores variable (or one if the number of reserved cores is greater than or equal to the number of available cores).
# K-fold cross-validation was used in exp_partial_sampling to assess the contribution of the distribution manipulation and requires fitting each of the three models 10 times. This analysis was included in an appendix. Set run_kfold to FALSE to skip this analysis.
# Bootstrap confidence intervals were generated for each omega estimate. This analysis was included in an appendix and can take a couple of hours to run. Set run_bootstrap to FALSE to skip this analysis.

options(quick_version = TRUE)
options(overwrite_saved_models = FALSE)
run_diagnostics = FALSE
reserved_cores = 2
run_kfold = FALSE
run_bootstrap = FALSE

# Convert reserved cores into used cores
cores <- max(c(detectCores() - reserved_cores, 1))

# Run as many chains as there are available cores
chains <- cores

# Set default contrast type to -1, 1 for factors
options(contrasts = c("contr.sum", "contr.poly"))

## Set priors ----

priors <- list()

# Priors for intercept and slopes
priors$basic <- c(
  prior(student_t(7, 0, 0.5), class = Intercept),
  prior(student_t(7, 0, 0.5), class = b)
)

# Prior for standard deviation of varying intercepts
priors$varying_intercepts <- c(prior(student_t(7, 0, 0.5), class = sd))

# Prior for correlation between varying intercepts and slopes
priors$varying_slopes <- prior(lkj(4), class = cor)

# Prior for standard deviation in linear models
priors$sigma <- prior(student_t(7, 0, 0.5), class = sigma)

# Prior for simplex parameters of monotonic models (simo)
# simo class priors generally need to be set individually for each coef
# generate_simo_priors() generates priors for all simo parameters of the model using priors$simo
priors$simo <- prior(dirichlet(3), class = simo)

# Prior for monotonic slope (SD = 0.25 is roughly the same scale as other parameters)
priors$mo <- prior(student_t(7, 0, 0.25), class = b, coef = moblock)

# Prior for excess dispersion parameter of negative binomial
priors$negative_binomial <- prior(student_t(7, 0, 0.5), class = disp)

# Fit models to the experimental data ------------------------------------------

## 7. Partial-feedback ---------------------------------------------------------

### EARS questionnaire ----

exp_partial_feedback$models$ears <- brm(
  response ~ experiment * condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | gr(participant, by = condition)) + 
    (1 + experiment * risky * condition | gr(question, by = uncertainty_type)), 
  data = exp_partial_feedback$ears_long,  
  family = cumulative("probit"),
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes), 
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(30000 / chains) + 2000),
  file = save_model("ears", "exp_partial_feedback"),
  control = list(adapt_delta = 0.99),
  init = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$ears <- run_mcmc_diagnostics(
    exp_partial_feedback$models$ears, 
    type = "bars", 
    categorical = c("experiment", "condition", "risky", "uncertainty_type"))
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$ears_risky <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "risky", 
  conditional_levels = "Risky"
)

exp_partial_feedback$hypotheses$ears_risky_epistemic <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Risky", "Epistemic")
)

exp_partial_feedback$hypotheses$ears_risky_aleatory <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Risky", "Aleatory")
)

exp_partial_feedback$hypotheses$ears_safe <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images:Epistemic < Constant images:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "risky", 
  conditional_levels = "Safe"
)

exp_partial_feedback$hypotheses$ears_safe_epistemic <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Safe", "Epistemic")
)

exp_partial_feedback$hypotheses$ears_safe_aleatory <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Safe", "Aleatory")
)

exp_partial_feedback$hypotheses$risky_epistemic <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Safe > Risky", 
  effect_variables = "risky", 
  conditional_variables = c("uncertainty_type"), 
  conditional_levels = c("Epistemic")
)

exp_partial_feedback$hypotheses$risky_aleatory <- conditional_hypothesis(
  exp_partial_feedback$models$ears, 
  hypothesis = "Safe < Risky", 
  effect_variables = "risky", 
  conditional_variables = c("uncertainty_type"), 
  conditional_levels = c("Aleatory")
)

### Choices ----

#### Choice by condition ----

exp_partial_feedback$models$choices_condition <- brm(
  risky_count | trials(n_choices) ~ 1 + condition * experiment + 
    (1 | participant),
  data = exp_partial_feedback$choices_summary,
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_condition", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$choices_condition <- run_mcmc_diagnostics(
    exp_partial_feedback$models$choices_condition, 
    type = "density", 
    categorical = c("condition", "experiment"))
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$choices_condition <- conditional_hypothesis(
  exp_partial_feedback$models$choices_condition, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = c("condition"), 
  conditional_variables = "none"
)

#### Choices by responses to the epistemic questions for the risky option ----

exp_partial_feedback$models$choices_epistemic_risky <- brm(
  risky_count | trials(n_choices) ~ 1 + epistemic * experiment + 
    (1 | participant),
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Risky") %>% 
    mutate(epistemic = standardise(epistemic)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_epistemic_risky", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$choices_epistemic_risky <- run_mcmc_diagnostics(
    exp_partial_feedback$models$choices_epistemic_risky, 
    type = "density", 
    categorical = "experiment", 
    continuous = "epistemic")
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$choices_epistemic_risky <- conditional_hypothesis(
  exp_partial_feedback$models$choices_epistemic_risky, 
  hypothesis = "epistemic > 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "none"
)

# Mediation analysis (treatment = condition; mediator = epistemic; response = prop_risky)

# Set prior for all response variables
temp$exp_partial_feedback$choices_condition_epistemic_risky$prior <- map_df(
  c("proprisky", "epistemic"), 
  ~ mutate(c(priors$basic, priors$sigma), resp = .)
)

exp_partial_feedback$models$choices_condition_epistemic_risky <- brm(
  bf(epistemic ~ condition + experiment) + 
    bf(prop_risky ~ condition + epistemic + experiment) + 
    set_rescor(FALSE), 
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Risky") %>% 
    mutate(
      # Explicit factor coding is required for bayestestR::mediation()
      condition = ifelse(condition == "Unique images", 1, -1),
      experiment = ifelse(condition == "Experiment b", 1, -1),
      epistemic = standardise(epistemic)
    ), 
  prior = temp$exp_partial_feedback$choices_condition_epistemic_risky$prior,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_condition_epistemic_risky", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Use bayestestR to calculate direct and indirect paths
exp_partial_feedback$hypotheses$mediation_epistemic_risky <- bayestestR::mediation(
  exp_partial_feedback$models$choices_condition_epistemic_risky, 
  centrality = "median", method = "HDI"
) %>% 
  as_tibble()

#### Choices by responses to the aleatory questions for the safe option ----

exp_partial_feedback$models$choices_aleatory_safe <- brm(
  risky_count | trials(n_choices) ~ 1 + aleatory * experiment + 
    (1 | participant),
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Safe") %>% 
    mutate(aleatory = standardise(aleatory)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_aleatory_safe", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$choices_aleatory_safe <- run_mcmc_diagnostics(
    exp_partial_feedback$models$choices_aleatory_safe, 
    type = "density", 
    categorical = "experiment", 
    continuous = "aleatory"
  )
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$choices_aleatory_safe <- conditional_hypothesis(
  exp_partial_feedback$models$choices_aleatory_safe, 
  hypothesis = "aleatory > 0", 
  effect_variables = c("aleatory"), 
  conditional_variables = "none"
)

# Mediation analysis (treatment = condition; mediator = aleatory; response = prop_risky)

# Set prior for all response variables
temp$exp_partial_feedback$choices_condition_aleatory_safe$prior <- map_df(
  c("proprisky", "aleatory"), 
  ~ mutate(c(priors$basic, priors$sigma), resp = .)
)

exp_partial_feedback$models$choices_condition_aleatory_safe <- brm(
  bf(aleatory ~ condition + experiment) + 
    bf(prop_risky ~ condition + aleatory + experiment) + 
    set_rescor(FALSE), 
  data = exp_partial_feedback$choices_ears %>% 
    filter(risky == "Safe") %>% 
    mutate(
      # Explicit factor coding is required for bayestestR::mediation()
      condition = ifelse(condition == "Unique images", 1, -1), 
      experiment = ifelse(condition == "Experiment b", 1, -1),
      aleatory = standardise(aleatory)
    ), 
  prior = temp$exp_partial_feedback$choices_condition_aleatory_safe$prior,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices_condition_aleatory_safe", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

# Use bayestestR to calculate direct and indirect paths
exp_partial_feedback$hypotheses$mediation_aleatory_safe <- bayestestR::mediation(
  exp_partial_feedback$models$choices_condition_aleatory_safe, 
  centrality = "median", method = "HDI"
) %>% 
  as_tibble()

### Ellsberg ----

# Choices ~ condition * Ellsberg
exp_partial_feedback$models$ellsberg_choices_condition <- brm(
  risky_count | trials(n_choices) ~ condition * ellsberg_score + (1 | participant),
  data = exp_partial_feedback$ellsberg_choices %>% 
    filter(risky == "Risky") %>% 
    mutate(ellsberg_score = standardise(ellsberg_score)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("ellsberg_choices_condition", "exp_partial_feedback"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$ellsberg_choices_condition <- run_mcmc_diagnostics(
    exp_partial_feedback$models$ellsberg_choices_condition, 
    type = "density", 
    categorical = "condition",
    continuous = "ellsberg_score"
  )
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$ellsberg_choices_condition <- conditional_hypothesis(
  exp_partial_feedback$models$ellsberg_choices_condition, 
  hypothesis = "ellsberg_score:Unique images < ellsberg_score:Constant images", 
  effect_variables = c("ellsberg_score", "condition"), 
  conditional_variables = "none"
)

# Choices ~ epistemic * Ellsberg
exp_partial_feedback$models$ellsberg_choices_epistemic <- brm(
  risky_count | trials(n_choices) ~ 1 + epistemic * ellsberg_score + (1 | participant),
  data = exp_partial_feedback$ellsberg_choices %>% 
    filter(risky == "Risky") %>% 
    mutate(
      epistemic = standardise(epistemic),
      ellsberg_score = standardise(ellsberg_score)
    ),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("ellsberg_choices_epistemic", "exp_partial_feedback"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_partial_feedback$diagnostics$ellsberg_choices_epistemic <- run_mcmc_diagnostics(
    exp_partial_feedback$models$ellsberg_choices_epistemic, 
    type = "bars",
    continuous = c("epistemic", "ellsberg_score")
  )
}

## Test hypotheses based on model
exp_partial_feedback$hypotheses$ellsberg_choices_epistemic <- conditional_hypothesis(
  exp_partial_feedback$models$ellsberg_choices_epistemic, 
  hypothesis = "ellsberg_score:epistemic < 0", 
  effect_variables = c("ellsberg_score", "epistemic"), 
  conditional_variables = "none"
)

### Outcome sequences (distribution) ----

if (run_kfold) {
  
  # Choices (without sequence)
  exp_partial_feedback$models$choices_without_distribution <- brm(
    risky_count| trials(n_choices) ~ 1 + condition + (1 | participant),
    data = exp_partial_feedback$choices_summary %>% filter(experiment == "Experiment a"),
    family = binomial,
    prior = c(priors$basic, priors$varying_intercepts),
    warmup = ifelse(getOption("quick_version"), 1000, 2000),
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model("choices_without_distribution", "exp_partial_feedback"),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
  
  ## Generate rank histogram, density plots, and posterior predictive plots
  exp_partial_feedback$diagnostics$choices_without_distribution <- run_mcmc_diagnostics(exp_partial_feedback$models$choices_without_distribution, type = "density", categorical = "condition")
  
  # Choices (with varying intercepts for sequence)
  exp_partial_feedback$models$choices_distribution_intercepts <- brm(
    risky_count | trials(n_choices) ~ 1 + condition + (1 | participant) + (1 | distribution),
    data = exp_partial_feedback$choices_summary %>% filter(experiment == "Experiment a"),
    family = binomial,
    prior = c(priors$basic, priors$varying_intercepts),
    warmup = ifelse(getOption("quick_version"), 1000, 2000),
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model("choices_distribution_intercepts", "exp_partial_feedback"),
    control = list(adapt_delta = 0.99),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
  
  ## Generate rank histogram, density plots, and posterior predictive plots
  exp_partial_feedback$diagnostics$choices_distribution_intercepts <- run_mcmc_diagnostics(exp_partial_feedback$models$choices_distribution_intercepts, type = "density", categorical = "condition")
  
  # Choices (with varying slopes for sequence)
  exp_partial_feedback$models$choices_distribution_slopes <- brm(
    risky_count | trials(n_choices) ~ 1 + condition + (1 | participant) + (1 + condition | distribution),
    data = exp_partial_feedback$choices_summary %>% filter(experiment == "Experiment a"),
    family = binomial,
    prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
    warmup = ifelse(getOption("quick_version"), 1000, 2000),
    iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
    file = save_model("choices_distribution_slopes", "exp_partial_feedback"),
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    cores = cores,
    chains = chains,
    backend = "cmdstanr"
  )
  
  ## Generate rank histogram, density plots, and posterior predictive plots
  exp_partial_feedback$diagnostics$choices_distribution_slopes <- run_mcmc_diagnostics(exp_partial_feedback$models$choices_distribution_slopes, type = "density", categorical = "condition")
  
  ## K-fold cross-validation to look at the contribution of the distribution variable
  exp_partial_feedback$model_comparison$choices_distribution_kfold <- kfold(
    exp_partial_feedback$models$choices_without_distribution,
    exp_partial_feedback$models$choices_distribution_intercepts,
    exp_partial_feedback$models$choices_distribution_slopes,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    warmup = 2000,
    iter = 5000,
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
  
  # Save k-fold model comparison
  write_rds(exp_partial_feedback$model_comparison, file = here("output", "model_summaries", "exp_partial_feedback", "exp_partial_feedback_model_comparison.rds"))
  
}

### Save hypothesis summaries ----

save_tidy_data(
  exp_partial_feedback$hypotheses,
  parent_folder = here("output", "hypotheses"),
  data_name = "exp_partial_feedback",
  save_csv = FALSE
)

## 8. Information or reward ----------------------------------------------------

### EARS questionnaire ----

exp_information_or_reward$models$ears <- brm(
  response ~ condition * risky * uncertainty_type + 
    (1 + risky * uncertainty_type | gr(participant, by = condition)) + 
    (1 + risky * condition | gr(question, by = uncertainty_type)), 
  data = exp_information_or_reward$ears_long,  
  family = cumulative("probit"),
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes), 
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(35000 / chains) + 2000),
  file = save_model("ears", "exp_information_or_reward"),
  control = list(adapt_delta = 0.99),
  init = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$ears <- run_mcmc_diagnostics(
    exp_information_or_reward$models$ears, 
    type = "bars", 
    categorical = c("condition", "risky", "uncertainty_type")
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$ears_risky <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "risky", 
  conditional_levels = "Risky"
)

exp_information_or_reward$hypotheses$ears_risky_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Risky", "Epistemic")
)

exp_information_or_reward$hypotheses$ears_risky_aleatory <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Risky", "Aleatory")
)

exp_information_or_reward$hypotheses$ears_safe <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images:Epistemic < Constant images:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "risky", 
  conditional_levels = "Safe"
)

exp_information_or_reward$hypotheses$ears_safe_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Safe", "Epistemic")
)

exp_information_or_reward$hypotheses$ears_safe_aleatory <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = c("risky", "uncertainty_type"), 
  conditional_levels = c("Safe", "Aleatory")
)

exp_information_or_reward$hypotheses$risky_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Safe > Risky", 
  effect_variables = "risky", 
  conditional_variables = c("uncertainty_type"), 
  conditional_levels = c("Epistemic")
)

exp_information_or_reward$hypotheses$risky_aleatory <- conditional_hypothesis(
  exp_information_or_reward$models$ears, 
  hypothesis = "Safe < Risky", 
  effect_variables = "risky", 
  conditional_variables = c("uncertainty_type"), 
  conditional_levels = c("Aleatory")
)

### Sampling ----

# Observe or claim (condition)
exp_information_or_reward$models$observe_or_claim_condition <- brm(
  observe_count | trials(n_choices) ~ 1 + condition * risky + 
    (risky | participant),
  data = exp_information_or_reward$observe_or_claim,
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_condition", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$observe_or_claim_condition <- run_mcmc_diagnostics(
    exp_information_or_reward$models$observe_or_claim_condition, 
    type = "density", 
    categorical = c("condition", "risky"))
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$observe_or_claim_condition <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_condition, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = c("condition"), 
  conditional_variables = "none"
)

## Test hypotheses based on model
exp_information_or_reward$hypotheses$observe_or_claim_condition_interaction <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_condition, 
  hypothesis = "Unique images:Risky > Constant images:Risky", 
  effect_variables = c("condition", "risky"), 
  conditional_variables = "none"
)

## Test hypotheses based on model
exp_information_or_reward$hypotheses$observe_or_claim_condition_risky <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_condition, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = "risky", 
  conditional_levels = "Risky"
)

exp_information_or_reward$hypotheses$observe_or_claim_condition_safe <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_condition, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = "risky", 
  conditional_levels = "Safe"
)

exp_information_or_reward$hypotheses$observe_or_claim_risky <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_condition, 
  hypothesis = "Risky > Safe", 
  effect_variables = c("risky"), 
  conditional_variables = "none"
)

# Observe or claim (EARS)
exp_information_or_reward$models$observe_or_claim_epistemic <- brm(
  observe_count | trials(n_choices) ~ 1 + epistemic * risky + (risky | participant),
  data = exp_information_or_reward$observe_or_claim_ears %>% 
    mutate(epistemic = standardise(epistemic)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(50000 / chains) + 2000),
  file = save_model("observe_or_claim_epistemic", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$observe_or_claim_epistemic <- run_mcmc_diagnostics(
    exp_information_or_reward$models$observe_or_claim_epistemic, 
    type = "density", 
    categorical = "risky", 
    continuous = "epistemic"
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$observe_or_claim_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_epistemic, 
  hypothesis = "epistemic > 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "none"
)

exp_information_or_reward$hypotheses$observe_or_claim_epistemic_risky <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_epistemic, 
  hypothesis = "epistemic > 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "risky", 
  conditional_levels = "Risky"
)

exp_information_or_reward$hypotheses$observe_or_claim_epistemic_safe <- conditional_hypothesis(
  exp_information_or_reward$models$observe_or_claim_epistemic, 
  hypothesis = "epistemic > 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "risky", 
  conditional_levels = "Safe"
)

### Choices ----

# Safe or risky (condition)
exp_information_or_reward$models$safe_or_risky_condition <- brm(
  risky_count | trials(n_choices) ~ 1 + condition + (1 | participant),
  data = exp_information_or_reward$safe_or_risky %>% filter(claim == "Claim"),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
  file = save_model("safe_or_risky_condition", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$safe_or_risky_condition <- run_mcmc_diagnostics(
    exp_information_or_reward$models$safe_or_risky_condition, 
    type = "density", 
    categorical = "condition"
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$safe_or_risky_condition <- conditional_hypothesis(
  exp_information_or_reward$models$safe_or_risky_condition, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = c("condition"), 
  conditional_variables = "none"
)

# Safe or risky (EARS)
exp_information_or_reward$models$safe_or_risky_epistemic <- brm(
  risky_count | trials(n_choices) ~ 1 + epistemic + (1 | participant),
  data = exp_information_or_reward$safe_or_risky_ears %>% 
    filter(claim == "Claim") %>% 
    mutate(epistemic = standardise(epistemic)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(120000 / chains) + 2000),
  file = save_model("safe_or_risky_epistemic", "exp_information_or_reward"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$safe_or_risky_epistemic <- run_mcmc_diagnostics(
    exp_information_or_reward$models$safe_or_risky_epistemic, 
    type = "density", 
    continuous = "epistemic"
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$safe_or_risky_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$safe_or_risky_epistemic, 
  hypothesis = "epistemic < 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "none"
)

### Ellsberg ----

# Choice ~ condition * Ellsberg
exp_information_or_reward$models$ellsberg_choices_condition <- brm(
  risky_count | trials(n_choices) ~ 1 + condition * ellsberg_score + (1 | participant),
  data = exp_information_or_reward$ellsberg_safe_or_risky %>% 
    filter(claim == "Claim") %>% 
    mutate(ellsberg_score = standardise(ellsberg_score)),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(200000 / chains) + 2000),
  file = save_model("ellsberg_choices_condition", "exp_information_or_reward"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$ellsberg_safe_or_risky <- run_mcmc_diagnostics(
    exp_information_or_reward$models$ellsberg_safe_or_risky, 
    type = "density", 
    categorical = "condition",
    continuous = "ellsberg_score"
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$ellsberg_choices_condition <- conditional_hypothesis(
  exp_information_or_reward$models$ellsberg_choices_condition, 
  hypothesis = "ellsberg_score:Unique images < ellsberg_score:Constant images", 
  effect_variables = c("ellsberg_score", "condition"), 
  conditional_variables = "none"
)

# Choices ~ epistemic * Ellsberg
exp_information_or_reward$models$ellsberg_choices_epistemic <- brm(
  risky_count | trials(n_choices) ~ 1 + epistemic * ellsberg_score + (1 | participant),
  data = exp_information_or_reward$ellsberg_safe_or_risky %>% 
    filter(claim == "Claim") %>%
    mutate(
      epistemic = standardise(epistemic),
      ellsberg_score = standardise(ellsberg_score)
    ),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("ellsberg_choices_epistemic", "exp_information_or_reward"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_information_or_reward$diagnostics$ellsberg_choices_epistemic <- run_mcmc_diagnostics(
    exp_information_or_reward$models$ellsberg_choices_epistemic, 
    type = "bars",
    continuous = c("epistemic", "ellsberg_score")
  )
}

## Test hypotheses based on model
exp_information_or_reward$hypotheses$ellsberg_choices_epistemic <- conditional_hypothesis(
  exp_information_or_reward$models$ellsberg_choices_epistemic, 
  hypothesis = "ellsberg_score:epistemic < 0", 
  effect_variables = c("ellsberg_score", "epistemic"), 
  conditional_variables = "none"
)

### Save hypothesis summaries ----

save_tidy_data(
  exp_information_or_reward$hypotheses,
  parent_folder = here("output", "hypotheses"),
  data_name = "exp_information_or_reward",
  save_csv = FALSE
)

## 9. Free sampling ------------------------------------------------------------

### EARS questionnaire ----

temp$exp_free_sampling$ears$formula <- 
  response ~ condition * uncertainty_type * mo(block) + 
  (uncertainty_type | gr(participant, by = condition)) + 
  (1 + condition | gr(question, by = uncertainty_type))

temp$exp_free_sampling$ears$data <- 
  exp_free_sampling$ears_long %>% 
  # brms didn't like the space in condition for monotonic models
  mutate(
    condition = condition %>% 
      str_remove(" ") %>% 
      factor() %>% 
      named_contr_sum(., return_contr = FALSE)
  ) 

exp_free_sampling$models$ears <- brm(
  temp$exp_free_sampling$ears$formula, 
  data = temp$exp_free_sampling$ears$data,
  family = cumulative("probit"),
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$prior$mo,
    generate_simo_priors(priors$simo, temp$exp_free_sampling$ears)
  ), 
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
  file = save_model("ears", "exp_free_sampling"),
  control = list(adapt_delta = 0.995),
  init = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)   

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_free_sampling$diagnostics$ears <- run_mcmc_diagnostics(
    exp_free_sampling$models$ears, 
    type = "bars", 
    categorical = c("condition", "uncertainty_type", "sampling_type"))
}

## Test hypotheses based on model
exp_free_sampling$hypotheses$ears_risky <- conditional_hypothesis(
  exp_free_sampling$models$ears, 
  hypothesis = "Uniqueimages:Epistemic > Constantimages:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "none"
)

exp_free_sampling$hypotheses$ears_risky_epistemic <- conditional_hypothesis(
  exp_free_sampling$models$ears, 
  hypothesis = "Uniqueimages > Constantimages", 
  effect_variables = "condition", 
  conditional_variables = "uncertainty_type", 
  conditional_levels = "Epistemic"
)

exp_free_sampling$hypotheses$ears_risky_aleatory <- conditional_hypothesis(
  exp_free_sampling$models$ears, 
  hypothesis = "Uniqueimages < Constantimages", 
  effect_variables = "condition", 
  conditional_variables = "uncertainty_type", 
  conditional_levels = "Aleatory"
)

### Sampling ----

# Negative binomial regression corresponds to poisson regression when the dispersion/shape/phi parameter is infinite. Therefore, the Stan authors recommend setting the prior on the negative square root of this parameter (https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations). This is achieved using a custom family in brms.
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

# Sampling (condition)
temp$exp_free_sampling$sampling_condition$formula <- 
  n_samples ~ condition * mo(block) + (1 + mo(block) | participant)

temp$exp_free_sampling$sampling_condition$data <- 
  exp_free_sampling$sampling_summary %>% 
  filter(sampling_type == "Free") %>% 
  # brms didn't like the space in condition for monotonic models
  mutate(
    condition = condition %>% 
      str_remove(" ") %>% 
      factor() %>% 
      named_contr_sum(., return_contr = FALSE)
  )

exp_free_sampling$models$sampling_condition <- brm(
  temp$exp_free_sampling$sampling_condition$formula, 
  data = temp$exp_free_sampling$sampling_condition$data,
  family = negbinomial_inverse_prior,
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$negative_binomial,
    generate_simo_priors(priors$simo, temp$exp_free_sampling$sampling_condition)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("sampling_condition", "exp_free_sampling"),
  control = list(adapt_delta = 0.995),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  # exp_free_sampling$diagnostics$sampling_condition <- run_mcmc_diagnostics(
  # exp_free_sampling$models$sampling_condition, 
  # type = "density", 
  # categorical = "condition"
  # )
}

## Test hypotheses based on model
exp_free_sampling$hypotheses$sampling_condition <- conditional_hypothesis(
  exp_free_sampling$models$sampling_condition, 
  hypothesis = "Uniqueimages > Constantimages", 
  effect_variables = c("condition"), 
  conditional_variables = "none"
)

# Sampling (EARS)
temp$exp_free_sampling$sampling_epistemic$formula <- 
  n_samples ~ epistemic * mo(block) + (1 + mo(block) | participant)

temp$exp_free_sampling$sampling_epistemic$data <- 
  exp_free_sampling$sampling_ears %>% 
  filter(sampling_type == "Free") %>% 
  mutate(
    epistemic = standardise(epistemic),
    # brms didn't like the space in condition for monotonic models
    condition = condition %>% 
      str_remove(" ") %>% 
      factor() %>% 
      named_contr_sum(., return_contr = FALSE)
  )

exp_free_sampling$models$sampling_epistemic <- brm(
  temp$exp_free_sampling$sampling_epistemic$formula, 
  data = temp$exp_free_sampling$sampling_epistemic$data,   
  family = negbinomial_inverse_prior,
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    priors$negative_binomial,
    generate_simo_priors(priors$simo, temp$exp_free_sampling$sampling_epistemic)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(20000 / chains) + 2000),
  file = save_model("sampling_epistemic", "exp_free_sampling"),
  control = list(adapt_delta = 0.995),
  cores = cores, 
  chains = chains,
  stanvars = stanvars,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  # exp_free_sampling$diagnostics$sampling_epistemic <- run_mcmc_diagnostics(
  #   exp_free_sampling$models$sampling_epistemic, 
  #   type = "density", 
  #   continuous = "epistemic"
  # )
}

## Test hypotheses based on model
exp_free_sampling$hypotheses$sampling_epistemic <- conditional_hypothesis(
  exp_free_sampling$models$sampling_epistemic, 
  hypothesis = "epistemic > 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "none"
)

### Choices ----

# Choices (condition)
temp$exp_free_sampling$choices_condition$formula <- 
  risky ~ condition * mo(comparison) * mo(block) + 
  (1 + mo(comparison) * mo(block) | participant)

temp$exp_free_sampling$choices_condition$data <- 
  exp_free_sampling$choices %>% 
  # brms didn't like the space in condition for monotonic models
  mutate(
    condition = condition %>% 
      str_remove(" ") %>% 
      factor() %>% 
      named_contr_sum(., return_contr = FALSE) 
  )

exp_free_sampling$models$choices_condition <- brm(
  temp$exp_free_sampling$choices_condition$formula,
  data = temp$exp_free_sampling$choices_condition$data,
  family = bernoulli,
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    generate_simo_priors(priors$simo, temp$exp_free_sampling$choices_condition)
  ),  
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
  file = save_model("choices_condition", "exp_free_sampling"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_free_sampling$diagnostics$choices_condition <- run_mcmc_diagnostics(
    exp_free_sampling$models$choices_condition, 
    type = "density", 
    categorical = c("condition", "comparison", "sampling_type")
  )
}

## Test hypotheses based on model
exp_free_sampling$hypotheses$choices_condition <- conditional_hypothesis(
  exp_free_sampling$models$choices_condition, 
  hypothesis = "Uniqueimages < Constantimages", 
  effect_variables = c("condition"), 
  conditional_variables = "none"
)

# Choices (epistemic)
temp$exp_free_sampling$choices_epistemic$formula <- 
  risky ~ epistemic * mo(comparison) * mo(block) + 
  (1 + mo(comparison) * mo(block) | participant)

temp$exp_free_sampling$choices_epistemic$data <- 
  exp_free_sampling$choices_ears %>% 
  mutate(epistemic = standardise(epistemic))

exp_free_sampling$models$choices_epistemic <- brm(
  temp$exp_free_sampling$choices_epistemic$formula,
  data = temp$exp_free_sampling$choices_epistemic$data,
  family = bernoulli,
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mo,
    generate_simo_priors(priors$simo, temp$exp_free_sampling$choices_epistemic)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(25000 / chains) + 2000),
  file = save_model("choices_epistemic", "exp_free_sampling"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_free_sampling$diagnostics$choices_epistemic <- run_mcmc_diagnostics(
    exp_free_sampling$models$choices_epistemic, 
    type = "density", 
    categorical = c("comparison", "sampling_type"), 
    continuous = "epistemic"
  )
}

## Test hypotheses based on model
exp_free_sampling$hypotheses$choices_epistemic <- conditional_hypothesis(
  exp_free_sampling$models$choices_epistemic, 
  hypothesis = "epistemic < 0", 
  effect_variables = c("epistemic"), 
  conditional_variables = "none"
)

### Save hypothesis summaries ----

save_tidy_data(
  exp_free_sampling$hypotheses,
  parent_folder = here("output", "hypotheses"),
  data_name = "exp_free_sampling",
  save_csv = FALSE
)

# Fit other reported statistics ------------------------------------------------

## Combined analysis of EARS responses -----------------------------------------

# EARS questionnaire
exp_combined$models$ears <- brm(
  response ~ 1 + experiment * condition * uncertainty_type + 
    (1 + uncertainty_type | gr(participant, by = condition)) +
    (1 + condition | gr(question, by = uncertainty_type)),
  data = exp_combined$ears_long %>% filter(risky == "Risky"), 
  family = cumulative("probit"),
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes), 
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(45000 / chains) + 2000),
  file = save_model("ears", "exp_combined"),
  control = list(adapt_delta = 0.995),
  init = 0,
  cores = cores, 
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_combined$diagnostics$ears <- run_mcmc_diagnostics(exp_combined$models$ears, type = "bars", categorical = c("condition", "uncertainty_type"))
}

## Test hypotheses based on model
exp_combined$hypotheses$ears_risky <- conditional_hypothesis(
  exp_combined$models$ears, 
  hypothesis = "Unique images:Epistemic > Constant images:Epistemic", 
  effect_variables = c("condition", "uncertainty_type"), 
  conditional_variables = "none"
)

exp_combined$hypotheses$ears_risky_epistemic <- conditional_hypothesis(
  exp_combined$models$ears, 
  hypothesis = "Unique images > Constant images", 
  effect_variables = "condition", 
  conditional_variables = "uncertainty_type", 
  conditional_levels = "Epistemic"
)

exp_combined$hypotheses$ears_risky_aleatory <- conditional_hypothesis(
  exp_combined$models$ears, 
  hypothesis = "Unique images < Constant images", 
  effect_variables = "condition", 
  conditional_variables = "uncertainty_type", 
  conditional_levels = "Aleatory"
)

### Save hypothesis summaries ----

save_tidy_data(
  exp_combined$hypotheses,
  parent_folder = here("output", "hypotheses"),
  data_name = "exp_combined",
  save_csv = FALSE
)

## Factor analysis and reliability ---------------------------------------------

# Specify lavaan models used for confirmatory factor analysis and reliability (omega)
lavaan_model <- "
    aleatory =~ Q1 + Q2 + Q3 + Q4
    epistemic =~ Q5 + Q6 + Q7 + Q8 + Q9 + Q10
"

lavaan_model_aleatory <-  "aleatory =~ Q1 + Q2 + Q3 + Q4"

lavaan_model_epistemic <- "epistemic =~ Q5 + Q6 + Q7 + Q8 + Q9 + Q10"

### Partial feedback (risky) ---------------------------------------------------

#### Exploratory factor analysis ----

exp_partial_feedback$factors$ears_risky <- list()

# Extract responses in wide format, reverse code aleatory items
exp_partial_feedback$factors$ears_risky$questions_reversed <- exp_partial_feedback$ears %>% 
  filter(risky == "Risky") %>% 
  select(starts_with("question")) %>% 
  mutate_at(paste0("question_", 1:4), ~ 6 - .) 

# Abbreviate column names
names(exp_partial_feedback$factors$ears_risky$questions_reversed) <- paste0("Q", 1:10)

# Calculate polychoric correlation matrices
exp_partial_feedback$factors$ears_risky$polychoric <- exp_partial_feedback$factors$ears_risky$questions_reversed %>%
  polychoric()

# Correlation plot
exp_partial_feedback$factors$ears_risky$cor_plot <- cor.plot(exp_partial_feedback$factors$ears_risky$polychoric$rho, main = "Partial feedback (risky)")

# Check sampling adequacy using KMO test
exp_partial_feedback$factors$ears_risky$kmo <-  KMO(exp_partial_feedback$factors$ears_risky$polychoric$rho)

# Check correlation adequacy using Bartlett's test
exp_partial_feedback$factors$ears_risky$bartlett <-  cortest.bartlett(exp_partial_feedback$factors$ears_risky$polychoric$rho, n = 240)

# Cluster structure using polychoric correlations
exp_partial_feedback$factors$ears_risky$cluster <-  iclust(exp_partial_feedback$factors$ears_risky$polychoric$rho, title = "Partial feedback (risky)")

# Exploratory factor analysis
exp_partial_feedback$factors$ears_risky$efa <- fa(exp_partial_feedback$factors$ears_risky$polychoric$rho, nfactors = 2, n.obs = 240, fm = "pa")

# EFA diagram
exp_partial_feedback$factors$ears_risky$efa_diagram <- fa.diagram(exp_partial_feedback$factors$ears_risky$efa, main = "Partial feedback (risky) EFA")

#### Confirmatory factor analysis ----

exp_partial_feedback$factors$ears_risky$cfa <- cfa(
  lavaan_model,
  data = exp_partial_feedback$factors$ears_risky$questions_reversed,
  ordered = TRUE
)

# CFA summary
exp_partial_feedback$factors$ears_risky$cfa_summary <- summary(
  exp_partial_feedback$factors$ears_risky$cfa, 
  fit.measure = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# CFA diagram
exp_partial_feedback$factors$ears_risky$cfa_diagram <- semPaths(
  exp_partial_feedback$factors$ears_risky$cfa,
  what = "std", 
  whatLabels = "std",
  intercepts = FALSE,
  nCharNodes = 0,
)

# Modification indices
exp_partial_feedback$factors$ears_risky$moindices <-  modindices(
  exp_partial_feedback$factors$ears_risky$cfa, 
  sort. = TRUE, maximum.number = 5
)

#### Reliability (omega) ----

if (run_bootstrap) {
  
  # Omega for aleatory items (bias-corrected and accelerated bootstrap ci)
  exp_partial_feedback$factors$ears_risky$omega_aleatory <- ci.reliability(exp_partial_feedback$factors$ears_risky$questions_reversed[, 1:4], type = "categorical", interval.type = "bca")
  
  # Omega for epistemic items (bias-corrected and accelerated bootstrap ci)
  exp_partial_feedback$factors$ears_risky$omega_epistemic <- ci.reliability(exp_partial_feedback$factors$ears_risky$questions_reversed[, 5:10], type = "categorical", interval.type = "bca") 
  
}

### Partial feedback (safe) ----------------------------------------------------

#### Exploratory factor analysis ----

exp_partial_feedback$factors$ears_safe <- list()

# Extract responses in wide format, reverse code aleatory items
exp_partial_feedback$factors$ears_safe$questions_reversed <- exp_partial_feedback$ears %>% 
  filter(risky == "Safe") %>% 
  select(starts_with("question")) %>% 
  mutate_at(paste0("question_", 1:4), ~ 6 - .)

# Abbreviate column names
names(exp_partial_feedback$factors$ears_safe$questions_reversed) <- paste0("Q", 1:10)

# Calculate polychoric correlation matrices
exp_partial_feedback$factors$ears_safe$polychoric <- exp_partial_feedback$factors$ears_safe$questions_reversed %>%
  polychoric()

# Correlation plot
exp_partial_feedback$factors$ears_safe$cor_plot <- cor.plot(exp_partial_feedback$factors$ears_safe$polychoric$rho, main = "Partial feedback (safe)")

# Check sampling adequacy using KMO test
exp_partial_feedback$factors$ears_safe$kmo <-  KMO(exp_partial_feedback$factors$ears_safe$polychoric$rho)

# Check correlation adequacy using Bartlett's test
exp_partial_feedback$factors$ears_safe$bartlett <-  cortest.bartlett(exp_partial_feedback$factors$ears_safe$polychoric$rho, n = 240)

# Cluster structure using polychoric correlations
exp_partial_feedback$factors$ears_safe$cluster <-  iclust(exp_partial_feedback$factors$ears_safe$polychoric$rho, title = "Partial feedback (safe)")

# Exploratory factor analysis
exp_partial_feedback$factors$ears_safe$efa <-  fa(exp_partial_feedback$factors$ears_safe$polychoric$rho, nfactors = 2, n.obs = 240, fm = "pa")

# EFA diagram
exp_partial_feedback$factors$ears_safe$efa_diagram <- fa.diagram(exp_partial_feedback$factors$ears_safe$efa, main = "Partial feedback (safe) EFA")

#### Confirmatory factor analysis ----

exp_partial_feedback$factors$ears_safe$cfa <- cfa(
  lavaan_model,
  data = exp_partial_feedback$factors$ears_safe$questions_reversed,
  ordered = TRUE
)

# CFA summary
exp_partial_feedback$factors$ears_safe$cfa_summary <- summary(
  exp_partial_feedback$factors$ears_safe$cfa, 
  fit.measure = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# CFA diagram
exp_partial_feedback$factors$ears_safe$cfa_diagram <- semPaths(
  exp_partial_feedback$factors$ears_safe$cfa,
  what = "std", 
  whatLabels = "std",
  intercepts = FALSE,
  nCharNodes = 0,
)

# Modification indices
exp_partial_feedback$factors$ears_safe$moindices <-  modindices(
  exp_partial_feedback$factors$ears_safe$cfa, 
  sort. = TRUE, maximum.number = 5
)

#### Reliability (omega) ----

if (run_bootstrap) {
  
  # Omega for aleatory items (bias-corrected and accelerated bootstrap ci)
  exp_partial_feedback$factors$ears_safe$omega_aleatory <- ci.reliability(exp_partial_feedback$factors$ears_safe$questions_reversed[, 1:4], type = "categorical", interval.type = "bca")
  
  # Omega for epistemic items (bias-corrected and accelerated bootstrap ci)
  exp_partial_feedback$factors$ears_safe$omega_epistemic <- ci.reliability(exp_partial_feedback$factors$ears_safe$questions_reversed[, 5:10], type = "categorical", interval.type = "bca")
  
}

#### Save factor analysis summaries ----

save_tidy_data(
  exp_partial_feedback$factors,
  parent_folder = here("output", "factors"),
  data_name = "exp_partial_feedback",
  save_csv = FALSE
)

### Information or reward (risky) ----------------------------------------------

#### Exploratory factor analysis ----

exp_information_or_reward$factors$ears_risky <- list()

# Extract responses in wide format, reverse code aleatory items
exp_information_or_reward$factors$ears_risky$questions_reversed <- exp_information_or_reward$ears %>% 
  filter(risky == "Risky") %>% 
  select(starts_with("question")) %>% 
  mutate_at(paste0("question_", 1:4), ~ 6 - .)

# Abbreviate column names
names(exp_information_or_reward$factors$ears_risky$questions_reversed) <- paste0("Q", 1:10)

# Calculate polychoric correlation matrices
exp_information_or_reward$factors$ears_risky$polychoric <- exp_information_or_reward$factors$ears_risky$questions_reversed %>%
  polychoric()

# Correlation plot
exp_information_or_reward$factors$ears_risky$cor_plot <- cor.plot(exp_information_or_reward$factors$ears_risky$polychoric$rho, main = "Information or reward (risky)")

# Check sampling adequacy using KMO test
exp_information_or_reward$factors$ears_risky$kmo <-  KMO(exp_information_or_reward$factors$ears_risky$polychoric$rho)

# Check correlation adequacy using Bartlett's test
exp_information_or_reward$factors$ears_risky$bartlett <-  cortest.bartlett(exp_information_or_reward$factors$ears_risky$polychoric$rho, n = 137)

# Cluster structure using polychoric correlations
exp_information_or_reward$factors$ears_risky$cluster <-  iclust(exp_information_or_reward$factors$ears_risky$polychoric$rho, title = "Information or reward (risky)")

# Exploratory factor analysis
exp_information_or_reward$factors$ears_risky$efa <-  fa(exp_information_or_reward$factors$ears_risky$polychoric$rho, nfactors = 2, n.obs = 137, fm = "pa")

# EFA diagram
exp_information_or_reward$factors$ears_risky$efa_diagram <- fa.diagram(exp_information_or_reward$factors$ears_risky$efa, main = "Information or reward (risky) EFA")

#### Confirmatory factor analysis ----

# Confirmatory factor analysis
exp_information_or_reward$factors$ears_risky$cfa <- cfa(
  lavaan_model,
  data = exp_information_or_reward$factors$ears_risky$questions_reversed,
  ordered = TRUE
)

# CFA summary
exp_information_or_reward$factors$ears_risky$cfa_summary <- summary(
  exp_information_or_reward$factors$ears_risky$cfa, 
  fit.measure = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# CFA diagram
exp_information_or_reward$factors$ears_risky$cfa_diagram <- semPaths(
  exp_information_or_reward$factors$ears_risky$cfa,
  what = "std", 
  whatLabels = "std",
  intercepts = FALSE,
  nCharNodes = 0
)

# Modification indices
exp_information_or_reward$factors$ears_risky$moindices <-  modindices(
  exp_information_or_reward$factors$ears_risky$cfa, 
  sort. = TRUE, maximum.number = 5
)

#### Reliability (omega) ----

if (run_bootstrap) {
  
  # Omega for aleatory items (bias-corrected and accelerated bootstrap ci)
  exp_information_or_reward$factors$ears_risky$omega_aleatory <- ci.reliability(exp_information_or_reward$factors$ears_risky$questions_reversed[, 1:4], type = "categorical", interval.type = "bca")
  
  # Omega for epistemic items (bias-corrected and accelerated bootstrap ci)
  exp_information_or_reward$factors$ears_risky$omega_epistemic <- ci.reliability(exp_information_or_reward$factors$ears_risky$questions_reversed[, 5:10], type = "categorical", interval.type = "bca")
  
}

### Information or reward (safe) -----------------------------------------------

#### Exploratory factor analysis ----

exp_information_or_reward$factors$ears_safe <- list()

# Extract responses in wide format, reverse code aleatory items
exp_information_or_reward$factors$ears_safe$questions_reversed <- exp_information_or_reward$ears %>% 
  filter(risky == "Safe") %>% 
  select(starts_with("question")) %>% 
  mutate_at(paste0("question_", 1:4), ~ 6 - .) 

# Abbreviate column names
names(exp_information_or_reward$factors$ears_safe$questions_reversed) <- paste0("Q", 1:10)

# Calculate polychoric correlation matrices
exp_information_or_reward$factors$ears_safe$polychoric <- exp_information_or_reward$factors$ears_safe$questions_reversed %>%
  polychoric()

# Correlation plot
exp_information_or_reward$factors$ears_safe$cor_plot <- cor.plot(exp_information_or_reward$factors$ears_safe$polychoric$rho, main = "Information or reward (safe)")

# Check sampling adequacy using KMO test
exp_information_or_reward$factors$ears_safe$kmo <-  KMO(exp_information_or_reward$factors$ears_safe$polychoric$rho)

# Check correlation adequacy using Bartlett's test
exp_information_or_reward$factors$ears_safe$bartlett <-  cortest.bartlett(exp_information_or_reward$factors$ears_safe$polychoric$rho, n = 137)

# Cluster structure using polychoric correlations
exp_information_or_reward$factors$ears_safe$cluster <-  iclust(exp_information_or_reward$factors$ears_safe$polychoric$rho, title = "Information or reward (safe)")

# Exploratory factor analysis
exp_information_or_reward$factors$ears_safe$efa <-  fa(exp_information_or_reward$factors$ears_safe$polychoric$rho, nfactors = 2, n.obs = 137, fm = "pa")

# EFA diagram
exp_information_or_reward$factors$ears_safe$efa_diagram <- fa.diagram(exp_information_or_reward$factors$ears_safe$efa, main = "Information or reward (safe) EFA")

#### Confirmatory factor analysis ----

exp_information_or_reward$factors$ears_safe$cfa <- cfa(
  lavaan_model,
  data = exp_information_or_reward$factors$ears_safe$questions_reversed,
  ordered = TRUE
)

# CFA summary
exp_information_or_reward$factors$ears_safe$cfa_summary <- summary(
  exp_information_or_reward$factors$ears_safe$cfa, 
  fit.measure = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# CFA diagram
exp_information_or_reward$factors$ears_safe$cfa_diagram <- semPaths(
  exp_information_or_reward$factors$ears_safe$cfa,
  what = "std", 
  whatLabels = "std",
  intercepts = FALSE,
  nCharNodes = 0
)

# Modification indices
exp_information_or_reward$factors$ears_safe$moindices <-  modindices(
  exp_information_or_reward$factors$ears_safe$cfa, 
  sort. = TRUE, maximum.number = 5
)

#### Reliability (omega) ----

if (run_bootstrap) {
  
  # Omega for aleatory items (bias-corrected and accelerated bootstrap ci)
  exp_information_or_reward$factors$ears_safe$omega_aleatory <- ci.reliability(exp_information_or_reward$factors$ears_safe$questions_reversed[, 1:4], type = "categorical", interval.type = "bca")
  
  # Omega for epistemic items (bias-corrected and accelerated bootstrap ci)
  exp_information_or_reward$factors$ears_safe$omega_epistemic <- ci.reliability(exp_information_or_reward$factors$ears_safe$questions_reversed[, 5:10], type = "categorical", interval.type = "bca")
  
}

#### Save factor analysis summaries ----

save_tidy_data(
  exp_information_or_reward$factors,
  parent_folder = here("output", "factors"),
  data_name = "exp_information_or_reward",
  save_csv = FALSE
)

### Free sampling ----

exp_free_sampling$factors$ears <- list()

# Extract responses in wide format, reverse code aleatory items
exp_free_sampling$factors$ears$questions_reversed <- exp_free_sampling$ears %>%
  select(starts_with("question")) %>% 
  mutate_at(paste0("question_", c(1, 3)), ~ 6 - .)

# Abbreviate column names
names(exp_free_sampling$factors$ears$questions_reversed) <- paste0("Q", c(1, 3, 7, 8))

# Calculate polychoric correlation matrices
exp_free_sampling$factors$ears$polychoric <- exp_free_sampling$factors$ears$questions_reversed %>%
  polychoric()

# Correlation plot
exp_free_sampling$factors$ears$cor_plot <- cor.plot(exp_free_sampling$factors$ears$polychoric$rho, main = "Free sampling (risky)")

if (run_bootstrap) {
  
  # Omega for aleatory items (bias-corrected and accelerated bootstrap ci)
  exp_free_sampling$factors$ears$omega_aleatory <- ci.reliability(exp_free_sampling$factors$ears$questions_reversed[, 1:2], type = "categorical", interval.type = "bca")
  
  # Omega for epistemic items (bias-corrected   and accelerated bootstrap ci)
  exp_free_sampling$factors$ears$omega_epistemic <- ci.reliability(exp_free_sampling$factors$ears$questions_reversed[, 3:4], type = "categorical", interval.type = "bca")
  
}

#### Save factor analysis summaries ----

save_tidy_data(
  exp_free_sampling$factors,
  parent_folder = here("output", "factors"),
  data_name = "exp_free_sampling",
  save_csv = FALSE
)
