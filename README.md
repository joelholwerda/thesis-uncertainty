# PhD thesis analysis code (Chapter 6)

In this repository, you can find the analysis code for the experiments in Chapter 6 that examined epistemic and aleatory uncertainty.

## Prerequisites

- To run the analysis code you will need [R](https://cran.rstudio.com/) and [RStudio](https://posit.co/download/rstudio-desktop/) installed on your computer
- You will also need to install [CmdStanR](https://mc-stan.org/cmdstanr/) by following the instructions [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)

## Getting started

The easiest way to access the analysis code is downloading the `.zip` file. To do this: 

1. Click the "Code" button and then click the "Download ZIP" option
2. This will download a `.zip` file containing the code to your computer
3. Extract the contents of the `.zip` file to a directory of your choice

Alternatively, you can open the Terminal or Command Prompt, navigate to the folder where you want to clone this repository, and run the following command: `git clone https://github.com/joelholwerda/thesis-uncertainty.git`.

## Running the analysis

1. Open the `00_open_project.Rproj` file. This opens a new session in RStudio and sets the working directory to the correct location
2. Open the `01_wrangle_data.R` file. This wrangles the raw data and outputs `.csv` files loaded in the subsequent models and figures. 
3. Click the "Source" button to run the entire script or highlight sections and press Cmd + Enter
4. Open and run the `02_model_data.R` file. This uses [brms](https://paul-buerkner.github.io/brms/) to fit the Bayesian models and test the reported hypotheses. Some of these models take several minutes to run. See the "Options" section below for ways to speed up this process 
5. Open and run the `03_create_figures.R` file. This creates the figures and saves them as `.pdf` files in the `output/figures` folder
6. Open and run the `04_alternate_models.R` file. This includes numerous other models that could have been used to analyse our results and examines whether our conclusions were contingent on the reported models

RStudio might prompt you to install missing packages. Alternatively, you can run the following code: `install.packages("tidyverse", "lubridate", "forcats", "R.matlab", "jsonlite", "standardize", "rlang", "here", "parallel", "brms", "bayestestR", "tidybayes", "bayesplot", "ggridges", "ggstance", "scales", "cowplot", "ggtext", "ggh4x")`

## Options

You can change the following options in the `02_model_data.R` file:

- Setting `options(quick_version = TRUE)` allows faster but less precise parameter estimation by reducing the number of samples taken in the `brms` models. Set to `FALSE` to reproduce the reported values
- The fitted models are cached in `output/fitted_models`. Setting `options(overwrite_saved_models = TRUE)` ensures that the models are run every time instead of loading a cached version. In order to save time, this should be set to `FALSE` unless changes have been made to the model
- Set `run_diagnostics` to `TRUE` to create additional diagnostic plots for the `brms` models (e.g., rank histograms, posterior predictive checks). Even if `FALSE`, the Stan warnings (e.g., divergences, rhat) will still be displayed if applicable
- The number of cores used for the `brms` models will be the number of available cores minus the value of the `reserved_cores` variable (or one if the number of reserved cores is greater than or equal to the number of available cores)
- K-fold cross-validation was used in `exp_partial_sampling` to assess the contribution of the outcome-sequence manipulation and requires fitting each of the three models 10 times. This analysis was included in Appendix E. Set `run_kfold` to `FALSE` to skip this analysis
- Bootstrap confidence intervals were generated for each omega estimate. This analysis was included in Appendix B and can take a couple of hours to run. Set `run_bootstrap` to `FALSE` to skip this analysis.

## Other files

- The `src` folder contains various functions used to run the analysis

- Information about each experiment is stored in the `src/wrangle/exp_info` folder. This is used to import the raw data using `import_data.R` and perform initial wrangling using `wrangle_all.R`

## Get help

If you have trouble running the code in this repository or have questions, contact me at joeldavidholwerda@gmail.com.

