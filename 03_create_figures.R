# Experiments ------------------------------------------------------------------

# 7a. `exp_partial_feedback_past`: Examined the influence of stimulus variability on choices with the EARS referring to past choices
# 7b. `exp_partial_feedback_future`: Examined the influence of stimulus variability on choices with the EARS referring to a specific future choice
# 8. `exp_information_or_reward`: Examined the influence of stimulus variability on whether participants chose to observe or claim the outcome of their choices
# 9. exp_free_sampling: Examined the influence of stimulus variability the number of outcomes participants chose to observe before making a consequential choice

# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(ggridges)
library(ggstance)
library(scales)
library(cowplot)
library(ggtext)
library(ggh4x)
library(rlang)
library(here)

here::here("src", "utilities", "load_tidy_data.R") %>% source()
here::here("src", "figures", "save_figures.R") %>% source()

save_figures_to_file <- TRUE

## Load data ----

data_names <-  c(
  "exp_partial_feedback", 
  "exp_information_or_reward",
  "exp_free_sampling"
)

load_tidy_data(data_names)

# Functions used to create figures ---------------------------------------------

## Function to make density plots ----

create_density_plot <- function(data, mapping, title, x_axis_title, shared_theme, vline = TRUE, x_breaks = waiver(), x_labels = waiver(), x_limits, facet_rows = NULL, facet_cols = NULL) {
  
  figure <- ggplot(data, mapping)
  
  # Add vertical line at indifference
  if (vline) {figure <- figure + geom_vline(xintercept = mean(x_limits), colour = "grey80")}
  
  figure <- figure +
    # Add density plots
    geom_density_ridges(
      # Make sure ridges don't overlap
      scale = 0.5,
      panel_scaling = FALSE,
      # Remove long tails
      rel_min_height = 0.01,
      # Add rug plot
      jittered_points = TRUE,
      position = position_points_jitter(width = 0.05, height = 0),
      point_shape = '|', point_size = 1, point_alpha = 0.5
    ) +
    # Make sure the jittered rug plot doesn't plot impossible values
    scale_x_continuous(
      limits = x_limits, oob = squish,
      breaks = x_breaks,
      labels = x_labels
    ) +
    # Add white circle for the median
    stat_summary(fun = median, geom = "point", fill = "white", shape = 21, size = 1) +
    #Split by facet variables
    facet_grid(
      rows = facet_rows, 
      cols = facet_cols,
      scales = "free_y",
      space = "free_y"
    ) +
    # Title and labels
    labs(title = title, x = x_axis_title, y = NULL) +
    scale_fill_manual(values = c(`Constant images` = "#D55E00", `Unique images` = "#0072B2")) +
    shared_theme
  
  return(figure)
}

## Function to make bar charts ----

create_bar_chart <- function(data, mapping, title, x_axis_title, shared_theme, facet_rows = NULL, facet_cols = NULL) {
  figure <- ggplot(data, mapping) + # mapping <- aes(x = x_variable, y = y_variable, fill = fill)
    # Add vertical line at indifference
    geom_vline(xintercept = 0.5, colour = "grey80") +
    # Add bar plots
    geom_barh(stat = "count", position = position_fill(reverse = TRUE), colour = "black", linewidth = 0.5) +
    scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("0", ".25", ".5", ".75", "1")
    ) +
    facet_grid(
      rows = facet_rows, 
      cols = facet_cols,
      scales = "free_y", 
      space = "free_y"
    ) +
    # Title and labels
    labs(
      title = title, 
      x = x_axis_title,
      y = NULL, 
      fill = NULL
    ) +
    scale_fill_identity() +
    shared_theme
  
  return(figure)
}

## Function to make line graphs ----

create_line_graph <- function(data, mapping, title, x_axis_title, y_axis_title, shared_theme, x_breaks = waiver(), x_labels = waiver(), x_limits, y_breaks = waiver(), y_labels = waiver(), y_limits, facet_rows = NULL, facet_cols = NULL) {
  
  figure <- ggplot(data, mapping) +
    # Add vertical and horizontal lines at indifference
    geom_vline(xintercept = mean(x_limits), colour = "grey80") +
    geom_hline(yintercept = mean(y_limits), colour = "grey80") +
    # Add points
    geom_jitter() +
    # Add line
    geom_smooth(method = "lm") +
    # Make sure the jittered points don't plot impossible values
    scale_x_continuous(
      limits = x_limits, oob = squish,
      breaks = x_breaks,
      labels = x_labels
    ) +
    scale_y_continuous(
      limits = y_limits, oob = squish,
      breaks = y_breaks,
      labels = y_labels
    ) +
    #Split by facet variables
    facet_grid(
      rows = facet_rows, 
      cols = facet_cols,
      scales = "free_y",
      space = "free_y"
    ) +
    # Title and labels
    labs(title = title, x = x_axis_title, y = y_axis_title) +
    scale_colour_manual(values = c(`Constant images` = "#D55E00", `Unique images` = "#0072B2")) +
    shared_theme
  
  return(figure)
}

## Function to write captions ----

write_subfigure_phrase <- function(letter, title, description) {
  
  # If letter argument is blank do not include brackets
  letter_phrase <- ifelse(letter == "", "", paste0(letter, ") "))
  
  subfigure_phrase <- paste0(
    "<b>", letter_phrase, title, "</b><br><span style = 'font-size:6pt'>", description, 
    " in the <span style = 'color:#0072B2;'>Unique images</span> and <span style = 'color:#D55E00;'>Constant images</span> conditions.</span>"
  )
  
  return(subfigure_phrase)
}

## Shared theme elements ----

# Shared theme elements assigned to each figure
shared_theme <- theme_bw() +
  theme(
    # Remove legend and grid
    legend.position = 'none',
    panel.grid = element_blank(),
    # Change facet colour
    strip.background = element_rect(fill = "#e8e8e8"),
    # Change font sizes
    axis.text = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 6),
    strip.text = element_text(size = 6),
    panel.spacing = unit(4, "pt"),
    plot.margin = margin(2, 2, 2, 2),
    # Edit title appearance with ggtext
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 8,
      lineheight = 1,
      padding = margin(2, 4, 2, 4),
      margin = margin(0, 0, 4, 0),
      fill = "#e8e8e8"
    )
  )

proportion_labels <- c("0", ".25", ".5", ".75", "1")
ears_labels <- c("1 \n           Not at all", "2", "3", "4", "5", "6", "7 \n Very much            ")
ellsberg_labels <- c("0 \n                 Ambiguity seeking", "25", "50 \n Indifference", "75", "100 \n Ambiguity aversion                  ")

# Create the figures for each experiment ---------------------------------------

## 7. Partial-feedback ---------------------------------------------------------

exp_partial_feedback$figures <- list()

# EARS questionnaire (risky)
exp_partial_feedback$figures$ears_risky <- create_density_plot(
  exp_partial_feedback$ears_summary %>% filter(risky == "Risky"), 
  mapping = aes(x = mean_response, y = condition, fill = condition), 
  title = write_subfigure_phrase("A", "Epistemic and aleatory rating scale (risky option)", "Average item ratings on the aleatory and epistemic subscales for the risky option"), 
  x_axis_title = "Average item rating", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(1, 7), 
  x_breaks = 1:7,
  x_labels = ears_labels, 
  facet_rows = vars(experiment),
  facet_cols = vars(uncertainty_type)
)

# EARS questionnaire (safe)
exp_partial_feedback$figures$ears_safe <- create_density_plot(
  exp_partial_feedback$ears_summary %>% filter(risky == "Safe"), 
  mapping = aes(x = mean_response, y = condition, fill = condition), 
  title = write_subfigure_phrase("B", "Epistemic and aleatory rating scale (safe option)", "Average item ratings on the aleatory and epistemic subscales for the safe option"), 
  x_axis_title = "Average item rating", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(1, 7), 
  x_breaks = 1:7,
  x_labels = ears_labels,
  facet_rows = vars(experiment), 
  facet_cols = vars(uncertainty_type)
)

# Choices (condition)
exp_partial_feedback$figures$choices <- create_density_plot(
  exp_partial_feedback$choices_summary, 
  mapping = aes( x = prop_risky, y = condition, fill = condition), 
  title = write_subfigure_phrase("C", "Choices", "The proportion of choices for the risky option"), 
  x_axis_title = "Proportion of choices for the risky option", 
  shared_theme = shared_theme,
  x_labels = proportion_labels, 
  x_limits = c(0, 1), 
  facet_cols = vars(experiment)
)

# Combine into multi-plot figure
exp_partial_feedback$figures$combined = plot_grid(
  exp_partial_feedback$figures$ears_risky, 
  exp_partial_feedback$figures$ears_safe, 
  exp_partial_feedback$figures$choices,
  nrow = 3, align = "h", axis = "lr"
)

### Additional figures for dynamic Ellsberg task ----

exp_partial_feedback$figures$ellsberg_choices <- create_line_graph(
  exp_partial_feedback$ellsberg_choices,
  mapping = aes(x = ellsberg_score, y = prop_risky, colour = condition), 
  title = write_subfigure_phrase("D", "Dynamic Ellsberg task", "Final scores for the dynamic Ellsberg task"), 
  x_axis_title = "Ambiguity preference score", 
  x_limits = c(0, 100),
  x_labels = ellsberg_labels, 
  y_axis_title = "Proporion of choices for the risky option", 
  y_labels = proportion_labels, 
  y_limits = c(0, 1),
  shared_theme = shared_theme
)

exp_partial_feedback$figures$ellsberg <- create_density_plot(
  exp_partial_feedback$ellsberg, 
  mapping = aes(x = ellsberg_score , y = condition, fill = condition), 
  title = write_subfigure_phrase("", "Dynamic Ellsberg task", "Final scores for the dynamic Ellsberg task"), 
  x_axis_title = "Ambiguity preference score", 
  x_limits = c(0, 100),
  x_labels = ellsberg_labels, 
  shared_theme = shared_theme
)

## 8. Information or reward ----------------------------------------------------

exp_information_or_reward$figures <- list()

# EARS questionnaire (risky)
exp_information_or_reward$figures$ears_risky <- create_density_plot(
  exp_information_or_reward$ears_summary %>% filter(risky == "Risky"), 
  mapping = aes(x = mean_response, y = condition, fill = condition), 
  title = write_subfigure_phrase("A", "Epistemic and aleatory rating scale (risky option)", "Average item ratings on the aleatory and epistemic subscales for the risky option"), 
  x_axis_title = "Average item rating", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(1, 7), 
  x_breaks = 1:7,
  x_labels = ears_labels,
  facet_cols = vars(uncertainty_type)
)

# EARS questionnaire (safe)
exp_information_or_reward$figures$ears_safe <- create_density_plot(
  exp_information_or_reward$ears_summary %>% filter(risky == "Safe"), 
  mapping = aes(x = mean_response, y = condition, fill = condition), 
  title = write_subfigure_phrase("B", "Epistemic and aleatory rating scale (safe option)", "Average item ratings on the aleatory and epistemic subscales for the safe option"), 
  x_axis_title = "Average item rating", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(1, 7), 
  x_breaks = 1:7,
  x_labels = ears_labels,
  facet_cols = vars(uncertainty_type)
)

# Sampling (condition)
exp_information_or_reward$figures$observe_or_claim = create_density_plot(
  exp_information_or_reward$observe_or_claim, 
  mapping = aes( x = prop_observe, y = condition, fill = condition), 
  title = write_subfigure_phrase("C", "Choices (observe or claim)", "The proportion of choices for observing the outcome"), 
  x_axis_title = "Proportion of choices for observe", 
  shared_theme = shared_theme,
  x_labels = proportion_labels, 
  x_limits = c(0, 1),
  facet_rows = vars(risky)
)

# Choice (condition)
exp_information_or_reward$figures$safe_or_risky = create_density_plot(
  exp_information_or_reward$safe_or_risky %>% filter(claim == "Claim"), 
  mapping = aes( x = prop_risky, y = condition, fill = condition), 
  title = write_subfigure_phrase("D", "Choices (safe or risky)", "The proportion of choices for the risky option"), 
  x_axis_title = "Proportion of choices for the risky option", 
  shared_theme = shared_theme,
  x_labels = proportion_labels, 
  x_limits = c(0, 1)
)

# Combine into multi-plot figure
exp_information_or_reward$figures$combined = plot_grid(
  exp_information_or_reward$figures$ears_risky,
  exp_information_or_reward$figures$ears_safe,
  exp_information_or_reward$figures$observe_or_claim, 
  exp_information_or_reward$figures$safe_or_risky,
  nrow = 4, align = "h", axis = "lr"
)

## 9. Free sampling ------------------------------------------------------------

exp_free_sampling$figures <- list()

# EARS questionnaire
exp_free_sampling$figures$ears <- create_density_plot(
  exp_free_sampling$ears_summary, 
  mapping = aes(x = mean_response, y = condition, fill = condition), 
  title = write_subfigure_phrase("A", "Epistemic and aleatory rating scale", "Average item ratings for the aleatory and epistemic subscales"), 
  x_axis_title = "Average item rating", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(1, 7), 
  x_breaks = 1:7,
  x_labels = ears_labels, 
  facet_cols = vars(uncertainty_type)
)

# Sampling
exp_free_sampling$figures$sampling  <- create_density_plot(
  exp_free_sampling$sampling_summary %>% filter(sampling_type == "Free"), 
  mapping = aes( x = n_samples, y = condition, fill = condition), 
  title = write_subfigure_phrase("B", "Sampling", "The number of samples taken in each free-sampling block"), 
  x_axis_title = "Number of samples", 
  shared_theme = shared_theme,
  vline = FALSE,
  x_limits = c(0, 80)
)

# Choices
exp_free_sampling$figures$choices = create_bar_chart(
  exp_free_sampling$choices %>% mutate(
    fill = case_when(
      condition == "Constant images" & choice == "Risky" ~ "#D55E00", 
      condition == "Unique images" & choice == "Risky" ~ "#0072B2",
      TRUE ~ "#e8e8e8"
    )
  ),
  mapping = aes(y = condition, fill = fill, group = fill), 
  title = write_subfigure_phrase("C", "Choices", "The proportion of participants that selected the risky option"), 
  x_axis_title = "Proportion of participants that selected the risky option",
  shared_theme = shared_theme,
  facet_rows = vars(comparison),
  facet_cols = vars(sampling_type)
)

# Combine into multi-plot figure
exp_free_sampling$figures$combined = plot_grid(
  exp_free_sampling$figures$ears, 
  exp_free_sampling$figures$sampling, 
  exp_free_sampling$figures$choices, 
  nrow = 3, align = "h", axis = "lr"
)

# Save figures -----------------------------------------------------------------

# Save the figures as .rds files and as PDFs

if (save_figures_to_file) {
  
  save_figures(
    exp_partial_feedback$figures$combined, 
    width = 80, height = 200, 
    data_name = "exp_partial_feedback"
  )
  
  save_figures(
    exp_information_or_reward$figures$combined, 
    width = 80, height = 200, 
    data_name = "exp_information_or_reward"
  )
  
  save_figures(
    exp_free_sampling$figures$combined, 
    width = 80, height = 200, 
    data_name = "exp_free_sampling"
  )
  
}
