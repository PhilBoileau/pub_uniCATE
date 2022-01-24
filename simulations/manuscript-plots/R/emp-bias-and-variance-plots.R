################################################################################
# Empirical Bias and Variance Plots
################################################################################

# load the required libraries and functions
library(here)
library(readr)
library(purrr)
library(dplyr)
library(matrixStats)
library(tibble)
library(tidyr)
library(ggplot2)
library(ggpubr)

# Moderate dimensions simulation plots #########################################

source(here("simple-continuous", "R", "compute-bias-and-variance.R"))

# load the results tables
unicate_p100_res <- read_rds(
  here("simple-continuous", "results", "p100_unicate-results.Rds")
) %>%
  filter(dgp != "linear_sym_p100")

# generate the plots
kinked_p100 <- bias_and_var_plot(
  unicate_p100_res, "kinked_p100", 100, 5,
  "Kinked Data Generating Process: Non-Sparse, Moderate Dimensions, and Uncorrelated Biomakers"
)
linear_p100 <- bias_and_var_plot(
  unicate_p100_res, "linear_asym_ln_main_p100", 100, 5,
  "Linear Data Generating Process: Non-Sparse, Moderate Dimensions, and Uncorrelated Biomarkers"
)
nonlinear_p100 <- bias_and_var_plot(
  unicate_p100_res, "linear_asym_nl_main_p100", 100, 5,
  "Nonlinear Data Generating Process: Non-Sparse, Moderate Dimensions, and Uncorrelated Biomarkers"
)

# High dimensions simulation plots #############################################
source(here("realistic-continuous", "R", "compute-bias-and-variance.R"))

# load the results tables
unicate_res <- read_rds(
  here("realistic-continuous", "results", "unicate-results.Rds")
)

# load the population parameter estimates
pop_params <- read_rds(
  here("realistic-continuous", "data", "population-data",
       "pop-univariate-cate-params.Rds")
)

# generate the plots
kinked_p500 <- bias_and_var_plot(
  unicate_res, "kinked", pop_params$pop_params_tbl$kinked,
  "Kinked Data Generating Process: Sparse, High Dimensions, and Correlated Biomarkers"
)
linear_p500 <- bias_and_var_plot(
  unicate_res, "linear_asym_ln_main",
  pop_params$pop_params_tbl$linear_asym_ln_main,
  "Linear Data Generating Process: Sparse, High Dimensions, and Correlated Biomarkers"
)
nonlinear_p500 <- bias_and_var_plot(
  unicate_res, "linear_asym_nl_main",
  pop_params$pop_params_tbl$linear_asym_nl_main,
  "Nonlinear Data Generating Process: Sparse, High Dimensions, and Correlated Biomarkers"
)

# Combine the plots by conditional outcome type
kinked_combo_p <- ggarrange(
  kinked_p100, kinked_p500,
  nrow = 2, common.legend = TRUE, legend = "right", labels = "AUTO"
)
ggsave(
  plot = kinked_combo_p,
  filename = "bias-variance-plots_kinked.png",
  path = here("manuscript-plots", "plots"),
  width = 10,
  height = 8,
  dpi = "retina",
  scale = 1.3
)

linear_combo_p <- ggarrange(
  linear_p100, linear_p500,
  nrow = 2, common.legend = TRUE, legend = "right", labels = "AUTO"
)
ggsave(
  plot = linear_combo_p,
  filename = "bias-variance-plots_linear.png",
  path = here("manuscript-plots", "plots"),
  width = 10,
  height = 8,
  dpi = "retina",
  scale = 1.3
)

nonlinear_combo_p <- ggarrange(
  nonlinear_p100, nonlinear_p500,
  nrow = 2, common.legend = TRUE, legend = "right", labels = "AUTO"
)
ggsave(
  plot = nonlinear_combo_p,
  filename = "bias-variance-plots_nonlinear.png",
  path = here("manuscript-plots", "plots"),
  width = 10,
  height = 8,
  dpi = "retina",
  scale = 1.3
)

