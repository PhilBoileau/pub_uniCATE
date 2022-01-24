################################################################################
# 03b. Evaluate Bias and Standard Errors
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
kinked <- bias_and_var_plot(
  unicate_res, "kinked", pop_params$pop_params_tbl$kinked,
  "Kinked Data Generating Process: Sparse, High-Dimensiomal, and Correlated Biomarkers"
)
linear <- bias_and_var_plot(
  unicate_res, "linear_asym_ln_main",
  pop_params$pop_params_tbl$linear_asym_ln_main,
  "Linear Data Generating Process: Sparse, High-Dimensiomal, and Correlated Biomarkers"
)
nonlinear <- bias_and_var_plot(
  unicate_res, "linear_asym_nl_main",
  pop_params$pop_params_tbl$linear_asym_nl_main,
  "Nonlinear Data Generating Process: Sparse, High-Dimensiomal, and Correlated Biomarkers"
)

# save the plots
ggsave(
  plot = kinked,
  filename = "bias-variance-plot_kinked.png",
  path = here("realistic-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = linear,
  filename = "bias-variance-plots_linear.png",
  path = here("realistic-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = nonlinear,
  filename = "bias-variance-plots_nonlinear.png",
  path = here("realistic-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
