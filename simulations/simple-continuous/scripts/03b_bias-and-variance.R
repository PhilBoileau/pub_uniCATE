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
library(ggpubr)
source(here("simple-continuous", "R", "compute-bias-and-variance.R"))

# load the results tables
unicate_p100_res <- read_rds(
  here("simple-continuous", "results", "p100_unicate-results.Rds")
) %>%
  filter(dgp != "linear_sym_p100")
unicate_p500_res <- read_rds(
  here("simple-continuous", "results", "p500_unicate-results.Rds")
) %>%
  filter(dgp != "linear_sym_p500")

# generate the plots
# kinked DGPs
kinked_p100 <- bias_and_var_plot(
  unicate_p100_res, "kinked_p100", 100, 5,
  "Kinked Data Generating Process: Non-Sparse, Moderate Dimensions"
)
kinked_p500 <- bias_and_var_plot(
  unicate_p500_res, "kinked_p500", 500, 5,
  "Kinked Data Generating Process: Sparse, High Dimensions"
)

# linear DGPs
linear_p100 <- bias_and_var_plot(
  unicate_p100_res, "linear_asym_ln_main_p100", 100, 5,
  "Linear Data Generating Process: Non-Sparse, Moderate Dimensions"
)
linear_p500 <- bias_and_var_plot(
  unicate_p500_res, "linear_asym_ln_main_p500", 500, 5,
  "Linear Data Generating Process: Sparse, High Dimensions"
)

# non-linear DGPs
nonlinear_p100 <- bias_and_var_plot(
  unicate_p100_res, "linear_asym_nl_main_p100", 100, 5,
  "Non-Linear Data Generating Process: Non-Sparse, Moderate Dimensions"
)
nonlinear_p500 <- bias_and_var_plot(
  unicate_p500_res, "linear_asym_nl_main_p500", 500, 5,
  "Non-Linear Data Generating Process: Sparse, High Dimensions"
)

# save the plots
ggsave(
  plot = kinked_p100,
  filename = "bias-variance-plot_kinked-p100.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = kinked_p500,
  filename = "bias-variance-plots_kinked-p500.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = linear_p100,
  filename = "bias-variance-plots_linear-p100.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = linear_p500,
  filename = "bias-variance-plots_linear-p500.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = nonlinear_p100,
  filename = "bias-variance-plots_nonlinear-p100.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)
ggsave(
  plot = nonlinear_p500,
  filename = "bias-variance-plots_nonlinear-p500.jpeg",
  path = here("simple-continuous", "results"),
  width = 10,
  height = 4,
  dpi = "retina",
  scale = 1.3
)

# create combined plots for the manuscript
kinked_combo_p <- ggarrange(
  kinked_p100, kinked_p500,
  nrow = 2, common.legend = TRUE, legend = "right", labels = "AUTO"
)
ggsave(
  plot = kinked_combo_p,
  filename = "bias-variance-plots_kinked.png",
  path = here("simple-continuous", "results"),
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
  path = here("simple-continuous", "results"),
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
  path = here("simple-continuous", "results"),
  width = 10,
  height = 8,
  dpi = "retina",
  scale = 1.3
)
