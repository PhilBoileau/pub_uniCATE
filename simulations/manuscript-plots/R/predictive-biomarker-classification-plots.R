################################################################################
# Predictive Biomarker Classification Plots
################################################################################

# load the required libraries and functions
library(here)
library(readr)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(ggpubr)
source(here("shared-r", "compute-classification-results.R"))

# Moderate-Dimensional Simulations with Simple Correlation Structure ###########
# load the results tables
unicate_p100 <- read_rds(
  here("simple-continuous", "results", "p100_unicate-results.Rds")
) %>%
  mutate(
    results = map(
      results,
      function(x){
        x %>%
          mutate(biomarker = factor(biomarker, levels = paste0("W", seq_len(100)))) %>%
          arrange(biomarker) %>%
          pull(p_value_bh) %>%
          as.vector
      }
    )
  )
unicate_p500 <- read_rds(
  here("simple-continuous", "results", "p500_unicate-results.Rds")
) %>%
  mutate(
    results = map(
      results,
      function(x){
        x %>%
          mutate(biomarker = factor(biomarker, levels = paste0("W", seq_len(500)))) %>%
          arrange(biomarker) %>%
          pull(p_value_bh) %>%
          as.vector
      }
    )
  )
mod_cov_p100 <- read_rds(
  here("simple-continuous", "results", "p100_mod-cov-method.Rds")
)
mod_cov_p500 <- read_rds(
  here("simple-continuous", "results", "p500_mod-cov-method.Rds")
)
aug_mod_cov_p100 <- read_rds(
  here("simple-continuous", "results", "p100_aug-mod-cov-method.Rds")
)
aug_mod_cov_p500 <- read_rds(
  here("simple-continuous", "results", "p500_aug-mod-cov-method.Rds")
)

# combine into a single table, and remove the uninteresting dgp
results_tbl <- bind_rows(
  unicate_p100, unicate_p500,
  mod_cov_p100, mod_cov_p500,
  aug_mod_cov_p100, aug_mod_cov_p500
) %>%
  filter(!(dgp %in% c("linear_sym_p100", "linear_sym_p500")))

# clearnup
rm(
  unicate_p100, unicate_p500,
  mod_cov_p100, mod_cov_p500,
  aug_mod_cov_p100, aug_mod_cov_p500
)
gc()

# summarize classifications
md_classification_tbl <- results_tbl %>% compute_classification_results()

# High-Dimensional Simulations with Correlation Structure ######################

# load the results tables
unicate_res <- read_rds(
  here("realistic-continuous", "results", "unicate-results.Rds")
) %>%
  mutate(
    results = map(
      results,
      function(x){
        x %>%
          mutate(
            biomarker = factor(biomarker, levels = paste0("W", seq_len(500)))
          ) %>%
          arrange(biomarker) %>%
          pull(p_value_bh) %>%
          as.vector
      }
    )
  )
mod_cov_res <- read_rds(
  here("realistic-continuous", "results", "mod-cov-method.Rds")
)
aug_mod_cov_res <- read_rds(
  here("realistic-continuous", "results", "aug-mod-cov-method.Rds")
)

# combine into a single table, and remove the uninteresting dgp
results_tbl <- bind_rows(unicate_res, mod_cov_res, aug_mod_cov_res)

# clearnup
rm(unicate_res, mod_cov_res, aug_mod_cov_res)
gc()

# summarize classifications
hd_classification_tbl <- results_tbl %>%
  compute_classification_results(simple = FALSE)

# Create Plot for Manusrript ###################################################

# plot the moderate dim results
md_results_plot <- md_classification_tbl %>%
  select(-sim_idx) %>%
  filter(p == 100) %>%
  mutate(size = factor(size, levels = c("125", "250", "500"))) %>%
  group_by(dgp, size, method) %>%
  summarize_all(mean) %>%
  pivot_longer(
    cols = c(tnr, tpr, fdp), names_to = "metric", values_to = "value"
  ) %>%
  ungroup() %>%
  mutate(
    metric = if_else(metric == "fdp", "FDR",
                     if_else(metric == "tpr", "TPR", "TNR")),
    dgp = if_else(dgp == "kinked_p100", "Kinked",
                  if_else(dgp == "linear_asym_ln_main_p100", "Linear", "Nonlinear")),
    dgp = factor(dgp, levels = c("Linear", "Kinked", "Nonlinear")),
    method = if_else(method == "unicate", "uniCATE",
                     if_else(method == "mod cov", "Mod. Cov.", "Aug. Mod. Cov.")),
    method = factor(method, levels = c("Mod. Cov.", "Aug. Mod. Cov.", "uniCATE"))
  ) %>%
  ggplot(aes(x = size, y = value, fill = method)) +
  facet_grid(rows = vars(metric), cols = vars(dgp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Sample Size") +
  ylab("Mean of 200 Replicates") +
  ggtitle(
    "Classification of Non-Sparse, Moderate-Dimensional, and Uncorrelated Predictive Biomarkers"
  ) +
  scale_fill_viridis_d(name = "Method", option = "E", end = 0.8) +
  theme_bw()

# plot the high-dim results
hd_results_plot <- hd_classification_tbl %>%
  select(-sim_idx) %>%
  mutate(size = factor(size, levels = c("125", "250", "500"))) %>%
  group_by(dgp, size, method) %>%
  summarize_all(mean) %>%
  pivot_longer(
    cols = c(tnr, tpr, fdp), names_to = "metric", values_to = "value"
  ) %>%
  ungroup() %>%
  mutate(
    metric = if_else(metric == "fdp", "FDR",
                     if_else(metric == "tpr", "TPR", "TNR")),
    dgp = if_else(dgp == "kinked", "Kinked",
                  if_else(dgp == "linear_asym_ln_main", "Linear", "Nonlinear")),
    dgp = factor(dgp, levels = c("Linear", "Kinked", "Nonlinear")),
    method = if_else(method == "unicate", "uniCATE",
                     if_else(method == "mod cov", "Mod. Cov.", "Aug. Mod. Cov.")),
    method = factor(method, levels = c("Mod. Cov.", "Aug. Mod. Cov.", "uniCATE"))
  ) %>%
  ggplot(aes(x = size, y = value, fill = method)) +
  facet_grid(rows = vars(metric), cols = vars(dgp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Sample Size") +
  ylab("Mean of 200 Replicates") +
  ggtitle(
    "Classification of Sparse, High-Dimensional, and Correlated Predictive Biomarkers"
  ) +
  scale_fill_viridis_d(name = "Method", option = "E", end = 0.8) +
  theme_bw()

# combine the plots
combo_p <- ggarrange(
  md_results_plot, hd_results_plot,
  nrow = 2, common.legend = TRUE, legend = "right", labels = "AUTO"
)

ggsave(
  plot = combo_p,
  filename = "classification-plot.eps",
  path = here("manuscript-plots", "plots"),
  dpi = "retina",
  width = 9,
  height = 8,
  scale = 1.5
)
ggsave(
  plot = combo_p,
  filename = "classification-plot.png",
  path = here("manuscript-plots", "plots"),
  dpi = "retina",
  width = 9,
  height = 8,
  scale = 1.5
)
