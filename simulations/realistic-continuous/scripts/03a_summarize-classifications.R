################################################################################
# 03a. Summarize Treatment-Biomarker Classifications
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

# summarize classifications
classification_tbl <- results_tbl %>%
  compute_classification_results(simple = FALSE)

# save the classification table
write_rds(
  classification_tbl,
  file = here("realistic-continuous", "results", "classification-results.Rds")
)

# plot the results
results_plot <- classification_tbl %>%
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

# save the individual plots
ggsave(
  plot = results_plot,
  filename = "high-dim-classification-plot.jpeg",
  path = here("realistic-continuous", "results"),
  dpi = "retina",
  width = 9,
  height = 5,
  scale = 1.5
)
