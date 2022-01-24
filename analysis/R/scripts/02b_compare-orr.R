################################################################################
# 02b. Comparison of Subgroup's ORRs
################################################################################

# load the required libraries and functions
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(personalized)
library(viridis)
source(here("R/helpers/unsupervized-clustering-table.R"))
source(here("R/helpers/compute-treatment-scores.R"))

set.seed(5512385)

# load the IMmotion151 data
train_data <- readRDS(here("data/processed-IMmotion150.Rds"))
test_data <- readRDS(here("data/processed-IMmotion151.Rds"))

# load the biomarker data
mc_biomarkers <- readRDS(here("results/mod-cov-selected-interactions.Rds"))
amc_biomarkers <- readRDS(here("results/aug-mod-cov-selected-interactions.Rds"))
unicate_results <- readRDS(here("results/uniCATE-results.Rds"))
unicate_biomarkers <- unicate_results %>%
  filter(p_value_bh <= 0.05) %>%
  pull(biomarker)

# predict the treatment assignment using the various methods ###################

# Unsupervised clustering ######################################################

# Unsupervised clustering: 2 Clusters
unsupervised_unicate_k2 <- test_data %>%
  cluster_participants(unicate_biomarkers) %>%
  cluster_response_table() %>%
  group_by(cluster_assignment) %>%
  mutate(
    diff = mean_responders - lead(mean_responders),
    size = sum(num_patients),
    method = "uniCATE (K=2)",
    group_assignment = if_else(
      cluster_assignment == 2, "Atezo + Bev", "Suni"
    )
  ) %>%
  ungroup() %>%
  drop_na %>%
  select(method, group_assignment, diff, size)


# Unsupervised clustering: 3 Clusters
unsupervised_unicate_k3 <- test_data %>%
  cluster_participants(unicate_biomarkers, k = 3) %>%
  cluster_response_table() %>%
  group_by(cluster_assignment) %>%
  mutate(
    diff = mean_responders - lead(mean_responders),
    size = sum(num_patients),
    method = "uniCATE (K=3)",
    group_assignment = if_else(
      cluster_assignment == 1, "Suni", "Atezo + Bev"
    )
  ) %>%
  ungroup() %>%
  drop_na %>%
  select(method, group_assignment, diff, size)

# Unsupervised clustering: 4 Clusters
unsupervised_unicate_k4 <- test_data %>%
  cluster_participants(unicate_biomarkers, k = 4) %>%
  cluster_response_table() %>%
  group_by(cluster_assignment) %>%
  mutate(
    diff = mean_responders - lead(mean_responders),
    size = sum(num_patients),
    method = "uniCATE (K=4)",
    group_assignment = if_else(
      cluster_assignment %in% c(1, 4), "Suni", "Atezo + Bev"
    )
  ) %>%
  ungroup() %>%
  drop_na %>%
  select(method, group_assignment, diff, size)

# Treatment rule

# prepare the test set design matrix
test_design <- test_data %>%
  mutate(
    ARM = factor(ARM),
    SEX = factor(SEX),
    rsp = if_else(rsp == "Response", 1, 0),
    ARM = if_else(ARM == "SUNITINIB", 0, 1)
  ) %>%
  select(
    -c(USUBJID, RACE, ETHNIC, ARMCD, COUNTRY, AVAL_OS, AVALU_OS, AVAL_PFS,
       AVALU_PFS, ARM)
  )
test_design <- model.matrix(rsp ~ ., data = test_design)[, -1]

# load the (augmented) modified covariates results
mod_cov_fit <- readRDS(here("results/mod-cov-fit.Rds"))
aug_mod_cov_fit <- readRDS(here("results/aug-mod-cov-fit.Rds"))

# predict their assignments on the test set
mod_cov_test_res <- test_data %>%
  mutate(
    group_assignment = predict(
      mod_cov_fit, newx = test_design, type = "trt.group"
    ),
    group_assignment = if_else(group_assignment == 0, "Suni", "Atezo + Bev")
  )
aug_mod_cov_test_res <- test_data %>%
  mutate(
    group_assignment = predict(
      aug_mod_cov_fit, newx = test_design, type = "trt.group"
    ),
    group_assignment = if_else(group_assignment == 0, "Suni", "Atezo + Bev")
  )


# compute the ORRs
mod_cov_group_resp <- mod_cov_test_res %>% group_response_table()
aug_mod_cov_group_resp <- aug_mod_cov_test_res %>% group_response_table()

# prepare the data for plotting
mc_performance <- mod_cov_group_resp %>%
  group_by(group_assignment) %>%
  mutate(
    diff = mean_responders - lead(mean_responders),
    size = sum(num_patients),
    method = "Mod. Cov."
  ) %>%
  ungroup %>%
  drop_na %>%
  select(method, group_assignment, diff, size)
amc_performance <- aug_mod_cov_group_resp %>%
  group_by(group_assignment) %>%
  mutate(
    diff = mean_responders - lead(mean_responders),
    size = sum(num_patients),
    method = "Aug. Mod. Cov."
  ) %>%
  ungroup %>%
  drop_na %>%
  select(method, group_assignment, diff, size)

# final results plot for paper
set.seed(9345634)
comp_plot <- bind_rows(
  mc_performance, amc_performance, unsupervised_unicate_k2,
  unsupervised_unicate_k3, unsupervised_unicate_k4
) %>%
  mutate(
    method = factor(
      method,
      levels = c("Mod. Cov.", "Aug. Mod. Cov.", "uniCATE (K=2)",
                 "uniCATE (K=3)", "uniCATE (K=4)")
    )
  ) %>%
  ggplot(aes(x = size, y = diff, shape = method, colour = group_assignment)) +
    geom_jitter(size = 5, height = 0, width = 4) +
    geom_hline(
      yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.3
    ) +
    xlab("Subgroup Size") +
    ylab("(ORR Atezolizumab + Bevacizumab) - (ORR Sunitinib)") +
    scale_shape_manual(
      name = "Method:",
      values = c(3, 4, 16, 17, 15, 13)
    ) +
    scale_colour_viridis_d(
      name = "Benefits more from:", option = "E", end = 0.8
    ) +
    theme_classic()

ggsave(
  filename = "orr-comparison-plot.png",
  path = here("results"),
  plot = comp_plot,
  width = 10,
  height = 5,
  scale = 1.4,
  dpi = "retina"
)
