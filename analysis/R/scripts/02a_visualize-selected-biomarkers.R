################################################################################
# 02a. Comparison of Selected Biomarkers
################################################################################

# load the required libraries and functions
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(stringr)
library(pheatmap)
library(personalized)
source(here("R/helpers/heatmap-function.R"))

# load the IMmotion151 data
train_data <- readRDS(here("data/processed-IMmotion150.Rds"))
test_data <- readRDS(here("data/processed-IMmotion151.Rds"))

# load the biomarker data ######################################################
mc_biomarkers <- readRDS(here("results/mod-cov-selected-interactions.Rds"))
amc_biomarkers <- readRDS(here("results/aug-mod-cov-selected-interactions.Rds"))
unicate_results <- readRDS(here("results/uniCATE-results.Rds"))
unicate_biomarkers <- unicate_results %>%
  filter(p_value_bh <= 0.05) %>%
  pull(biomarker)

# construct the heatmaps for the IMmotion 150 study ############################
plot_heatmap(
  train_data, mc_biomarkers, "Modified Covariates Selection",
  filename = "IMmotion150-mc-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)
plot_heatmap(
  train_data, amc_biomarkers, "Augmented Modified Covariates Selection",
  filename = "IMmotion150-amc-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)
plot_heatmap(
  train_data, unicate_biomarkers, "uniCATE Selection",
  filename = "IMmotion150-uniCATE-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)

# construct the heatmaps for the IMmotion 151 study ############################
plot_heatmap(
  test_data, mc_biomarkers, "Modified Covariates Selection",
  filename = "IMmotion151-mc-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)
plot_heatmap(
  test_data, amc_biomarkers, "Augmented Modified Covariates Selection",
  filename = "IMmotion151-amc-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)
plot_heatmap(
  test_data, unicate_biomarkers, "uniCATE Selection",
  filename = "IMmotion151-uniCATE-biomarkers-heatmap.png",
  path = here("results/heatmaps")
)

# create the plot for the manuscript
mc_heat <- plot_heatmap(
  test_data, mc_biomarkers, "A    Modified Covariates Selection"
)
amc_heat <- plot_heatmap(
  test_data, amc_biomarkers, "B    Augmented Modified Covariates Selection"
)
uni_heat <- plot_heatmap(
  test_data, unicate_biomarkers, "C    uniCATE Selection"
)
combo_heatmap <- grid.arrange(
  grobs = list(
    mc_heat[[4]], amc_heat[[4]], uni_heat[[4]]
  ),
  ncol = 3,
  left = "Patients", bottom = "Predictive Biomarkers"
)
ggsave(
  filename = "heatmaps.png",
  plot = combo_heatmap,
  path = here("results/heatmaps"),
  width = 10,
  height = 5,
  dpi = "retina",
  scale = 1.7
)

# check if XIST biomarker is truly predictive ##################################
xist_plot <- test_data %>%
  mutate(
    outcome = if_else(rsp == "Response", 1, 0),
    treatment = if_else(ARM == "SUNITINIB", "Sunitinib",
                        "Atezolizumab + Bevacizumab")
  ) %>%
  ggplot(aes(x = XIST, y = outcome, colour = treatment)) +
    geom_point() +
    geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") +
    xlab("log(XIST)") +
    ylab("Response") +
    ggtitle("Association Between Log-Transformed XIST and Objective Response") +
    scale_colour_viridis_d(name = "Treatment Arm", option = "E", end = 0.8) +
    theme_classic()
ggsave(
  filename = "xist-predictiveness.png",
  plot = xist_plot,
  path = here("results"),
  width = 8,
  height = 4,
  scale = 1.3,
  dpi = 300
)
