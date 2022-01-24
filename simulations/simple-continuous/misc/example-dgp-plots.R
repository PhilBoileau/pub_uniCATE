################################################################################
# Generate Sketches of DGPs' Biomarker-Treatment Interactions
################################################################################

library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# load required functions
source(here("simple-continuous", "misc", "sketch-dgps.R"))

set.seed(510514)

# plot examples of treatment-biomarker interactions ############################

# plot of kinked treatment-biomarker interaction
kinked_df <- kinked_dgp(500, 5)
kinked_p <- kinked_df %>%
  mutate(A = if_else(A == "treatment", "Treatment", "Control")) %>%
  ggplot(aes(x = W1, y = Y, colour = A)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  xlab("Biomarker") +
  ylab("Outcome") +
  scale_colour_viridis_d(name = "Group Assignment", option = "E", end = 0.8) +
  ggtitle('Kinked Conditional Outcome Regression') +
  theme_classic()
ggsave(
  filename = "kinked-treatment-biomarker-example.png",
  path = here("simple-continuous", "misc", "sketch-dgp-plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)

# plot of linear treatment-biomarker interaction
linear_df <- linear_asym_linear_main_dgp(500, 5)
linear_p <- linear_df %>%
  mutate(A = if_else(A == "treatment", "Treatment", "Control")) %>%
  ggplot(aes(x = W1, y = Y, colour = A)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Biomarker") +
  ylab("Outcome") +
  scale_colour_viridis_d(name = "Group Assignment", option = "E", end = 0.8) +
  ggtitle("Linear Conditional Outcome Regression") +
  theme_classic()
ggsave(
  filename = "prognostic-biomarker-example.png",
  path = here("simple-continuous", "misc", "sketch-dgp-plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)

# plot of non-linear treatment-biomarker interaction
nonlinear_df <- linear_asym_nl_main_dgp(500, 5)
nonlinear_p <- nonlinear_df %>%
  mutate(A = if_else(A == "treatment", "Treatment", "Control")) %>%
  ggplot(aes(x = W1, y = Y, colour = A)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  xlab("Biomarker") +
  ylab("Outcome") +
  ylim(c(-5, 50)) +
  scale_colour_viridis_d(name = "Group Assignment", option = "E", end = 0.8) +
  ggtitle("Nonlinear Conditional Outcome Regression") +
  theme_classic()
ggsave(
  filename = "nonlinear-treatment-biomarker-example.png",
  path = here("simple-continuous", "misc", "sketch-dgp-plots"),
  dpi = "retina",
  width = 6,
  height = 4,
  scale = 1.2
)

# combine into a single figure
combo_p <- ggarrange(
  linear_p, kinked_p, nonlinear_p,
  nrow = 1,
  common.legend = TRUE, legend = "right", align = "h"
)
ggsave(
  plot = combo_p,
  filename = "dgp-sketches.png",
  path = here("simple-continuous", "misc", "sketch-dgp-plots"),
  dpi = "retina",
  width = 10,
  height = 2.5,
  scale = 1.7
)
