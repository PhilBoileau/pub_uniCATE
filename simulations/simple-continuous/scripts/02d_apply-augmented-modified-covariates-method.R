################################################################################
# 02d. Apply Augmented Modified Covariate Model Method to Sample Data
################################################################################

# load the required libraries and functions
library(here)
library(tidyverse)
library(furrr)
source(here("shared-r", "apply-augmented-modified-covariates.R"))

# set the seed
set.seed(825624)

# set up the future plan
plan(multicore, workers = 28)

# read in the samples data
sample_tbls <- readRDS(here(
  "simple-continuous", "data", "sample-data", "sample-tbls.Rds"
))

# apply the method to the data
p100_results_tbl <- apply_aug_mod_cov(sample_tbls %>% filter(p == 100), p = 100)
p500_results_tbl <- apply_aug_mod_cov(sample_tbls %>% filter(p == 500), p = 500)

# and save to the results folder
saveRDS(
  p100_results_tbl,
  file = here("simple-continuous", "results", "p100_aug-mod-cov-method.Rds")
)
saveRDS(
  p500_results_tbl,
  file = here("simple-continuous", "results", "p500_aug-mod-cov-method.Rds")
)
