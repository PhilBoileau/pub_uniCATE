################################################################################
# 02d. Apply Augmented Modified Covariate Model Method to Sample Data
################################################################################

# load the required libraries and functions
library(here)
library(tidyverse)
library(furrr)
source(here("shared-r", "apply-augmented-modified-covariates.R"))

# set the seed
set.seed(61235)

# set up the future plan
plan(multisession, workers = 10)

# read in the samples data
sample_tbls <- readRDS(
  here("realistic-continuous", "data", "sample-data", "sample-tbls.Rds")
)

# apply the method to the data
results_tbl <- apply_aug_mod_cov(sample_tbls, p = 500)

# and save to the results folder
saveRDS(
  results_tbl,
  file = here("realistic-continuous", "results", "aug-mod-cov-method.Rds")
)
