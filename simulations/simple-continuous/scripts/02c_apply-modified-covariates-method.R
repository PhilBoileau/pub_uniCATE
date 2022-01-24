################################################################################
# 02c. Apply Modified Covariate Model Method to Sample Data
################################################################################

# load the required libraries and functions
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(furrr)
source(here("shared-r", "apply-modified-covariates.R"))

# set the seed
set.seed(63452)

# set up the future plan
plan(multicore, workers = 20)

# read in the samples data
sample_tbls <- readRDS(here(
  "simple-continuous", "data", "sample-data", "sample-tbls.Rds"
))

# apply the method to the data
p100_results_tbl <- apply_mod_cov(sample_tbls %>% filter(p == 100), p = 100)
p500_results_tbl <- apply_mod_cov(sample_tbls %>% filter(p == 500), p = 500)

# and save to the results folder
saveRDS(
  p100_results_tbl,
  file = here("simple-continuous", "results", "p100_mod-cov-method.Rds")
)
saveRDS(
  p500_results_tbl,
  file = here("simple-continuous", "results", "p500_mod-cov-method.Rds")
)
