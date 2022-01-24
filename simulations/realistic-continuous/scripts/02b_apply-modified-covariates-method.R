################################################################################
# 02b. Apply Modified Covariate Model Method to Sample Data
################################################################################

# load the required libraries and functions
library(here)
library(readr)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(furrr)
source(here("shared-r", "apply-modified-covariates.R"))

# set the seed
set.seed(71234)

# set up the future plan
plan(multisession, workers = 10)

# read in the samples data
sample_tbls <- read_rds(
  here("realistic-continuous", "data", "sample-data", "sample-tbls.Rds")
)

# apply the method to the data
results_tbl <- apply_mod_cov(sample_tbls, p = 500)

# and save to the results folder
saveRDS(
  results_tbl,
  file = here("realistic-continuous", "results", "mod-cov-method.Rds")
)
