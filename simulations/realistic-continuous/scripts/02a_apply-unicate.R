################################################################################
# 02a. Apply uniCATE to Realistic Datasets
################################################################################

# set warnings to print as they occur
options(warn = 1)

# load the required libraries and functions
library(here)
library(tidyverse)
library(future)
library(furrr)
library(sl3)
source(here("shared-r", "apply-unicate.R"))

# set up the future plan
plan(multisession, workers = 20)

# set the seed
set.seed(925418563)

# read in the samples data
sample_tbls <- read_rds(
  here("realistic-continuous", "data", "sample-data", "sample-tbls.Rds")
)

# apply the method to the data
linear_tbl <- sample_tbls %>%
  filter(dgp == "linear_asym_ln_main") %>%
  apply_unicate(p = 500)
saveRDS(
  linear_tbl,
  file = here(
    "realistic-continuous", "results", "unicate-results_linear-dgp.Rds"
  )
)

kinked_tbl <- sample_tbls %>%
  filter(dgp == "kinked") %>%
  apply_unicate(p = 500)
saveRDS(
  kinked_tbl,
  file = here(
    "realistic-continuous", "results", "unicate-results_kinked-dgp.Rds"
  )
)

nonlinear_tbl <- sample_tbls %>%
  filter(dgp == "linear_asym_nl_main") %>%
  apply_unicate(p = 500)
saveRDS(
  nonlinear_tbl,
  file = here(
    "realistic-continuous", "results", "unicate-results_nonlinear-dgp.Rds"
  )
)

# combine the results
results_tbl <- bind_rows(linear_tbl, kinked_tbl, nonlinear_tbl)

# and save to the results folder
saveRDS(
  results_tbl,
  file = here(
    "realistic-continuous", "results", "unicate-results.Rds"
  )
)
