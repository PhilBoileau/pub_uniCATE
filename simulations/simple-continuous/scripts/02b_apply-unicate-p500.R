################################################################################
# 02b. Apply uniCATE to Datasets with 500 Biomarkers
################################################################################

# set warnings to print as they occur
options(warn = 1)

# load the required libraries and functions
library(here)
library(tidyverse)
library(furrr)
library(sl3)
source(here("shared-r", "apply-unicate.R"))

# set up the future plan
plan(multicore, workers = 20)

# set the seed
set.seed(71343420)

# read in the samples data
sample_tbls <- readRDS(
  here("simple-continuous", "data", "sample-data", "sample-tbls.Rds")
) %>%
  filter(p == 500) %>%
  filter(dgp != "linear_sym_p500")

# apply the method to the data
results_tbl <- apply_unicate(sample_tbls, p = 500)

# and save to the results folder
saveRDS(
  results_tbl,
  file = here(
    "simple-continuous", "results", "p500_unicate-results.Rds"
  )
)
