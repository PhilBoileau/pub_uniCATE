################################################################################
# 02a. Apply uniCATE to Datasets with 100 Biomarkers
################################################################################

# set warnings to print as they occur
options(warn = 1)

# load the required libraries and functions
library(here)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(furrr)
library(sl3)
source(here("shared-r", "apply-unicate.R"))

# set up the future plan
plan(multicore, workers = 28)

# set the seed
set.seed(9310316)

# read in the samples data
sample_tbls <- readRDS(
  here("simple-continuous", "data", "sample-data", "sample-tbls.Rds")
) %>%
  filter(p == 100)

# apply the method to the data
results_tbl <- apply_unicate(sample_tbls, p = 100)

# and save to the results folder
saveRDS(
  results_tbl,
  file = here(
    "simple-continuous", "results", "p100_unicate-results.Rds"
  )
)
