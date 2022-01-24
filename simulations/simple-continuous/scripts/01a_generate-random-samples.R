################################################################################
# 01. Generate Random Samples From Populations
################################################################################

# load the required libraries and functions
library(here)
library(tidyverse)
library(future)
library(furrr)
source(here("simple-continuous", "R", "generate-random-samples.R"))

# debugging
options(warn = 1)

# set the seed
set.seed(23847)

# set the future plan
plan(multicore, workers = 20)

# generate the populations
sample_tbls <- generate_random_samples(
  n_samp = 200, sizes = c(125, 250, 500)
)

# save the random samples to the appropriate directory
saveRDS(
  sample_tbls,
  file = here("simple-continuous", "data", "sample-data", "sample-tbls.Rds")
)
