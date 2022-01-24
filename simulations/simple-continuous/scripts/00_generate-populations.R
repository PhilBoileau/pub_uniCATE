################################################################################
# 00. Generate Population Datasets
################################################################################

# load the required libraries and functions
library(here)
library(future.apply)
source(here("simple-continuous", "R", "data-generating-processes.R"))
source(here("simple-continuous", "R", "generate-populations.R"))

# set the future plan
plan(multicore, workers = 20)

# set the seed
set.seed(5123234)

# generate the populations
generate_populations(n = 100000)
