################################################################################
# 01b. Compute Linear Approximations of Univariate CATEs in Population Data
################################################################################

# load the required libraries and functions
library(here)
library(tidyverse)
source(here("shared-r", "compute-pop-univariate-cates.R"))

# compute the projected linear models' interaction coefficients
pop_cates_tbls <- compute_pop_cates(scenario_dir = "simple-continuous")

# save the coefficients
saveRDS(
  pop_cates_tbls,
  file = here(
    "simple-continuous", "data", "population-data",
    "pop-univariate-cate-params.Rds"
  )
)
