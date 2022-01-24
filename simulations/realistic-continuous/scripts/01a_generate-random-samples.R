################################################################################
# 01. Generate Random Samples From Populations
################################################################################

# load the required libraries and set future options
library(here)
library(tidyverse)
library(future)
library(furrr)
options(future.globals.maxSize = 4.5 * 1024^3)

# set the seed
set.seed(85135)

# set the future plan
plan(multicore, workers = 20)

# set the number samples and the sample sizes
n_samp <- 200
sizes <- c(125, 250, 500)

# load the population data and create sample tables
kinked <- readRDS(
  here("realistic-continuous", "data", "population-data", "kinked-pop.Rds")
)
kinked_samples <- map_dfr(
  sizes,
  function(size){
    seq_len(n_samp) %>%
      future_map_dfr(
        function(idx) {
          samp_row <- tibble_row(
            dgp = "kinked",
            sim_idx = idx,
            size = size,
            p = 500,
            sample = list(
              tibble(sample_n(kinked, size = size, replace = FALSE))
            )
          )
          return(samp_row)
        },
        .options = furrr_options(seed = TRUE)
      )
  }
)
rm(kinked)

linear_asym_ln_main <- readRDS(
  here(
    "realistic-continuous", "data", "population-data",
    "linear-asym-ln-main-pop.Rds"
  )
)
linear_samples <- map_dfr(
  sizes,
  function(size){
    seq_len(n_samp) %>%
      future_map_dfr(
        function(idx) {
          samp_row <- tibble_row(
            dgp = "linear_asym_ln_main",
            sim_idx = idx,
            size = size,
            p = 500,
            sample = list(
              tibble(
                sample_n(linear_asym_ln_main, size = size, replace = FALSE)
              )
            )
          )
          return(samp_row)
        },
        .options = furrr_options(seed = TRUE)
      )
  }
)
rm(linear_asym_ln_main)

linear_asym_nl_main <- readRDS(
  here(
    "realistic-continuous", "data", "population-data",
    "linear-asym-nl-main-pop.Rds"
  )
)
nonlinear_samples <- map_dfr(
  sizes,
  function(size){
    seq_len(n_samp) %>%
      future_map_dfr(
        function(idx) {
          samp_row <- tibble_row(
            dgp = "linear_asym_nl_main",
            sim_idx = idx,
            size = size,
            p = 500,
            sample = list(
              tibble(
                sample_n(linear_asym_nl_main, size = size, replace = FALSE)
              )
            )
          )
          return(samp_row)
        },
        .options = furrr_options(seed = TRUE)
      )
  }
)
rm(linear_asym_nl_main)

# save the random samples to the appropriate directory
saveRDS(
  bind_rows(kinked_samples, linear_samples, nonlinear_samples),
  file = here("realistic-continuous", "data", "sample-data", "sample-tbls.Rds")
)
