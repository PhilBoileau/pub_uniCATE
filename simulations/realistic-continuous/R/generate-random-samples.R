################################################################################
# Create Random Samples From Simulated Populations
################################################################################

# Input:
#   n_samp: the number of samples to generate from each population
#   sizes: a vector of the number of sample sizes.
# Output
#   A nested tibble, where each row contains a tibble that is a sample from
#   one of the simulate populations.
generate_random_samples <- function(n_samp, sizes) {

  # load the data
  kinked <- readRDS(
    here("realistic-continuous", "data", "population-data", "kinked-pop.Rds")
  )
  linear_asym_ln_main <- readRDS(
    here(
      "realistic-continuous", "data", "population-data",
      "linear-asym-ln-main-pop.Rds"
    )
  )
  linear_asym_nl_main <- readRDS(
    here(
      "realistic-continuous", "data", "population-data",
      "linear-asym-nl-main-pop.Rds"
    )
  )

  # store as a list
  pop_list <- list(
    "kinked" = kinked,
    "linear_asym_ln_main" = linear_asym_ln_main,
    "linear_asym_nl_main" = linear_asym_nl_main
  )
  rm(kinked, linear_asym_ln_main, linear_asym_nl_main)

  # loop through each population tibble
  samples_tbl <- pop_list %>%
    map(
      function(pop_tbl) {

        # generate n_samp samples of sizes in sizes
        map_dfr(sizes, function(size){
          seq_len(n_samp) %>%
            future_map_dfr(
              function(idx) {
                samp_row <- tibble_row(
                  sim_idx = idx,
                  size = size,
                  sample = list(
                    tibble(sample_n(pop_tbl, size = size, replace = FALSE))
                  )
                )
                return(samp_row)
              },
              .options = furrr_options(seed = TRUE)
            )
        })
      }
    ) %>%
    # merge into a single tibble
    map2(
      names(.),
      function(tbl, tbl_name) {
        tbl %>%
          mutate(
            dgp = tbl_name,
            p = 500
          ) %>%
          dplyr::select(dgp, p, sim_idx, size, sample)
      }
    ) %>%
    bind_rows


  return(samples_tbl)
}
