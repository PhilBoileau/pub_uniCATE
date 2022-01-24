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
  linear_sym_p100 <- readRDS(
    here(
      "simple-continuous", "data", "population-data", "linear-sym-p100-pop.Rds"
    )
  )
  kinked_p100 <- readRDS(
    here("simple-continuous", "data", "population-data", "kinked-p100-pop.Rds")
  )
  linear_asym_ln_main_p100 <- readRDS(
    here(
      "simple-continuous", "data", "population-data",
      "linear-asym-ln-main-p100-pop.Rds"
    )
  )
  linear_asym_nl_main_p100 <- readRDS(
    here(
      "simple-continuous", "data", "population-data",
      "linear-asym-nl-main-p100-pop.Rds"
    )
  )
  linear_sym_p500 <- readRDS(
    here(
      "simple-continuous", "data", "population-data", "linear-sym-p500-pop.Rds"
    )
  )
  kinked_p500 <- readRDS(
    here("simple-continuous", "data", "population-data", "kinked-p500-pop.Rds")
  )
  linear_asym_ln_main_p500 <- readRDS(
    here(
      "simple-continuous", "data", "population-data",
      "linear-asym-ln-main-p500-pop.Rds"
    )
  )
  linear_asym_nl_main_p500 <- readRDS(
    here(
      "simple-continuous", "data", "population-data",
      "linear-asym-nl-main-p500-pop.Rds"
    )
  )

  # store as a list
  pop_list <- list(
    "linear_sym_p100" = linear_sym_p100,
    "kinked_p100" = kinked_p100,
    "linear_asym_ln_main_p100" = linear_asym_ln_main_p100,
    "linear_asym_nl_main_p100" = linear_asym_nl_main_p100,
    "linear_sym_p500" = linear_sym_p500,
    "kinked_p500" = kinked_p500,
    "linear_asym_ln_main_p500" = linear_asym_ln_main_p500,
    "linear_asym_nl_main_p500" = linear_asym_nl_main_p500
  )
  rm(linear_sym_p100, kinked_p100, linear_asym_ln_main_p100,
     linear_asym_nl_main_p100, linear_sym_p500, kinked_p500,
     linear_asym_ln_main_p500, linear_asym_nl_main_p500)

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
              .options = future_options(seed = TRUE)
            )
        })
      }
    ) %>%
    # merge into a single tibble
    map2(
      names(.),
      function(tbl, tbl_name) {
        tbl %>%
          mutate(dgp = tbl_name) %>%
          dplyr::select(dgp, sim_idx, size, sample)
      }
    ) %>%
    bind_rows %>%
    mutate(p = as.numeric(str_extract(dgp, "\\d+")))


  return(samples_tbl)
}
