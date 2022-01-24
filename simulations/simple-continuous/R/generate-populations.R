################################################################################
# Generate Populations from DGPs
################################################################################

# generate populations of n individuals for each DGP in R/dgps.R
generate_populations <- function(n) {

  # generate the populations for each DGP
  linear_sym_p100_pop <- linear_sym_dgp(n = n, p = 100)
  kinked_p100_pop <- kinked_dgp(n = n, p = 100)
  linear_asym_ln_main_p100_pop <- linear_asym_linear_main_dgp(n = n, p = 100)
  linear_asym_nl_main_p100_pop <- linear_asym_nl_main_dgp(n = n, p = 100)
  linear_sym_p500_pop <- linear_sym_dgp(n = n, p = 500)
  kinked_p500_pop <- kinked_dgp(n = n, p = 500)
  linear_asym_ln_main_p500_pop <- linear_asym_linear_main_dgp(n = n, p = 500)
  linear_asym_nl_main_p500_pop <- linear_asym_nl_main_dgp(n = n, p = 500)

  # save the simulated populations
  saveRDS(
    linear_sym_p100_pop,
    file = here(
      "simple-continuous", "data", "population-data", "linear-sym-p100-pop.Rds"
    )
  )
  saveRDS(
    kinked_p100_pop,
    file = here(
      "simple-continuous", "data", "population-data", "kinked-p100-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_ln_main_p100_pop,
    file = here(
      "simple-continuous", "data", "population-data",
      "linear-asym-ln-main-p100-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_nl_main_p100_pop,
    file = here(
      "simple-continuous", "data", "population-data",
      "linear-asym-nl-main-p100-pop.Rds"
    )
  )
  saveRDS(
    linear_sym_p500_pop,
    file = here(
      "simple-continuous", "data", "population-data", "linear-sym-p500-pop.Rds"
    )
  )
  saveRDS(
    kinked_p500_pop,
    file = here(
      "simple-continuous", "data", "population-data", "kinked-p500-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_ln_main_p500_pop,
    file = here(
      "simple-continuous", "data", "population-data",
      "linear-asym-ln-main-p500-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_nl_main_p500_pop,
    file = here(
      "simple-continuous", "data", "population-data",
      "linear-asym-nl-main-p500-pop.Rds"
    )
  )
}
