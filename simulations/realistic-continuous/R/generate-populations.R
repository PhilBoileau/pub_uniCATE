################################################################################
# Generate Populations from DGPs
################################################################################

# generate populations of n individuals for each DGP in R/dgps.R
generate_populations <- function(n, cov_mat) {

  # generate the populations for each DGP
  kinked_pop <- kinked_dgp(n, cov_mat)
  linear_asym_ln_main_pop <- linear_asym_linear_main_dgp(n, cov_mat)
  linear_asym_nl_main_pop <- linear_asym_nl_main_dgp( n, cov_mat)

  # save the simulated populations
  saveRDS(
    kinked_pop,
    file = here(
      "realistic-continuous", "data", "population-data", "kinked-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_ln_main_pop,
    file = here(
      "realistic-continuous", "data", "population-data",
      "linear-asym-ln-main-pop.Rds"
    )
  )
  saveRDS(
    linear_asym_nl_main_pop,
    file = here(
      "realistic-continuous", "data", "population-data",
      "linear-asym-nl-main-pop.Rds"
    )
  )
}
