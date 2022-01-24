################################################################################
# Create OLS Parameters For Simulated Populations
################################################################################

# Input
#   scenario_dir: A character indicating simulation scenario directory
# Output
#   A tibble containing the projected treatment-biomarker interaction effects
#   for each population.
compute_pop_cates <- function(scenario_dir) {

  # compute the interaction parameters
  if (scenario_dir == "simple-continuous") {
    linear_sym_p100_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data", "linear-sym-p100-pop.Rds"
      )
    ) %>%
      get_cates

    kinked_p100_coefs <- readRDS(
      here(scenario_dir, "data", "population-data", "kinked-p100-pop.Rds")
    ) %>%
      get_cates

    linear_asym_ln_main_p100_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-ln-main-p100-pop.Rds"
      )
    ) %>%
      get_cates

    linear_asym_nl_main_p100_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-nl-main-p100-pop.Rds"
      )
    ) %>%
      get_cates

    linear_sym_p500_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data", "linear-sym-p500-pop.Rds"
      )
    ) %>%
      get_cates

    kinked_p500_coefs <- readRDS(
      here(scenario_dir, "data", "population-data", "kinked-p500-pop.Rds")
    ) %>%
      get_cates

    linear_asym_ln_main_p500_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-ln-main-p500-pop.Rds"
      )
    ) %>%
      get_cates

    linear_asym_nl_main_p500_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-nl-main-p500-pop.Rds"
      )
    ) %>%
      get_cates

  } else {

    kinked_coefs <- readRDS(
      here(scenario_dir, "data", "population-data", "kinked-pop.Rds")
    ) %>%
      get_cates(subgroups = TRUE)

    linear_asym_ln_main_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-ln-main-pop.Rds"
      )
    ) %>%
      get_cates(subgroups = TRUE)

    linear_asym_nl_main_coefs <- readRDS(
      here(
        scenario_dir, "data", "population-data",
        "linear-asym-nl-main-pop.Rds"
      )
    ) %>%
      get_cates(subgroups = TRUE)

  }

  # assemble the population parameters into tibbles
  if (scenario_dir == "simple-continuous") {
    pop_params_p100_tbl <- list(
      "linear_sym_p100" = linear_sym_p100_coefs,
      "kinked_p100" = kinked_p100_coefs,
      "linear_asym_ln_main_p100" = linear_asym_ln_main_p100_coefs,
      "linear_asym_nl_main_p100" = linear_asym_nl_main_p100_coefs
    ) %>%
      bind_cols

    pop_params_p500_tbl <- list(
      "linear_sym_p500" = linear_sym_p500_coefs,
      "kinked_p500" = kinked_p500_coefs,
      "linear_asym_ln_main_p500" = linear_asym_ln_main_p500_coefs,
      "linear_asym_nl_main_p500" = linear_asym_nl_main_p500_coefs
    ) %>%
      bind_cols

    return(list(
      "pop_params_p100_tbl" = pop_params_p100_tbl,
      "pop_params_p500_tbl" = pop_params_p500_tbl
    ))

  } else {

    pop_params_tbl <- list(
      "kinked" = kinked_coefs,
      "linear_asym_ln_main" = linear_asym_ln_main_coefs,
      "linear_asym_nl_main" = linear_asym_nl_main_coefs
    ) %>%
      bind_cols

    return(list(
      "pop_params_tbl" = pop_params_tbl
    ))
  }

}

# Input:
#   pop_tbl: a tibble representing population data.
#   subgroup: a flag indicating whether subgroups are present in the data
# Output:
#   A vector of coefficients corresponding to the univariate CATE parameters.
get_cates <- function(pop_tbl, subgroups = FALSE) {

  # compute the difference in potential outcomes and center the column data
  pop_data <- pop_tbl %>%
    mutate(
      Y_diff = Y_treat - Y_cont
    ) %>%
    dplyr::select(-Y, -Y_treat, -Y_cont, -A) %>%
    scale(center = TRUE, scale = FALSE) %>%
    as_tibble()

  # extract the biomarker names
  biomarkers <- colnames(pop_data)[colnames(pop_data) != "Y_diff"]

  # estimate the univariate CATE for each biomarker
  univariate_cate_params <- sapply(
    biomarkers,
    function(b) {
      sum(pop_data$Y_diff * pop_data[[b]]) / sum(pop_data[[b]]^2)
    }
  )
  names(univariate_cate_params) <- biomarkers

  if (subgroups) {
    subgroup_var_idx <- which(names(univariate_cate_params) != "subgroup")
    univariate_cate_params <- univariate_cate_params[subgroup_var_idx]
  }

  # return the parameters for the given population data
  return(univariate_cate_params)
}
