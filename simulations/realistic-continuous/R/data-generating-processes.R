################################################################################
# Data Generating Processes
################################################################################

# This file contains the DGP functions for the realistic simulation study with a
# continuous outcome.

################################################################################

library(MASS)
library(tibble)
library(dplyr)
library(future.apply)

################################################################################

# The first DGP assumes two different relationships between treatment
# arms. One treatment arm has a linear relationship with a handful of
# biomarkers. The other treatment arm has a "kinked" linear relationship.

# Input:
#   n, an integer representing the number of observations
#   cov_mat, a p x p covariance matrix of the biomarkers' distribution
# Output:
#   A data.frame object containing the 500 covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
kinked_dgp <- function(n, cov_mat) {

  # randomly generate n synthetic biomarker measurement vectors with subgroups
  # produce the vector of sub-group predictive biomarker means
  U <- runif(n = n)
  U <- if_else(U <= 1/2, -1, 1)
  beta_treat <- c(rep(10, 4), rep(0, 496))

  # simulate n observations
  obs_list <- future_lapply(
    U,
    function(u) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(
        n = 1,
        mu = c(rep(u, 4), rep(0, 496)),
        Sigma = cov_mat
      )

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)

      # treatment outcome
      Y_treat <- sum(W * beta_treat) + epsilon

      # control outcome
      cont_terms <- sapply(
        seq_len(500),
        function(w_idx) {
          w_term <- beta_treat[w_idx] * W[w_idx]
          if (W[w_idx] > 0 & w_idx <= 4) {
            w_term <- 0
          }
          return(w_term)
        }
      )
      Y_cont <- 0.5 + sum(cont_terms) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)
      subgroup <- if_else(u == -1, 1, 2)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y, subgroup))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(500)), "A", "Y_treat", "Y_cont", "Y", "subgroup"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)

}


################################################################################

# So far, none of the DGPs model the main effects. In the next few DGPs, we
# model the main effects to evaluate how main effect model mispecification (or
# complete lack of modeling in the modified covariates case) might affect
# estimation (and/or efficiency).

# This next DGP is a modified version of the asymmetric, positive linear DGP
# from above. Here, the first four biomarkers are possess main effect terms in
# the conditional outcome regression model.

# Input:
#   n, an integer representing the number of observations
#   cov_mat, a p x p covariance matrix of the biomarkers' distribution
# Output:
#   A data.frame object containing the 500 covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
linear_asym_linear_main_dgp <- function(n, cov_mat) {

  # randomly generate n synthetic biomarker measurement vectors
  U <- runif(n = n)
  U <- if_else(U <= 1/2, -1, 1)

  # define the sparse coefficients vector for the main effects, and controls and
  # treatment interactions
  beta_main <- c(2, 2, 2, 2, 2, rep(0, 495))
  beta_cont <- rep(0, 500)
  beta_treat <- c(rep(5, 4), rep(0, 496))

  # simulate n observations
  obs_list <- future_lapply(
    U,
    function(u) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(
        n = 1,
        mu = c(rep(u, 4), rep(0, 496)),
        Sigma = cov_mat
      )

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)
      main_effects <- sum(W * beta_main)
      Y_treat <- main_effects + sum(W * beta_treat) + epsilon
      Y_cont <-  main_effects + sum(W * beta_cont) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)
      subgroup <- if_else(u == -1, 1, 2)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y, subgroup))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(500)), "A", "Y_treat", "Y_cont", "Y", "subgroup"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)
}


################################################################################

# This last DGP is another modified version of the asymetric, positive linear
# DGP from above. Here, the first four biomarkers possess nonlinear main effects
# on the outcome variable.

# Input:
#   n, an integer representing the number of observations
#   cov_mat, a p x p covariance matrix of the biomarkers' distribution
# Output:
#   A data.frame object containing the 500 covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
linear_asym_nl_main_dgp <- function(n, cov_mat) {

  # randomly generate n synthetic biomarker measurement vectors
  U <- runif(n = n)
  U <- if_else(U <= 1/2, -1, 1)

  # define the sparse coefficients vector for the main effects, and controls and
  # treatment interactions
  beta_main <- c(1, 1, 1, 1, 1, rep(0, 495))
  beta_cont <- rep(0, 500)
  beta_treat <- c(rep(5, 4), rep(0, 496))

  # simulate n observations
  obs_list <- future_lapply(
    U,
    function(u) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(
        n = 1,
        mu = c(rep(u, 4), rep(0, 496)),
        Sigma = cov_mat
      )

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)
      main_effects <- sum(exp(abs(W * beta_main)))
      Y_treat <- main_effects + sum(W * beta_treat) + epsilon
      Y_cont <-  main_effects + sum(W * beta_cont) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)
      subgroup <- if_else(u == -1, 1, 2)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y, subgroup))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(500)), "A", "Y_treat", "Y_cont", "Y", "subgroup"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)
}
