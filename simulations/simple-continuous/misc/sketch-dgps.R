################################################################################
# Data Generating Processes for Sketch of DGPs
################################################################################

library(MASS)
library(tibble)
library(dplyr)
library(future.apply)

# The first DGP assumes an opposite but symmetric linear response between RCT
# arms.


################################################################################

# This DGP assumes two different relationships between treatment
# arms. One treatment arm has a linear relationship with a handful of
# biomarkers. The other treatment arm has a "kinked" linear relationship.

# Input:
#   n, an integer representing the number of observations
#   p, an integer representing the number of biomarkers
# Output:
#   A data.frame object containing the p covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
kinked_dgp <- function(n, p) {

  # randomly generate n synthetic biomarker measurement vectors
  cov_mat <- diag(1, nrow = p)
  mu <- rep(0, p)

  # define the sparse coefficients vector for the controls and treatment
  beta_treat <- c(c(rep(2, 4), rep(0, p - 4)))

  # simulate n observations
  obs_list <- future_lapply(
    seq_len(n),
    function(i) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(n = 1, mu = mu, Sigma = cov_mat)

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)

      # treatment outcome
      Y_treat <- 0.5 + sum(W * beta_treat) + epsilon

      # control outcome
      b1_term <- if_else(W[1] > 0, -W[1], beta_treat[1] * W[1])
      b2_term <- if_else(W[2] > 0, -W[2], beta_treat[2] * W[2])
      b3_term <- if_else(W[3] > 0, -W[3], beta_treat[3] * W[3])
      b4_term <- if_else(W[4] > 0, -W[4], beta_treat[4] * W[4])
      Y_cont <- 0.5 + sum(b1_term, b2_term, b3_term, b4_term) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(p)), "A", "Y_treat", "Y_cont", "Y"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)

}

################################################################################

# So far, none of the DGPs model the main effects. In the next few DGPs, we
# model the main effects to evaluate how main effect model mispecification (or
# complete lack of modeling in the modified covariates case) might affect
# estimation (and/or efficiency).

# This next DGP is a modified version of the asymetric, positive linear DGP from
# above. Here, the first four biomarkers are possess main effect terms in the
# conditional outcome regression model.

# Input:
#   n, an integer representing the number of observations
#   p, an integer representing the number of biomarkers
# Output:
#   A data.frame object containing the p covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
linear_asym_linear_main_dgp <- function(n, p) {

  # randomly generate n synthetic biomarker measurement vectors
  cov_mat <- diag(1, nrow = p)
  mu <- rep(0, p)

  # define the sparse coefficients vector for the main effects, and controls and
  # treatment interactions
  beta_main <- matrix(c(5, -8, 3, -10, 5, rep(0, p - 5)))
  beta_cont <- matrix(rep(0, p))
  beta_treat <- matrix(c(rep(5, 4), rep(0, p - 4)))

  # simulate n observations
  obs_list <- future_lapply(
    seq_len(n),
    function(i) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(n = 1, mu = mu, Sigma = cov_mat)

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)
      main_effects <- sum(W * beta_main)
      Y_treat <- 3 + main_effects + sum(W * beta_treat) + epsilon
      Y_cont <-  main_effects + sum(W * beta_cont) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(p)), "A", "Y_treat", "Y_cont", "Y"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)
}


################################################################################

# This next DGP is another modified version of the asymetric, positive linear
# DGP from above. Here, the first four biomarkers possess nonlinear main effects
# on the outcome variable.

# Input:
#   n, an integer representing the number of observations
#   p, an integer representing the number of biomarkers
# Output:
#   A data.frame object containing the p covariates, treatment indicator, and
#   outcomes under each treatment group of n independently simulated
#   observations.
linear_asym_nl_main_dgp <- function(n, p) {

  # randomly generate n synthetic biomarker measurement vectors
  cov_mat <- diag(1, nrow = p)
  mu <- rep(0, p)

  # define the sparse coefficients vector for the main effects, and controls and
  # treatment interactions
  beta_main <- matrix(c(-1, -1, 1, 1, 0.5, rep(0, p - 5)))
  beta_cont <- matrix(rep(0, p))
  beta_treat <- matrix(c(rep(5, 4), rep(0, p - 4)))

  # simulate n observations
  obs_list <- future_lapply(
    seq_len(n),
    function(i) {

      # generate the biomarker measurements
      W <- MASS::mvrnorm(n = 1, mu = mu, Sigma = cov_mat)

      # simulate the outcomes under both treatments
      epsilon <- rnorm(1, 0, 0.5)
      main_effects <- sum(exp(abs(W * beta_main)))
      Y_treat <- 5 + main_effects + sum(W * beta_treat) + epsilon
      Y_cont <-  main_effects + sum(W * beta_cont) + epsilon

      # assign an outcome, and select an observed outcome
      A <- ifelse(runif(1) < 0.5, 0, 1)
      Y <- ifelse(A == 0, Y_cont, Y_treat)

      # return the simulated observation
      return(c(W, A, Y_treat, Y_cont, Y))

    },
    future.seed = TRUE
  )

  # assemble list of observations into a tibble
  sim_data <- as_tibble(do.call(rbind, obs_list), .name_repair = "minimal")
  colnames(sim_data) <- c(
    paste0("W", seq_len(p)), "A", "Y_treat", "Y_cont", "Y"
  )
  sim_data <- sim_data %>% mutate(A = if_else(A == 0, "control", "treatment"))

  return(sim_data)
}
