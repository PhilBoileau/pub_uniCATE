################################################################################
# Apply the Modified Covariates Approach of Tian et al.
################################################################################

library(personalized)

apply_mod_cov <- function(sample_tbls, p) {

  biomarker_names <- paste0("W", seq_len(p))

  # apply modified covariates to each sample in the tibble of samples
  sample_tbls %>%
    mutate(
      method = "mod cov",
      results = future_map(
        sample,
        function(sample) {

          # prepare the data
          x <- sample %>%
            dplyr::select(all_of(biomarker_names)) %>%
            as.matrix()
          y <- sample %>% pull(Y)
          trt <- sample %>% pull(A)
          propensity_func <- function(x, trt) 0.5

          # apply the method
          mod_cov <- fit.subgroup(
            x = x,
            y = y,
            trt = trt,
            propensity.func = propensity_func,
            loss = "sq_loss_lasso",
            nfolds = 10
          )

          # extract the estimated coefficients
          coefs <- mod_cov$coefficients[biomarker_names, ] %>% as.vector

          return(coefs)
        },
        .options = furrr_options(seed = TRUE)
      )
    )
}
