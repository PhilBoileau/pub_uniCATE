################################################################################
# Apply the Augmented Modified Covariates Approach of Tian et al.
################################################################################

library(personalized)
apply_aug_mod_cov <- function(sample_tbls, p) {

  # define the biomarker names
  biomarker_names <- paste0("W", seq_len(p))

  # define the requisite functions
  propensity_func <- function(x, trt) 0.5
  augment_func <- function(x, y) # taken from personalized vignette
  {
    df.x  <- data.frame(x)

    # add all squared terms to model
    form <- eval(paste(" ~ 1 + ", paste(colnames(df.x), collapse = " + ")))
    mm <- model.matrix(as.formula(form), data = df.x)
    cvmod <- cv.glmnet(y = y, x = mm, nfolds = 10)
    predictions <- predict(cvmod, newx = mm, s = "lambda.min")
    predictions
  }

  # apply modified covariates to each sample in the tibble of samples
  sample_tbls %>%
    mutate(
      method = "aug mod cov",
      results = future_map(
        sample,
        function(sample) {

          # prepare the data
          x <- sample %>%
            dplyr::select(all_of(biomarker_names)) %>%
            as.matrix()
          y <- sample %>% pull(Y)
          trt <- sample %>% pull(A)

          # apply the method
          mod_cov <- fit.subgroup(
            x = x,
            y = y,
            trt = trt,
            propensity.func = propensity_func,
            augment.func = augment_func,
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
