################################################################################
# 01b. Select Interactions Using the Augmented Modified Covariate Approach
################################################################################

# In this script, the modified augmented covariate approach is used to identify
# the biomarkers that best explain response.

# load the required libraries
library(here)
library(dplyr)
library(personalized)

set.seed(713451)

# load and prepare the training data
train_data <- readRDS(here("data/processed-IMmotion150.Rds")) %>%
  mutate(
    ARM = factor(ARM),
    SEX = factor(SEX)
  ) %>%
  select(
    -c(USUBJID, RACE, ETHNIC, ARMCD, COUNTRY, AVAL_OS, AVALU_OS, AVAL_PFS,
       AVALU_PFS)
  )
trt <- train_data$ARM
y <- train_data$rsp
train_data <- train_data %>%
  select(-ARM,)
x <- model.matrix(rsp ~ ., data = train_data)[, -1]

propensity_func <- function(x, trt) 0.5

augment_func <- function(x, y, trt) {
  data <- data.frame(x, y, trt)
  xm <- model.matrix(y~trt*x-1, data = data)
  
  lmod <- cv.glmnet(y = y, x = xm, family = "binomial")
  ## get predictions when trt = 1
  data$trt <- 1
  xm <- model.matrix(y~trt*x-1, data = data)
  preds_1  <- predict(lmod, xm, s = "lambda.min")
  
  ## get predictions when trt = -1
  data$trt <- -1
  xm <- model.matrix(y~trt*x-1, data = data)
  preds_n1  <- predict(lmod, xm, s = "lambda.min")
  
  ## return predictions averaged over trt
  return(0.5 * (preds_1 + preds_n1))
}

# fit modified covariates approach
aug_mod_cov <- fit.subgroup(
  x = x,
  y = y,
  trt = trt,
  propensity.func = propensity_func,
  loss = "logistic_loss_lasso",
  augment.func = augment_func,
  nfolds = 10
)

# extract the coefficients that have nonzero coefficients and save
coefs <- aug_mod_cov$coefficients
nonzero_interactions <- coefs[which(coefs != 0), ][-1]
saveRDS(
  names(nonzero_interactions),
  file = here("results/aug-mod-cov-selected-interactions.Rds")
)
saveRDS(
  aug_mod_cov,
  file = here("results/aug-mod-cov-fit.Rds")
)
