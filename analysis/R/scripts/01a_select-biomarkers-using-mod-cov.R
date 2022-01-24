################################################################################
# 01b. Select Interactions Using the Modified Covariate Approach
################################################################################

# In this script, the modified covariate approach is used to identify the
# biomarkers that best explain response.

# load the required libraries
library(here)
library(dplyr)
library(personalized)

set.seed(612341)

# load and prepare the training data
train_data <- readRDS(here("data/processed-IMmotion150.Rds")) %>%
  mutate(
    ARM = factor(ARM),
    SEX = factor(SEX),
    rsp = if_else(rsp == "Response", 1, 0),
    ARM = if_else(ARM == "SUNITINIB", 0, 1)
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

# fit modified covariates approach
mod_cov <- fit.subgroup(
  x = x,
  y = y,
  trt = trt,
  propensity.func = propensity_func,
  loss = "logistic_loss_lasso",
  nfolds = 10
)

# extract the coefficients that have nonzero coefficients and save
nonzero_interactions <- mod_cov$coefficients[which(mod_cov$coefficients != 0), ][-1]
saveRDS(
  names(nonzero_interactions),
  file = here("results/mod-cov-selected-interactions.Rds")
)
saveRDS(
  mod_cov,
  file = here("results/mod-cov-fit.Rds")
)
