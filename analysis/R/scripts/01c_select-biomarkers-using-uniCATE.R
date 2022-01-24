################################################################################
# 01c. Select Interactions Using the Semiparametric Approach
################################################################################

# In this script, a penalized logistic regression model is used to identify the
# genes that best explain response.

# load the required libraries
library(here)
library(dplyr)
library(future)
library(uniCATE)

set.seed(2342337)

plan(multicore, workers = 5L)

# load and prepare the training data
train_data <- readRDS(here("data/processed-IMmotion150.Rds")) %>%
  mutate(
    SEX = factor(SEX),
    ARM = factor(ARM, levels = c("SUNITINIB", "MPDL3280A AND BEVACIZUMAB")),
    rsp = if_else(rsp == "Response", 1, 0)
  ) %>%
  select(
    -c(USUBJID, RACE, ETHNIC, ARMCD, COUNTRY, AVAL_OS, AVALU_OS, AVAL_PFS,
       AVALU_PFS)
  )

# define the required arguments
var_names <- colnames(train_data)
covariates <- var_names[which(var_names != "rsp" & var_names != "ARM")]
biomarkers <- covariates[which(covariates != "SEX" & covariates != "AGE")]
propensity_score_ls <- list(
  "SUNITINIB" = 0.5, "MPDL3280A AND BEVACIZUMAB" = 0.5
)

# identify the biomarkers that are associated with the outcome based on the risk
# difference
results <- unicate(
  train_data,
  outcome = "rsp",
  treatment = "ARM",
  covariates = covariates,
  biomarkers = biomarkers,
  propensity_score_ls = propensity_score_ls,
  v_folds = 5L,
  parallel = TRUE
)


# save the biomarkers that interact with the response
saveRDS(
  results,
  file = here("results/uniCATE-results.Rds")
)
