################################################################################
# Apply unicate
################################################################################

library(uniCATE)

apply_unicate <- function(sample_tbls, p) {

  biomarker_names <- paste0("W", seq_len(p))

  # apply unicate to each sample in the tibble of samples
  sample_tbls %>%
    mutate(
      method = "unicate",
      results = future_map(
        sample,
        unicate,
        outcome = "Y",
        treatment = "A",
        covariates = biomarker_names,
        biomarkers = biomarker_names,
        propensity_score_ls = list("treatment" = 0.5, "control" = 0.5),
        v_folds = 5L,
        .options = furrr_options(seed = TRUE)
      )
    )

}
