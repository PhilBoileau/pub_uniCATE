################################################################################
# Compute Biomarker Classification Results
################################################################################

# Input:
#   results: A named list containing a vector of treatment-biomarker interaction
#     effect estimates.
#   biomarkers: A vector indicating which biomarkers that have large
#     population effects for treatment-biomarker interactions based on the cutoff
#     argument.
#   method: A character indicating the method used to generate the results
# Output:
#   A numeric indicating the true negative rate for the given
#   treatment-biomarker interaction effect estimates.
compute_tnr <- function(results, biomarkers, method) {

  # get the coefficient classifications
  if (method != "unicate") {
    inter_coef_hits <- results != 0
  } else {
    inter_coef_hits <- results < 0.05
  }

  # compare the interaction coefficients with the biomarker vector
  true_neg <- sum(if_else(inter_coef_hits == 0 & biomarkers == 0, 1, 0))
  all_neg <- sum(biomarkers == 0)

  # return the tnr
  return(true_neg / all_neg)
}


# Input:
#   results: A named list containing a vector of treatment-biomarker interaction
#     effect estimates.
#   biomarkers: A vector indicating which biomarkers that have large
#     population effects for treatment-biomarker interactions based on the cutoff
#     argument.
#   method: A character indicating the method used to generate the results
# Output:
#   A numeric indicating the true positive rate for the given
#   treatment-biomarker interaction effect estimates.
compute_tpr <- function(results, biomarkers, method) {

  # get the coefficient classifications
  if (method != "unicate") {
    inter_coef_hits <- results != 0
  } else {
    inter_coef_hits <- results < 0.05
  }

  # compare the interaction coefficients with the biomarker vector
  true_pos <- sum(if_else(inter_coef_hits != 0 & biomarkers == 1, 1, 0))
  all_pos <- sum(biomarkers == 1)

  # return the tnr
  return(true_pos / all_pos)
}


# Input:
#   results: A named list containing a vector of treatment-biomarker interaction
#     effect estimates.
#   biomarkers: A vector indicating which biomarkers that have large
#     population effects for treatment-biomarker interactions based on the cutoff
#     argument.
#   method: A character indicating the method used to generate the results
# Output:
#   A numeric indicating the false discovery proportion for the given
#   treatment-biomarker interaction effect estimates.
compute_fdp <- function(results, biomarkers, method) {

  # get the coefficient classifications
  if (method != "unicate") {
    inter_coef_hits <- results != 0
  } else {
    inter_coef_hits <- results < 0.05
  }

  # compare the interaction coefficients with the biomarker vector
  false_pos <- sum(if_else(inter_coef_hits != 0 & biomarkers == 0, 1, 0))
  pred_pos <- sum(inter_coef_hits != 0)

  # compute the fdp
  if (pred_pos != 0)
    fdp <- false_pos / pred_pos
  else
    fdp <- 0

  # return the tnr
  return(fdp)
}

# Input:
#   res_tbl: The combined tibble of results output by the candidate methods
#     after being applied to the simulated samples.
#   simple: A flag indicating whether the DGP is simple or realistic.
# Output:
#   A tibble reporting the true negative rate (tnr), true positive rate (tpr),
#   and false discovery proportion (fdp) with respect to the DGP for each
#   simulated dataset and each method.
compute_classification_results <- function(res_tbl, simple = TRUE) {

  res_tbl %>%
    mutate(
      biomarkers = if_else(p == 100 & simple, list(c(rep(1, 50), rep(0, 50))),
                     if_else(p == 500 & simple, list(c(rep(1, 5), rep(0, 495))),
                             list(c(rep(1, 4), rep(0, 496))))),
      tnr = unlist(pmap(list(results, biomarkers, method), compute_tnr)),
      tpr = unlist(pmap(list(results, biomarkers, method), compute_tpr)),
      fdp = unlist(pmap(list(results, biomarkers, method), compute_fdp))
    ) %>%
    dplyr::select(dgp, sim_idx, size, p, method, tnr, tpr, fdp)

}
