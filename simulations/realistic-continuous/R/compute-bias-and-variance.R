################################################################################
# Compute Bias and Variance of an Estimator When Applied to a DGP
################################################################################


bias_and_var_plot <- function(unicate_res, dgp_type, effect_vector, title) {

  # compute the bias of the coefficients
  bias_tbl <- unicate_res %>%
    filter(dgp == dgp_type) %>%
    mutate(
      effects = list(effect_vector),
      estimates = map2(results, p, function(x, p){
        x %>%
          mutate(biomarker = factor(
            biomarker, levels = paste0("W", seq_len(p))
          )
          ) %>%
          arrange(biomarker) %>%
          pull(coef)
      }
      ),
      bias = map2(estimates, effects, function(x, params){
        x - params
      }
      )
    ) %>%
    select(size, estimates, bias)

  # create vectors
  bias_125 <- bias_tbl %>%
    filter(size == 125) %>%
    pull(bias) %>%
    bind_rows() %>%
    colMeans()
  var_125 <- bias_tbl %>%
    filter(size == 125) %>%
    pull(estimates) %>%
    bind_rows() %>%
    as.matrix() %>%
    colVars()
  bias_250 <- bias_tbl %>%
    filter(size == 250) %>%
    pull(bias) %>%
    bind_rows() %>%
    colMeans()
  var_250 <- bias_tbl %>%
    filter(size == 250) %>%
    pull(estimates) %>%
    bind_rows() %>%
    as.matrix() %>%
    colVars()
  bias_500 <- bias_tbl %>%
    filter(size == 500) %>%
    pull(bias) %>%
    bind_rows() %>%
    colMeans()
  var_500 <- bias_tbl %>%
    filter(size == 500) %>%
    pull(estimates) %>%
    bind_rows() %>%
    as.matrix() %>%
    colVars()

  # create indicator of predictiveness
  predictiveness <- c(rep("Predictive", 4), rep("Nonpredictive", 496))

  # create plots
  p <- 500
  bias_tbl <- tibble(
    "value" = c(bias_125, bias_250, bias_500),
    "biomarker" = rep(seq_len(p), 3),
    "sample_size" = rep(c("n = 125", "n = 250", "n = 500"), each = p),
    "predictiveness" = rep(predictiveness, 3),
    "type" = "Bias"
  )
  var_tbl <- tibble(
    "value" = c(var_125, var_250, var_500),
    "biomarker" = rep(seq_len(p), 3),
    "sample_size" = rep(c("n = 125", "n = 250", "n = 500"), each = p),
    "predictiveness" = rep(predictiveness, 3),
    "type" = "Variance"
  )

  bias_tbl %>%
    bind_rows(var_tbl) %>%
    mutate(
      predictiveness = factor(
        predictiveness, levels = c("Predictive", "Nonpredictive")
      )
    ) %>%
    ggplot(aes(x = biomarker, y = value, colour = predictiveness)) +
      geom_point(alpha = 0.5, size = 0.8) +
      xlab("Biomarker Index") +
      ylab("") +
      ggtitle(title) +
      facet_grid(vars(type), vars(sample_size), scales = "free_y") +
      scale_colour_viridis_d(name = "Biomarker Type", option = "E", end = 0.8) +
      theme_bw()

}
