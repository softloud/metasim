#' Second attempt at metatrial function
#'
#' This time, let's try making function so that it can be wrapped in safely at
#' the metasim level.
#'
#' @param true_effect The value of the control population median.
#' @inheritParams sim_stats
#' @param test "knha" or "z" for [metafor::rma].
#'
#' @export

anothertrial <- function(true_median = 50,
                         tau_sq = 0.6,
                         median_ratio = 1.2,
                         rdist = "norm",
                         parameters = list(mean = 50, sd = 0.2),
                         n_df = sim_n(k = 3),
                         knha = TRUE,
                         true_effect = 50,
                         test = "knha"
                         ) {
  # calculate true effects
  true_effects <- tibble::tibble(measure = c("m", "lr"),
                                 true_effects = c(true_median,
                                                  log(median_ratio)))

  # simualte data
  metadata <- sim_stats(
    n_df = n_df,
    rdist = rdist,
    par = parameters,
    tau_sq = tau_sq,
    median_ratio = median_ratio,
    wide = TRUE
  ) %>%
    mutate(
      median = median_c,
      median_se = pmap_dbl(
        list(
          centre = median_c,
          spread = iqr_c,
          n = n_c
        ),
        varameta::effect_se,
        centre_type = "median",
        spread_type = "iqr"
      ),
      median_se_i = pmap_dbl(
        list(
          centre = median_i,
          spread = iqr_i,
          n = n_i
        ),
        varameta::effect_se,
        centre_type = "median",
        spread_type = "iqr"
      ),
      lr = log(median_i / median_c),
      lr_se = sqrt(median_se^2 / median^2 + median_se_i / median_i^2)
    ) %>%
    select(study, median, median_se, lr, lr_se, median_se_i)

  # meta-analyse


  list(true_effects,
       metadata)
}
