#' calculate the standard error of the log-ratio of two random samples
#'
#' Provides sample estimator for the squared-root of the variance of the log-ratio of two random samples
#'
#' @param
#'

lr_se <- function(measure, n_c, effect_c, effect_se_c, n_i, effect_i, effect_se_i) {
  case_when(
    measure == "median" ~ sqrt(effect_se_c ^ 2 / effect_c ^ 2 +
                                 effect_se_i ^ 2 / effect_i ^ 2),
    measure == "mean" ~ sqrt(1 / sum(n_c, n_i) *
                               (1 - ((sum(n_c, n_i) - 1) / (sum(n_c, n_i) - 1))) *
                               1 / effect_c^2 *
                               ((effect_i/effect_c)^2 * effect_se_c^2 +
                                  effect_se_i^2)))
}
