#' Plot ratio-comparisons
#'
#' Example plot, by rights should be in varameta - todo:
#'
#' Right now it' convenient to have this function here.
#'
#' @export

estimator_comparison <- function() {
  varameta::sim_par %>%
    filter(sample_size == 15) %>%  # ns cancel, so only need to consider on sample size.
    mutate(median_se = pmap_dbl(
      list(true_median, rdensity, par_1, par_2, sample_size),
      .f = function(true_median, rdensity, par_1, par_2, sample_size) {
        if (is.null(par_2)) 1 / (2 * sqrt(sample_size) * rdensity(true_median, par_1))
        else 1 / (2 * sqrt(sample_size) * rdensity(true_median, par_1, par_2))
      }
    )) %>% mutate(
      # calculate estimator
      exp = map2_dbl(true_median, sample_size, varameta:::g_exp),
      cauchy = pmap_dbl(list(sample_size, true_median,  true_iqr), varameta:::g_cauchy),
      norm = pmap_dbl(list(sample_size, true_median,  true_iqr), varameta:::g_norm),
      lnorm = pmap_dbl(list(sample_size, true_median,  true_iqr), varameta:::g_lnorm)
      # lnorm = 1
    ) %>% # go longform!
    gather(key = "estimator",
           value = "estimate",
           exp, cauchy, norm, lnorm) %>%
    mutate(ratio = estimate / median_se)
}
