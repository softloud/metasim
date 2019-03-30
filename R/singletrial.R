#' Single-study trial for coverage of medians
#'
#' This trial samples one paired set of data and returns the confidence interval, bias, and coverage.
#'
#' NB: bias is effect - true effect.
#'
#' @export

singletrial <- function(
  true_median = 50,
  tau_sq = 0,
  median_ratio = 1.2,
  rdist = "norm",
  parameters = list(mean = 50, sd = 0.2),
  n_df = sim_n(k = 1),
  true_effect = 50
) {

  # calculate true effects
  true_effects <- tibble::tibble(measure = c("m", "lr"),
                                 true_effect = c(true_median,
                                                 log(median_ratio)))

  # simualte data
  ssdata <- sim_stats(
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
     )  %>%
    select(median, median_se, lr, lr_se, median_se_i)

  results <- ssdata %>%
    {
      tibble(
        measure = c("m", "lr"),
        effect = c(.$median, .$lr),
        effect_se = c(.$median_se, .$lr_se),
      )
    } %>%
    full_join(true_effects, by = "measure") %>%
    mutate(ci_lb = effect - qnorm(0.975) * effect_se,
           ci_ub = effect + qnorm(0.975) * effect_se,
           coverage = ci_lb < true_effect & true_effect < ci_ub,
           bias = effect - true_effect,
           tau2 = 1)

  results

}
