#' Single-study trial for coverage of medians
#'
#' This trial samples one paired set of data and returns the confidence interval, bias, and coverage.
#'
#' NB: bias is effect - true effect.
#'
#' @export

singletrial <- function(
  measure = "median",
  meausure_spread = "iqr",
  tau_sq = 0,
  effect_ratio = 1.2,
  rdist = "norm",
  parameters = list(mean = 50, sd = 0.2),
  n_df = sim_n(k = 1),
  true_effect = 50,
  knha = TRUE
) {

  # calculate true effects
  true_effects <- tibble::tibble(measure = c("m", "lr"),
                                 true_effect = c(true_effect,
                                                 log(effect_ratio)))

  # simulate data
  ssdata <- sim_stats(
    n_df = n_df,
    rdist = rdist,
    par = parameters,
    tau_sq = tau_sq,
    effect_ratio = effect_ratio,
    wide = TRUE
  ) %>%
    mutate(
      effect = measure,
      effect_se = pmap_dbl(
        list(
          centre = effect,
          spread = effect_se_c,
          n = n_c
        ),
        varameta::effect_se,
        centre_type = "measure",
        spread_type = "measure_spread"
      ),
      effect_se_i = pmap_dbl(
        list(
          centre = effect_i,
          spread = effect_se_i,
          n = n_i
        ),
        varameta::effect_se,
        centre_type = "measure",
        spread_type = "measure_spread"
      ),
      lr = log(effect_i / effect_c),
      lr_se = sqrt(effect_se^2 / effect^2 + effect_se_i / effect_i^2)
     )  %>%
    select(median, effect_se, lr, lr_se, effect_se_i)

  results <- ssdata %>%
    {
      tibble(
        measure = c("m", "lr"),
        effect = c(.$effect, .$lr),
        effect_se = c(.$effect_se, .$lr_se),
      )
    } %>%
    full_join(true_effects, by = "measure") %>%
    mutate(conf_low = effect - qnorm(0.975) * effect_se,
           conf_high = effect + qnorm(0.975) * effect_se,
           coverage = conf_low < true_effect & true_effect < conf_high,
           bias = effect - true_effect,
           tau_sq = 0)

  results

}
