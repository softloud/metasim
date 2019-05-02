#' Single-study trial for coverage of medians
#'
#' This trial samples one paired set of data and returns the confidence interval, bias, and coverage.
#'
#' NB: bias is effect - true effect.
#'
#' @export

singletrial <- function(
  measure = "median",
  measure_spread = "iqr",
  tau_sq = 0,
  effect_ratio = 1.2,
  rdist = "norm",
  parameters = list(mean = 50, sd = 0.2),
  n_df = sim_n(k = 1),
  true_effect = 50,
  knha = TRUE
) {

  measures <- c(measure, paste0("lr_", measure))

  # calculate true effects
  true_effects <- tibble::tibble(measure = c("effect", "lr"),
                                 true_effect = c(true_effect,
                                                 log(effect_ratio)))

  # simulate data
  ssdata <- sim_stats(
    measure = measure,
    measure_spread = measure_spread,
    n_df = n_df,
    rdist = rdist,
    par = parameters,
    tau_sq = tau_sq,
    effect_ratio = effect_ratio,
    wide = TRUE
  ) %>%
    mutate(
      effect_se_c = pmap_dbl(
        list(
          centre = effect_c,
          spread = effect_spread_c,
          n = n_c
        ),
        varameta::effect_se,
        centre_type = measure,
        spread_type = measure_spread
      ),
      effect_se_i = pmap_dbl(
        list(
          centre = effect_i,
          spread = effect_spread_i,
          n = n_i
        ),
        varameta::effect_se,
        centre_type = measure,
        spread_type = measure_spread
      ),
      lr = log(effect_i / effect_c),
      lr_se = pmap_dbl(
        list(
          measure = measure,
          n_c = n_c,
          effect_c = effect_c,
          effect_se_c = effect_se_c,
          n_i = n_i,
          effect_i = effect_i,
          effect_se_i = effect_se_i
        ),
        .f = lr_se)
     )  %>%
    rename(effect = effect_c,
           effect_se = effect_se_c) %>%
    mutate(effects = map2(effect, effect_se, list),
           lrs = map2(lr, lr_se, list)) %>%
    select(effects, lrs) %>%
    rename(effect = effects, lr = lrs) %>%
    gather(measure, results) %>%
    mutate(
      effect = map_dbl(results, 1),
      effect_se = map_dbl(results, 2)
    ) %>%
    select(-results) %>%
    full_join(true_effects, by = "measure") %>%
    mutate(conf_low = effect - qnorm(0.975) * effect_se,
           conf_high = effect + qnorm(0.975) * effect_se,
           coverage = conf_low < true_effect & true_effect < conf_high,
           bias = effect - true_effect,
           tau_sq = 0,
           measure = case_when(
             measure == "effect" ~ measures[[1]],
             measure == "lr" ~ measures[[2]]
           ))

  # results

  ssdata
}
