#' Generate meta-analysis data and calculate estimator
#'
#' Simulate data based on simulation parameters and meta-analyse.
#'
#' NB: bias is effect - true effect.
#'
#' @param true_effect The value of the control population median.
#' @inheritParams sim_stats
#' @param test "knha" or "z" for [metafor::rma].
#'
#' @export

metatrial <- function(measure = "median",
                      measure_spread = "iqr",
                      tau_sq = 0.6,
                      effect_ratio = 1.2,
                      rdist = "norm",
                      parameters = list(mean = 50, sd = 0.2),
                      n_df = sim_n(k = 3),
                      knha = TRUE,
                      true_effect = 50,
                      test = "knha") {
  measures <- c(measure, paste0("lr_", measure))

  # calculate true effects
  true_effects <- tibble::tibble(measure = measures,
                                 true_effect = c(true_effect,
                                                 log(effect_ratio)))
  # # simulate data
  metadata <- sim_stats(
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
      ) ,
      lr = log(effect_i / effect_c),
      # to do: have a look at variance for log-ratio of means, might just
      # need to specify this component for means if else
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
      .f = lr_se)) %>%
  select(study, effect_c, effect_se_c, lr, lr_se) %>%
  mutate(effect = map2(effect_c, effect_se_c,
                       .f = function(x, y) list(x, y))) %>%
  select(-effect_c, -effect_se_c) %>%
  rename(lr_value = lr) %>%
  mutate(lr = map2(lr_value, lr_se, list)) %>%
  select(-lr_value, -lr_se) %>%
  gather(key = measure, value = obs, effect, lr) %>%
  mutate(centre = map_dbl(obs, 1),
         error = map_dbl(obs, 2)) %>%
  select(-obs) %>%
  group_nest(measure) %>%
  mutate(model = map(data, .f = function(df){
    metamodel(df %>% pluck(2), df %>% pluck(3))
  })) %>%
  select(-data) %>%
  unnest(model) %>%
  # now to rename with informative effect labels
  mutate(measure = case_when(measure == "effect" ~ measures[[1]],
                             measure == "lr" ~ measures[[2]])) %>%
  full_join(true_effects, by = "measure") %>%
  mutate(
    coverage = conf_low < true_effect & true_effect < conf_high,
    # can't scale bc log(1) = 0
    bias = effect - true_effect,
    scaled_bias = case_when(
      measure == measures[[1]] ~ bias / true_effect,
      measure == measures[[2]] ~ bias
    )
  )

return(metadata)
}
