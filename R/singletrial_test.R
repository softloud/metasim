#' testing singletrial function
#'
#' @export

singletrial_test <- function(
  tau_sq = 0,
  median_ratio = 1.2,
  rdist = "norm",
  parameters = list(mean = 50, sd = 0.2),
  n_df = sim_n(k = 1),
  true_effect = 50,
  knha = TRUE
) {

  # calculate true effects
  true_effects <- tibble::tibble(measure = c("m", "lr"),
                                 true_effect = c(true_effect,
                                                 log(median_ratio)))

  # simulate data
  ssdata <- sim_stats(
    n_df = n_df,
    rdist = rdist,
    par = parameters,
    tau_sq = tau_sq,
    median_ratio = median_ratio,
    wide = TRUE
  ) %>% mutate(
    mean = mean_c,
    sd = sd_c,
    lr = log(mean_i / mean_c),
    lr_se = sqrt(sd_c^2 / median^2 + sd_i^2 / median_i^2)
  )  %>%
    select(mean_c, sd_c, lr, lr_se)

  results <- ssdata %>%
    {
      tibble(
        measure = c("m", "lr"),
        effect = c(.$mean, .$lr),
        effect_se = c(.$sd, .$lr_se),
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
