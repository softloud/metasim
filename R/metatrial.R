rma_safely <- purrr::safely(metafor::rma)

#' one trial
#'
#' @inheritParams sim_stats
#' @param knha Knapp-Hartung test instead of the default "z" test in `metafor::rma`.
#'
#' @export

metatrial <- function(tau = 0.6,
                      median_ratio = 1.2,
                      rdist = "norm",
                      parameters = list(mean = 50, sd = 0.2),
                      n_df = sim_n(k = 3),
                      knha = TRUE,
                      true_effect = 50) {
  # calculate true effects
  true_effect <-  tibble::tibble(
    effect_type = c("m", "md", "lr"),
    true_effect = c(
      true_effect,
      true_effect * median_ratio - true_effect,
      log(median_ratio)
    )
  )


  # simulate data
  metadata <- sim_stats(
    n_df = n_df,
    rdist = rdist,
    par = parameters,
    tau = tau,
    median_ratio = median_ratio,
  ) %>%
    dplyr::mutate(median_se = purrr::pmap_dbl(
      list(
        centre = median,
        spread = iqr,
        n = n,
        centre_type = "median",
        spread_type = "iqr"
      ),
      .f = varameta::effect_se
    )) %>%
    dplyr::select(-min,-max,-mean,-sd,-first_q,-third_q,-iqr,-control_indicator) %>%
    dplyr::arrange(study, group)

  # split simulated data into two dfs for easier calculations
  control <- metadata %>%
    dplyr::filter(group == "control")

  intervention <- metadata %>%
    dplyr::filter(group == "intervention")

  # meta-analyse the effects of interest
  results <- list(
    m = control %>%
      dplyr::select(-n,-group) %>%
      dplyr::rename(effect = median,
                    effect_se = median_se) %>%
      dplyr::mutate(effect_type = "m"),
    md = tibble::tibble(
      study = paste0("study_", seq(1, nrow(control))),
      effect = abs(intervention$median - control$median),
      effect_se = sqrt(control$median_se ^ 2 + intervention$median_se ^ 2),
      effect_type = "md"
    ),
    lr = tibble::tibble(
      study = paste0("study_", seq(1, nrow(control))),
      effect = log(intervention$median / control$median),
      effect_se = sqrt(
        control$median_se ^ 2 / control$median ^ 2 +
          intervention$median_se ^ 2 / intervention$median ^ 2
      ),
      effect_type = "lr"
    )
  ) %>%
    purrr::map(function(ma_df) {
      # default to Knapp-Hartung test
      if (knha == TRUE)
        test = "knha"
      else
        test = "z"

      # rma_safely(
      #   test = test,
      #   data = ma_df,
      #   yi = effect,
      #   sei = effect_se
      # ) %>% pluck("results")

      ## metafor::rma(test = test, data = ma_df, yi = effect, sei = effect_se)
      metafor::rma(test = test, data = ma_df, yi = effect, sei = effect_se)

      })

  # if (!is.null(results)) {
  #   results %>%
  #     {
  #       tibble::tibble(
  #         ci_lb = purrr::map_dbl(., "ci.lb"),
  #         ci_ub = purrr::map_dbl(., "ci.ub"),
  #         i2 = purrr::map_dbl(., "I2"),
  #         tau2 = purrr::map_dbl(., "tau2"),
  #         b = purrr::map_dbl(., "b"),
  #         effect_type = names(.)
  #       )
  #     } %>%
  #     dplyr::full_join(true_effect, by = "effect_type") %>%
  #     dplyr::mutate(in_ci = ci_lb < true_effect &
  #                     true_effect < ci_ub)} else {
  #     results
  #   }

    results

  }
