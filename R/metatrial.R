




# could not get this to play nice
# rma_safely <- purrr::safely(metafor::rma)

# one of emily's fabulous contributions, thank you, emily!
try_rma <- function(code, silent = FALSE) {
  tryCatch(
    code,
    error = function(c) {
      msg <- conditionMessage(c)
      if (!silent)
        message(c)
      invisible(structure(msg, class = "try-error"))
    }
  )
}


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
    ))

  # make sure median and estimated standard error are numeric
  if (!is.numeric(metadata$median) |
      !is.numeric(metadata$median_se) |
      !all(metadata$median > 0) |  # and positive
      !all(metadata$median_se > 0)) {
    results <- NULL
  } else {
    groups <- metadata %>%
      dplyr::select(-min,-max,-mean,-sd,
                    -first_q,-third_q,-iqr,-control_indicator) %>%
      dplyr::arrange(study, group)

    # split simulated data into two dfs for easier calculations
    control <- groups %>%
      dplyr::filter(group == "control")

    intervention <- groups %>%
      dplyr::filter(group == "intervention")

    # meta-analyse the effects of interest
    models <- list(
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
        if (knha == TRUE)
          # default to Knapp-Hartung test
          test = "knha"
        else
          test = "z"

        try_rma(metafor::rma(
          test = test,
          data = ma_df,
          yi = effect,
          sei = effect_se
        ))
      })

    # check if the
    if (class(models) == "try-error") {
      results <- NULL
    } else {
      results <- models %>% {
        tibble::tibble(
          ci_lb = purrr::map_dbl(., "ci.lb"),
          ci_ub = purrr::map_dbl(., "ci.ub"),
          # i2 = purrr::map_dbl(., "I2"),
          tau2 = purrr::map_dbl(., "tau2"),
          b = purrr::map_dbl(., "b"),
          effect_type = names(.)
        )
      } %>%
        dplyr::full_join(true_effect, by = "effect_type") %>%
        dplyr::mutate(in_ci = ci_lb <= true_effect &
                        true_effect <= ci_ub)
    }
  }

  return(results)
}
