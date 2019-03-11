

unsafe_metatrial <- function(tau = 0.6,
                      median_ratio = 1.2,
                      rdist = "norm",
                      parameters = list(mean = 50, sd = 0.2),
                      n_df = sim_n(k = 3),
                      knha = TRUE,
                      true_effect = 50) {

  # calculate true effects
  true_effect <-  tibble::tibble(
    measure = c("m", "md", "lr"),
    true_effect = c(
      true_effect,
      true_effect * median_ratio - true_effect,
      log(median_ratio)
    )
  )

  # check that true effects are non-negative
  # assertthat::assert_that(all(true_effect$true_effect > 0),
  #                         msg =
  #                           "haven't coded this for negative true effects yet")


  # simulate data
  # todo: this is where a safely/collateral thing might be good
  metadata <- sim_stats(
      n_df = n_df,
      rdist = rdist,
      par = parameters,
      tau = tau,
      median_ratio = median_ratio
    )

  # assertthat::assert_that(metadata %>% nrow() > 2,
  #                         msg = "metadata does not have 2 rows;
  #                         failed to sample data.")

  # return error if sample couldn't be generated
  # Q: Why doesn't this assert work as I think it should?
  # assertthat::assert_that(
  #   is.null(metadata),
  #   msg = "distribution and parameters failed to sample")

    groups <- metadata %>%
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
      dplyr::select(-min,-max,-mean,-sd, -first_q,-third_q,-iqr) %>%
      dplyr::arrange(study, group)

    # split simulated data into two dfs for easier calculations
    control <- groups %>%
      dplyr::filter(group == "control")

    intervention <- groups %>%
      dplyr::filter(group == "intervention")

    # calculate effects of interest
    models <- list(
      # median
      m = control %>%
        dplyr::select(-n,-group) %>%
        dplyr::rename(effect = median,
                      effect_se = median_se) %>%
        dplyr::mutate(effect_type = "m") %>%
        select(-this_study_error),

      # difference of medians
      md = tibble::tibble(
        study = paste0("study_", seq(1, nrow(control))),
        effect = abs(intervention$median - control$median),
        effect_se = sqrt(control$median_se ^ 2 + intervention$median_se ^ 2),
        effect_type = "md"
      ),

      # log-ratio of medians
      lr = tibble::tibble(
        study = paste0("study_", seq(1, nrow(control))),
        effect = log(intervention$median / control$median),
        effect_se = sqrt(
          control$median_se ^ 2 / control$median ^ 2 +
            intervention$median_se ^ 2 / intervention$median ^ 2
        ),
        effect_type = "lr"
      )
    )

    # meta-analyse with rma or fe
    model_results <- models %>% {
      tibble::tibble(measure = names(.),
             rma = purrr::map(., function(ma_df) {
               if (knha == TRUE)
                 # default to Knapp-Hartung test
                 test = "knha"
               else
                 test = "z"

               toss(
                 metafor::rma(
                   test = test,
                   data = ma_df,
                   yi = effect,
                   sei = effect_se
                 ),
                 error_msg = "metafor::rma reml threw an error",
                 warning_msg = "metafor::rma reml threw a warning"
               )
             })) %>%
        dplyr::mutate(fe = if_else(
          is.character(rma),
          toss(
            metafor::rma(
              method = "FE",
              test = test,
              data = ma_df,
              yi = effect,
              sei = effect_se
            ),
            error_msg = "metafor::rma fe threw an error",
            warning_msg = "metafor::rma fe threw a warning"
          ),
          "rma worked"
        ))
    }
  #
  #   # assertthat::assert_that(is.data.frame(model_results),
  #   #                         msg = "model_results failed to instantiate")
  #
  #
  # # extract the models that ran
  results_first <- model_results %>%
    dplyr::filter(!(is.character(rma) & is.character(fe))) %>%
    dplyr::mutate(results = map2(
      rma,
      fe,
      .f =
        function(rma, fe) {
          if (is.list(rma))
            return (rma)
          else
            return(fe)
        }
    )) %>%
    dplyr::select(-rma, -fe)

   # probably can join this into a pipe later
  results <- results_first %>%
    purrr::pluck("results") %>%
    map_df(metabroom::tidy) %>%
     mutate(
      method = results_first %>% pluck("results") %>% map_chr("method"),
      measure = results_first %>% pluck("measure")
    ) %>%
    full_join(true_effect, by = "measure") %>%
    mutate(coverage = ci_lb < true_effect & true_effect < ci_ub,
           bias = true_effect - effect)

  ma_errors <- model_results %>%
    dplyr::filter(is.character(rma) & is.character(fe))

  return(list(results = results, ma_errors = ma_errors))
}

# then make it safe
safely_metatrial <- purrr::safely(unsafe_metatrial)


#' one trial
#'
#' @inheritParams sim_stats
#' @param knha Knapp-Hartung test instead of the
#' default "z" test in `metafor::rma`.
#'
#' @import metafor
#' @import purrr
#' @import tibble
#' @import dplyr
#' @import assertthat
#'
#' @export
#'

metatrial <- function(...){
  safely_metatrial(...) %>% {
    list(
      results = pluck(., "result", "results"),
      errors = list(
        model = pluck(., "result", "ma_errors"),
        safely = pluck(., "error")
      )
      )}
}
