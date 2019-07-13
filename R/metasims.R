#' metasims
#'
#' coverage probability simulations of an estimator for various
#'
#' @param single_study Set this to TRUE for a simulation of single samples; i.e., one control and one intervention group.
#' @param measure Calculate sample median or mean. Defaults to median.
#' @param measure_spread Specify "iqr", "range", "sd", "var". Will have to check how many of these are done right now.
#' @param k Vector of desired numbers studies to simulate for.
#' @param tau_sq_true
#' @param distributions a dataframe with a `dist` column of R distributions, i.e., norm, exp, pareto, and a list-column `par` of parameter sets. Defaults to [default_parmetres]
#' @param single_study When set to TRUE, will override simulation parameters with k = 1 and tau_sq_true = 0.
#' @param k Simulate for different numbers of studies.
#' @param tau_sq_true Variance \eqn{\gamma_k \sim N(0, \tau^2)} associated with the random effect
#' @param unequal_effect_ratio \eqn{\nu_I / \nu_C := \rho} where \eqn{\rho = 1} and whatever arguments passed to this. Defaults to 1.2, so that the cases \eqn{\rho = 1, 1.2} are considered.
#' @param beep Turn on beep alert when successfully finished.
#' @param progress Turn progress bar on and off.
#' @inheritParams sim_n
#'
#' @export

metasims <- function(single_study = FALSE,
                     measure = "median",
                     measure_spread = "iqr",
                     distributions = default_parameters,
                     k = c(3, 7, 10),
                     tau_sq_true = seq(from = 0, to = 0.4, by = 0.2),
                     unequal_effect_ratio = 1.2,
                     min_n = 20,
                     max_n = 200,
                     prop = 0.5,
                     prop_error = 0.1,
                     trials = 3,
                     trial_fn = metatrial,
                     beep = FALSE,
                     knha = TRUE,
                     progress = TRUE) {
  # simulation-level parameters ---------------------------------------------

  # Ultimately, I'd love to extract this as a function, but I'm not sure how.
  match_call <- match.call()
  names_match_call <- names(match_call)

  specified_args <- if (!is.null(names_match_call)) {
    as.list(match_call)[-1] %>%  {
      tibble(argument = names(.),
             specified = as.list(.))

    }
  } else {
    "no specified args"
  }

  defaults <- formals() %>% {
    tibble(argument = names(.),
           default = as.list(.))
  }


  arguments <- if (!is.character(specified_args)) {
    defaults %>%
      full_join(specified_args, by = "argument") %>%
      mutate(
        default_flag = map_lgl(specified, is.null),
        value_raw = if_else(default_flag,
                            default,
                            specified)
      ) %>%
      arrange(argument) %>%
      mutate(
        value = case_when(
          class(value_raw) == "language" ~ eval(value_raw),
          TRUE ~ value_raw
        ),
        value = as.character(eval(value))
      )

  } else {
    defaults %>% rename(value = default)
  }


  # simulation-level tweaks -------------------------------------------------

  # include the case where the control and median are equal
  effect_ratio_values <- c(1, unequal_effect_ratio) %>%
    as.numeric()

  # to avoid confusion with measure column
  measure_string <- measure

  # set variance between studies to 0 whenthere's only one study
  if (isTRUE(single_study)) {
    k <- 1
    tau_sq_true <- 0
  }


  # instantiate simulation --------------------------------------------------


  # set up simulation parameters
  simpars <- sim_df(
    dist_tribble = distributions,
    k = k,
    tau2 = tau_sq_true,
    effect_ratio = effect_ratio_values,
    min_n = min_n,
    max_n = max_n,
    prop = prop,
    prop_error = prop_error
  )


  # progress bar ------------------------------------------------------------

  # set progress bar
  if (isTRUE(progress)) {
    cat(paste(
      "\nperforming ",
      nrow(simpars),
      " simulations of ",
      trials,
      " trials\n"
    ))

    pb <- progress_estimated(nrow(simpars))

    simulation <- function(...) {
      pb$tick()$print()
      metasim(...)
      # you can add additional operations on data_read, or
      # decide on entirely different task that this function should do.
    }
  } else {
    simulation <- metasim
  }


  # run simulation ----------------------------------------------------------

  # # simulate
  simulations <- simpars  %>%
    mutate(
      sim_results = pmap(
        list(
          tau_sq = tau_sq_true,
          effect_ratio = effect_ratio,
          rdist = rdist,
          parameters = parameters,
          n_df = n,
          true_effect = true_effect,
          id = id
        ),
        simulation,
        measure = measure,
        measure_spread = measure_spread,
        trials = trials,
        trial_fn = trial_fn,
        knha = knha
      )
    )

  results <- simulations %>%
    pluck("sim_results") %>%
    bind_rows() %>%
    full_join(simulations, by = "id") %>%
    mutate(effect_ratio = map2_chr(
      measure,
      effect_ratio,
      .f = function(x, y) {
        output <- if (x == measure_string) {
          NA
        } else {
          y
        }
        return(output)
      }
    ))


  # wrangle simulation output------------------------------------------------

  dist_out <- distributions
  class(dist_out) <- c("data.frame", "distributions")

  # bind all the output together
  # tried to use unnest to do this, but to no avail
  sim <- list(
    results = results,
    arguments = arguments,
    sim_pars = simpars,
    distributions = dist_out
  )

  # define simulation class
  class(sim) <- if_else(!isTRUE(single_study),
                        "sim_ma",
                        "sim_ss") %>% c("list")

  # at the end --------------------------------------------------------------

  if (isTRUE(progress))
    cat("\n")

  if (isTRUE(beep))
    beepr::beep("treasure")

  return(sim)
}
