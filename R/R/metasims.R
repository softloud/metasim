#' coverage paobability simulations of an estimator for various
#'
#' @param distributions a dataframe with a `dist` column of R distributions, i.e., norm, exp, pareto, and a list-column `par` of parameter sets. Defaults to [default_parmetres]
#' @param single_study When set to TRUE, will override simulation parameters with k = 1 and tau2_true = 0.
#' @param k Simulate for different numbers of studies.
#' @param tau2_true Variance \eqn{\gamma_k \sim N(0, \tau^2)} associated with the random effect
#' @param median_ratio \eqn{\nu_I / \nu_C := \rho} where \eqn{\rho} denotes the ratio of medians.
#' @param beep Turn on beep alert when successfully finished.
#' @param progress Turn progress bar on and off.
#' @inheritParams sim_n
#'
#' @export

metasims <- function(single_study = FALSE,
                     measure = "median",
                     meausure_spread = "iqr",
                     distributions = default_parameters,
                     k = c(3, 7, 10),
                     tau2_true = seq(from = 0, to = 0.4, by = 0.2),
                     effect_ratio = c(1, 1.2),
                     min_n = 20,
                     max_n = 200,
                     prop = 0.5,
                     prop_error = 0.1,
                     trials = 3,
                     trial_fn = metatrial,
                     beep = FALSE,
                     loop_output = FALSE,
                     progress = TRUE) {


  # set variance between studies to 0 when there's only one study
  if (isTRUE(single_study)) {
    k <- 1
    tau2_true <- 0
  }

  # set up simulation parameters
  simpars <- sim_df(
    dist_tribble = distributions,
    k = k,
    tau2 = tau2_true,
    effect_ratio = effect_ratio,
    min_n = min_n,
    max_n = max_n,
    prop = prop,
    prop_error = prop_error
  )

  # set progress bar
  if (isTRUE(progress)) {
    cat(paste(
      "\nperforming ",
      nrow(simpars),
      " simulations of ",
      trials,
      " trials\n"
    ))}

  # output
  # return(results_df)
  # solution for simulation-level arguments ---------------------------------

  results <- simpars %>%
    mutate(sim_results = pmap(
      list(
        tau_sq = tau2_true,
        effect_ratio = effect_ratio,
        rdist = rdist,
        parameters = parameters,
        n_df = n,
        true_effect = true_effect,
        id = id
      ),
      metasim,
      trials = trials,
      trial_fn = trial_fn,
      knha = knha
    ))

  # bind all the output together
  # tried to use unnest to do this, but to no avail
  output <- results %>%
    pluck("sim_results") %>%
    bind_rows() %>%
    full_join(results, by = "id")


  if (isTRUE(progress)) cat("\n")

  if (isTRUE(beep)) beepr::beep("treasure")

  return(output)

}
