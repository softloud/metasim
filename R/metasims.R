#' simulate over all
#'
#' @param k Simulate for different numbers of studies.
#' @param between_study_variation Variance \eqn{\tau^2} associated with the random effect \eqn{\gamma_k} describing the deviation of the \eqn{k)th results.
#' @param prop Proportion of sample size we expect to cohorts to vary by most of the time
#' @param median_ratio \eqn{\nu_I / \nu_C := \rho} where \eqn{\rho} denotes the ratio of medians.
#' @param probar Turn progress bar on and off.
#'
#' @export

metasims <- function(distributions =default_parameters,
                     k = c(3, 7, 10),
                     between_study_variation = seq(from = 0, to = 0.4, by = 0.2),
                     median_ratio = c(1, 1.2),
                     prop = 0.3,
                     trials = 10,
                     trial_fn = metatrial,
                     probar = TRUE) {
  # set up simulation parameters
    dist_tribble = distributions,
  simpars <- sim_df(
    k = k,
    between_study_variation = between_study_variation,
    median_ratio = median_ratio,
    prop = prop
  )

  # cat(paste(
  #   "performing ",
  #   nrow(simpars),
  #   " simulations of ",
  #   trials,
  #   " trials\n"
  # ))

  # set progress bar
  if (isTRUE(probar)) {
    pb <- txtProgressBar(min = 0,
                         max = nrow(simpars),
                         style = 3)
  }

  # intialise results
  results <-
    vector("list", length = 2 * nrow(simpars))

  # loop through simuations
  # this is possibly an application for rap::
  for (i in 1:nrow(simpars)) {
    suppressMessages({
      results[[i]] <-  metasim(
        tau_sq = simpars$between_study_variation[[i]],
        median_ratio = simpars$median_ratio[[i]],
        rdist = simpars$rdist[[i]],
        parameters = simpars$parameters[[i]],
        n_df = simpars$n[[i]],
        knha = TRUE,
        true_median = simpars$true_median[[i]],
        id = simpars$id[[i]],
        trial_fn = trial_fn,
        trials = trials
      ) %>% pluck("results_summary")

    })

    # cat(paste("simulation", i, "\n"))

    if (isTRUE(probar)) {
      setTxtProgressBar(pb, i)
    }
  }

  # transform list of results to df with sim parameters
  results_df <-
    simpars %>% full_join(results %>% bind_rows(), by = "id")


  # output of function
  return(results_df)
  # list(simpars, results))

}
