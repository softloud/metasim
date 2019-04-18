#' simulate over all
#'
#' @param k Simulate for different numbers of studies.
#' @param tau2_true Variance \eqn{\gamma_k \sim N(0, \tau^2)} associated with the random effect
#' @param prop Proportion of sample size we expect to cohorts to vary by most of
#' the time
#' @param median_ratio \eqn{\nu_I / \nu_C := \rho} where \eqn{\rho} denotes the ratio of medians.
#' @param probar Turn progress bar on and off.
#' @export

metasims <- function(distributions = default_parameters,
                     k = c(3, 7, 10),
                     tau2_true = seq(from = 0, to = 0.4, by = 0.2),
                     median_ratio = c(1, 1.2),
                     prop = 0.3,
                     trials = 10,
                     trial_fn = metatrial,
                     beep = FALSE,
                     probar = TRUE) {
  # set up simulation parameters
  simpars <- sim_df(
    dist_tribble = distributions,
    k = k,
    tau2 = tau2_true,
    median_ratio = median_ratio,
    prop = prop
  )



  # set progress bar
  if (isTRUE(probar)) {

    cat(paste(
      "\nperforming ",
      nrow(simpars),
      " simulations of ",
      trials,
      " trials\n"
    ))
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
    suppressMessages({ # expect some rma models not to converge
      results[[i]] <-  metasim(
        tau_sq = simpars$tau2_true[[i]],
        median_ratio = simpars$median_ratio[[i]],
        rdist = simpars$rdist[[i]],
        parameters = simpars$parameters[[i]],
        n_df = simpars$n[[i]],
        knha = TRUE,
        true_effect = simpars$true_effect[[i]],
        id = simpars$id[[i]],
        trial_fn = trial_fn,
        trials = trials
      ) %>% pluck("results")

    })

    # cat(paste("\nsimulation", i, "\n"))

    if (isTRUE(probar)) {
      setTxtProgressBar(pb, i)
    }
  }

  # transform list of results to df with sim parameters
  results_df <- simpars %>%
    ungroup()  %>%
    mutate(id = as.character(id)) %>%
    full_join(results %>%
                bind_rows() %>%
                mutate(id = as.character(id)),
              by = "id")

  if (isTRUE(probar)) cat("\n")
  if (isTRUE(beep)) beepr::beep("treasure")

  # output of function
  return( results_df)
   # list(simpars, results))



}
