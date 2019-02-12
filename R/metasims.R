#' simulate over all
#'
#' @param k Simulate for different numbers of studies.
#' @param between_study_variation tau
#' @param within_study_variation epsilon
#' @param prop proportion of sample size we expect to cohorts to vary by most of the time
#'
#' @export

metasims <- function(dist_tribble =
                       tibble::tribble(
                         ~ dist, ~ par,
                         "norm", list(mean = 67, sd = 0.3),
                         "exp",
                         list(rate = 3),
                         "pareto",
                         list(shape = 3, scale = 3),
                         "pareto",
                         list(shape = 2, scale = 1),
                         "pareto",
                         list(shape = 0.5, scale = 1),
                         "lnorm",
                         list(mean = 44, sd = 0.3)
                       ),
                     k = c(3, 7, 50),
                     between_study_variation = seq(0, 0.4, 0.2),
                     median_ratio = c(1, 1.2),
                     prop = 0.3,
                     trials = 10,
                     trial_fn = metatrial) {

  # set up simulation parameters
  simpars <- sim_df(
    dist_tribble = dist_tribble,
    k = k,
    between_study_variation = between_study_variation,
    median_ratio = median_ratio,
    prop = prop
  )

  cat(paste("performing ",
            nrow(simpars),
            " simulations of ", trials, " trials\n"))

  # set progress bar
  pb <- txtProgressBar(min = 0,
                       max = nrow(simpars),
                       style = 3)

  # intialise results
  results <- vector("list", length = nrow(simpars))

  #
  for (i in 1:nrow(dist_tribble)) {
    results[[i]] <- toss(
      metasim(
        tau = simpars$between_study_variation[[i]],
        median_ratio = simpars$median_ratio[[i]],
        rdist = simpars$rdist[[i]],
        parameters = simpars$parameters[[i]],
        n_df = simpars$n[[i]],
        knha = TRUE,
        true_effect = simpars$true_median[[i]],
        id = simpars$id[[i]],
        trial_fn = metatrial,
        trials = trials
      )
    )

    # cat(paste("trial ", i, " - so far, so good! \n"))
    setTxtProgressBar(pb, i)
  }

  results_df <- results %>% bind_rows() %>%
    full_join(simpars, by = "id")


  return(results_df)

  # sim_df(
  #   dist_tribble = dist_tribble,
  #   k = k,
  #   between_study_variation = between_study_variation,
  #   within_study_variation = within_study_variation,
  #   median_ratio = median_ratio,
  #   prop = prop
  # ) %>%
  #   # apply(., 1, metasim, trials = trials, trial_fn = trial_fn)
  # # %>%
  #   rap::rap(
  #     sim_results = list() ~ metasim(
  #       tau = between_study_variation,
  #       median_ratio = median_ratio,
  #       rdist = rdist,
  #       n_df = n,
  #       parameters = parameters,
  #       true_effect = true_median,
  #       id = id,
  #       trial_fn = metatrial,
  #       trials = trials
  #   ))


}
