#' simulate over all
#'
#' @inheritParams meta_df
#'
#' @export

metasims <- function(dist_tribble =
                       tibble::tribble(~ dist,  ~ par,
                                       "norm", list(mean = 50, sd = 0.2),
                                       "exp", list(rate = 2)),
                     k = c(3, 7, 50),
                     between_study_variation = seq(0, 0.4, 0.2),
                     within_study_variation = seq(0, 0.4, 0.2),
                     median_ratio = c(1, 1.2),
                     prop = 0.3,
                     trials = 10,
                     trial_fn = metatrial) {
  meta_df(
    dist_tribble = dist_tribble,
    k = k,
    between_study_variation = between_study_variation,
    within_study_variation = within_study_variation,
    median_ratio = median_ratio,
    prop = prop
  ) %>%
    # apply(., 1, metasim, trials = trials, trial_fn = trial_fn)
  # %>%
    rap::rap(
      sim_results = list() ~ metasim(
        between_study_variation = between_study_variation,
        within_study_variation = within_study_variation,
        median_ratio = median_ratio,
        rdist = rdist,
        n_df = n,
        parameters = parameters,
        true_effect = true_median,
        id = id,
        trial_fn = metatrial,
        trials = trials
    ))
}
