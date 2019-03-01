#' one row, one simulations
#'
#' runs on one row, returns coverage probability
#'
#' @param trial_fn the function to repeat
#' @param trials the number of trials per simulation
#' @param ... \code{trial_fn} arguments
#' @inheritParams metatrial
#'
#' @export

metasim <- function(...,
                    id = "simulation1",
                    trial_fn = metatrial,
                    trials = 4) {
  results <- purrr::rerun(.n = trials, trial_fn(...))

  results_summary <- results  %>%
    map_df("results")  %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(
      tau2 = mean(tau2),
      ci_width = mean(ci_ub - ci_lb),
      bias = mean(bias),
      coverage_count = sum(coverage),
      successful_trials = length(coverage),
      coverage = coverage_count / successful_trials
    ) %>%
    mutate(id = id)

  errors <- results %>% map_df("errors") %>% mutate(id = id)

  return(list(results_summary = results_summary,
              errors = errors))

}
