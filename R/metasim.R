#' one row, one simulations
#'
#' runs on one row, returns coverage probability
#'
#' @param trial_fn the function to repeat
#' @param trials the number of trials per simulation
#' @param ... \code{trial_fn} arguments, i.e., simulation nparameters
#' @inheritParams metatrial
#'
#' @export

metasim <- function(...,
                    id = "simulation1",
                    trial_fn = metatrial,
                    trials = 4) {
  all_trials <-
    map_peacefully(1:trials, .f = function(x) {trial_fn(...)})

  results <- all_trials %>%
    transpose() %>%
    pluck("result") %>%
    keep(is.data.frame) %>%
    keep( ~ nrow(.) >= 1) %>% # keep non-empty results
    bind_rows() %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(
      tau_sq = mean(tau_sq),
      ci_width = mean(conf_high - conf_low),
      bias = mean(bias),
      coverage_count = sum(coverage),
      successful_trials = length(coverage),
      coverage = coverage_count / successful_trials
    ) %>%
    mutate(id = id,
           errors = tally_errors(all_trials),
           warnings = tally_warnings(all_trials),
           messages = tally_messages(all_trials),
           result = tally_results(all_trials)
    )

  return(results)

}
