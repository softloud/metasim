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
  safe_trial_fn <- purrr::safely(trial_fn)

  results <- purrr::rerun(.n = trials, safe_trial_fn(...)) %>%
    transpose() %>%
    pluck("result") %>%
    keep(is.data.frame) %>%
    keep( ~ nrow(.) >= 1) %>% # keep non-empty results
    bind_rows() %>%
    dplyr::group_by(measure) %>%
    clean_names() %>%
    dplyr::summarise(
      tau_sq = mean(tau_sq),
      ci_width = mean(conf_high - conf_low),
      bias = mean(bias),
      coverage_count = sum(coverage),
      successful_trials = length(coverage),
      coverage = coverage_count / successful_trials
    ) %>% mutate(id = id)

  errors <- results %>%
    transpose() %>%
    purrr::pluck("error")

  list(errors = errors,
       results = results)

}
