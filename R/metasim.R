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

  results <- purrr::rerun(.n = trials, safe_trial_fn(...))

  results_df <- results  %>%
    transpose() %>%
    pluck("result") %>%
    keep(is.data.frame) %>%
    bind_rows() %>%
     dplyr::group_by(measure) %>%
  dplyr::summarise(
    tau2 = mean(tau2),
    ci_width = mean(ci_ub - ci_lb),
    bias = mean(bias),
    coverage_count = sum(coverage),
    successful_trials = length(coverage),
    coverage = coverage_count / successful_trials
  ) %>% mutate(id = id)

  errors <- results %>%
    transpose() %>%
    purrr::pluck("error")

    list(
              errors = errors,
              results_summary = results_df
    )


}
