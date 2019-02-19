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

metasim <- function(
  ...,
  id = "simulation1",
  trial_fn = metatrial,
  trials = 4
) {

  results <- purrr::rerun(.n = trials, trial_fn(...))

  results
  # results %>%
  #   dplyr::bind_rows()  %>%
  #   dplyr::group_by(effect_type) %>%
  #   dplyr::summarise(ci_width =
  #                      mean(ci_ub - ci_lb),
  #             ci_lb = mean(ci_lb),
  #             ci_ub = mean(ci_ub),
  #             tau2 = mean(tau2),
  #             # i2 = mean(i2),
  #             bias = mean(bias),
  #             cp_sum = sum(in_ci),
  #             cp_length = length(in_ci),
  #             cp = sum(in_ci) / length(in_ci)) %>%
  #   dplyr::mutate(id = id)



}
