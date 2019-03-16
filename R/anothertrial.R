#' Second attempt at metatrial function
#'
#' This time, let's try making function so that it can be wrapped in safely at
#' the metasim level.
#'
#' @param true_effect The value of the control population median.
#' @inheritParams sim_stats
#'
#' @export

anothertrial <- function(
  true_median = 50,
  ...
) {
  # calculate true effects
  effects_tibble <- tibble::tibble(
    measure = c("m", "lr"),
    true_effects = c(
      true_median,
      log(median_ratio))
  )

  metadata <- sim_stats(...)

  effects_tibble
}
