#' Simulate a meta-analysis dataset
#'
#' @param n_df \code{data.frame} of sample sizes,
#' such as output by \code{\link{meta_n}}.
#' @param wide Logical indicating if wide format, as is expected by \code{\link{metafor}::}.
#' @inheritParams sim_sample
#'
#' @importFrom assertthat assert_that
#' @import tibble
#' @import purrr
#' @import dplyr
#' @export

sim_stats <- function(measure = "median",
                      measure_spread = "iqr",
                      n_df = sim_n(),
                      wide = FALSE,
                      rdist = "norm",
                      par = list(mean = 50, sd = 0.2),
                      tau_sq = 0.4,
                      effect_ratio = 1.2) {
  # check inputs to function are as required
  assertthat::assert_that("data.frame" %in% class(n_df),
                          msg = "n_f = argument requires a dataframe")
  assertthat::assert_that("character" %in% class(rdist),
                          msg = "rdist = argument requires a character
                          string: norm, exp, lnorm, or pareto")
  assertthat::assert_that(
    "list" %in% class(par),
    length(par) > 0,
    length(par) <= 2,
    msg = "par = argument expects a list
                          vector of length one or two, dependening
                          on choice of distribution"
  )
  assertthat::assert_that("numeric" %in% class(tau_sq),
                          length(tau_sq) == 1,
                          msg = "tau_sq = argument should be numeric
                          of length 1")
  assertthat::assert_that("numeric" %in% class(effect_ratio),
                          length(effect_ratio) == 1,
                          msg = "effect_ratio = argument requires a number")


  # generate study-level random effect
  samples <-
    tibble::tibble(
      this_study_error =
        rnorm(nrow(n_df) / 2, 0, tau_sq) / 2,
      study =
        paste0("study_", seq(1, nrow(n_df) / 2))
    ) %>%
    # join to df
    dplyr::full_join(n_df, by = "study") %>%
    dplyr::mutate(
      control_indicator = group == "control" ,
      sample =
        purrr::pmap(
          list(
            n = n,
            this_study_error = this_study_error,
            control = control_indicator
          ),
          sim_sample,
          rdist = rdist,
          par = par,
          effect_ratio = effect_ratio
        )
    ) %>% dplyr::select(-control_indicator)

  summary_stats <- samples %>%
    dplyr::mutate(
      min = purrr::map_dbl(sample, min),
      max = purrr::map_dbl(sample, max),
      mean = purrr::map_dbl(sample, mean),
      sd = purrr::map_dbl(sample, sd),
      first_q = purrr::map_dbl(sample, quantile, 0.25),
      median = purrr::map_dbl(sample, quantile, 0.5),
      third_q = purrr::map_dbl(sample, quantile, 0.75),
      iqr = third_q - first_q
    ) %>%
    # remove the sample and return the effect stats
    dplyr::select(-sample) %>%
    mutate(effect = !!sym(measure),
           effect_spread = !!sym(measure_spread)) %>%
    select(study, group, effect, effect_spread, n)

  if (!is.data.frame(samples) | nrow(samples) <= 1) {
    summary_stats <- NULL
  } else if (wide == TRUE) {
    summary_stats <- full_join(
      summary_stats %>% filter(group == "control") %>%
        select(-group) %>%
        rename(
          effect_c = effect,
          effect_spread_c = effect_spread,
          n_c = n
        ),
      summary_stats %>%
        filter(group == "intervention") %>%
        select(-group) %>%
        rename(
          effect_i = effect,
          effect_spread_i = effect_spread,
          n_i = n
    ))}

  return(summary_stats %>% dplyr::arrange(study))
}
