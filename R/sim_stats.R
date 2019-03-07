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

sim_stats <- function(n_df = sim_n(),
                      wide = FALSE,
                      rdist = "norm",
                      par = list(mean = 50, sd = 0.2),
                      tau_sq = 0.4,
                      median_ratio = 1.2) {

  # check inputs to function are as required
  assertthat::assert_that("data.frame" %in% class(n_df),
                          msg = "n_f = argument requires a dataframe")
  assertthat::assert_that("character" %in% class(rdist),
                          msg = "rdist = argument requires a character
                          string: norm, exp, lnorm, or pareto")
  assertthat::assert_that("list" %in% class(par),
                          length(par) > 0,
                          length(par) <= 2,
                          msg = "par = argument expects a list
                          vector of length one or two, dependening
                          on choice of distribution")
  assertthat::assert_that("numeric" %in% class(tau_sq),
                          length(tau_sq) == 1,
                          msg = "tau_sq = argument should be numeric
                          of length 1")
  assertthat::assert_that("numeric" %in% class(median_ratio),
                          length(median_ratio) == 1,
                          msg = "median_ratio = argument requires a number")


  # generate study-level random effect
  samples <-
    tibble::tibble(this_study_error =
                     rnorm(nrow(n_df) / 2, 0, tau_sq) / 2,
                   study =
                     paste0("study_", seq(1, nrow(n_df) / 2))) %>%
    # join to df
    dplyr::full_join(n_df, by = "study") %>%
    dplyr::mutate(control_indicator = group == "control" ,
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
                      median_ratio = median_ratio
                    )
                  ) %>% select(-control_indicator)

  if (!is.data.frame(samples) | nrow(samples) <= 1) {
    summary_stats <- NULL
  } else if (wide == FALSE) {
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
        # remove the sample and return the summary stats
        dplyr::select(-sample)

  } else if (wide == TRUE) {
    summary_stats <- samples %>%
      dplyr::select(-n) %>%
      tidyr::spread(key = "group", value = "sample") %>%
    dplyr::mutate(
      n_c = purrr::map_dbl(control, length),
      min_c = purrr::map_dbl(control, min),
      max_c = purrr::map_dbl(control, max),
      mean_c = purrr::map_dbl(control, mean),
      sd_c = purrr::map_dbl(control, sd),
      first_q_c = purrr::map_dbl(control, quantile, 0.25),
      median_c = purrr::map_dbl(control, quantile, 0.5),
      third_q_c = purrr::map_dbl(control, quantile, 0.75),
      iqr_c = third_q_c - first_q_c,
      n_i = purrr::map_dbl(intervention, length),
      min_i = purrr::map_dbl(intervention, min),
      max_i = purrr::map_dbl(intervention, max),
      mean_i = purrr::map_dbl(intervention, mean),
      sd_i = purrr::map_dbl(intervention, sd),
      first_q_i = purrr::map_dbl(intervention, quantile, 0.25),
      median_i = purrr::map_dbl(intervention, quantile, 0.5),
      third_q_i = purrr::map_dbl(intervention, quantile, 0.75),
      iqr_i = third_q_i - first_q_i
    ) %>%
      # get ride of the samples
      dplyr::select(-control, -intervention)
  }

  return(summary_stats %>% arrange(study, group))
}
