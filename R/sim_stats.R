#' simulate a meta-analysis dataset
#'
#' @param n_df \code{data.frame} of sample sizes,
#' such as output by \code{\link{meta_n}}.
#' @inheritParams sim_sample
#'
#' @export

sim_stats <- function(n_df = sim_n(),
                      rdist = "norm",
                      par = list(mean = 50, sd = 0.2),
                      tau = 0.4,
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
  assertthat::assert_that("numeric" %in% class(tau),
                          length(tau) == 1,
                          msg = "tau = argument should be numeric
                          of length 1")
  assertthat::assert_that("numeric" %in% class(median_ratio),
                          length(median_ratio) == 1,
                          msg = "median_ratio = argument requires a number")


  # generate study-level random effect
  samples <-
    tibble::tibble(this_study_error =
                     rnorm(nrow(n_df) / 2, 0, tau) / 2,
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
                  )

  if (!is.data.frame(samples) | nrow(samples) <= 1) {
    summary_stats <- NULL
  } else {
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

  }

  return(summary_stats)
}
