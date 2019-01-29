#' simulate a meta-analysis dataset
#'
#' @param n_df \code{data.frame} of sample sizes, such as output by \code{\link{meta_n}}.
#' @inheritParams sim_sample
#'
#' @export

sim_stats <- function(n_df = sim_n(),
                      rdist = "norm",
                      par = list(mean = 50, sd = 0.2),
                      tau = 0.4,
                      epsilon = 0.2,
                      median_ratio = 1.2) {

  # generate study-level random effects
  tibble::tibble(
    bn_study_error = abs(rnorm(nrow(n_df) / 2, 0, tau) / 2),
    wn_study_error = abs(rnorm(nrow(n_df) / 2, 0, epsilon) / 2),
    study = paste0("study_", seq(1, nrow(n_df) / 2))
  ) %>%
    # join to df
    dplyr::full_join(n_df, by = "study") %>%
    dplyr::mutate(
    control_indicator = group == "control" ,
           sample = purrr::pmap(
             list(n = n,
                  tau = tau,
                  control = control_indicator),
             sim_sample,
             rdist = rdist,
             par = par,
             median_ratio = median_ratio),
           min = purrr::map_dbl(sample, min),
           max = purrr::map_dbl(sample, max),
           mean = purrr::map_dbl(sample, mean),
           sd = purrr::map_dbl(sample, sd),
           first_q = purrr::map_dbl(sample, quantile, 0.25),
           median = purrr::map_dbl(sample, quantile, 0.5),
           third_q = purrr::map_dbl(sample, quantile, 0.75),
           iqr = third_q - first_q)
#  %>%
  # dplyr::select(-sample) # remove the sample and return the summary stats

  }
