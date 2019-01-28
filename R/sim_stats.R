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
  tibble(
    bn_study_error = rnorm(nrow(n_df) / 2, 0, tau) / 2,
    wn_study_error = rnorm(nrow(n_df) / 2, 0, epsilon) / 2,
    study = paste0("study_", seq(1, nrow(n_df) / 2))
  ) %>%
    # join to df
    full_join(n_df, by = "study") %>%
    mutate(control_indicator = group == "control",
           sample = pmap(list(n = n, tau = tau, epsilon = epsilon, control = control_indicator), sim_sample, rdist = rdist, par = par, median_ratio = median_ratio),
           min = map_dbl(sample, min),
           max = map_dbl(sample, max),
           mean = map_dbl(sample, mean),
           sd = map_dbl(sample, sd),
           first_q = map_dbl(sample, quantile, 0.25),
           median = map_dbl(sample, quantile, 0.5),
           third_q = map_dbl(sample, quantile, 0.75),
           iqr = third_q - first_q) %>%
    select(-sample) # remove the sample and return the summary stats

  }
