#' simulate a meta-analysis dataset
#'
#' @param n_df \code{data.frame} of sample sizes, such as output by \code{\link{meta_n}}.
#' @param sample_fn
#'
#' @export

metastats <- function(n_df,
                      sampling_fn = rnorm,
                      fn_parameters = list(mean = 50, sd = 0.2),
                      between_study_variation = 0.4,
                      within_study_variation = 0.2,
                      median_ratio = 1.2
                      ) {
  n_df %>%
    tidyr::spread(group, n) %>%
    # add random effect for study
    dplyr::mutate(
      study_var = rnorm(nrow(.), mean = 0, sd = between_study_variation) / 2,
      study_err = rnorm(nrow(.), mean = 0, sd = within_study_variation) / 2
    ) %>%
    tidyr::gather(key = "group",
           value = "n",
           control, intervention
           ) %>%
    dplyr::mutate(summary_stats = purrr::pmap(list(n, study_var, study_err),
                                .f = function(n, study_var, study_err){
                                  # figure out splicing thing eventually
                                  if (length(fn_parameters) ==  1) {
                                    sampling_fn(n, fn_parameters[[1]] *
                                                  exp(study_var + study_err)) }
                                  if (length(fn_parameters) == 2) {
                                    sampling_fn(n, fn_parameters[[1]] *
                                                  exp(study_var + study_err),
                                                fn_parameters[[2]]) } %>%
                                      {tibble::tibble(
                                        min = min(.),
                                        max = max(.),
                                        first_quartile = quantile(., 0.25),
                                        third_quartile = quantile(., 0.75),
                                        mean = mean(.),
                                        median = median(.),
                                        var = var(.),
                                        n = length(.)
                                      )}
                                }))


  }
