#' simulate meta-analysis sample sizes
#'
#' Generate a set of meta-analysis sample sizes. We assume that control and intervention sizes are mostly within some proportion \code{prop} of the same sample size.
#'
#' @param k number of studies, defaults to 3
#' @param min_n lower bound for sample sizes, defaults to 20
#' @param max_n upper bound for sample sizes, defaults to 200
#' @param prop proportion of n we expect
#' @param prop_error what do we expect 90% of proportions to fall within?
#' @param wide if true, case and control sample sizes are provided in one row
#'
#' @export

sim_n <- function(k = 3,
                  min_n = 20,
                  max_n = 200,
                  prop = 0.5,
                  prop_error = 0.1,
                  wide = FALSE) {
  assert_that(min_n > 0,
              msg = "minimum sample size must be positive")
  assert_that(max_n > 0,
              msg = "maximum sample size must be positive")
  assert_that(min_n <= max_n,
              msg = "minimum sample size cannot exceed maximum sample size")

  # set up study label and sample kth sample size of control + intervention
  n_df <- tibble::tibble(study = paste0("study_", seq(1, k)),
                         study_n = sample(seq(min_n, max_n),
                                          size = k, replace = TRUE)) %>%
    dplyr::mutate(
      # get kth proportion for the intervention group
      intervention_proportion = intervention_proportion(
        n = k,
        proportion = prop,
        error = prop_error
      ),
      # get intervention and control sample sizes
      intervention = round(intervention_proportion * study_n),
      control = round((1 - intervention_proportion) * study_n)
    ) %>%
    dplyr::select(-study_n, -intervention_proportion)

    if (wide == FALSE) {
      return(n_df %>%
               tidyr::gather(key = "group",
                             value = "n",
                             control, intervention))
    } else {
      return(n_df)
    }
}
