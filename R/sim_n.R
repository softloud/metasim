#' simulate meta-analysis sample sizes
#'
#' Generate a set of meta-analysis sample sizes. We assume that control and intervention sizes are mostly within some proportion \code{prop} of the same sample size.
#'
#' @param k number of studies, defaults to 3
#' @param min_n lower bound for sample sizes, defaults to 20
#' @param max_n upper bound for sample sizes, defaults to 200
#' @param prop proportion of n we expect
#'
#' @export

sim_n <- function(k = 3,
                   min_n = 20,
                   max_n = 200,
                   prop = 0.3,
                   wide = FALSE) {
  n_df <- tibble::tibble(study = paste0("study_", seq(1, k)),
                         study_n = sample(seq(min_n, max_n),
                                          size = k, replace = TRUE)) %>%
    dplyr::mutate(
      study_n_sd = prop * study_n,
      control = purrr::map2_int(
        study_n,
        study_n_sd,
        .f = function(study_n, study_n_sd) {
          rnorm(1, study_n, study_n_sd) %>% round() %>% as.integer()
        }
      ),
      intervention = purrr::map2_int(
        study_n,
        study_n_sd,
        .f = function(study_n, study_n_sd) {
          rnorm(1, study_n, study_n_sd) %>% round() %>% as.integer()
        }
      )
    ) %>%
    dplyr::select(-study_n,-study_n_sd)

  if (wide == FALSE) {
    return(n_df %>%
             tidyr::gather(key = "group",
                           value = "n",
                           control, intervention))
  } else {
    return(n_df)
  }

}


