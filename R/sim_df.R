#' generate simulation parameter dataframe
#'
#'
#' @param dist_tribble A \code{\link{tibble::tribble}} with one column for distribution, and one column for the parameters
#' @inheritParams meta_n
#' @inheritParams metasims
#'
#' @export


sim_df <- function(dist_tribble,
                   k,
                   between_study_variation,
                   within_study_variation,
                   median_ratio,
                   prop) {
  dist_tribble %>%
    dplyr::mutate(distribution =
                    purrr::map2(dist, par,
                                function(x, y) {
                                  list(dist = x, par = y)
                                })) %>%
    purrr::pluck("distribution") %>% {
      purrr::cross_df(
        list(
          distribution = .,
          k = k,
          between_study_variation = between_study_variation,
          within_study_variation = within_study_variation,
          median_ratio = median_ratio,
          prop = prop
        )
      )
    } %>%
    dplyr::mutate(
      rdist = purrr::map_chr(distribution, "dist"),
      parameters = purrr::map(distribution, "par")
    ) %>%
    dplyr::select(-distribution) %>%
    dplyr::mutate(n = purrr::map2(k, prop, meta_n),
                  id = paste0("sim_", seq(1, nrow(.)))) %>%
    dplyr::mutate(true_median = purrr::map2_dbl(rdist, parameters, density_fn, type = "q", x = 0.5)) %>%
    dplyr::select(
      k,
      between_study_variation,
      between_study_variation,
      within_study_variation,
      median_ratio,
      rdist,
      parameters,
      n,
      true_median,
      id
    )

}
