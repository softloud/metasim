#' generate simulation parameter dataframe
#'
#'
#' @param dist_tribble A \code{\link{tibble::tribble}}
#' with one column for distribution, and one column for the parameters
#' @inheritParams sim_n
#' @inheritParams metasims
#'
#' @import purrr
#' @import tibble
#' @import dplyr
#'
#' @export



sim_df <- function(dist_tribble =
                     tibble::tribble(
                       ~ dist, ~ par,
                       "norm", list(mean = 67, sd = 0.3),
                       "exp",
                       list(rate = 3),
                       "pareto",
                       list(shape = 3, scale = 3),
                       "pareto",
                       list(shape = 2, scale = 1),
                       # "pareto",
                       # list(shape = 0.5, scale = 1),
                       "lnorm",
                       list(mean = 44, sd = 0.3)
                     ),
                   k = c(3, 7, 50),
                   between_study_variation = seq(0, 0.4, 0.2),
                   median_ratio = c(1, 1.2),
                   prop = 0.3) {
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
          median_ratio = median_ratio,
          prop = prop
        )
      )
    } %>%
    dplyr::mutate(
      rdist = purrr::map_chr(distribution, "dist"),
      parameters = purrr::map(distribution, "par")
    )  %>%
    dplyr::select(-distribution) %>%
    dplyr::mutate(n = purrr::map2(k, prop, sim_n),
                  id = paste0("sim_", seq(1, nrow(.))))  %>%
    dplyr::mutate(true_median =
                    purrr::map2_dbl(
                      rdist,
                      parameters,
                      .f = function(rdist, parameters) {
                        if (rdist == "pareto") {
                          actuar::qpareto2(0.5, shape = parameters[[1]], scale = parameters[[2]])
                        } else {
                          density_fn(
                            distribution = rdist,
                            parameters = parameters,
                            type = "q",
                            x = 0.5
                          )}}))}

