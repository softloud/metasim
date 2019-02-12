#' generate simulation parameter dataframe
#'
#'
#' @param dist_tribble A \code{\link{tibble::tribble}}
#' with one column for distribution, and one column for the parameters
#' @inheritParams sim_n
#' @inheritParams metasims
#'
#' @export



sim_df <- function(dist_tribble =
                     tibble::tribble( ~ dist,  ~ par,
                                      "norm", list(mean = 50, sd = 0.2),
                                      "exp", list(rate = 2)),
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
                    map2_dbl(
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

                  #               dplyr::case_when(
                  # rdist != "pareto" ~ purrr::map2_dbl(rdist, parameters, density_fn, type = "q", x = 0.5)),
                  # rdist == "pareto" ~ purrr::map2_dbl(rdist, parameters, .f = function(x,y) {
                  #     actuar::qpareto2(0.5, shape = parameters[[1]], scale = parameters[[2]])
                  #   })) # %>%
                  #   dplyr::select(
                  #     k,
                  #     between_study_variation,
                  #     between_study_variation,
                  #     median_ratio,
                  #     rdist,
                  #     parameters,
                  #     n,
                  #     true_median,
                  #     id
                  #   )

# 1}
