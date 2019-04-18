#' useful simulation statistics
#'
#' Returns statistics from what are currently [metabroom::glance]
#' and [metabroom::tidy] functions that are used in the [metasim::]
#' package for analysis of simulation results.
#'
#' todo: this will port to broom:: proper eventually.
#'
#' @export

tidy_sim <- function(rma_model) {
  rma_model %>%
    {
      tibble(
        conf_low = pluck(., "ci.lb"),
        conf_high = pluck(., "ci.ub"),
        tau_sq = pluck(., "tau2"),
        k = pluck(., "k"),
        effect = pluck(., "beta") %>% as.numeric()
      )
    }
}
