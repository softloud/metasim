#' useful simulation statistics
#'
#' Returns statistics from what are currently [metabroom::glance]
#' and [metabroom::tidy] functions that are used in the [metasim::]
#' package for analysis of simulation results.
#'
#' todo: this will port to broom:: proper eventually.
#'
#' @export

tidy_sim <- function(rma_model){
    bind_cols(
      rma_model %>%
        tidy() %>%
        filter(type == "summary") %>%
        select(conf.low, conf.high, std.error, std.error, estimate) %>%
        janitor::clean_names(),
      rma_model %>%
        glance() %>%
        select(tau.squared, k, method, tau.squared.se, i.squared, h.squared, QE, QE_p, QM, QM_p) %>%
        janitor::clean_names()
    )}
