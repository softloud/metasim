#' return meta-analysis results
#'
#' should broom this
#'
#' @import dplyr
#' @import tibble
#' @export

trial_returns <- function(rem, true_effect) {
  tibble::tibble(
    ci_lb = rem[["ci.lb"]],
    ci_ub = rem[["ci.ub"]],
    i2 = rem[["I2"]],
    tau2 = rem[["tau2"]],
    effect = as.numeric(rem[["beta"]])
  ) %>%
    dplyr::mutate(in_ci = ci_lb < true_effect & true_effect < ci_ub)

}
