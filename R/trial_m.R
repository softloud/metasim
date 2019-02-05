#' meta-analyse medians of control group
#'
#' @import metafor
#' @import dplyr
#' @export

trial_m <- function(df, true_effect) {
  df %>%
    dplyr::filter(group == "control") %>%
    metafor::rma(data = ., yi = effect, sei = abs(effect_se)) %>%
    trial_returns(true_effect = true_effect)
}
