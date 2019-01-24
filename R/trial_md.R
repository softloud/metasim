#' meta-analyse difference of medians
#'
#' @export

trial_md <- function(df, true_effect) {
  # calculate effects
  df %>%
    # calculate median difference
    dplyr::group_by(study) %>%
    dplyr::summarise(effect = abs(diff(effect)),
              effect_se = sqrt(sum(effect_se))/2) %>%
    metafor::rma(data = ., yi = effect, vi = abs(effect_se)) %>%
    trial_returns(true_effect = true_effect)
}
