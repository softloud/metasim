#' meta-analyse difference of medians
#'
#' @export

trial_md <- function(df, true_effect) {
  # calculate effects
  df %>%
    # calculate median difference
    group_by(study) %>%
    summarise(effect = abs(diff(effect)),
              effect_se = sqrt(sum(effect_se))/2) %>%
    metafor::rma(data = ., yi = effect, vi = effect_se) %>%
    trial_returns(true_effect = true_effect)
}
