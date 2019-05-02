#' Calculate proportion of intervention group
#'
#' @param proportion Expected proportion for the intervention group
#' @param error Within what value will 90 per cent of the proportion of intervention groups fall within?
#'
#' @export

intervention_proportion <- function(n, proportion, error) {
  alpha <- proportion * (
    ((10 * proportion^2) / error^2) *
      (1 / proportion - 1) - 1
  )

  beta <- alpha / proportion - alpha

  rbeta(n, shape1 = alpha, shape2 = beta)
}
