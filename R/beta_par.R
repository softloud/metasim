#' calculate beta parameters
#'
#' @param proportion Expected proportion for the intervention group
#' @param error Within what value will 90 per cent of the proportion of intervention groups fall within?
#'
#' @export
#'

beta_par <- function(proportion, error) {
  alpha <- proportion * (
    ((10 * proportion^2) / error^2) *
      (1 / proportion - 1) - 1
  )

  beta <- alpha / proportion - alpha

  return(list(
    alpha = alpha,
    beta = beta
  ))
}
