#' Calculate proportion of intervention group
#'
#' @inheritParams beta_par
#'
#' @export

intervention_proportion <- function(n, proportion, error) {

  par <- beta_par(proportion, error)

  rbeta(n, shape1 = par$alpha, shape2 = par$beta)
}
