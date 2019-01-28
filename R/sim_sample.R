#' Simulate a sample
#'
#' Given a distribution, parameters, sample size, generate a sample.
#'
#' @param n sample size
#' @param rdist random sampling function, such as \code{\link{rnorm}} and \code{\link{rexp}}.
#' @param par list of parameter arguments
#' @param control value of first parameter of distribution is determined by median ratio
#' @param median_ratio ratio of population medians

sim_sample <- function(n, tau, epsilon, rdist, par, control = TRUE, median_ratio = 1.2) {
  # check inputs are valid
  assertthat::assert_that(length(par) <= 2,
                          msg = "haven't coded this for more than two parameters")
  assertthat::assert_that(rdist %in% c( "exp", "norm", "lnorm", "pareto"),
                          msg = "choose exp, norm, lnorm, and pareto")

  if (rdist == "norm") {

    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) par[[1]] * median_ratio else par[[1]]

    # generate sample
    return(rnorm(n, mean = par_1 * exp(tau + epsilon), sd = par[[2]]))

  } else if (rdist == "log-normal") {

    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) par[[1]] + log(median_ratio) else par[[1]]

    # generate sample
    return(rlnorm(n, par_1 + tau + epsilon, par[[2]]))

  } else if (rdist == "pareto") {

    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) par[[1]] * median_ratio else par[[1]]

    # generate sample
    return(actuar::rpareto2(n, par_1 * exp(tau + epsilon), par[[2]]))

  } else if (rdist == "exponential") {

    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) par[[1]]/median_ratio else par[[1]]

    # generate sample
    return(rexp(n, par_1 * exp(tau + epsilon)))
  }

}
