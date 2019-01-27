#' Simulate a sample
#'
#' Given a distribution, parameters, sample size, generate a sample.
#'
#' @param n sample size
#' @param rdist random sampling function, such as \code{\link{rnorm}} and \code{\link{rexp}}.
#' @param par list of parameter arguments

sim_sample <- function(n, rdist, par) {
  # check inputs are valid
  assertthat::assert_that(length(par) <= 2,
                          msg = "haven't coded this for more than two parameters")

  if (length(par) == 1) {
    rdist(n, par[[1]])
  } else {
    rdist(n, par[[1]], par[[2]])
  }
}
