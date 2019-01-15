#' Estimator, assuming a log-normal distribution.
#'
#' This estimator is one choice for density substitution in the variance of the sample median estimator.
#'
#' @param median sample median
#' @param spread iqr or range value
#' @param n sample size
#' @param spread_type iqr or range, defaults to iqr

g_lnorm <- function(median, spread, n = 1, spread_type = "iqr") {
  # Estimate parameters.

  # Approximate mean parameter.
  mu <- log(median)

  # Quantile is calculated case-wise for spread type.
  if (spread_type == "iqr") {
    p <- 3 / 4
  } else if (spread_type == "range") {
    p <- (n - 1 / 2) / n
  }

  # Approximate standard deviation parameter.
  sigma <- 1 / qnorm(p) *
    log((spread * exp(-mu) + sqrt(spread ^ 2 * exp(-2 * mu) + 4)) / 2)

  # evaluate density at sample median
  dlnorm(x = median,
         meanlog = mu,
         sdlog = sigma)

}
