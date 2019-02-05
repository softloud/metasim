#' Simulate a sample
#'
#' Given a distribution, parameters, sample size, generate a sample.
#'
#' @param n sample size
#' @param this_study_error this study error
#' @param epsilon within study error
#' @param rdist string indicating distribution, "norm", "lnorm", "exp", or "pareto"
#' @param par list of parameter arguments
#' @param control value of first parameter of distribution is determined by median ratio
#' @param median_ratio ratio of population medians
#' @export

sim_sample <- function(n = 18,
                       this_study_error = 0.2,
                       rdist = "norm",
                       par = list(mean = 20, sd = 0.2),
                       control = TRUE,
                       median_ratio = 1.2) {
  # check inputs are valid
  assertthat::assert_that(length(par) <= 2,
                          msg = "haven't coded this for more than two parameters")
  assertthat::assert_that(rdist %in% c("exp", "norm", "lnorm", "pareto"),
                          msg = "choose exp, norm, lnorm, and pareto")
  assertthat::assert_that(is.numeric(n),
                          length(n) == 1,
                          round(n) == n,
                          msg = "n argument requires an integer")
  assertthat::assert_that(is.numeric(this_study_error),
                          length(this_study_error) == 1,
                          msg = "this_study_error should requires a number")
  assertthat::assert_that(is.logical(control),
                          msg = "control argument needs to be a logical indicating if in control group")
  assertthat::assert_that(is.numeric(median_ratio),
                          msg = "median_ratio needs to be a numeric")

  if (rdist == "norm") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] * median_ratio
    else
      par[[1]]

    # generate sample
    return(rnorm(n, mean = par_1 * exp(this_study_error), sd = par[[2]]))

  } else if (rdist == "lnorm") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] + log(median_ratio)
    else
      par[[1]]

    # generate sample
    return(rlnorm(n, par_1 + this_study_error , par[[2]]))

  } else if (rdist == "pareto") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] * median_ratio
    else
      par[[1]]

    # generate sample
    return(actuar::rpareto2(n, par_1 * exp(this_study_error), par[[2]]))

  } else if (rdist == "exp") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] / median_ratio
    else
      par[[1]]

    # generate sample
    return(toss(rexp(n, par_1 * exp(this_study_error))))
  }

}
