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
#' @param effect_ratio ratio of population effects intervention / control
#' @export

sim_sample <- function(n = 18,
                       this_study_error = 0.2,
                       rdist = "norm",
                       par = list(mean = 20, sd = 0.2),
                       control = TRUE,
                       effect_ratio = 1.2) {
  # check inputs are valid
  assert_that(length(par) <= 2,
                          msg = "haven't coded this
                          for more than two parameters")
  assert_that(rdist %in% c("exp", "norm", "lnorm", "pareto"),
                          msg = "choose exp, norm, lnorm, and pareto")
  assert_that(is.numeric(n),
                          length(n) == 1,
                          round(n) == n,
                          msg = "n argument requires an integer")
  assert_that(is.numeric(this_study_error),
                          length(this_study_error) == 1,
                          msg = "this_study_error should requires a number")
  assert_that(is.logical(control),
                          msg = "control argument needs to be a logical
                          indicating if in control group")
  assert_that(is.numeric(effect_ratio),
                          msg = "effect_ratio needs to be a numeric")

  # set up sign for different arms
  beta <- if (control == TRUE) {
    -1
  } else {
    1
  }

  if (rdist == "norm") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] * effect_ratio
    else
      par[[1]]

    # generate sample
    return(rnorm(
      n,
      mean = par_1 * exp(beta * this_study_error),
      sd = par[[2]]
    ))

  } else if (rdist == "lnorm") {
    # set value of first parameter to ensure median ratio
    par_1 <-
      if (control == FALSE)
        par[[1]] + log(effect_ratio)
    else
      par[[1]]

    # generate sample
    return(rlnorm(n, par_1 + beta * this_study_error , par[[2]]))

  } else if (rdist == "pareto") {
    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) {
      par[[1]] * effect_ratio
    } else {
      par[[1]]
    }

    # generate sample
    return(actuar::rpareto2(n, par_1 * exp(beta * this_study_error), par[[2]]))

  } else if (rdist == "exp") {
    # set value of first parameter to ensure median ratio
    par_1 <- if (control == FALSE) {
      par[[1]] * effect_ratio # todo: double check this maths
    } else {
      par[[1]]
    }

    # generate sample
    return(rexp(n, par_1 * exp(beta * this_study_error)))
  }

}
