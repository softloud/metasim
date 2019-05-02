#' Meta-analyse with error handling
#'
#' If random-effects model fails to converge, then replace with fixed-effects model,
#' else, produce error string.
#'
#' @param test "knha" or "z"
#'
#' @export


metamodel <- function(
  y = rnorm(10, mean = 20, sd = 0.2),
  se = runif(10, 0, 0.8),
  test = "knha",
  data = NULL
){

  # try a re model; may fail to converge.
  rma <- try(
    rma(yi = y,  sei = se, test = test, data = data) %>%
      tidy_sim(),
    silent = TRUE
             )

  # return re if or calculate fe
  if (
    "data.frame" %in% class(rma)
  ) {
    rma
  } else {
    try(
      rma(yi = y, sei = se, method = "FE", data = data) %>%
        tidy_sim()
      )
  }
}
