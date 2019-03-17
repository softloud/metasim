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
    metafor::rma(yi = y,  sei = se, test = test, data = data) %>%
      metabroom::tidy() %>%
      mutate(method = "REML")
             )


  # return re if or calculate fe
  if (
    "data.frame" %in% class(rma)
  ) {
    rma
  } else {
    try(
      metafor::rma(yi = y, sei = se, method = "FE", data = data) %>%
        metabroom::tidy() %>%
        mutate(method = "FE")
      )
  }
}
