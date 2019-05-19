#' get an arbitrary
#'
#' @export

density_fn <- function(x, distribution = "norm",
                       parameters = list(mean = 50, sd = 0.2),
                                         type = "q") {
  fn <- get(paste0(type, distribution))
  if (length(parameters == 1)) fn(x, parameters[[1]])
  else fn(x, parameters[[1]], parameters[[2]])

  }
