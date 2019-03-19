#' Fix errant n
#'
#' @export

n_check <- function(x) {
  if (x < 10) abs(x) + 10 else x
}
