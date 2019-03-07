#' Toss away bad simulated data
#'
#' This function returns a function's output if the function runs, or allows the
#' user to customise the error and warning messages if the function throws
#' either.
#'
#' @examples
#'
#' sum(c(1, 2, 3))
#'
#' # an element of wrong type will cause sum to throw an error
#' \dontrun{
#' sum(c(1, 2, "a"))
#' }
#'
#' # toss returns function output if function runs
#' toss(sum(c(1, 2, 3)))
#'
#' # toss returns NULL by default if function does not run
#' toss(sum(c(1, 2, "a")))
#'
#' # toss allows you to customise the error message
#' toss(sum(c(1, 2, "a")), error_msg = "the cake is a lie")
#'
#' @export

toss <- function(code, error_msg = NULL, warning_msg = NULL) {

  tryCatch(
    expr = {
      code
    },
    error = function(e) {
      error_msg
    },
    warning = function(w) {
      warning_msg
    }

  )
}
