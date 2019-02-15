# could not get this to play nice
# rma_safely <- purrr::safely(metafor::rma)
#
# one of emily's fabulous contributions, thank you, emily!

#' Toss away bad simulated data
#'
#' This function (hopefully) will toss away crappy stuff from the
#' \code{\link{metatrial}} and metasim functions.

toss <- function(code, silent = FALSE) {
  # tryCatch(
  #   code,
  #   error = function(c) { # message c is the error message
  #     msg <- conditionMessage(c)
  #     if (!silent)
  #       message(c)
  #     invisible(structure(msg, class = "try-error"))
  #   }
  # )
  tryCatch(
    expr = {
      code
    },
    error = function(e) {
      NULL
    },
    warning = function(w) {
      NULL
    }

  )
}
