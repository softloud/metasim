#' @import dplyr
#' @import purrr
#' @importFrom metafor rma escalc forest
#' @importFrom janitor clean_names
#' @importFrom tibble tibble as_tibble
#' @importFrom stats dist median quantile rexp rlnorm rnorm runif sd
#' @importFrom varameta effect_se
#' @importFrom graphics par
#' @importFrom stats qnorm
#' @importFrom utils setTxtProgressBar txtProgressBar
NULL

#' @export
magrittr::`%>%`

#' @export
metafor::rma

#' @export
metafor::forest

#' @export
metafor::escalc

#' @export
purrr::pluck

#' @export
janitor::clean_names
