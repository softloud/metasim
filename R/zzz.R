#' @import dplyr
#' @import purrr
#' @import broom
#' @importFrom metafor rma escalc forest
#' @importFrom tibble tibble as_tibble
#' @importFrom stats dist median quantile rexp rlnorm rnorm runif sd
#' @importFrom varameta effect_se
#' @importFrom dontpanic dist_name
#' @importFrom janitor clean_names
NULL

#' todo: update this when these methods are integrated into broom::
#' @export
metabroom::tidy

#' todo: update this when these methods are integrated into broom::
#' @export
metabroom::glance

#' todo: update this when these methods are integrated into broom::
#' @export
metabroom::augment

#' @export
magrittr::`%>%`

#' @export
metafor::rma

#' @export
metafor::forest

#' @export
metafor::escalc

#' @export
dontpanic::dist_name

#' @export
purrr::pluck
