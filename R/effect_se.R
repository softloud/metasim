#' estimator for variance of sample median
#'
#' @param centre a mean or median
#' @param spread a sd, var, iqr, or range
#' @param centre_type specify "mean" or "median"
#' @param spread_type specify "sd", "var", "iqr", or "range"
#'
#' @export

effect_se <-
  function(centre,
           spread,
           centre_type = "median",
           spread_type = "iqr",
           n = 1) {
    if (centre == "mean") {
      if (spread_type == "var") {
        return(sqrt(spread / n))
      } else if (spread_type == "sd") {
        return(spread / sqrt(n))
      }
    } else {
      1 / (
        sqrt(n) * 2 *
          metasim:::g_lnorm(
            median = centre,
            spread = spread,
            spread_type = spread_type
          )
      )
    }

  }
