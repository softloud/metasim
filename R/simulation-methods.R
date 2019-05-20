#' A first pass at S3 methods for simulation objects
#'
#' Methods for simulation results.
NULL

#' Plot coverage probability for single-study simulation
#'
#' Plot single-study simulation results.
#'
#' @param sim A `sim_ss` object from [metasim].
#' @param caption Whether or not to include the caption.
#'
#' @export

plot.sim_ss <- function(sim = single_sim, caption = NULL) {
  # shorthand for now
  sim %>%
    pluck("results") %>%
    # labels
    mutate(
      distribution = map_chr(rdist, dist_name),
      effect_ratio = if_else(
        is.na(effect_ratio),
        "single measure",
        as.character(round(as.numeric(effect_ratio), 1))
      ),
      measure = if_else(
        str_detect(measure, "lr_"),
        paste0("log-ratio of ", str_remove(measure, "lr_"), "s"),
        measure
      )
    ) %>%
    ggplot(aes(x = distribution, y = coverage)) +
    geom_point(
      position = "jitter",
      aes(colour = distribution,
          shape = effect_ratio),
      alpha = 0.6,
      size = 2) +
    scale_shape_discrete(name = "Effect ratio", drop = FALSE) +
    hrbrthemes::scale_colour_ipsum(name = "Distribution") +
    labs(x = "Distribution",
         y = "Coverage",
         title = str_wrap("Simulation results for estimating the variance of the sample median")) +
    theme(
      axis.text.x = element_text(
        angle = 35,
        hjust = 1,
        vjust = 1
      )) +
    facet_grid(~ measure)
}

#' Generic caption function.

caption <- function(sim) {
  UseMethod("caption")
}

#' Caption for simulation object.
#'
#' Output caption as a character string.
#'
#' @export

