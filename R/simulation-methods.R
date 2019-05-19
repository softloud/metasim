#' A first pass at S3 methods for simulation objects
#'
#' Methods for simulation results.
NULL

#' Plot coverage probability for single-study simulation
#'
#' Plot single-study simulation results.
#'
#' @param sim A `sim_ss` object from [metasim].
#'
#' @export

plot.sim_ss <- function(sim = single_sim) {
  # shorthand for now
  sim %>%
    pluck("results") %>%
    # labels
    mutate(
      distribution = map_chr(rdist, dist_name),
      effect_ratio = if_else(
        is.na(effect_ratio),
        "single measure",
        as.character(effect_ratio)
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
    scale_shape_discrete(namet = "Effect ratio", drop = FALSE) +
    hrbrthemes::scale_colour_ipsum(name = "Distribution") +
    labs(x = "Distribution",
         y = "Coverage",
         title = "Simulation results for estimating the variance \nof the sample median") +
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

caption.sim_ss <- function(sim) {
  k <- metasims_args %>% pluck("k") %>% eval()
  tau_sq <- metasims_args %>% pluck("tau2_true") %>% eval()
  effect_ratio <- metasims_args %>% pluck("effect_ratio") %>% eval()

  caption_distributions <- distributions %>%
    mutate(
      distribution = map_chr(dist, dist_name),
      output = paste0("the ", distribution,
                      " distribution, with parameters "),
      par_count = map_int(par, length),
      par_1 = map_dbl(par, 1),
      par_2 = map(par, 2),
      output = case_when(
        par_count == 1 ~ paste0(output, par_1),
        par_count == 2 ~ paste0(output, par_1, " and ", par_2)
      )
    ) %>% pluck("output") %>% paste(., collapse = ", ")



  single_sim_caption <- paste0(
    "Simulation results for the proportion, \emph{coverage}, of confidence intervals that contain the true measure of interest, the ",
    measure,
    " and, log-ratio of the control ",
    measure,
    " with intervention ",
    measure,
    ". The case where the control ",
    measure,
    " is equal to the intervention ",
    measure,
    " is considered, as is unequal ratio = ",
    metasims_args$effect_ratio %>% eval() %>% paste(collapse = ","),
    ". The distributions considered were ",
    caption_distributions,
    "."
  )

}
