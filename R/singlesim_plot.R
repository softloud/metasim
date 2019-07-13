#' plot coverage probability
#'
#' Plot single-study simulation data
#'
#' Expects input of the return of [metasims] (with default `single_study = FALSE`).
#'
#' @param sim The output of the [metasims] function.
#'
#' @export

metasim_plot <- function(sim = meta_sim) {

  sim %>%
    mutate(distribution = map_chr(rdist, dist_name)) %>%
    ggplot(aes(x = distribution, y = coverage)) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          plot.caption = element_text(hjust = 0)) +
    labs(x = "Distribution",
         y = "Coverage"
    )  +  geom_point(position = "jitter",
                     aes(colour = distribution,
                         shape = as.character(effect_ratio)),
                     alpha = 0.6, size = 2) +
    facet_grid(k ~ tau_sq_true, scales = "free") +
    scale_shape_discrete(name = "Effect ratio") +
    hrbrthemes::scale_colour_ipsum(name = "Distribution") +
    labs(title = str_wrap("Simulated meta-analysis results for estimating the variance of the median"),
         caption = NULL)
}

#' caption distributions
#'
#' caption for [metasim_plot].
#'
#' @export


#' Plot single-study simulation data
#'
#' Expects input of the return of [metasims] with `single_study = TRUE`.
#'
#' @param sim The output of the [metasims] function.
#'
#' @export



#' caption distributions
#'
#' caption for [singlesim_plot]
#'
#' @export

singlesim_caption <- function(sim,  distributions = default_parameters,
                                cap_width = 110,
                                measure = "median",
                                metasims_args = args(metasims) %>% as.list()) {
  # store values to parameterise this

  # return
  single_sim_caption


}
