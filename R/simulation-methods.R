#' A first pass at S3 methods for datulation objects
#'
#' Methods for datulation results.
#' @name plots
NULL

# parameters --------------------------------------------------------------
#' extract parmeters
#'
#' @rdname plots
#' @export
dat_parameter <- function(dat, arg) {
  dat %>%
    purrr::pluck("arguments") %>%
    dplyr::filter(argument == arg) %>%
    purrr::pluck("value")
}

# captions ----------------------------------------------------------------

#' Generic caption function.
#' @export

caption <- function(dat) {
  UseMethod("caption")
}

#' @export

caption.default <- function(dat) {
  "metadat::caption() requires an object produced by metasim::metasims()"
}

#' @export
#' @rdname plots

caption_text <- function(dat) {
  unequal_effect_ratio <-
    dat %>% dat_parameter("unequal_effect_ratio") %>%
    as.numeric()

  trials <- dat %>% dat_parameter("trials") %>% as.numeric()

  measure <- dat %>% dat_parameter("measure")


  paste0(
    "a) These simulation results are summarised by the proportion, coverage, of ",
    trials,
    " confidence intervals that contain the true measure $\\nu$, or log-ratio $\\log(\\rho)$ of control $\\nu_C$ and intervention $\\nu_I$ measures, with $\\rho := \\nu_I/\\nu_C$.\n
The case where the control median $\\nu_C$ is equal to the intervention median $\\nu_I$ is considered, $\\rho= 1$, as is an unequal ratio, $\\rho =$ ",
    unequal_effect_ratio,
    ".\n Data for the control and intervention arms sampled from distributions in accompanying b) Distributions plot.\n
Summary statistics were calculated from the samples, for the estimator of the variance $\\mathcal V(m)$ of the sample ",
    measure,
    ".\n"
  )
}

#' @rdname plots
#' @export

caption.sim_ma <- function(dat) {
  k <- dat %>% dat_parameter("k") %>% parse(text = .) %>% eval()

  tau_sq_true <-
    dat %>% dat_parameter("tau_sq_true") %>% parse(text = .) %>% eval()

  trials <- dat %>% dat_parameter('trials') %>% as.numeric()

  dat %>%
    caption_text() # %>%
  #     paste0(
  #       "For each of the ",
  #       trials,
  #       " $K$ studies' control and intervention arms were simulated. A meta-analysis was performed via rma from the metafor package. The confidence interval that informs the coverage was extracted from the model results.
  # Simulations, ",
  #       trials,
  #       " trials, were performed for each of $K =$ `r k` studies.
  # The cases where there where there is no variation between the studies, $\tau^2 = 0$, and some variation introduced by independent factors other than the measure of interest for a given study,  $\tau^2 =$",
  #       tau_sq_true[-1],
  #       "are provided."
  #     )
}

#' Plot coverage probability for single-study simulation
#'
#' Plot single-study simulation results.
#'
#' @param sim A `sim_ss` object from [metasim].
#' @param caption Whether or not to include the caption.
#'
#' @rdname plots
#' @export


caption.sim_ss <- function(dat) {
  measure <- dat %>% dat_parameter("measure")
  trials <- dat %>% dat_parameter("trials") %>% as.numeric()

  dat %>%
    caption_text() %>%
    paste0(
      "For each of the ",
      trials,
      " trials, a confidence interval, $\\theta \\pm \\Phi^{-1}(0.975) \\sqrt{\\mathcal V(\\theta)}$, was calculated, where $\\theta$ estimates the true ",
      measure,
      " or log-ratio of control and intervention ",
      measure,
      "s."
    )  # %>% str_wrap(width = width) %>% paste0("\n")

}

#' @rdname plots
#' @export


caption.distributions <- function(dat) {
  dat %>%
    mutate(distribution = map_chr(dist, dist_name),
           output = paste0("the ", distribution,
                           " distribution, with parameters "),
           par_count = map_int(par, length),
           par_1 = map_dbl(par, 1),
           par_2 = map(par, 2),
           output = case_when(
             par_count == 1 ~ paste0(output, par_1),
             par_count == 2 ~ paste0(output, par_1, " and ", par_2)
           )) %>% pluck("output") %>%
    paste(., collapse = ", ") %>%
    # maybe: might want to extend sampled ot not just random sampling at some point
    paste0("Distributions compared: ", ., ".")
}


# plots -------------------------------------------------------------------

#' @rdname plots
#' @export

plot.sim_ss <- function(dat = single_sim, caption = NULL) {
  # shorthand for now
  dat %>%
    pluck("results") %>%
    # labels
    mutate(
      distribution = map_chr(rdist, dist_name),
      effect_ratio = if_else(is.na(effect_ratio),
                             "single measure",
                             as.character(round(
                               as.numeric(effect_ratio), 1
                             ))),
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
      size = 2
    ) +
    scale_shape_discrete(name = "Effect ratio", drop = FALSE) +
    facet_grid(~ measure) +
    labs(
      x = "Distribution",
      y = "Coverage",
      title = str_wrap(
        "Simulation results for estimating the variance of the sample median"
      ),
      caption = caption
    ) +
    theme(axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      vjust = 1
    )) +
    hrbrthemes::scale_colour_ipsum(name = "Distribution")
}

#' @rdname plots
#' @export

plot.sim_ma <- function(dat, caption = NULL) {
  dat %>%
    pluck("results") %>%
    # labels
    mutate(
      distribution = map_chr(rdist, dist_name),
      effect_ratio = if_else(is.na(effect_ratio),
                             "single measure",
                             as.character(round(
                               as.numeric(effect_ratio), 1
                             ))),
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
      size = 2
    ) +
    scale_shape_discrete(name = "Effect ratio", drop = FALSE) +
    facet_grid(k ~ tau_sq_true) +
    labs(
      x = "Distribution",
      y = "Coverage",
      title = str_wrap(
        "Simulation results for estimating the variance of the sample median"
      ),
      caption = NULL
    ) +
    theme(axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      vjust = 1
    )) +
    hrbrthemes::scale_colour_ipsum(name = "Distribution")
}

#' @rdname plots
#' @export

plot.dist <- function(dat, caption = NULL){
  dat %>%
    uncount(weights = 50, .id = "x") %>%
    mutate(
      x = x / 10,
      y = pmap_dbl(
        list(
          x = x,
          dist = dist,
          par = par
        ), .f = function(x, dist, par) {
          if (dist == "pareto") actuar::dpareto2(x, par[[1]], par[[2]])
          else density_fn(x, distribution = dist, parameters = par, type = "d")
        }
      ),
      density = map2_chr(dist, par, .f = function(x,y) {
        paste(dist_name(x), paste(y, collapse = ", "))
      })) %>%
    mutate(distribution = map_chr(dist, dist_name)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(aes(group = density,
                  colour = distribution),
              linetype = "dotted") +
    hrbrthemes::scale_color_ipsum() +
    facet_grid(distribution ~ ., scales = "free") +
    labs(title = "Distributions sampled",
         x = NULL,
         y = "density",
         caption = str_wrap(caption))
}
