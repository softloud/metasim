#' A first pass at S3 methods for simulation objects
#'
#' Methods for simulation results.
#' @name plots
NULL

# parameters --------------------------------------------------------------
#' extract parmeters
#'
#' @rdname plots
#' @export
sim_parameter <- function(sim, arg) {
  sim %>%
    purrr::pluck("arguments") %>%
    dplyr::filter(argument == arg) %>%
    purrr::pluck("value")
}

# captions ----------------------------------------------------------------

#' Generic caption function.
#' @export

caption <- function(sim) {
  UseMethod("caption")
}

#' @export

caption.default <- function(sim) {
  "requires an object produced by metasims()"
}

#' @export
#' @rdname plots

caption_text <- function(sim) {
  unequal_effect_ratio <-
    sim %>% sim_parameter("unequal_effect_ratio") %>%
    as.numeric()

  trials <- sim %>% sim_parameter("trials") %>% as.numeric()

  measure <- sim %>% sim_parameter("measure")


  paste0(
    "a) These simulation results are summarised by the proportion, coverage, of ",
    trials,
    " confidence intervals that contain the true measure $\\nu$, or log-ratio $\\log(\\rho)$ of control $\\nu_C$ and intervention $\\nu_I$ measures, with $\\rho := \\nu_I/\\nu_C$.\n
The case where the control median $\\nu_C$ is equal to the intervention median $\\nu_I$ is considered, $\\rho= 1$, as is an unequal ratio, $\\rho =$ ",
    unequal_effect_ratio,
    ". Data for the control and intervention arms sampled from distributions in accompanying Distributions plot.
Summary statistics were calculated from the samples, for the estimator of the variance $\\mathcal V(m)$ of the sample ",
    measure,
    ".\n"
  )
}

#' @rdname plots
#' @export

caption.sim_ma <- function(sim) {
  k <- sim %>% sim_parameter("k") %>% parse(text = .) %>% eval()

  tau_sq_true <- sim %>% sim_parameter("tau_sq_true") %>% parse(text = .) %>% eval()

  trials <- sim %>% sim_parameter('trials') %>% as.numeric()

  sim %>%
    caption_text() %>%
    paste0(
      "For each of the ",
      trials,
      " $K$ studies' control and intervention arms were simulated. A meta-analysis was performed via rma from the metafor package. The confidence interval that informs the coverage was extracted from the model results.
Simulations, ",
      trials,
      " trials, were performed for each of $K =$ `r k` studies.
The cases where there where there is no variation between the studies, $\tau^2 = 0$, and some variation introduced by independent factors other than the measure of interest for a given study,  $\tau^2 =$",
      tau_sq_true[-1],
      "are provided."
    )
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


caption.sim_ss <- function(sim) {
  measure <- sim %>% sim_parameter("measure")
  trials <- sim %>% sim_parameter("trials") %>% as.numeric()

  sim %>%
    caption_text() %>%
    paste0(
      "For each of the ",
      trials,
      " trials, a confidence interval, $\theta \\pm \\Phi^{-1}(0.975) \\sqrt(\\mathcal V(\theta))$, was calculated, where $\theta$ estimates the true ",
      measure,
      "or log-ratio of control and intervention ",
      measure,
      "s."
    )
}


  caption.sim_ss <- function(sim) {
    sim %>%
      caption_text()
  }


# plots -------------------------------------------------------------------

#' @rdname plots
#' @export

plot.sim_ss <- function(sim = single_sim, caption = NULL) {
  # shorthand for now
  sim %>%
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
    hrbrthemes::scale_colour_ipsum(name = "Distribution") +
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
    facet_grid( ~ measure)
}






