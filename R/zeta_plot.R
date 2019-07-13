#' plot the distribution of the proportion allocated to the intervention group
#'
#' @export

zeta_plot <- function(mu, epsilon) {
  # calculate parameters
  par <- beta_par(mu, epsilon)

  # return plot of beta distribution with parameters
  tibble(x = c(0, 1)) %>%
    ggplot(aes(x = x)) +
    geom_rect(xmin = mu - epsilon, xmax = mu + epsilon, ymin = 0, ymax = Inf, alpha = 0.2) +
    geom_vline(xintercept = mu, linetype = "dashed", alpha = 0.8) +
    stat_function(fun = dbeta,
                  linetype = "dotted",
                  args = list(shape1 = par$alpha, shape2 = par$beta)) +
    labs(title = str_wrap("Distribution of expected proportion of intervention cohort"
),
      x = TeX("$\\zeta$"),
      y = NULL,
      caption = str_wrap(paste0(
        "We assume a beta distribution with expected centre ",
        mu,
        " and 90% of values falling within ",
        epsilon,
        "; i.e, within the interval [",
        mu - epsilon,
        ",",
        mu + epsilon,
        "]"), width = 50)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


}
