#' plot the distribution of the proportion allocated to the intervention group
#'
#' @export

zeta_plot <- function(mu, epsilon) {
  assert_that(mu < 1 && mu > 0, msg = "mu must be between 0 and 1")
  assert_that(epsilon < 1 && epsilon > 0, msg = "epsilon must be between 0 and 1")

  # calculate parameters
  par <- beta_par(mu, epsilon)

  # return plot of beta distribution with parameters
  tibble(x = c(0, 1)) %>%
    ggplot(aes(x = x)) +
    geom_vline(xintercept = mu, linetype = "dashed", alpha = 0.3) +
    geom_rect(
      alpha = 0.1,
      xmin = mu - epsilon,
      xmax = mu + epsilon,
      ymin = 0,
      ymax = Inf) +
    stat_function(fun = dbeta,
                  size = 1,
                  linetype = "dotted",
                  args = list(shape1 = par$alpha, shape2 = par$beta)) +
    labs(title = str_wrap(paste0(
      "Distribution of possible intervention cohort proportions" )),
    caption = str_wrap(paste0("with expected centre ",
      mu,
      " and 90% of values falling within ",
      epsilon,
    ". Vertical dashed line represents expected centre, and shaded area represents what values we are 90 per cent confident the observed proportion of the case sample size, of the combined case and control groups, will take.")),
    x = str_wrap("possible proportion of case group of total sample size"),  y = NULL) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
}
