#' plot the distribution of the proportion allocated to the intervention group
#'
#' @export

zeta_plot <- function(mu, epsilon) {

  # calculate parameters
  par <- beta_par(mu, epsilon)

  # return plot of beta distribution with parameters
  tibble(x = c(0, 1)) %>%
    ggplot(aes(x = x)) +
    stat_function(fun = dbeta, args = list(shape1 = par$alpha, shape2 = par$beta)) +
    labs(title = str_wrap(paste0(
      "beta distribution with expected centre ",
      mu,
      " and 90% of values falling within ",
      epsilon
    ))) +
    theme(title = element_text(size = 5))
}
