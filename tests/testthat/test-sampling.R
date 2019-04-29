context("sampling")

library(tidyverse)
library(metasim)
set.seed(38)

big <- runif(3, 5, 100)
small <- runif(3, 0.1, 0.9)


# sample sizes ------------------------------------------------------------


samples_sizes <- rerun(1000, sim_n(k = 10)) %>% bind_rows()

test_that("sample sizes are positive", {
  expect_equal(samples_sizes %>%
                 filter(n < 0) %>%
                 nrow(), 0)
  expect_error(sim_n(min_n = -30))
  expect_error(sim_n(min_n = -30, max_n = -100))
  expect_error(sim_n(min_n= 50, max_n = 2))
})

# samples -----------------------------------------------------------------

test_sample_norm <-
  sim_sample(10, 0, "norm", list(mean = 20, sd = 1))
test_sample_norm_another <-
  sim_sample(10, 0, "norm", list(mean = 104, sd = 0.3))
test_sample_pareto <- sim_sample(10, 0, "pareto", list(1, 2))

test_that("samples are plausible", {
  expect_is(test_sample_norm, "numeric")
  expect_lt(test_sample_norm %>% mean(), 50)
  expect_gt(test_sample_norm %>% mean(), 5)
  expect_lt(test_sample_norm %>% mean(), 22)
  expect_gt(test_sample_norm %>% mean(), 18)

  expect_is(test_sample_norm_another, "numeric")
  expect_lt(test_sample_norm_another %>% mean(), 200)
  expect_gt(test_sample_norm_another %>% mean(), 5)
  expect_lt(test_sample_norm_another %>% mean(), 106)
  expect_gt(test_sample_norm_another %>% mean(), 102)

  expect_is(test_sample_pareto, "numeric")
  expect_lt(test_sample_pareto %>% mean(), 100)
  expect_gt(test_sample_pareto %>% mean(), 0)

  # test the exponetial
  expect_is(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)),
            "numeric")
  expect_is(sim_sample(rdist = "exp", par = list(rate = 2)), "numeric")
  expect_is(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)),
            "numeric")

  expect_gt(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)) %>%
              length, 2)
  expect_gt(sim_sample(rdist = "exp", par = list(rate = 2)) %>% length, 2)
  expect_gt(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)) %>%
              length,
            2)
  expect_gt(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)) %>%
              unique %>% length, 2)
  expect_gt(sim_sample(rdist = "exp", par = list(rate = 2)) %>%
              unique %>% length, 2)
  expect_gt(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)) %>%
              unique %>% length, 2)

  expect_is(sim_sample(rdist = "lnorm",
                       par = list(meanlog = big[[1]], sd = small[1])),
            "numeric")
  expect_gt(sim_sample(rdist = "lnorm",
                       par = list(meanlog = big[[1]], sd = small[1])) %>%
              length, 2)
})

  test_that("sim stats gives non-empty dataframe",{

    expect_is(sim_stats(), "data.frame")
    expect_gt(sim_stats() %>% nrow(), 2)

    # test distributions

    # norm
    expect_is(sim_stats(rdist = "norm", par = list(mean = 57, sd = 0.2)), "data.frame")
    expect_gt(sim_stats(rdist = "norm", par = list(mean = 57, sd = 0.2)) %>% nrow(), 2)
    expect_is(sim_stats(rdist = "norm", par = list(mean = big[[1]], sd = small[[1]])), "data.frame")
    expect_gt(sim_stats(rdist = "norm", par = list(mean = big[[1]], sd = small[[1]])) %>% nrow(), 2)

    # lnorm
    expect_is(sim_stats(rdist = "lnorm", par = list(mean = 57, sd = 0.2)), "data.frame")
    expect_gt(sim_stats(rdist = "lnorm", par = list(mean = 57, sd = 0.2)) %>% nrow(), 2)
    expect_is(sim_stats(rdist = "lnorm", par = list(mean = big[[1]], sd = small[[1]])), "data.frame")
    expect_gt(sim_stats(rdist = "lnorm", par = list(mean = big[[1]], sd = small[[1]])) %>% nrow(), 2)

    # exp
    expect_is(sim_stats(rdist = "exp", par = list(rate = 3)), "data.frame")
    expect_gt(sim_stats(rdist = "exp", par = list(rate = 3)) %>% nrow(), 2)
    expect_is(sim_stats(rdist = "exp", par = list(rate = round(big[[1]]))), "data.frame")
    expect_gt(sim_stats(rdist = "exp", par = list(rate = round(big[[1]]))) %>% nrow(), 2)

    # pareto
    expect_is(sim_stats(rdist = "pareto", par = list(shape = 3, scale = 2)), "data.frame")
    expect_gt(sim_stats(rdist = "pareto", par = list(shape = 3, scale = 2)) %>% nrow(), 2)
  })
