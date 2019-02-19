context("sampling")

library(tidyverse)
library(metasim)

big <- runif(3, 5, 100)
small <- runif(3, 0.1, 0.9)

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
