context("bug hunt")

set.seed(38)

library(tidyverse)


test_that("rerun metatrial captures errors", {
  # different values of n
  expect_is(purrr::rerun(.n = 10, metatrial(
  )) %>% bind_rows(),
  "data.frame")

  expect_is(purrr::rerun(.n = 100, metatrial(
  )) %>% bind_rows(),
  "data.frame")
})


test_that("exponential is parsed throughout", {

  # check sample
  expect_equal(sim_sample(10, rdist = "exp", par = list(rate = 3)) %>%
                 length,
               10)
  # check samples
  expect_is(sim_stats(rdist = "exp", par = list(rate = 3)), "data.frame")

  # check trial
  expect_is(metatrial(rdist = "exp", parameters = list(rate = 3),
                      true_median = log(2) / 3),
            "data.frame")

  expect_is(purrr::rerun(.n = 10, metatrial(
  )) %>% bind_rows(), "data.frame")

  # check sim
  expect_is(purrr::rerun(.n = 10, metatrial(
    rdist = "exp",
    parameters = list(rate = 3)
  )) %>% bind_rows(), "data.frame")

})
