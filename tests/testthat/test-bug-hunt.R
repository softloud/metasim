context("bug hunt")

set.seed(38)

library(tidyverse)
library(metasim)

test_that("metasim runs for different n", {
  expect_is(metasim(), 'data.frame')
  expect_is(metasim(trials = 100) , "data.frame")
  # expect_is(metasim(trials = 1000) , "data.frame")
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
                      true_effect = log(2) / 3),
            "data.frame")

  expect_is(purrr::rerun(.n = 10, metatrial(
  )) %>% bind_rows(), "data.frame")

  # check sim
  expect_is(purrr::rerun(.n = 10, metatrial(
    rdist = "exp",
    parameters = list(rate = 3),
    true_effect = log(2) / 3
  )) %>% bind_rows(), "data.frame")

})
