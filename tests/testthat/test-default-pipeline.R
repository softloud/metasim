context("default pipeline")

library(testthat)
library(metasim)

test_that("work upwards through algorithm", {
  expect_is(sim_n(), "data.frame")
  # sim_df calls sim_n
  expect_is(sim_df(), "data.frame")
  # metasim calls metatrial
  expect_is(metatrial, "data.frame")
  expect_is(singletrial(), "data.frame") # alternate trial
  expect_is(metasim(), "data.frame")
  # metasims calls sim_df & metasim
  expect_is(metasims(), "data.frame")
})
