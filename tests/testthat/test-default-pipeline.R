context("default pipeline")

library(testthat)
library(metasim)

test_that("work upwards through algorithm", {
  expect_is(sim_n(), "data.frame")
  expect_gt(sim_n() %>% nrow(), 1)
  # sim_df calls sim_n
  expect_is(sim_df(), "data.frame")
  expect_is(sim_stats(), "data.frame")
  # metasim calls metatrial
  expect_is(metatrial(), "data.frame")
  expect_is(singletrial(), "data.frame") # alternate trial
  expect_is(metasim(trials = 3), "data.frame")
  # metasims calls sim_df & metasim
  expect_is(metasims(single_study = FALSE, trials = 3, probar = FALSE), "data.frame")
})



# test each component on defaults

test_that("sim_n", {
  expect_is(sim_n(), "data.frame")
})

test_that("sim_df", {
  expect_is(sim_df(), "data.frame")
})

test_that("metatrial", {
  # metasim calls metatrial
  expect_is(metatrial(), "data.frame")
})

test_that("singletrial", {
  expect_is(singletrial(), "data.frame") # alternate trial
})

test_that("metasim", {
  expect_is(metasim(trials =  3), "data.frame")
})

test_that("metasims", {
  expect_is(metasims(single_study = FALSE, trials = 3, probar = FALSE), "data.frame")
})
