library(testthat)
context("time-intensive")

library(metasim)

test_that("simulation can handle more trials", {
  expect_is(metasim(trials = 10), "data.frame")
  expect_gte(metasim(trials = 10) %>% nrow(), 2)
  expect_is(metasim(trials = 100), "data.frame")
  expect_gte(metasim(trials = 100) %>% nrow(), 2)
  expect_is(metasim(trials = 1000), "data.frame")
  expect_gte(metasim(trials = 1000) %>%  nrow(), 2)
})
