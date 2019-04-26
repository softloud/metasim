library(testthat)
context("time-intensive")

library(metasim)

test_that("simulation can handle more trials", {
  expect_is(metasim(trials = 10) %>% pluck("results"), "data.frame")
  expect_gte(metasim(trials = 10) %>% pluck("results") %>% nrow(), 2)
  expect_is(metasim(trials = 100) %>% pluck("results"), "data.frame")
  expect_gte(metasim(trials = 100) %>% pluck("results") %>% nrow(), 2)
  expect_is(metasim(trials = 1000) %>% pluck("results"), "data.frame")
  expect_gte(metasim(trials = 1000) %>% pluck("results") %>%  nrow(), 2)
})
