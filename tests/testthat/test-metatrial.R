context("metatrial")

library(metasim)


# fuzz samples ------------------------------------------------------------

big <- runif(3, 4, 100)
small <- runif(3, 0.1, 0.9)

# tests -------------------------------------------------------------------

test_that("defaults return expected", {
  expect_is(metatrial(), "data.frame")
  expect_equal(nrow(metatrial()), 2)
})

test_that("true effect arg", {
  expect_is(metatrial(true_effect = 5), "data.frame")
})

test_that("distributions", {
  expect_is(metatrial(rdist = "pareto",
                      parameters = list(shape = 2, scale = 2)),
            "data.frame")
  expect_is(metatrial(
    rdist = "pareto",
    parameters = list(shape = round(big[[1]]), round(big[[2]]))
  ),
  "data.frame")
  expect_is(metatrial(rdist = "norm",
                      parameters = list(mean = big[[1]], sd = 2)),
            "data.frame")
  expect_is(metatrial(rdist = "exp",
                      parameters = list(rate = round(big[[1]]))),
            "data.frame")
  expect_is(metatrial(rdist = "lnorm",
                      parameters = list(mean = 3, sd = 1)),
            "data.frame")
})
