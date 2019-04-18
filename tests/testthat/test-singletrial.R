context("singletrial")

library(metasim)

# fuzz samples ------------------------------------------------------------

big <- runif(3, 4, 100)
small <- runif(3, 0.1, 0.9)

# tests -------------------------------------------------------------------

test_that("defaults return expected", {
  expect_is(singletrial(), "data.frame")
  expect_equal(nrow(singletrial()), 2)
})

test_that("true effect arg", {
  expect_is(singletrial(true_effect = 5), "data.frame")
})

test_that("effect ratio", {
  expect_is(singletrial(median_ratio = 1), "data.frame")
  expect_gt(singletrial(median_ratio = 1) %>% nrow(), 1)
  expect_is(metasim(trial_fn = singletrial, median_ratio = 1) %>%
              pluck("results"),
            "data.frame")
  expect_is(
    metasim(trial_fn = singletrial, median_ratio = 1.2)  %>%
      pluck("results"),
    "data.frame"
  )
  expect_is(metasim(trial_fn = singletrial, median_ratio = 1.2) %>%
              pluck("results"),
            "data.frame")

  })

test_that("distributions", {
  expect_is(singletrial(rdist = "pareto",
                        parameters = list(shape = 2, scale = 2)),
            "data.frame")
  expect_is(singletrial(
    rdist = "pareto",
    parameters = list(shape = round(big[[1]]), round(big[[2]]))
  ),
  "data.frame")
  expect_is(singletrial(rdist = "norm",
                        parameters = list(mean = big[[1]], sd = 2)),
            "data.frame")
  expect_is(singletrial(rdist = "exp",
                        parameters = list(rate = round(big[[1]]))),
            "data.frame")
  expect_is(singletrial(rdist = "lnorm",
                        parameters = list(mean = 3, sd = 1)),
            "data.frame")
})

test_that("singletrial in sims", {
  expect_is(metasim(trial_fn = singletrial) %>% pluck("results"),
            "data.frame")
  expect_is(metasims(
    trial_fn = singletrial,
    probar = FALSE,
    k = 1,
    tau2_true = 0
  ),
  "data.frame")
})
