context("metasims")

set.seed(38)

default_metasims <- metasims(progress = FALSE, trials = 100)

test_that("default trial_fn metatrial", {
  expect_is(default_metasims, "data.frame")
  expect_true(nrow(default_metasims) > 0)
  expect_true("k" %in% colnames(default_metasims))
  expect_true("id" %in% colnames(default_metasims))
  expect_true("effect_ratio" %in% colnames(default_metasims))
})

single_metasims <- metasims(single_study = TRUE,
                            trials = 100,
                            trial_fn = singletrial,
                            progress = FALSE)

test_that("singletrial trial_fn", {
  expect_is(single_metasims, "data.frame")
  expect_true(nrow(single_metasims) > 0)
  expect_true("k" %in% colnames(single_metasims))
  expect_true("effect_ratio" %in% colnames(single_metasims))
})

test_that("coverage is as expected", {
  expect_lt(default_metasims %>% pluck("coverage") %>% mean(), 1.0001)
  expect_gt(default_metasims %>% pluck("coverage") %>% mean(), 0.80)
  expect_lt(single_metasims %>% pluck("coverage") %>% mean(), 1.0001)
  expect_gt(single_metasims %>% pluck("coverage") %>% mean(), 0.80)
})
