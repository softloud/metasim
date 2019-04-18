context("metasims")

test_that("default trial_fn metatrial", {
  expect_is(metasims(probar = FALSE), "data.frame")
  expect_true(nrow(metasims(probar = FALSE)) > 0)
  expect_true("k" %in% colnames(metasims(probar = FALSE)))
  expect_true("id" %in% colnames(metasims(probar = FALSE)))
  expect_true("median_ratio" %in% colnames(metasims(probar = FALSE)))
})

test_that("singletrial trial_fn", {
  expect_is(metasims(trial_fn = singletrial, probar = FALSE), "data.frame")
  expect_true(nrow(metasims(
    trial_fn = singletrial, probar = FALSE
  )) > 0)
  expect_true("k" %in% colnames(metasims(
    trial_fn = singletrial, probar = FALSE
  )))
  expect_true("sim" %in% colnames(metasims(
    trial_fn = singletrial, probar = FALSE
  )))
  expect_true("median_ratio" %in% colnames(metasims(
    trial_fn = singletrial, probar = FALSE
  )))
})
