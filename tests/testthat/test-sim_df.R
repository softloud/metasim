context("sim_df")

test_that("from user inputs, generate a simulation overview dataframe", {
  expect_is(testdf, "data.frame")
  expect_gt(nrow(testdf), 0)
  expect_is(sim_df(), "data.frame")
  expect_is(testdf %>% pluck("rdist"), "character")
  expect_is(testdf %>% pluck("n"), "list")
  expect_true("median_ratio" %in% colnames(testdf))
  expect_true("true_median" %in% colnames(testdf))
  expect_is(sim_df(prop = 0.4), "data.frame")
  expect_is(sim_df(prop = prop), "data.frame")

  # check the sample size dataset has control and intervention rows for k studies
  expect_is(testdf %>% pluck("n") %>% map_int(nrow) / 2, "numeric")
  expect_equal(testdf %>% pluck("n") %>% map_int(nrow) / 2,
               testdf %>% pluck("k") %>% as.integer())
  expect_gt(testdf %>% nrow(), 2)

  expect_is(sim_df(between_study_variation = 0.3), "data.frame")

  expect_is(
    sim_df(
      dist_tribble =
        tibble::tribble(
          ~ dist,
          ~ par,
          "norm",
          list(mean = 50, sd = 0.2),
          "exp",
          list(rate = 2)
        ),
      k = k,
      between_study_variation = between_study_variation,
      median_ratio = median_ratio,
      prop = prop
    ),
    "data.frame"
  )

  # check that simulation id is unique
  expect_equal(testdf %>% pluck("id") %>% unique() %>% length(),
               testdf %>% nrow())
})
