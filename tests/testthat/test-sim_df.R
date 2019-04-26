context("sim_df")

testdf <- sim_df()
prop <- runif(1, 0.2, 0.7)
k <- sample(seq(3, 100, by = 3), 1)
median_ratio <- runif(1, 0.8, 1.2)

test_that("from user inputs, generate a simulation overview dataframe", {
  expect_is(testdf, "data.frame")
  expect_gt(nrow(testdf), 0)
  expect_is(sim_df(), "data.frame")
  expect_is(testdf %>% pluck("rdist"), "character")
  expect_is(testdf %>% pluck("n"), "list")
  expect_true("median_ratio" %in% colnames(testdf))
  expect_true("true_effect" %in% colnames(testdf))
  expect_is(sim_df(prop = 0.4), "data.frame")
  expect_is(sim_df(prop = prop), "data.frame")

  # check the sample size dataset has control and intervention rows for k studies
  expect_is(testdf %>% pluck("n") %>% purrr::map_int(nrow) / 2, "numeric")
  expect_equal(testdf %>% pluck("n") %>% purrr::map_int(nrow) / 2,
               testdf %>% pluck("k") %>% as.integer())
  expect_gt(testdf %>% nrow(), 2)

  expect_is(sim_df(tau2 = 0.3), "data.frame")

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
      median_ratio = median_ratio,
      prop = prop
    ),
    "data.frame"
  )

  # check that simulation id is unique
  expect_equal(testdf %>% pluck("id") %>% unique() %>% length(),
               testdf %>% nrow())
})
