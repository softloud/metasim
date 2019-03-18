context("testing for zero vals")


test_that("exponential is parsed throughout", {

  # check sample
  expect_equal(sim_sample(10, rdist = "exp", par = list(rate = 3)) %>%
                 length,
               10)
  # check samples
  expect_is(sim_stats(rdist = "exp", par = list(rate = 3)), "data.frame")

  # check trial
  expect_is(metatrial(rdist = "exp", parameters = list(rate = 3),
                      true_median = log(2) / 3),
            "data.frame")

  # check sim

})
