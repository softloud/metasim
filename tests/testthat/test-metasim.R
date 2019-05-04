context("metasim")

set.seed(38)

effect_ratio <- runif(1, 0.8, 1.2)
tau <- runif(1, 0.2, 0.8)

test_that("simulation runs over defaults", {
  # test defaults work
  expect_is(metasim() , "data.frame")
  expect_gt(metasim()  %>% nrow(), 1)

})

test_that("simulation runs over other inputs", {
  # todo: problems

  expect_is(metasim(
    rdist = "pareto",
    par = list(shape = 2, scale = 3)
  ), "data.frame")
  expect_is(metasim(
    rdist = "lnorm",
    par = list(meanlog = 67, sdlog = 0.25)), "data.frame")

  # test simulation

  expect_is(metasim(rdist = "norm",
                    par = list(mean = 67, sd = 0.25)), "data.frame")
  expect_is(metasim(rdist = "exp", par = list(rate = 3)),
            "data.frame")
  # test equal rataios
  expect_is(metasim(effect_ratio = 1) ,
            "data.frame")
  expect_is(metasim(effect_ratio = 1.3) ,
            "data.frame")
  expect_is(metasim(effect_ratio = effect_ratio) ,
            "data.frame")
  # test tau values
  expect_is(metasim(tau = 0) , "data.frame")
  expect_is(metasim(tau = tau) , "data.frame")

  # # check that coverage probability is above 0.9.
  expect_gt(metasim(trials = 100) %>%
            pluck("coverage") %>%
            mean(), 0.9)
  expect_lt(metasim(trials = 100) %>%
              pluck("coverage") %>%
              mean(), 1.0000001)

  # # check simualation id is parsed
  expect_equal(metasim(id = "sim_4") %>% pluck("id") %>% unique(), "sim_4")
})
