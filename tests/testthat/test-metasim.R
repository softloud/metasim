context("metasim")

median_ratio <- runif(1, 0.8, 1.2)
tau <- runif(1, 0.2, 0.8)

test_that("simulation runs over other inputs", {
  # test defaults work
  expect_is(metasim() %>% pluck("results"), "data.frame")
  expect_gt(metasim() %>% pluck("results") %>% nrow(), 1)

  # todo: problems

  expect_error(metasim(
    rdist = "pareto",
    par = list(shape = 2, scale = 3) %>% pluck("results")
  ))
  expect_error(metasim(
    rdist = "lnorm",
    par = list(meanlog = 67, sdlog = 0.25) %>%
      pluck("results")))

  # test simulation

  expect_is(metasim(rdist = "norm",
                    par = list(mean = 67, sd = 0.25)), "list")
  expect_is(metasim(rdist = "exp", par = list(rate = 3)) %>%
              pluck("results"),
            "data.frame")
  # test equal rataios
  expect_is(metasim(median_ratio = 1) %>% pluck("results"),
            "data.frame")
  expect_is(metasim(median_ratio = 1.3) %>% pluck("results"),
            "data.frame")
  expect_is(metasim(median_ratio = median_ratio) %>% pluck("results"),
            "data.frame")
  # test tau values
  expect_is(metasim(tau = 0) %>% pluck("results"), "data.frame")
  expect_is(metasim(tau = tau) %>% pluck("results"), "data.frame")

  # # check that coverage probability is above 0.9.
  # expect_gt(metasim(trials = 100) %>%
  #           pluck("cp") %>%
  #           mean(), 0.9)
  # expect_lt(metasim(trials = 100) %>%
  #             pluck("cp") %>%
  #             mean(), 1.0000001)
  #
  # # check simualation id is parsed
  #
  # expect_equal(metasim(id = "sim_4") %>% pluck("id") %>% unique(), "sim_4")
})
