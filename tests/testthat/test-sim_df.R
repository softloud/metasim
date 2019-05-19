context("sim_df")

testdf <- sim_df()
prop <- runif(1, 0.2, 0.7)
k <- sample(seq(3, 100, by = 3), 1)
effect_ratio <- runif(1, 0.8, 1.2)

test_that("from user inputs, generate a simulation overview dataframe", {
  expect_is(testdf, "data.frame")
  expect_gt(nrow(testdf), 0)
  expect_is(sim_df(), "data.frame")
  expect_is(testdf %>% pluck("rdist"), "character")
  expect_is(testdf %>% pluck("n"), "list")
  expect_true("effect_ratio" %in% colnames(testdf))
  expect_true("true_effect" %in% colnames(testdf))
  expect_is(sim_df(prop = 0.4), "data.frame")
  expect_is(sim_df(prop = prop), "data.frame")

  # check the sample size dataset has control and intervention rows for k studies
  expect_is(testdf %>% pluck("n") %>% purrr::map_int(nrow) / 2, "numeric")
  expect_equal(
    testdf %>% pluck("n") %>% purrr::map_int(nrow) / 2,
    testdf %>% pluck("k") %>% as.integer()
  )
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
      effect_ratio = effect_ratio,
      prop = prop
    ),
    "data.frame"
  )

  # check that simulation id is unique
  expect_equal(testdf %>% pluck("id") %>% unique() %>% length(),
               testdf %>% nrow())
})

sim_df_test  <- function(...) {
  sim_df(...) %>%
    dplyr::select(id, n) %>%
    tidyr::unnest(n) %>%
    tidyr::spread(group, n) %>%
    dplyr::mutate(p = intervention / (intervention + control),
                  n = intervention + control)
}

test_that("sim_df produces correct sample sizes", {
  expect_lt(sim_df_test() %>% pluck("p") %>% mean(), 0.6)
  expect_gt(sim_df_test() %>% pluck("p") %>% mean(), 0.4)
  expect_lt(sim_df_test(prop = 0.2, prop_error = 0.01) %>% pluck("p") %>% mean(),
            0.25)
  expect_gt(sim_df_test(prop = 0.2, prop_error = 0.01) %>% pluck("p") %>% mean(),
            0.15)
  expect_gte(sim_df_test(min_n = 300, max_n = 350) %>% pluck("n") %>% min(),
             300)
  expect_lte(sim_df_test(min_n = 300, max_n = 350) %>% pluck("n") %>% max(),
             350)
})

metasims_test  <- function(...) {
  metasims(...) %>%
    dplyr::filter(measure == "median") %>%
    dplyr::select(id, n) %>%
    tidyr::unnest(n) %>%
    tidyr::spread(group, n) %>%
    dplyr::mutate(p = intervention / (intervention + control),
                  n = intervention + control)
}

test_that("metasims parses sample size arguments", {
  expect_lt(metasims_test(progress = FALSE) %>% pluck("p") %>% mean(), 0.6)
  expect_gt(metasims_test(progress = FALSE) %>% pluck("p") %>% mean(), 0.4)
  expect_lt(
    metasims_test(
      prop = 0.2,
      prop_error = 0.01,
      progress = FALSE
    ) %>% pluck("p") %>% mean(),
    0.25
  )
  expect_gt(
    metasims_test(
      prop = 0.2,
      prop_error = 0.01,
      progress = FALSE
    ) %>% pluck("p") %>% mean(),
    0.15
  )
  expect_gte(metasims_test(
    min_n = 300,
    max_n = 350,
    progress = FALSE
  ) %>% pluck("n") %>% min(),
  300)
  expect_lte(metasims_test(
    min_n = 300,
    max_n = 350,
    progress = FALSE
  ) %>% pluck("n") %>% max(),
  350)
})
