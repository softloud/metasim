context("simulations")

library(metasim)
library(tidyverse)

# for reproducibility
# set.seed(38)

# testing parameters ------------------------------------------------------

# k studies

# row level
big <- runif(2, 1, 101)
small <- runif(2, 0, 1)

k <- sample(c(3, 7, 50), 1)
trials <- 10
n <- sample(seq(20, 200), 1)
spread <- runif(1, 0.5, 1.5)
centre <- runif(1, 20, 70)
prop <- runif(1, 0.1, 0.9)
between_study_variation <- runif(1, 0.2, 1.5)
tau <- runif(1, 0.1, 0.8)
within_study_variation <- runif(1, 0.2, 1.5)
median_ratio <- runif(1, 0.5, 1.5)
rdist <- "norm"
parameters <- list(mean = 50, sd = 0.2)

testdf <- sim_df(
  dist_tribble =
    tibble::tribble(~ dist,  ~ par,
                    "norm", list(mean = 50, sd = 0.2),
                    "exp", list(rate = 2)),
  k = c(3, 7, 50),
  between_study_variation = seq(0, 0.4, 0.2),
  within_study_variation = seq(0, 0.4, 0.2),
  median_ratio = c(1, 1.2),
  prop = 0.3
)

# initialise simulations --------------------------------------------------

test_that("different inputs parse in sim_n", {
  # sanity check
  expect_is(sim_n(), "data.frame")
  # check that we can specify the number of studies
  expect_true(nrow(sim_n(k = 5)) == 10)
  expect_true(nrow(sim_n(k = k)) == 2 * k)

  # check what the column names are
  # there are probably better tests for this
  expect_true(sum(colnames(sim_n(k = k)) == c("study", "group", "n")) == 3)
  expect_true(sum(unique(sim_n(k = k)$group) == c("control", "intervention")) == 2)
  expect_true(length(unique(sim_n(k = k)$study)) == k)

  # check the table is non-empty
  expect_gt(nrow(testdf), 0)
})

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
      within_study_variation = within_study_variation,
      median_ratio = median_ratio,
      prop = prop
    ),
    "data.frame"
  )

  # check that simulation id is unique
  expect_equal(testdf %>% pluck("id") %>% unique() %>% length(),
               testdf %>% nrow())
})



# test sampling -----------------------------------------------------------

test_that("sim stats gives non-empty dataframe",{
  expect_is(sim_stats(), "data.frame")
  expect_gt(sim_stats() %>% nrow(), 2)

  # test distributions

  # norm
  expect_is(sim_stats(rdist = "norm", par = list(mean = 57, sd = 0.2)), "data.frame")
  expect_gt(sim_stats(rdist = "norm", par = list(mean = 57, sd = 0.2)) %>% nrow(), 2)
 expect_is(sim_stats(rdist = "norm", par = list(mean = big[[1]], sd = small[[1]])), "data.frame")
  expect_gt(sim_stats(rdist = "norm", par = list(mean = big[[1]], sd = small[[1]])) %>% nrow(), 2)

  # lnorm
  expect_is(sim_stats(rdist = "lnorm", par = list(mean = 57, sd = 0.2)), "data.frame")
  expect_gt(sim_stats(rdist = "lnorm", par = list(mean = 57, sd = 0.2)) %>% nrow(), 2)
  expect_is(sim_stats(rdist = "lnorm", par = list(mean = big[[1]], sd = small[[1]])), "data.frame")
  expect_gt(sim_stats(rdist = "lnorm", par = list(mean = big[[1]], sd = small[[1]])) %>% nrow(), 2)

  # exp
  expect_is(sim_stats(rdist = "exp", par = list(rate = 3)), "data.frame")
  expect_gt(sim_stats(rdist = "exp", par = list(rate = 3)) %>% nrow(), 2)
  expect_is(sim_stats(rdist = "exp", par = list(rate = round(big[[1]]))), "data.frame")
  expect_gt(sim_stats(rdist = "exp", par = list(rate = round(big[[1]]))) %>% nrow(), 2)

  # pareto
  expect_is(sim_stats(rdist = "pareto", par = list(shape = 3, scale = 2)), "data.frame")
  expect_gt(sim_stats(rdist = "pareto", par = list(shape = 3, scale = 2)) %>% nrow(), 2)


})


# simulations -------------------------------------------------------------

test_sample_norm <-
  sim_sample(10, 0, "norm", list(mean = 20, sd = 1))
test_sample_norm_another <-
  sim_sample(10, 0, "norm", list(mean = 104, sd = 0.3))
test_sample_pareto <- sim_sample(10, 0, "pareto", list(1, 2))

test_that("samples are plausible", {
  expect_is(test_sample_norm, "numeric")
  expect_lt(test_sample_norm %>% mean(), 50)
  expect_gt(test_sample_norm %>% mean(), 5)
  expect_lt(test_sample_norm %>% mean(), 22)
  expect_gt(test_sample_norm %>% mean(), 18)

  expect_is(test_sample_norm_another, "numeric")
  expect_lt(test_sample_norm_another %>% mean(), 200)
  expect_gt(test_sample_norm_another %>% mean(), 5)
  expect_lt(test_sample_norm_another %>% mean(), 106)
  expect_gt(test_sample_norm_another %>% mean(), 102)

  expect_is(test_sample_pareto, "numeric")
  expect_lt(test_sample_pareto %>% mean(), 100)
  expect_gt(test_sample_pareto %>% mean(), 0)

  # test the exponetial
  expect_is(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)),
            "numeric")
  expect_is(sim_sample(rdist = "exp", par = list(rate = 2)), "numeric")
  expect_is(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)),
            "numeric")

  expect_gt(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)) %>% length, 2)
  expect_gt(sim_sample(rdist = "exp", par = list(rate = 2)) %>% length, 2)
  expect_gt(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)) %>% length,
            2)
  expect_gt(sim_sample(rdist = "norm", par = list(mean = 437, sd = 0.7)) %>%
              unique %>% length, 2)
  expect_gt(sim_sample(rdist = "exp", par = list(rate = 2)) %>%
              unique %>% length, 2)
  expect_gt(sim_sample(rdist = "lnorm", par = list(meanlog = 49, sd = 0.2)) %>%
              unique %>% length, 2)

  expect_is(sim_sample(rdist = "lnorm",
                       par = list(meanlog = big[[1]], sd = small[1])),
            "numeric")

  expect_gt(sim_sample(rdist = "lnorm",
                       par = list(meanlog = big[[1]], sd = small[1])) %>%
              length, 2)
})

test_that("one trial for one simulation", {
  expect_is(metatrial(), "data.frame")
  expect_equal(nrow(metatrial()), 3)
  expect_is(metatrial(true_effect = 5), "data.frame")
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

test_that("simulation can handle more trials", {
  expect_is(metasim(), "data.frame")
  expect_gt(metasim() %>% nrow(), 2)
  expect_is(metasim(trials = 10), "data.frame")
  expect_gt(metasim(trials = 10) %>% nrow(), 2)
  expect_is(metasim(trials = 100), "data.frame")
  expect_gt(metasim(trials = 100) %>% nrow(), 2)
  # expect_is(metasim(trials = 1000), "data.frame")
  # expect_gt(metasim(trials = 1000) %>% nrow(), 2)
})

test_that("simulation runs over other inputs", {
  # test simulation
  # expect_is(metasim(rdist = "norm",
  #                   par = list(mean = 67, sd = 0.25)), "data.frame")
  # expect_is(metasim(rdist = "pareto",
  #                   par = list(shape = 2, scale = 3)), "data.frame")
  # expect_is(metasim(rdist = "lnorm",
  #                   par = list(meanlog = 67, sdlog = 0.25)), "data.frame")
  # expect_is(metasim(rdist = "exp", par = list(rate = 3)), "data.frame")
  # expect_is(metasim(median_ratio = 1), "data.frame")
  # expect_is(metasim(median_ratio = 1.3), "data.frame")
  # expect_is(metasim(median_ratio = median_ratio), "data.frame")

  # expect_is(metasim(tau = 0), "data.frame")
  # expect_is(metasim(tau = tau), "data.frame")

  # check that coverage probability is above 0.9.
  # expect_gt(metasim(trials = 100) %>%
  #           pluck("cp") %>%
  #           mean(), 0.9)
  # expect_lt(metasim(trials = 100) %>%
  #             pluck("cp") %>%
  #             mean(), 1.0000001)

  # all simulations

  # check simualation id is parsed

  # expect_equal(metasim(id = "sim_4") %>% pluck("id") %>% unique(), "sim_4")

  # expect_is(metasims(), "data.frame")
  # expect_true(nrow(metasims()) > 0)
  # expect_true("k" %in% colnames(metasims()))
  # expect_true("sim" %in% colnames(metasims()))
  # expect_true("median_ratio" %in% colnames(metasims()))
})
