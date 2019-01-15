context("simulations")

library(metasim)
library(tidyverse)

set.seed(38)

# testing parameters ------------------------------------------------------

# k studies

# row level
k <- sample(c(3, 7, 50), 1)
trials <- 10
n <- sample(seq(20, 200), 1)
spread <- runif(1, 0.5, 1.5)
centre <- runif(1, 20, 70)
prop <- runif(1, 0.1, 0.9)
between_study_variation <- runif(1, 0.2, 1.5)
within_study_variation <- runif(1, 0.2, 1.5)
median_ratio <- runif(1, 0.5, 1.5)
rdist <- "norm"
parameters <- list(mean = 50, sd = 0.2)

metadata <- metastats(meta_n(k = k)) %>%
  mutate(
    effect = map_dbl(summary_stats, "mean"),
    effect_se = map_dbl(summary_stats, "var") / sqrt(n)
  )

simdf <- metadata %>%
  metafor::rma(yi = effect, sei = effect_se, data = .)

# simulations -------------------------------------------------------------

test_that("simulation on one row works", {
  expect_is(metatrial(), "data.frame")
  expect_equal(nrow(metatrial()), 2)
  expect_is(metatrial(true_effect = 5), "data.frame")
  # expect_is(metatrial(sampling_fn = actuar::rpareto2, fn_parameters = list(shape = 2, scale = 2)), "data.frame")

  # test simulation
  expect_is(metasim(), "data.frame")
  expect_gt(metasim() %>% length(), 2)
  expect_is(metasim(median_ratio = 1), "data.frame")
  expect_is(metasim(median_ratio = median_ratio), "data.frame")
  expect_is(metasim(between_study_variation = between_study_variation),
            "data.frame")
  expect_is(metasim(within_study_variation = within_study_variation),
            "data.frame")

  # expect_is(purrr::pmap(
  #   list(
  #     between_study_variation = between_study_variation,
  #     within_study_variation = within_study_variation,
  #     median_ratio = median_ratio,
  #     rdist = rdist,
  #     n_df = n,
  #     parameters = parameters,
  #     true_effect = 50,
  #     id = id
  #   ),
  #   metasim,
  #   trial_fn = metatrial,
  #   trials = trials
  # ),
  # "data.frame")

  # test trial returns
  expect_is(trial_returns(simdf, true_effect = 1), "data.frame")
  expect_is(trial_returns(simdf, true_effect = 1) %>% mutate(test = "a"),
            "data.frame")


})



test_that("metasimulation runs as I think it does", {
  expect_is(metasims(), "data.frame")
  # expect_true(nrow(metasims()) > 0)
  # expect_true("k" %in% colnames(metasims()))
  # expect_true("sim" %in% colnames(metasims()))
  # expect_true("median_ratio" %in% colnames(metasims()))
})

# test inputs work --------------------------------------------------------

test_that("different inputs parse in meta_n", {
  # sanity check
  expect_is(meta_n(), "data.frame")
  # check that we can specify the number of studies
  expect_true(nrow(meta_n(k = 5)) == 10)
  expect_true(nrow(meta_n(k = k)) == 2 * k)

  # check what the column names are
  # there are probably better tests for this
  expect_true(sum(colnames(meta_n(k = k)) == c("study", "group", "n")) == 3)
  expect_true(sum(unique(meta_n(k = k)$group) == c("control", "intervention")) == 2)
  expect_true(length(unique(meta_n(k = k)$study)) == k)
  # how to test if a vector is a vector of integers?
})

test_that("simulation parameter set up", {
  expect_is(meta_df(), "data.frame")
  expect_is(meta_df() %>% pluck("rdist"), "character")
  expect_is(meta_df() %>% pluck("n"), "list")
  expect_true("median_ratio" %in% colnames(meta_df()))
  expect_true("true_median" %in% colnames(meta_df()))
  expect_is(meta_df(prop = 0.4), "data.frame")
  expect_is(meta_df(prop = prop), "data.frame")

  # check the sample size dataset has control and intervention rows for k studies
  expect_is(meta_df() %>% pluck("n") %>% map_int(nrow) / 2, "numeric")
  expect_equal(meta_df() %>% pluck("n") %>% map_int(nrow) / 2,
               meta_df() %>% pluck("k"))
  expect_gt(meta_df() %>% nrow(), 2)

  expect_is(meta_df(between_study_variation = 0.3), "data.frame")

  expect_is(
    meta_df(
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
})


test_that("effect_se works for various inputs", {
  expect_is(effect_se(
    centre = centre,
    spread = spread,
    n = n
  ), "numeric")
  expect_true(length(effect_se(
    centre = centre,
    spread = spread,
    n = n
  )) == 1)
  expect_true(effect_se(
    centre = centre,
    spread = spread,
    n = n
  ) > 0)

})

# trials ------------------------------------------------------------------

test_that("meta-analyses run", {
  # check the sample dataset is what I think it is
  expect_is(metadata, 'data.frame')
  expect_true(sum(colnames(metadata) == "effect") == 1)

  expect_gt(
    metadata %>% filter(group == "control") %>%  metafor::rma(data = ., yi = effect, sei = effect_se) %>% length(),
    20
  )

  # check returns
  expect_is(
    metadata %>% filter(group == "control") %>%  metafor::rma(data = ., yi = effect, sei = effect_se) %>% trial_returns(true_effect = 1),
    "data.frame"
  )
  expect_is(
    metadata %>% filter(group == "control") %>% metafor::rma(data = ., yi = effect, sei = effect_se) %>% trial_returns(true_effect = 1),
    "data.frame"
  )
  expect_gt(
    metadata %>% filter(group == "control") %>% metafor::rma(data = ., yi = effect, sei = effect_se) %>% trial_returns(true_effect = 1) %>% length(),
    3
  )
  expect_is(
    metadata %>% filter(group == "control") %>% metafor::rma(data = ., yi = effect, sei = effect_se) %>% trial_returns(true_effect = 1) %>% length(),
    "integer"
  )

  expect_true(sum("effect_se" %in% colnames(metadata)) ==  1)
  expect_true(sum("effect" %in% colnames(metadata)) == 1)

  expect_gt(length(trial_m(metadata, true_effect = 1)), 1)

  # m tests

  expect_is(trial_m(metadata, true_effect = 1), "data.frame")
  expect_gt(metadata %>% trial_m(true_effect = 1) %>% length(), 3)
  expect_true("ci_lb" %in% names(trial_m(metadata, true_effect = 1)))
  expect_true("ci_ub" %in% names(trial_m(metadata, true_effect = 1)))
  expect_true("in_ci" %in% names(trial_m(metadata, true_effect = 1)))

  # md tests

  expect_is(trial_md(metadata, true_effect = 1), "data.frame")
  expect_is(metadata %>% group_by(study) %>% summarise(md = diff(effect)),
            "data.frame")
  expect_is(metadata %>% group_by(study) %>% summarise(md = diff(effect), se = sqrt(sum(effect_se)) /
                                                         2),
            "data.frame")
  expect_gt(metadata %>% trial_md(true_effect = 1) %>% length(), 3)

  # expect_is(trial_lr(metadata), "list")
})
