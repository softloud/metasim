context("sim_n")

k <- sample(seq(3, 100, by = 3), 1)


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
})

test_that("proportion and error", {
  # test proportion
  expect_lt(sim_n(wide = TRUE) %>% mutate(prop = intervention / (intervention + control)) %>% pluck("prop") %>% mean(),
            0.6)
  expect_gt(sim_n(wide = TRUE) %>% mutate(prop = intervention / (intervention + control)) %>% pluck("prop") %>% mean(),
            0.4)
  expect_lt(
    sim_n(
      wide = TRUE,
      prop = 0.2,
      prop_error = 0.2
    ) %>% mutate(prop = intervention / (intervention + control)) %>% pluck("prop") %>% mean(),
    0.4
  )
  expect_gt(
    sim_n(
      wide = TRUE,
      prop = 0.2,
      prop_error = 0.2
    ) %>% mutate(prop = intervention / (intervention + control)) %>% pluck("prop") %>% mean(),
    0.01
  )


})
