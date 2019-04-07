context("sim_n")

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
