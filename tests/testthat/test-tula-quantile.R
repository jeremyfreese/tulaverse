# Snapshot tests for tula.rq() / tula.rqs() — quantile regression via
# quantreg::rq(). A single tau produces a standard coefficient table; a vector
# of taus produces the multi-quantile (rqs) output, including parallel mode.
# summary.rq() can warn ("non-positive fis") on small data, so the snapshotted
# call is wrapped to keep the recording deterministic.

test_that("single-quantile (median) regression output is stable", {
  skip_if_not_installed("quantreg")
  local_reproducible_output(width = 80)
  m <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = 0.5)
  expect_snapshot(suppressWarnings(print(tula(m))))
})

test_that("multi-quantile (rqs) stacked output is stable", {
  skip_if_not_installed("quantreg")
  local_reproducible_output(width = 80)
  m <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = c(0.25, 0.5, 0.75))
  expect_snapshot(suppressWarnings(print(tula(m))))
})

test_that("multi-quantile (rqs) parallel output is stable", {
  skip_if_not_installed("quantreg")
  local_reproducible_output(width = 100)
  m <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = c(0.25, 0.5, 0.75))
  expect_snapshot(suppressWarnings(print(tula(m, parallel = TRUE))))
})
