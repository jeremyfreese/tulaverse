# Snapshot tests for tula.multinom() — multinomial logit via nnet::multinom().
# multinom() prints convergence trace to stderr, so it is fit with
# trace = FALSE; the fit itself is outside expect_snapshot().

test_that("multinomial logit stacked output is stable", {
  skip_if_not_installed("nnet")
  local_reproducible_output(width = 80)
  skip_on_ci()
  m <- nnet::multinom(factor(cyl) ~ wt + hp, data = mtcars, trace = FALSE)
  expect_snapshot(tula(m))
})

test_that("multinomial logit parallel output is stable", {
  skip_if_not_installed("nnet")
  local_reproducible_output(width = 100)
  skip_on_ci()
  m <- nnet::multinom(factor(cyl) ~ wt + hp, data = mtcars, trace = FALSE)
  expect_snapshot(tula(m, parallel = TRUE))
})
