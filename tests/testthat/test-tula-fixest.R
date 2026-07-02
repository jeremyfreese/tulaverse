# Snapshot tests for tula.fixest() — fixed-effects models via fixest::feols().
# Skips wherever fixest is unavailable (it is only in Suggests).

test_that("fixed-effects OLS (feols) output is stable", {
  skip_if_not_installed("fixest")
  local_reproducible_output(width = 80)
  skip_on_ci()
  m <- fixest::feols(mpg ~ wt + hp | cyl, data = mtcars)
  expect_snapshot(tula(m))
})

test_that("fixed-effects OLS without fixed effects is stable", {
  skip_if_not_installed("fixest")
  local_reproducible_output(width = 80)
  skip_on_ci()
  m <- fixest::feols(mpg ~ wt + hp, data = mtcars)
  expect_snapshot(tula(m))
})
