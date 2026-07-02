# Snapshot test for tula.restriktor() — constrained estimation via
# restriktor::restriktor() applied to a fitted lm.

test_that("constrained linear model (restriktor) output is stable", {
  skip_if_not_installed("restriktor")
  local_reproducible_output(width = 80)
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  m   <- restriktor::restriktor(fit, constraints = "hp < 0")
  expect_snapshot(tula(m))
})
