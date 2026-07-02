# Snapshot tests for tula.glm() — logistic, Poisson, and exp = TRUE output.
# glm is in base R (via stats), so these never skip.

test_that("logistic regression output is stable", {
  local_reproducible_output(width = 80)
  m <- glm(am ~ cyl + wt, data = mtcars, family = binomial)
  expect_snapshot(tula(m))
})

test_that("logistic regression with a grouped factor is stable", {
  local_reproducible_output(width = 80)
  m <- glm(am ~ factor(cyl) + wt, data = mtcars, family = binomial)
  expect_snapshot(tula(m))
})

test_that("logistic regression with exp = TRUE (odds ratios) is stable", {
  local_reproducible_output(width = 90)
  m <- glm(am ~ cyl + wt, data = mtcars, family = binomial)
  expect_snapshot(tula(m, exp = TRUE))
})

test_that("Poisson regression output is stable", {
  local_reproducible_output(width = 80)
  m <- glm(carb ~ wt + hp, data = mtcars, family = poisson)
  expect_snapshot(tula(m))
})
