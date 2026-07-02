# Snapshot tests for the ordered-regression methods:
# tula.polr() (MASS::polr) and tula.clm() (ordinal::clm). Both render an
# ordered-outcome coefficient table plus cutpoint rows in the lower section.

test_that("proportional-odds logistic (polr) output is stable", {
  skip_if_not_installed("MASS")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- mtcars
  d$gear_ord <- factor(d$gear, ordered = TRUE)
  m <- MASS::polr(gear_ord ~ mpg + wt, data = d, Hess = TRUE)
  expect_snapshot(tula(m))
})

test_that("polr with a grouped factor and cutpoints is stable", {
  skip_if_not_installed("MASS")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- mtcars
  d$gear_ord <- factor(d$gear, ordered = TRUE)
  m <- MASS::polr(gear_ord ~ mpg + factor(cyl), data = d, Hess = TRUE)
  expect_snapshot(tula(m))
})

test_that("cumulative link model (clm) output is stable", {
  skip_if_not_installed("ordinal")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- mtcars
  d$gear_ord <- factor(d$gear, ordered = TRUE)
  m <- ordinal::clm(gear_ord ~ mpg + wt, data = d)
  expect_snapshot(tula(m))
})
