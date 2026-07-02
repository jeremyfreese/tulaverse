# Snapshot tests for the shared regression display options, exercised on lm/glm
# so the option behaviour is isolated from any one model type:
#   robust / vcov / cluster  — alternative standard errors
#   ref                       — show reference-level rows
#   select / selectheader / selectfooter — term subsetting

test_that("robust = TRUE (HC3) output is stable", {
  skip_if_not_installed("sandwich")
  local_reproducible_output(width = 80)
  m <- glm(am ~ cyl + wt, data = mtcars, family = binomial)
  expect_snapshot(tula(m, robust = TRUE))
})

test_that("robust with an explicit HC type (vcov = 'HC1') is stable", {
  skip_if_not_installed("sandwich")
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + hp, data = mtcars)
  expect_snapshot(tula(m, robust = TRUE, vcov = "HC1"))
})

test_that("cluster-robust SEs output is stable", {
  skip_if_not_installed("sandwich")
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + hp, data = mtcars)
  expect_snapshot(tula(m, cluster = "cyl"))
})

test_that("cluster variable aligns when the model drops rows for missingness", {
  # Regression guard: the cluster var (cyl) is not in the model frame, so it is
  # pulled from the raw data. With rows dropped for NA, the raw vector is longer
  # than the estimation sample; it must be subset via na.action, not misaligned.
  skip_if_not_installed("sandwich")
  d <- mtcars
  d$wt[c(3, 7, 15)] <- NA          # 3 rows dropped -> 29 used
  m <- lm(mpg ~ wt + hp, data = d)
  expect_equal(nobs(m), 29L)
  expect_no_error(invisible(capture.output(print(tula(m, cluster = "cyl")))))
})

test_that("ref = TRUE (reference-level rows) output is stable", {
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  expect_snapshot(tula(m, ref = TRUE))
})

test_that("select = a single continuous term is stable", {
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
  expect_snapshot(tula(m, select = "wt"))
})

test_that("select = a factor term pulls in header plus all levels", {
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
  expect_snapshot(tula(m, select = "cyl"))
})

test_that("select with selectheader = TRUE keeps the fit header", {
  local_reproducible_output(width = 80)
  m <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
  expect_snapshot(tula(m, select = "wt", selectheader = TRUE))
})
