# Snapshot tests for the rendered console output of tula() on lm models.
#
# The user-facing product is the exact formatted table — alignment, the
# vertical separators, the header block. Snapshot tests record that text on
# first run (to tests/testthat/_snaps/tula-lm.md) and diff against it on every
# later run, so any formatting drift across the whole pipeline shows up as a
# reviewable change. Bless intentional changes with testthat::snapshot_accept().
#
# local_reproducible_output() pins console width to 80 and disables colour so
# snapshots don't depend on the machine running them.

test_that("lm output is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  m <- lm(mpg ~ wt + hp, data = mtcars)
  expect_snapshot(tula(m))
})

test_that("lm output with a grouped factor is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  expect_snapshot(tula(m))
})

test_that("lm output honours wide = TRUE (confidence interval columns)", {
  local_reproducible_output(width = 100)
  skip_on_ci()
  m <- lm(mpg ~ wt + hp, data = mtcars)
  expect_snapshot(tula(m, wide = TRUE))
})
