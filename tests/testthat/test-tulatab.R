# Snapshot tests for tulatab() — one-way frequency tables and two-way
# crosstabs. Same rationale as test-tula-lm.R: pin the rendered text so
# column/percentage formatting can't drift unnoticed.

test_that("one-way frequency table is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tulatab(mtcars$cyl))
})

test_that("two-way crosstab is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tulatab(mtcars$cyl, mtcars$am))
})

test_that("crosstab with cell means is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tulatab(mtcars$cyl, mtcars$am, mean = mtcars$mpg))
})

test_that("one-way tabulation of a logical vector is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tulatab(as.logical(mtcars$am)))
})

test_that("crosstab with a logical dimension is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tulatab(mtcars$cyl, as.logical(mtcars$am)))
})
