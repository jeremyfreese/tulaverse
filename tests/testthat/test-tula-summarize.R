# Snapshot tests for the summarize path — tula() dispatched on a data frame or
# an atomic vector produces a Stata -summarize- style descriptive table.

test_that("data-frame summarize output is stable", {
  local_reproducible_output(width = 80)
  expect_snapshot(tula(mtcars))
})

test_that("summarize with median = TRUE (median/IQR) is stable", {
  local_reproducible_output(width = 80)
  expect_snapshot(tula(mtcars, median = TRUE))
})

test_that("summarize with mad = TRUE is stable", {
  local_reproducible_output(width = 80)
  expect_snapshot(tula(mtcars, mad = TRUE))
})

test_that("single-vector summarize output is stable", {
  local_reproducible_output(width = 80)
  expect_snapshot(tula(mtcars$mpg))
})

test_that("summarize of a subset with a factor column is stable", {
  local_reproducible_output(width = 80)
  d <- data.frame(
    mpg = mtcars$mpg,
    cyl = factor(mtcars$cyl),
    am  = as.logical(mtcars$am)
  )
  expect_snapshot(tula(d))
})

test_that("haven-labelled summarize output is stable", {
  skip_if_not_installed("haven")
  local_reproducible_output(width = 80)
  d <- data.frame(
    y = haven::labelled(mtcars$am, labels = c(auto = 0, manual = 1)),
    x = mtcars$mpg
  )
  expect_snapshot(tula(d))
})
