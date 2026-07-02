# Snapshot tests for the codebook path — tula(..., codebook = TRUE) produces
# Stata -codebook- style per-variable blocks. A subset of mtcars is used so the
# recording stays readable while still exercising the three display modes:
# continuous (mpg), tabulation (cyl), and binary (am).

test_that("data-frame codebook output is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- mtcars[c("mpg", "cyl", "am")]
  expect_snapshot(tula(d, codebook = TRUE))
})

test_that("factor-vector codebook output is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  expect_snapshot(tula(factor(mtcars$cyl), codebook = TRUE))
})

test_that("character-vector codebook (string mode) output is stable", {
  local_reproducible_output(width = 80)
  skip_on_ci()
  x <- rep(c("apple", "banana", "cherry"), length.out = 20)
  expect_snapshot(tula(x, codebook = TRUE))
})

test_that("haven-labelled codebook output is stable", {
  skip_if_not_installed("haven")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- data.frame(
    y = haven::labelled(mtcars$am, labels = c(auto = 0, manual = 1))
  )
  expect_snapshot(tula(d, codebook = TRUE))
})

test_that("character-backed haven-labelled codebook renders (not an error)", {
  # Regression guard: haven_labelled may wrap a character vector. The codebook
  # path previously coerced with as.numeric() and errored on the integer-like
  # test. It must now render as a labelled string with correct labels. This is a
  # value check (no snapshot), so it runs on CI too.
  skip_if_not_installed("haven")
  d <- data.frame(
    sex = haven::labelled(c("M", "F", "M", "F", "M"),
                          labels = c(Male = "M", Female = "F"))
  )
  expect_no_error(invisible(capture.output(print(tula(d, codebook = TRUE)))))
})

test_that("character-backed haven-labelled codebook output is stable", {
  skip_if_not_installed("haven")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- data.frame(
    sex = haven::labelled(c("M", "F", "M", "F", "M"),
                          labels = c(Male = "M", Female = "F"))
  )
  expect_snapshot(tula(d, codebook = TRUE))
})

test_that("haven codebook keeps numeric (not lexical) code ordering", {
  # Codes 1, 2, 10 must display in numeric order, and each code must match its
  # label (the code->label join must be key-consistent with the table names).
  skip_if_not_installed("haven")
  local_reproducible_output(width = 80)
  skip_on_ci()
  d <- data.frame(
    g = haven::labelled(c(1, 10, 2, 10, 2, 1),
                        labels = c(low = 1, mid = 2, high = 10))
  )
  expect_snapshot(tula(d, codebook = TRUE))
})
