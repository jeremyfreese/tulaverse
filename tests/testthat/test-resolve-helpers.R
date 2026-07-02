# Assertions on the option-resolution helpers in R/tula.R
# (.resolve_level, .resolve_width). Internal functions; testthat runs tests
# inside the package namespace, so they are reachable by bare name.

test_that(".resolve_level accepts both percentage and proportion scales", {
  expect_equal(.resolve_level(95), 95)
  expect_equal(.resolve_level(0.95), 95)
  expect_equal(.resolve_level(90), 90)
  expect_equal(.resolve_level(0.99), 99)
})

test_that(".resolve_level rejects out-of-range and malformed input", {
  expect_error(.resolve_level(30), "between 50 and 99.9")
  expect_error(.resolve_level(100), "between 50 and 99.9")
  expect_error(.resolve_level("95"), "single numeric")
  expect_error(.resolve_level(c(90, 95)), "single numeric")
  expect_error(.resolve_level(NA_real_), "single numeric")
})

test_that(".resolve_width floors sub-60 widths to 60 with a warning", {
  expect_warning(w <- .resolve_width(40), "cannot be less than 60")
  expect_equal(w, 60L)
})

test_that(".resolve_width leaves valid and infinite widths untouched", {
  expect_equal(.resolve_width(100), 100)
  expect_equal(.resolve_width(Inf), Inf)
})
