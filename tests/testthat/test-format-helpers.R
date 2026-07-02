# Value assertions on the pure formatting helpers in R/format_helpers.R.
# These are internal (unexported) functions, so we reach them with `:::`.

test_that("fmt_num pads to exactly the requested width", {
  expect_equal(nchar(fmt_num(3.14159, width = 10)), 10)
  expect_equal(nchar(fmt_num(-0.0001234, width = 10)), 10)
  expect_equal(nchar(fmt_num(0, width = 8)), 8)
})

test_that("fmt_num blanks NA to a full-width space field", {
  expect_equal(fmt_num(NA_real_, width = 10), strrep(" ", 10))
})

test_that("fmt_num strips the leading zero from fractions (Stata style)", {
  expect_match(fmt_num(0.5, width = 8), "\\.5", fixed = FALSE)
  expect_false(grepl("0\\.5", fmt_num(0.5, width = 8)))
})

test_that("fmt_num falls back to scientific notation when fixed form overflows", {
  # A value too wide for fixed notation should still fit the column via "g" form.
  expect_equal(nchar(fmt_num(123456789012, width = 10)), 10)
})

test_that("fmt_pval formats small p-values as '<.0001' and strips leading zero", {
  expect_match(fmt_pval(0.00001), "<\\.0001")
  expect_match(fmt_pval(0.5), "\\.5000")
  expect_equal(fmt_pval(NA_real_, width = 8), strrep(" ", 8))
})

# fmt_num's `width` is a MINIMUM, not a hard cap: when the value's
# representation is wider than `width` it is returned intact (never truncated),
# because the parallel-mode formatters call trimws(fmt_num(x, width = 1L)) to
# measure a number's natural width. (The review initially flagged the
# over-width return as a bug; it is in fact load-bearing behaviour.)
test_that("fmt_num pads to width but never truncates the value", {
  # Standard callers pass width >= the value's natural width -> exact padding.
  expect_equal(nchar(fmt_num(3.14159, width = 10)), 10)
  # width = 1L idiom returns the natural-width number string.
  expect_equal(trimws(fmt_num(6.765, width = 1L)), "6.765")
  expect_equal(trimws(fmt_num(-87.6, width = 1L)), "-87.6")
  # A value wider than a too-small width is returned intact, not truncated.
  expect_equal(trimws(fmt_num(32.23, width = 3)), "32.23")
})
